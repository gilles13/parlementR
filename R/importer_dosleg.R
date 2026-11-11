# Portage du lecteur PostgreSQL DOSLEG d'assembleer.
# Les donnees sont importees en VARCHAR sans conversion implicite.
.dosleg_structure = function(con) {
  definitions = list()
  repeat {
    ligne = readLines(con, n = 1L, warn = FALSE)
    if (!length(ligne)) break
    if (!grepl("^CREATE TABLE (public\\.)?[A-Za-z_][A-Za-z_0-9]* \\($", ligne)) next
    nom = sub("^CREATE TABLE (public\\.)?([A-Za-z_][A-Za-z_0-9]*) \\($", "\\2", ligne)
    colonnes = character()
    repeat {
      ligne = readLines(con, n = 1L, warn = FALSE)
      if (!length(ligne)) stop("CREATE TABLE incomplet : ", nom)
      if (identical(ligne, ");")) break
      ligne = trimws(sub(",$", "", ligne))
      if (grepl("^(CONSTRAINT|PRIMARY|FOREIGN|UNIQUE|CHECK)\\b", ligne, ignore.case = TRUE)) next
      if (!grepl("^[A-Za-z_][A-Za-z_0-9]*[[:space:]]", ligne)) stop("Colonne inconnue : ", ligne)
      colonne = sub("^([A-Za-z_][A-Za-z_0-9]*)[[:space:]].*", "\\1", ligne)
      colonnes = c(colonnes, colonne)
    }
    if (!length(colonnes) || anyDuplicated(colonnes)) stop("Colonnes invalides : ", nom)
    definitions[[nom]] = colonnes
  }
  definitions
}

# Decode le format texte de COPY PostgreSQL : NULL = \\N, champs separes par TAB.
.dosleg_decoder = function(x) {
  if (identical(x, "\\N")) return(NA_character_)
  if (!grepl("\\", x, fixed = TRUE)) return(x)
  # Les sequences COPY sont converties avant insertion ; \\N n'est NULL
  # que lorsqu'il constitue le champ brut entier.
  morceaux = strsplit(x, "", fixed = TRUE)[[1L]]
  resultat = character(length(morceaux))
  j = 0L
  i = 1L
  while (i <= length(morceaux)) {
    c = morceaux[[i]]
    if (c == "\\" && i < length(morceaux)) {
      i = i + 1L
      c = morceaux[[i]]
      c = switch(c, "n" = "\n", "r" = "\r", "t" = "\t", "b" = "\b",
                 "f" = "\f", "v" = "\v", c)
    }
    j = j + 1L
    resultat[[j]] = c
    i = i + 1L
  }
  paste0(resultat[seq_len(j)], collapse = "")
}

.dosleg_ajouter = function(con, table, colonnes, lignes) {
  if (!length(lignes)) return(invisible(NULL))
  champs = strsplit(lignes, "\t", fixed = TRUE)
  # strsplit supprime parfois le dernier champ vide : le conserver.
  champs = lapply(seq_along(champs), function(i) {
    x = champs[[i]]
    if (endsWith(lignes[[i]], "\t")) x = c(x, "")
    x
  })
  if (any(lengths(champs) != length(colonnes)))
    stop("Nombre de champs COPY incorrect dans ", table)
  valeurs = lapply(seq_along(colonnes), function(j)
    vapply(champs, function(x) .dosleg_decoder(x[[j]]), character(1)))
  names(valeurs) = colonnes
  DBI::dbAppendTable(con, DBI::Id(schema = "sen_dosleg", table = table),
                     tibble::as_tibble(valeurs, .name_repair = "minimal"))
  invisible(NULL)
}


#' Importer DOSLEG depuis une archive officielle locale
#'
#' Importe les 53 tables du dump PostgreSQL dans `sen_dosleg`.
#' Aucune connexion reseau n'est effectuee par cette fonction.
#' @param db Connexion DuckDB ; NULL utilise `connexion()`.
#' @param archive Chemin de `dosleg.zip` deja telecharge.
#' @param remplacer Autoriser le remplacement d'un schema DOSLEG deja complet.
#' @param taille_lot Nombre de lignes inserees par lot.
#' @param verbose Afficher les effectifs par table.
#' @return Invisiblement, les effectifs par table.
#' @export
importer_dosleg = function(db = NULL, archive, remplacer = FALSE,
                           taille_lot = 2000L, verbose = TRUE) {
  if (missing(archive) || length(archive) != 1L || is.na(archive) ||
      !file.exists(archive)) stop("Archive DOSLEG locale introuvable")
  if (!is.logical(remplacer) || length(remplacer) != 1L || is.na(remplacer))
    stop("'remplacer' doit etre TRUE ou FALSE")
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("'verbose' doit etre TRUE ou FALSE")
  if (length(taille_lot) != 1L || is.na(taille_lot) ||
      !is.finite(taille_lot) || taille_lot < 1 || taille_lot != as.integer(taille_lot))
    stop("'taille_lot' doit etre un entier positif")
  taille_lot = as.integer(taille_lot)
  con = if (is.null(db)) connexion(creer = TRUE) else .connexion_valide(db)
  journal = function(...) if (isTRUE(verbose)) {
    message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [DOSLEG] ", ...)
  }
  if (!identical(as.integer(system2("unzip", c("-tq", shQuote(archive)),
                                    stdout = FALSE, stderr = FALSE)), 0L))
    stop("Archive DOSLEG invalide")
  contenu = utils::unzip(archive, list = TRUE)$Name
  if (!identical(contenu, "dosleg.sql")) stop("Archive inattendue : dosleg.sql requis")
  lecteur = unz(archive, "dosleg.sql", open = "r")
  definitions = .dosleg_structure(lecteur)
  close(lecteur)
  if (length(definitions) != 53L) stop("Nombre inattendu de tables : ", length(definitions))

  lecteur = unz(archive, "dosleg.sql", open = "r")
  on.exit(try(close(lecteur), silent = TRUE), add = TRUE)
  DBI::dbBegin(con)
  valide = FALSE
  on.exit(if (!valide) DBI::dbRollback(con), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS sen_dosleg")
  presentes = .db_get_tibble(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'sen_dosleg'")$table_name
  if (length(presentes) && (!identical(sort(presentes), sort(names(definitions))) || !isTRUE(remplacer)))
    stop("Schema sen_dosleg deja occupe : remplacement explicite requis")
  for (nom in presentes) DBI::dbExecute(con, paste0('DROP TABLE sen_dosleg.', as.character(DBI::dbQuoteIdentifier(con, nom))))
  for (nom in names(definitions)) {
    colonnes = definitions[[nom]]
    declaration = paste(sprintf('"%s" VARCHAR', colonnes), collapse = ", ")
    DBI::dbExecute(con, sprintf('CREATE TABLE sen_dosleg."%s" (%s)', nom, declaration))
  }
  effectifs = integer()
  repeat {
    ligne = readLines(lecteur, n = 1L, warn = FALSE)
    if (!length(ligne)) break
    if (!grepl("^COPY (public\\.)?[A-Za-z_][A-Za-z_0-9]* \\(", ligne)) next
    nom = sub("^COPY (public\\.)?([A-Za-z_][A-Za-z_0-9]*) \\(.*", "\\2", ligne)
    if (!nom %in% names(definitions) || nom %in% names(effectifs))
      stop("COPY inconnu ou duplique : ", nom)
    texte = sub("^COPY [^(]+\\(([^)]+)\\) FROM stdin;$", "\\1", ligne)
    colonnes = trimws(strsplit(texte, ",", fixed = TRUE)[[1L]])
    if (!identical(colonnes, definitions[[nom]])) stop("Colonnes COPY non conformes : ", nom)
    lot = character()
    nombre = 0L
    repeat {
      ligne = readLines(lecteur, n = 1L, warn = FALSE)
      if (!length(ligne)) stop("Fin de COPY manquante : ", nom)
      if (identical(ligne, "\\.")) break
      lot = c(lot, ligne)
      if (length(lot) >= taille_lot) {
        .dosleg_ajouter(con, nom, colonnes, lot)
        nombre = nombre + length(lot)
        lot = character()
      }
    }
    .dosleg_ajouter(con, nom, colonnes, lot)
    nombre = nombre + length(lot)
    effectifs[nom] = nombre
    journal(nom, " : ", nombre, " lignes")
  }
  close(lecteur)
  if (length(effectifs) != 53L) stop("COPY manquants : ", 53L - length(effectifs))
  DBI::dbCommit(con)
  valide = TRUE
  journal("Import termine : ", length(effectifs), " tables, ", sum(as.double(effectifs)), " lignes")
  invisible(tibble::tibble(table = names(effectifs), lignes = unname(effectifs)))
}
