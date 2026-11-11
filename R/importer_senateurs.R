#' Importer les senateurs depuis une archive officielle locale
#'
#' Importe les 95 tables du dump PostgreSQL du Senat dans le schema DuckDB
#' `senateurs`, sans serveur PostgreSQL et sans acces reseau. Les tables sont
#' remplacees dans une transaction ; en cas d'erreur, l'import est annule.
#' Pour telecharger ou actualiser la source, utiliser
#' `actualiser(jeux = "senateurs")`.
#'
#' @param db Connexion DuckDB ; NULL utilise `connexion()`.
#' @param archive Chemin du fichier `export_sens.zip` deja telecharge.
#' @param verbose Afficher l'extraction et la progression table par table.
#' @return Invisiblement, le nombre de tables et de lignes importees.
#' @export
importer_senateurs = function(db = NULL, archive, verbose = TRUE) {
  if (missing(archive) || length(archive) != 1L || is.na(archive) ||
      !file.exists(archive)) stop("Archive locale introuvable", call. = FALSE)
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("'verbose' doit etre TRUE ou FALSE", call. = FALSE)
  db = if (is.null(db)) connexion(creer = TRUE) else .connexion_valide(db)
  journal = function(...) if (isTRUE(verbose)) message("[SENATEURS] ", ...)
  journal("Verification du ZIP local")
  statut = system2("unzip", c("-tq", shQuote(archive)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L))
    stop("Archive ZIP invalide : ", archive, call. = FALSE)
  journal("Debut de la transaction DuckDB")
  DBI::dbBegin(db)
  resultat = tryCatch({
    compte = .importer_senateur(db, archive, journal)
    DBI::dbCommit(db)
    compte
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  journal("Import termine : ", resultat$tables, " tables, ",
          resultat$lignes, " lignes")
  invisible(resultat)
}

# Import du dump PostgreSQL officiel du Senat dans un schema DuckDB dedie.
.importer_senateur = function(con, archive, journal) {
  contenu = utils::unzip(archive, list = TRUE)$Name
  if (length(contenu) != 1L || !identical(basename(contenu), "export_sens.sql"))
    stop("Archive des senateurs inattendue : export_sens.sql requis.", call. = FALSE)
  dossier = tempfile("parlementR-senateurs-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  journal("Extraction du dump SQL : ", contenu)
  utils::unzip(archive, files = contenu, exdir = dossier)
  lignes = readLines(file.path(dossier, contenu), warn = FALSE, encoding = "UTF-8")
  debuts = grep("^COPY [a-z_][a-z_0-9]* \\(.*\\) FROM stdin;$", lignes)
  fins = which(lignes == "\\.")
  if (length(debuts) != 95L || length(fins) != length(debuts) || any(fins <= debuts))
    stop("Structure COPY du dump Senat inattendue.", call. = FALSE)

  definitions = paste(lignes[seq_len(debuts[[1L]] - 1L)], collapse = "\n")
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS senateurs")
  ident = function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  literal = function(x) as.character(DBI::dbQuoteString(con, x))
  total = 0
  for (i in seq_along(debuts)) {
    commande = lignes[[debuts[[i]]]]
    nom = sub("^COPY ([a-z_][a-z_0-9]*) .*", "\\1", commande)
    colonnes_copy = strsplit(sub("^COPY [a-z_][a-z_0-9]* \\((.*)\\) FROM stdin;$", "\\1", commande), ", ", fixed = TRUE)[[1L]]
    # Les noms et l'ordre des colonnes sont verifies contre CREATE TABLE.
    motif = paste0("(?s)CREATE TABLE ", nom, " \\(\\n(.*?)\\n\\);")
    trouve = regmatches(definitions, regexec(motif, definitions, perl = TRUE))[[1L]]
    if (length(trouve) != 2L) stop("Definition absente : ", nom)
    definitions_colonnes = strsplit(trouve[[2L]], "\n", fixed = TRUE)[[1L]]
    definitions_colonnes = trimws(sub(",$", "", definitions_colonnes))
    noms = sub("^([a-z_][a-z_0-9]*) .*", "\\1", definitions_colonnes)
    if (!identical(noms, colonnes_copy)) stop("Colonnes incompatibles : ", nom)
    types = sub("^[a-z_][a-z_0-9]* +", "", definitions_colonnes)
    types = sub(" +DEFAULT .*", "", types)
    types = sub(" +NOT NULL$", "", types)
    types = sub("^timestamp without time zone$", "TIMESTAMP", types)
    types = sub("^character varying\\([0-9]+\\)$", "VARCHAR", types)
    types = sub("^character\\([0-9]+\\)$", "VARCHAR", types)
    if (any(!types %in% c("TIMESTAMP", "VARCHAR", "bigint", "smallint")))
      stop("Type PostgreSQL non pris en charge dans ", nom, " : ", paste(unique(types), collapse = ", "))
    cible = paste0("senateurs.", ident(nom))
    schema = paste(paste(ident(noms), types), collapse = ", ")
    DBI::dbExecute(con, paste0("CREATE OR REPLACE TABLE ", cible, " (", schema, ")"))
    fichier = file.path(dossier, paste0(nom, ".tsv"))
    donnees = if (fins[[i]] == debuts[[i]] + 1L) character() else lignes[seq.int(debuts[[i]] + 1L, fins[[i]] - 1L)]
    writeLines(donnees, fichier, useBytes = TRUE)
    if (length(donnees)) {
      DBI::dbExecute(con, paste0("COPY ", cible, " FROM ", literal(fichier),
        " (FORMAT CSV, DELIMITER ", literal("\t"), ", HEADER FALSE, NULLSTR ",
        literal("\\N"), ", QUOTE '', ESCAPE '')"))
      # PostgreSQL COPY echappe les retours a la ligne, tabulations et antislashs.
      if (any(grepl("\\", donnees, fixed = TRUE))) {
        textes = noms[types == "VARCHAR"]
        decoder = function(col) {
          x = ident(col)
          x = paste0("replace(", x, ", ", literal("\\\\"), ", chr(1))")
          for (pair in list(c("\\n", "\n"), c("\\r", "\r"), c("\\t", "\t")))
            x = paste0("replace(", x, ", ", literal(pair[[1L]]), ", ", literal(pair[[2L]]), ")")
          paste0("replace(", x, ", chr(1), ", literal("\\"), ")")
        }
        if (length(textes)) {
          modifications = paste(paste0(ident(textes), " = ", vapply(textes, decoder, character(1))), collapse = ", ")
          condition = paste(paste0("strpos(", ident(textes), ", ", literal("\\"), ") > 0"), collapse = " OR ")
          DBI::dbExecute(con, paste0("UPDATE ", cible, " SET ", modifications, " WHERE ", condition))
        }
      }
    }
    total = total + length(donnees)
    journal("Table ", i, "/", length(debuts), " : ", nom, " (", length(donnees), " lignes)")
    unlink(fichier)
  }
  list(tables = length(debuts), lignes = total)
}
