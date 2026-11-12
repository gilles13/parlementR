# Import du dump PostgreSQL des questions du Senat, entierement local.
.questions_ajouter = function(db, table, colonnes, lignes) {
  if (!length(lignes)) return(invisible(NULL))
  champs = strsplit(lignes, "\t", fixed = TRUE)
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
  DBI::dbAppendTable(db, DBI::Id(schema = "sen_questions", table = table),
                     tibble::as_tibble(valeurs))
  invisible(NULL)
}

#' Importer les questions du Senat depuis une archive officielle locale
#'
#' Lit `questions.sql` directement dans `questions.zip`, sans PostgreSQL
#' ni connexion reseau. Les huit tables sont conservees en texte dans DuckDB.
#' @param archive Chemin de `questions.zip`.
#' @param db Connexion DuckDB ; NULL utilise la connexion geree.
#' @param remplacer Remplacer un schema deja importe.
#' @param verbose Afficher les effectifs et horodatages.
#' @return Invisiblement, un tibble des effectifs par table.
#' @export
importer_questions = function(archive, db = NULL, remplacer = FALSE, verbose = TRUE) {
  archive = path.expand(archive)
  if (!is.character(archive) || length(archive) != 1L || is.na(archive) ||
      !file.exists(archive)) stop("Archive questions.zip introuvable")
  if (!is.logical(remplacer) || length(remplacer) != 1L || is.na(remplacer) ||
      !is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("'remplacer' et 'verbose' doivent etre TRUE ou FALSE")
  db = if (is.null(db)) connexion(creer = TRUE) else .connexion_valide(db)
  if (!identical(utils::unzip(archive, list = TRUE)$Name, "questions.sql"))
    stop("Archive inattendue : questions.sql requis")
  attendu = c("etatquestion", "legquestion", "naturequestion", "sortquestion",
              "tam_ministeres", "tam_questions", "tam_reponses", "the")
  journal = function(...) if (isTRUE(verbose)) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [QUESTIONS] ", ...)
  lecteur = unz(archive, "questions.sql", open = "r")
  on.exit(if (inherits(lecteur, "connection") && isOpen(lecteur)) close(lecteur), add = TRUE)
  definitions = .dosleg_structure(lecteur)
  close(lecteur)
  if (!setequal(names(definitions), attendu)) stop("Structure SQL inattendue")
  dossier = tempfile("parlementR-questions-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  lecteur = unz(archive, "questions.sql", open = "r")
  fichiers = stats::setNames(file.path(dossier, paste0(attendu, ".tsv")), attendu)
  compteurs = stats::setNames(numeric(length(attendu)), attendu)
  actif = NULL
  sortie = NULL
  on.exit(if (inherits(sortie, "connection") && isOpen(sortie)) close(sortie), add = TRUE)
  journal("Extraction des blocs COPY")
  repeat {
    bloc = readLines(lecteur, n = 100000L, warn = FALSE)
    if (!length(bloc)) break
    debut = 1L
    while (debut <= length(bloc)) {
      if (is.null(actif)) {
        positions = grep("^COPY [A-Za-z_][A-Za-z_0-9]* \\(", bloc[debut:length(bloc)])
        if (!length(positions)) break
        position = debut + positions[[1L]] - 1L
        ligne = bloc[[position]]
        nom = sub("^COPY ([A-Za-z_][A-Za-z_0-9]*) \\(.*", "\\1", ligne)
        if (!nom %in% attendu || file.exists(fichiers[[nom]]))
          stop("Bloc COPY inattendu : ", nom)
        colonnes = sub("^COPY [^(]+\\(([^)]+)\\) FROM stdin;$", "\\1", ligne)
        colonnes = trimws(strsplit(colonnes, ",", fixed = TRUE)[[1L]])
        if (!identical(colonnes, definitions[[nom]]))
          stop("Colonnes COPY incorrectes : ", nom)
        actif = nom
        sortie = file(fichiers[[nom]], open = "w")
        debut = position + 1L
      }
      if (debut > length(bloc)) break
      fin = which(bloc[debut:length(bloc)] == "\\.")
      if (length(fin)) {
        position = debut + fin[[1L]] - 1L
        if (position > debut) {
          lignes = bloc[debut:(position - 1L)]
          writeLines(lignes, sortie, useBytes = TRUE)
          compteurs[[actif]] = compteurs[[actif]] + length(lignes)
        }
        close(sortie)
        sortie = NULL
        journal(actif, " : ", compteurs[[actif]], " lignes extraites")
        actif = NULL
        debut = position + 1L
      } else {
        lignes = bloc[debut:length(bloc)]
        writeLines(lignes, sortie, useBytes = TRUE)
        compteurs[[actif]] = compteurs[[actif]] + length(lignes)
        break
      }
    }
  }
  if (!is.null(actif) || !all(file.exists(fichiers))) stop("Extraction COPY incomplete")
  close(lecteur)
  lecteur = NULL
  qid = function(x) as.character(DBI::dbQuoteIdentifier(db, x))
  qstr = function(x) as.character(DBI::dbQuoteString(db, x))
  decoder = function(x) {
    y = paste0("replace(", x, ", chr(92)||chr(92), chr(1))")
    for (paire in list(c("n", "10"), c("r", "13"), c("t", "9"),
                       c("b", "8"), c("f", "12"), c("v", "11")))
      y = paste0("replace(", y, ", chr(92)||", qstr(paire[[1L]]),
                 ", chr(", paire[[2L]], "))")
    paste0("replace(", y, ", chr(1), chr(92))")
  }
  DBI::dbBegin(db)
  valide = FALSE
  on.exit(if (!valide) try(DBI::dbRollback(db), silent = TRUE), add = TRUE)
  DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS sen_questions")
  presentes = DBI::dbGetQuery(db, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'sen_questions'")$table_name
  if (length(presentes) && !isTRUE(remplacer))
    stop("Schema deja occupe : utiliser remplacer = TRUE")
  for (nom in presentes) DBI::dbExecute(db, paste("DROP TABLE", qid(DBI::Id(schema = "sen_questions", table = nom))))
  effectifs = numeric()
  for (nom in attendu) {
    journal("Chargement DuckDB : ", nom)
    colonnes = definitions[[nom]]
    specification = paste(paste0(qstr(colonnes), ": 'VARCHAR'"), collapse = ", ")
    source = paste0("read_csv(", qstr(normalizePath(fichiers[[nom]])),
      ", delim = chr(9), header = false, quote = '', escape = '', nullstr = ",
      qstr("\\N"), ", columns = {", specification,
      "}, strict_mode = true, max_line_size = 10000000)")
    expressions = paste(vapply(colonnes, function(x)
      paste0(decoder(qid(x)), " AS ", qid(x)), character(1)), collapse = ", ")
    cible = qid(DBI::Id(schema = "sen_questions", table = nom))
    DBI::dbExecute(db, paste0("CREATE TABLE ", cible,
                              " AS SELECT ", expressions, " FROM ", source))
    n = DBI::dbGetQuery(db, paste0("SELECT count(*) AS n FROM ", cible))$n[[1L]]
    if (n != compteurs[[nom]])
      stop("Effectif incorrect pour ", nom, " : ", n, " au lieu de ", compteurs[[nom]])
    effectifs[nom] = n
    journal(nom, " importe : ", n, " lignes")
  }
  DBI::dbCommit(db)
  valide = TRUE
  journal("Import termine")
  invisible(tibble::tibble(table = names(effectifs), lignes = unname(effectifs)))
}
