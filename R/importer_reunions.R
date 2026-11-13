# Import des reunions de l'Assemblee nationale (agenda JSON).
.importer_reunions = function(db, archive, verbose = TRUE) {
  journal = function(...) if (isTRUE(verbose)) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [REUNIONS] ", ...)
  if (length(archive) != 1L || !file.exists(archive))
    stop("Archive des reunions introuvable")
  statut = system2("unzip", c("-tq", shQuote(archive)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L)) stop("Archive ZIP invalide")
  fichiers = system2("unzip", c("-Z1", shQuote(archive)), stdout = TRUE)
  if (!is.null(attr(fichiers, "status"))) stop("Inventaire ZIP impossible")
  json = fichiers[grepl("\\.json$", fichiers, ignore.case = TRUE)]
  if (!length(json) || anyDuplicated(json) ||
      any(grepl("(^/|(^|/)\\.\\.(/|$))", fichiers)) ||
      any(grepl("\\", fichiers, fixed = TRUE)))
    stop("Organisation inattendue du ZIP des reunions")
  dossier = tempfile("parlementR-reunions-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  journal("Extraction de ", length(json), " documents")
  statut = system2("unzip", c("-oq", shQuote(archive), "-d", shQuote(dossier)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L) ||
      !all(file.exists(file.path(dossier, json))))
    stop("Extraction des reunions incomplete")
  motif = as.character(DBI::dbQuoteString(db,
    file.path(dossier, "**", "*.json")))
  prefixe = as.character(DBI::dbQuoteString(db,
    paste0(normalizePath(dossier, winslash = "/"), "/")))
  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS an_reunions")
    DBI::dbExecute(db, paste0(
      "CREATE OR REPLACE TABLE an_reunions.reunions AS SELECT ",
      "json_extract_string(content, '$.reunion.uid') AS uid, ",
      "replace(filename, ", prefixe, ", '') AS chemin, ",
      "content::JSON AS contenu FROM read_text(", motif, ")"))
    controle = DBI::dbGetQuery(db, paste0(
      "SELECT count(*) AS n, count(DISTINCT uid) AS distincts, ",
      "count(*) FILTER (WHERE uid IS NULL OR uid = '') AS absents, ",
      "count(*) FILTER (WHERE json_type(contenu, '$.reunion') ",
      "IS DISTINCT FROM 'OBJECT') AS racines_invalides ",
      "FROM an_reunions.reunions"))
    if (controle$n != length(json) || controle$distincts != length(json) ||
        controle$absents != 0 || controle$racines_invalides != 0)
      stop("Controle d'integrite des reunions echoue")
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  journal("Reunions importees : ", controle$n)
  invisible(controle$n)
}
