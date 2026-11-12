# Import AMO30 en conservant les documents JSON originaux.
.importer_historique = function(db, archive, verbose = TRUE) {
  journal = function(...) if (isTRUE(verbose)) message("[AMO30] ", ...)
  if (!file.exists(archive)) stop("Archive AMO30 introuvable : ", archive)
  if (!identical(as.integer(system2("unzip", c("-tq", shQuote(archive)),
                                   stdout = FALSE, stderr = FALSE)), 0L))
    stop("Archive AMO30 invalide")
  fichiers = system2("unzip", c("-Z1", shQuote(archive)), stdout = TRUE)
  if (!is.null(attr(fichiers, "status"))) stop("Inventaire AMO30 impossible")
  json = fichiers[grepl("\\.json$", fichiers, ignore.case = TRUE)]
  familles = c("acteur", "organe", "deport")
  if (!length(json) || anyDuplicated(json) ||
      any(grepl("(^/|(^|/)\\.\\.(/|$))", fichiers)) ||
      any(!grepl("^json/(acteur|organe|deport)/[^/]+\\.json$", json)) ||
      any(!familles %in% sub("^json/([^/]+)/.*$", "\\1", json)))
    stop("Organisation inattendue du ZIP AMO30")
  attendus = stats::setNames(vapply(familles, function(f) {
    sum(startsWith(json, paste0("json/", f, "/")))
  }, integer(1)), familles)
  journal("Inventaire : ", paste(names(attendus), attendus, collapse = ", "))
  dossier = tempfile("parlementR-amo30-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  statut = system2("unzip", c("-oq", shQuote(archive), "-d", shQuote(dossier)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L) ||
      !all(file.exists(file.path(dossier, json)))) stop("Extraction AMO30 incomplete")
  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS an_historique")
    prefixe = as.character(DBI::dbQuoteString(db,
      paste0(normalizePath(dossier, winslash = "/"), "/")))
    for (famille in familles) {
      table = paste0("an_historique.", famille, "s")
      motif = as.character(DBI::dbQuoteString(db,
        file.path(dossier, "json", famille, "*.json")))
      uid = if (famille == "acteur") '$.acteur.uid."#text"' else
        paste0("$.", famille, ".uid")
      uid = as.character(DBI::dbQuoteString(db, uid))
      DBI::dbExecute(db, paste0(
        "CREATE OR REPLACE TABLE ", table, " AS SELECT ",
        "json_extract_string(content, ", uid, ") AS uid, ",
        "replace(filename, ", prefixe, ", '') AS chemin, ",
        "content::JSON AS contenu FROM read_text(", motif, ")"))
      controle = .db_get_tibble(db, paste0(
        "SELECT count(*) AS n, count(DISTINCT uid) AS distincts, ",
        "count(*) FILTER (WHERE uid IS NULL) AS absents, ",
        "count(*) FILTER (WHERE json_type(contenu, '$.", famille,
        "') != 'OBJECT') AS racines_invalides FROM ", table))
      if (controle$n != attendus[[famille]] ||
          controle$distincts != attendus[[famille]] ||
          controle$absents != 0 || controle$racines_invalides != 0)
        stop("Controle d'integrite AMO30 echoue pour ", famille)
      journal(famille, " : ", controle$n, " documents")
    }
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  invisible(attendus)
}
