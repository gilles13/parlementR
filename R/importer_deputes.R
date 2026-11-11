.importer_deputes = function(db,
                             cache = tools::R_user_dir("parlementR", "cache"),
                             archive = NULL, verbose = TRUE, forcer = FALSE) {
  journal = function(...) if (isTRUE(verbose)) message("[AMO10] ", ...)

  url = paste0(
    "https://data.assemblee-nationale.fr/static/openData/",
    "repository/17/amo/deputes_actifs_mandats_actifs_organes/",
    "AMO10_deputes_actifs_mandats_actifs_organes.json.zip"
  )
  dossier_cache = file.path(cache, "assemblee_nationale", "amo10")
  dir.create(dossier_cache, recursive = TRUE, showWarnings = FALSE)

  if (is.null(archive)) {
    archive = file.path(dossier_cache, basename(url))
    if (forcer || !file.exists(archive)) {
      journal("Telechargement de l'archive")
      partiel = paste0(archive, ".part")
      on.exit(unlink(partiel), add = TRUE)
      statut = system2("curl", c("--http1.1", "-fL", "--retry", "5",
                                  "--retry-all-errors", "-o", shQuote(partiel),
                                  shQuote(url)),
                        stdout = if (verbose) "" else FALSE,
                        stderr = if (verbose) "" else FALSE)
      if (!identical(as.integer(statut), 0L)) stop("Echec du telechargement AMO10")
      if (file.exists(archive)) unlink(archive)
      if (!file.rename(partiel, archive)) stop("Impossible de conserver l'archive")
    } else journal("Archive deja presente dans le cache")
  }
  if (!file.exists(archive)) stop("Archive introuvable : ", archive)

  journal("Verification de l'archive")
  statut = system2("unzip", c("-tq", shQuote(archive)), stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L)) stop("Archive ZIP invalide : ", archive)
  fichiers = system2("unzip", c("-Z1", shQuote(archive)), stdout = TRUE)
  if (!is.null(attr(fichiers, "status"))) stop("Inventaire ZIP impossible")
  json = fichiers[grepl("\\.json$", fichiers, ignore.case = TRUE)]
  familles = c("acteur", "organe", "deport")
  if (!length(json) || anyDuplicated(json) ||
      any(grepl("(^/|(^|/)\\.\\.(/|$))", fichiers)) ||
      any(!grepl("^json/(acteur|organe|deport)/[^/]+\\.json$", json)) ||
      any(!familles %in% sub("^json/([^/]+)/.*$", "\\1", json))) {
    stop("Organisation inattendue du ZIP AMO10")
  }
  attendus = stats::setNames(vapply(familles, function(f) {
    sum(startsWith(json, paste0("json/", f, "/")))
  }, integer(1)), familles)
  journal("Inventaire : ", paste(names(attendus), attendus, collapse = ", "))

  dossier = tempfile("parlementR-amo10-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  journal("Extraction temporaire")
  statut = system2("unzip", c("-oq", shQuote(archive), "-d", shQuote(dossier)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L) ||
      !all(file.exists(file.path(dossier, json)))) {
    stop("Extraction incomplete du ZIP AMO10")
  }

  journal("Transaction DuckDB")
  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS an_deputes")
    prefixe = paste0(normalizePath(dossier, winslash = "/"), "/")
    prefixe_sql = as.character(DBI::dbQuoteString(db, prefixe))

    for (famille in familles) {
      journal("Importation de ", famille)
      table = paste0("an_deputes.", famille, "s")
      motif = file.path(dossier, "json", famille, "*.json")
      motif_sql = as.character(DBI::dbQuoteString(db, motif))
      uid_path = if (famille == "acteur") {
        "$.acteur.uid.\"#text\""
      } else paste0("$.", famille, ".uid")
      uid_sql = as.character(DBI::dbQuoteString(db, uid_path))
      sql = paste0(
        "CREATE OR REPLACE TABLE ", table, " AS SELECT ",
        "json_extract_string(content, ", uid_sql, ") AS uid, ",
        "replace(filename, ", prefixe_sql, ", '') AS chemin, ",
        "content::JSON AS contenu ",
        "FROM read_text(", motif_sql, ")"
      )
      DBI::dbExecute(db, sql)
      controle = .db_get_tibble(db, paste0(
        "SELECT count(*) AS n, count(DISTINCT uid) AS distincts, ",
        "count(*) FILTER (WHERE uid IS NULL) AS absents, ",
        "count(*) FILTER (WHERE json_type(contenu, '$.", famille,
        "') != 'OBJECT') AS racines_invalides FROM ", table
      ))
      if (controle$n != attendus[[famille]] ||
          controle$distincts != attendus[[famille]] ||
          controle$absents != 0 || controle$racines_invalides != 0) {
        stop("Controle d'integrite echoue pour ", famille)
      }
      journal("Controle OK : ", controle$n, " documents ")
    }
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  journal("Importation terminee : ", sum(attendus), " documents")
  invisible(attendus)
}
