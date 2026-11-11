.importer_dossiers = function(db,
                              cache = tools::R_user_dir("parlementR", "cache"),
                              archive = NULL, verbose = TRUE, forcer = FALSE) {
  journal = function(...) if (isTRUE(verbose)) message("[DOSSIERS] ", ...)
  url = paste0(
    "https://data.assemblee-nationale.fr/static/openData/",
    "repository/17/loi/dossiers_legislatifs/",
    "Dossiers_Legislatifs.json.zip"
  )
  dossier_cache = file.path(cache, "assemblee_nationale", "dossiers_legislatifs")
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
      if (!identical(as.integer(statut), 0L)) stop("Echec du telechargement des dossiers")
      if (file.exists(archive)) unlink(archive)
      if (!file.rename(partiel, archive)) stop("Impossible de conserver l'archive")
    } else journal("Archive deja presente dans le cache")
  }
  if (length(archive) != 1L || !file.exists(archive)) {
    stop("Archive introuvable : ", paste(archive, collapse = ", "))
  }
  journal("Verification de l'archive")
  statut = system2("unzip", c("-tq", shQuote(archive)), stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L)) stop("Archive ZIP invalide : ", archive)
  fichiers = system2("unzip", c("-Z1", shQuote(archive)), stdout = TRUE)
  if (!is.null(attr(fichiers, "status"))) stop("Inventaire ZIP impossible")
  json = fichiers[grepl("\\.json$", fichiers, ignore.case = TRUE)]
  familles = c(dossierParlementaire = "dossiers", document = "documents")
  if (!length(json) || anyDuplicated(json) ||
      any(grepl("(^/|(^|/)\\.\\.(/|$))", fichiers)) ||
      any(!grepl("^json/(dossierParlementaire|document)/[^/]+\\.json$", json)) ||
      any(!names(familles) %in% sub("^json/([^/]+)/.*$", "\\1", json))) {
    stop("Organisation inattendue du ZIP des dossiers")
  }
  attendus = stats::setNames(vapply(names(familles), function(f) {
    sum(startsWith(json, paste0("json/", f, "/")))
  }, integer(1)), names(familles))
  journal("Inventaire : ", paste(names(attendus), attendus, collapse = ", "))
  dossier = tempfile("parlementR-dossiers-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  journal("Extraction temporaire")
  statut = system2("unzip", c("-oq", shQuote(archive), "-d", shQuote(dossier)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L) ||
      !all(file.exists(file.path(dossier, json)))) {
    stop("Extraction incomplete du ZIP des dossiers")
  }
  journal("Transaction DuckDB")
  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS an_dossiers")
    prefixe = paste0(normalizePath(dossier, winslash = "/"), "/")
    prefixe_sql = as.character(DBI::dbQuoteString(db, prefixe))
    for (famille in names(familles)) {
      journal("Importation de ", famille)
      table = paste0("an_dossiers.", familles[[famille]])
      motif = file.path(dossier, "json", famille, "*.json")
      motif_sql = as.character(DBI::dbQuoteString(db, motif))
      uid_sql = as.character(DBI::dbQuoteString(db, paste0("$.", famille, ".uid")))
      sql = paste0(
        "CREATE OR REPLACE TABLE ", table, " AS SELECT ",
        "json_extract_string(content, ", uid_sql, ") AS uid, ",
        "replace(filename, ", prefixe_sql, ", '') AS chemin, ",
        "content::JSON AS contenu FROM read_text(", motif_sql, ")"
      )
      DBI::dbExecute(db, sql)
      controle = .db_get_tibble(db, paste0(
        "SELECT count(*) AS n, count(DISTINCT uid) AS distincts, ",
        "count(*) FILTER (WHERE uid IS NULL) AS absents, ",
        "count(*) FILTER (WHERE json_type(contenu, '$.", famille,
        "') IS DISTINCT FROM 'OBJECT') AS racines_invalides FROM ", table
      ))
      if (controle$n != attendus[[famille]] ||
          controle$distincts != attendus[[famille]] ||
          controle$absents != 0 || controle$racines_invalides != 0) {
        stop("Controle d'integrite echoue pour ", famille)
      }
      journal("Controle OK : ", controle$n, " documents")
    }
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  journal("Importation terminee : ", sum(attendus), " documents")
  invisible(attendus)
}
