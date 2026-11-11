# Archive officielle complete : un import, puis des recherches exclusivement locales.
.importer_amendements = function(db,
    cache = tools::R_user_dir("parlementR", "cache"),
    archive = NULL, verbose = TRUE, forcer = FALSE) {
  journal = function(...) if (isTRUE(verbose)) message("[AMENDEMENTS] ", ...)
  url = paste0("https://data.assemblee-nationale.fr/static/openData/",
               "repository/17/loi/amendements_div_legis/Amendements.json.zip")
  repertoire = file.path(cache, "assemblee_nationale", "amendements")
  dir.create(repertoire, recursive = TRUE, showWarnings = FALSE)
  if (is.null(archive)) {
    archive = file.path(repertoire, basename(url))
    if (forcer || !file.exists(archive)) {
      temporaire = paste0(archive, ".part")
      on.exit(unlink(temporaire), add = TRUE)
      statut = system2("curl", c("--http1.1", "-fL", "--retry", "5",
                   "--retry-all-errors", "-o", shQuote(temporaire), shQuote(url)),
                   stdout = if (verbose) "" else FALSE,
                   stderr = if (verbose) "" else FALSE)
      if (!identical(as.integer(statut), 0L))
        stop("Echec du telechargement des amendements")
      if (file.exists(archive)) unlink(archive)
      if (!file.rename(temporaire, archive))
        stop("Impossible de conserver l'archive des amendements")
    }
  }
  if (length(archive) != 1L || is.na(archive) || !file.exists(archive))
    stop("Archive introuvable")
  if (!identical(as.integer(system2("unzip", c("-tq", shQuote(archive)),
                                      stdout = FALSE, stderr = FALSE)), 0L))
    stop("Archive ZIP invalide")
  fichiers = system2("unzip", c("-Z1", shQuote(archive)), stdout = TRUE)
  if (!is.null(attr(fichiers, "status")) || any(grepl("(^/|(^|/)\\.\\.(/|$))", fichiers)))
    stop("Chemins ZIP non valides")
  json = fichiers[grepl("\\.json$", fichiers, ignore.case = TRUE)]
  if (!length(json) || anyDuplicated(json)) stop("Aucun JSON unique dans l'archive")
  dossier = tempfile("parlementR-amendements-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  journal("Extraction de ", length(json), " fichiers JSON")
  statut = system2("unzip", c("-oq", shQuote(archive), "-d", shQuote(dossier)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L) ||
      !all(file.exists(file.path(dossier, json)))) stop("Extraction incomplete")
  # read_text conserve l'integralite du JSON et son chemin source.
  # La liste des fichiers est explicite : aucune hypothese sur les sous-repertoires.
  chemins = normalizePath(file.path(dossier, json), winslash = "/")
  journal("Debut de la transaction DuckDB : ", length(json), " amendements")
  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS an_amendements")
    DBI::dbExecute(db, paste0(
      "CREATE OR REPLACE TABLE an_amendements.amendements AS ",
      "SELECT CAST(NULL AS VARCHAR) uid, CAST(NULL AS VARCHAR) chemin, ",
      "CAST(NULL AS JSON) contenu WHERE FALSE"))
    # Lots pour ne pas construire une instruction SQL demesuree.
    blocs = split(seq_along(chemins), ceiling(seq_along(chemins) / 500L))
    for (i in seq_along(blocs)) {
      j = blocs[[i]]
      valeurs = paste(vapply(chemins[j], function(x)
        as.character(DBI::dbQuoteString(db, x)), character(1)), collapse = ",")
      prefixe = as.character(DBI::dbQuoteString(db,
                    paste0(normalizePath(dossier, winslash = "/"), "/")))
      sql = paste0(
        "INSERT INTO an_amendements.amendements ",
        "SELECT coalesce(json_extract_string(content, '$.amendement.uid'), ",
        "json_extract_string(content, '$.uid')) AS uid, ",
        "replace(filename, ", prefixe, ", '') AS chemin, content::JSON AS contenu ",
        "FROM read_text([", valeurs, "])")
      DBI::dbExecute(db, sql)
      journal("Lots importes : ", i, "/", length(blocs), " (",
              min(i * 500L, length(chemins)), "/", length(chemins), " JSON)")
    }
    journal("Controle d'integrite DuckDB")
    verif = .db_get_tibble(db, paste0(
      "SELECT count(*) n, count(DISTINCT uid) distincts, ",
      "count(*) FILTER (WHERE uid IS NULL OR uid = '') absents, ",
      "count(*) FILTER (WHERE json_type(contenu, '$.amendement') ",
      "IS DISTINCT FROM 'OBJECT') racines ",
      "FROM an_amendements.amendements"))
    if (verif$n != length(json) || verif$distincts != verif$n ||
        verif$absents != 0 || verif$racines != 0)
      stop("Controle d'integrite des amendements echoue : ",
           paste(unlist(verif), collapse = "/"))
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  journal("Importation terminee : ", length(json), " amendements")
  invisible(length(json))
}
