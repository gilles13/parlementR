# Portraits officiels conserves dans le cache local, hors DuckDB.
.photo_jpeg = function(fichier) {
  if (!file.exists(fichier) || is.na(file.info(fichier)$size) ||
      file.info(fichier)$size < 3) return(FALSE)
  identical(readBin(fichier, "raw", n = 3), as.raw(c(255, 216, 255)))
}

.photo_normaliser = function(x) {
  x = iconv(tolower(trimws(x)), from = "UTF-8", to = "ASCII//TRANSLIT")
  x = gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

.actualiser_photos = function(db, jeu, cache, forcer = FALSE, verbose = TRUE) {
  if (jeu == "photos_an") {
    if (!DBI::dbExistsTable(db, DBI::Id(schema = "an_amo10", table = "acteurs")))
      stop("Importer les deputes avant les photographies AN")
    ids = DBI::dbGetQuery(db, paste0(
      "SELECT DISTINCT uid FROM an_amo10.acteurs ",
      "WHERE uid LIKE 'PA%' ORDER BY uid"))$uid
    urls = sprintf("https://www.assemblee-nationale.fr/dyn/static/tribun/17/photos/carre/%s.jpg",
                   sub("^PA", "", ids))
    dossier = file.path(cache, "assemblee_nationale", "photos", "17")
  } else {
    if (!DBI::dbExistsTable(db, DBI::Id(schema = "senateurs", table = "sen")))
      stop("Importer les senateurs avant leurs photographies")
    sen = DBI::dbGetQuery(db, paste0(
      "SELECT senmat, sennomuse, senprenomuse FROM senateurs.sen ",
      "WHERE etasencod = 'ACTIF' ORDER BY senmat"))
    ids = sen$senmat
    identifiants = paste0(.photo_normaliser(sen$sennomuse), "_",
                         .photo_normaliser(sen$senprenomuse), tolower(ids))
    urls = sprintf("https://www.senat.fr/senimg/%s_carre.jpg", identifiants)
    dossier = file.path(cache, "senat", "photos")
  }
  if (!length(ids) || anyNA(urls) || anyDuplicated(ids))
    stop("Identifiants photographiques absents ou invalides : ", jeu)
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dossier)) stop("Repertoire de cache inaccessible : ", dossier)
  fichiers = file.path(dossier, paste0(ids, ".jpg"))
  valides = vapply(fichiers, .photo_jpeg, logical(1))
  precedent = DBI::dbGetQuery(db, paste0(
    "SELECT date_diff('second', coalesce(date_controle, date_import), ",
    "(current_timestamp AT TIME ZONE 'UTC')) / 3600.0 AS heures ",
    "FROM suivi.actualisations WHERE jeu = ?"), params = list(jeu))
  heures = if (nrow(precedent)) precedent$heures[1] else Inf
  controle_complet = forcer || !is.finite(heures) || heures < 0 || heures >= 30 * 24
  indices = if (controle_complet) seq_along(fichiers) else which(!valides)
  if (!length(indices)) {
    if (verbose) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"),
                         "] [", toupper(jeu), "] Controle recent : aucun acces reseau")
    return(invisible(fichiers))
  }
  # Un processus curl, avec un resultat individuel par transfert.
  travail = tempfile(".photos-", tmpdir = dossier)
  if (!dir.create(travail)) stop("Repertoire temporaire inaccessible")
  on.exit(unlink(travail, recursive = TRUE), add = TRUE)
  config = file.path(travail, "curl.conf")
  resultats = file.path(travail, "resultats.txt")
  temporaires = file.path(travail, paste0(indices, ".part"))
  option = function(nom, valeur) {
    paste0(nom, " = ", encodeString(valeur, quote = '"'))
  }
  blocs = lapply(seq_along(indices), function(j) {
    i = indices[j]
    c("fail", "location", "remote-time", "retry = 3",
      "connect-timeout = 30",
      option("url", urls[i]),
      option("output", temporaires[j]),
      if (valides[i]) option("time-cond", fichiers[i]),
      option("write-out", paste0(j, " %{http_code} %{exitcode}\n")))
  })
  writeLines(paste(vapply(blocs, paste, "", collapse = "\n"),
                   collapse = "\nnext\n"), config)
  if (verbose) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"),
                       "] [", toupper(jeu), "] Synchronisation de ", length(indices), " portraits")
  statut = system2("curl", c("--parallel", "--parallel-max", "8",
                             "--config", shQuote(config)), stdout = resultats)
  lignes = if (file.exists(resultats)) readLines(resultats, warn = FALSE) else character()
  succes = rep(FALSE, length(indices))
  for (j in seq_along(indices)) {
    i = indices[j]
    ligne = grep(paste0("^", j, " "), lignes, value = TRUE)
    if (length(ligne) != 1L) next
    if (identical(ligne, paste(j, "304 0")) && valides[i]) {
      succes[j] = TRUE
    } else if (identical(ligne, paste(j, "200 0")) && .photo_jpeg(temporaires[j])) {
      succes[j] = file.rename(temporaires[j], fichiers[i])
    }
  }
  ok = vapply(fichiers, .photo_jpeg, logical(1))
  if (!identical(as.integer(statut), 0L) || !all(succes) || !all(ok))
    stop(jeu, " : ", sum(ok), "/", length(ok), " JPEG valides ; ",
         "transferts en echec : ", paste(ids[indices[!succes]], collapse = ", "),
         " ; matricules manquants : ", paste(ids[!ok], collapse = ", "),
         " ; statut curl : ", statut)
  if (controle_complet) DBI::dbExecute(db, paste0(
    "INSERT INTO suivi.actualisations (jeu, date_import, date_controle) ",
    "VALUES (?, (current_timestamp AT TIME ZONE 'UTC'), (current_timestamp AT TIME ZONE 'UTC')) ",
    "ON CONFLICT (jeu) DO UPDATE SET date_controle = excluded.date_controle"),
    params = list(jeu))
  if (verbose) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"),
                       "] [", toupper(jeu), "] ", sum(ok), "/", length(ok), " JPEG valides")
  invisible(fichiers)
}
