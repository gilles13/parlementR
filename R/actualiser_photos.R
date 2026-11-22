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
    "current_localtimestamp()) / 3600.0 AS heures ",
    "FROM suivi.actualisations WHERE jeu = ?"), params = list(jeu))
  heures = if (nrow(precedent)) precedent$heures[1] else Inf
  if (!forcer && all(valides) && is.finite(heures) && heures >= 0 && heures < 24) {
    if (verbose) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                         "] [", toupper(jeu), "] Controle recent : aucun acces reseau")
    return(invisible(fichiers))
  }
  # Le serveur repond 304 pour les fichiers inchanges. La destination n'est
  # remplacee qu'apres un transfert complet, jamais pendant le telechargement.
  commandes = vapply(seq_along(fichiers), function(i) {
    fichier = fichiers[i]
    temporaire = paste0(fichier, ".part")
    condition = if (valides[i]) paste("-z", shQuote(fichier)) else ""
    paste0("rm -f ", shQuote(temporaire), "; ",
           "curl -fsSL -R --retry 3 --connect-timeout 30 ", condition,
           " -o ", shQuote(temporaire), " ", shQuote(urls[i]),
           " && { if test -s ", shQuote(temporaire),
           "; then mv -f ", shQuote(temporaire), " ", shQuote(fichier),
           "; fi; } ; resultat=$?; rm -f ", shQuote(temporaire),
           "; exit $resultat")
  }, character(1))
  if (verbose) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       "] [", toupper(jeu), "] Synchronisation de ", length(commandes), " portraits")
  statut = system2("xargs", c("-P", "8", "-I", "{}", "sh", "-c", shQuote("{}")),
                   input = commandes)
  ok = vapply(fichiers, .photo_jpeg, logical(1))
  if (!identical(as.integer(statut), 0L) || !all(ok))
    stop(jeu, " : ", sum(ok), "/", length(ok), " JPEG valides ; ",
         "matricules manquants : ", paste(ids[!ok], collapse = ", "))
  DBI::dbExecute(db, paste0(
    "INSERT INTO suivi.actualisations (jeu, date_import, date_controle) ",
    "VALUES (?, current_timestamp, current_timestamp) ",
    "ON CONFLICT (jeu) DO UPDATE SET date_controle = excluded.date_controle"),
    params = list(jeu))
  if (verbose) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       "] [", toupper(jeu), "] ", sum(ok), "/", length(ok), " JPEG valides")
  invisible(fichiers)
}
