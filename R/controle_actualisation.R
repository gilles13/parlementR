# Verification des tables exactes : manifeste construit apres un import reussi.
# Les bases historiques sans manifeste sont controlees de facon conservative.
.tables_schema = function(db, schema) {
  DBI::dbGetQuery(db, paste0(
    "SELECT table_name FROM information_schema.tables ",
    "WHERE table_schema = ? AND table_type = 'BASE TABLE' ORDER BY table_name"),
    params = list(schema))$table_name
}

.jeu_present = function(db, jeu, attendu) {
  if (is.null(attendu)) return(FALSE)
  if (jeu == "questions_senat") {
    return(setequal(.tables_schema(db, "sen_questions"), c(
      "etatquestion", "legquestion", "naturequestion", "sortquestion",
      "tam_ministeres", "tam_questions", "tam_reponses", "the")))
  }
  if (jeu %in% c("senateurs", "dosleg", "ameli")) {
    schema = attendu
    tables = .tables_schema(db, schema)
    if (!length(tables)) return(FALSE)
    if (!"tables_attendues" %in% .tables_schema(db, "suivi"))
      return(FALSE)
    manifeste = DBI::dbGetQuery(db,
      "SELECT table_name FROM suivi.tables_attendues WHERE jeu = ? ORDER BY table_name",
      params = list(jeu))$table_name
    return(length(manifeste) > 0L && identical(tables, manifeste))
  }
  all(vapply(attendu, function(nom) {
    morceaux = strsplit(nom, ".", fixed = TRUE)[[1L]]
    length(morceaux) == 2L &&
      morceaux[[2L]] %in% .tables_schema(db, morceaux[[1L]])
  }, logical(1)))
}

.memoriser_tables = function(db, jeu, attendu) {
  if (!jeu %in% c("senateurs", "dosleg", "ameli")) return(invisible(NULL))
  DBI::dbExecute(db, paste0("CREATE TABLE IF NOT EXISTS suivi.tables_attendues ",
    "(jeu VARCHAR, table_name VARCHAR, PRIMARY KEY (jeu, table_name))"))
  tables = .tables_schema(db, attendu)
  if (!length(tables)) stop("Aucune table importee pour ", jeu)
  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, "DELETE FROM suivi.tables_attendues WHERE jeu = ?", params = list(jeu))
    for (nom in tables) DBI::dbExecute(db,
      "INSERT INTO suivi.tables_attendues VALUES (?, ?)", params = list(jeu, nom))
    DBI::dbCommit(db)
  }, error = function(e) { DBI::dbRollback(db); stop(e) })
  invisible(NULL)
}

.empreinte_archive = function(chemin) unname(tools::md5sum(chemin))
# Empreinte SHA-256 de l archive brute ; jamais du contenu JSON extrait.
# sha256sum appartient a GNU coreutils (aucune dependance R supplementaire).
.sha256_archive = function(chemin) {
  if (!file.exists(chemin)) stop("Archive introuvable : ", chemin)
  outil = Sys.which("sha256sum")
  if (!nzchar(outil)) stop("SHA-256 obligatoire : installer sha256sum (coreutils).")
  sortie = suppressWarnings(system2(outil, shQuote(normalizePath(chemin)),
                                     stdout = TRUE, stderr = FALSE))
  statut = attr(sortie, "status")
  if ((!is.null(statut) && statut != 0L) || length(sortie) != 1L ||
      !grepl("^[0-9a-fA-F]{64}\\s", sortie))
    stop("Impossible de calculer SHA-256 : ", chemin)
  tolower(substr(sortie, 1L, 64L))
}


.zip_valide = function(chemin) {
  is.character(chemin) && length(chemin) == 1L && !is.na(chemin) &&
    file.exists(chemin) &&
    identical(as.integer(system2("unzip", c("-tq", shQuote(chemin)),
                                stdout = FALSE, stderr = FALSE)), 0L)
}

.controle_archive_locale = function(db, jeu, archive, presentes,
                                    autoriser_regression = FALSE) {
  if (!.zip_valide(archive)) stop("Archive ZIP locale absente ou invalide : ", archive)
  if (!presentes) return("importer")
  ancien = DBI::dbGetQuery(db,
    "SELECT md5 FROM suivi.actualisations WHERE jeu = ?", params = list(jeu))
  if (nrow(ancien) && !is.na(ancien$md5[[1L]]) &&
      identical(ancien$md5[[1L]], .empreinte_archive(archive))) return("identique")
  if (!autoriser_regression) stop(
    "Archive locale differente ou chronologie inconnue pour ", jeu,
    " : remplacement refuse. Restauration explicite : autoriser_regression = TRUE")
  "importer"
}

.suivi_controle = function(db, jeu, etag, modification) {
  DBI::dbExecute(db, paste0(
    "UPDATE suivi.actualisations SET date_controle = (current_timestamp AT TIME ZONE 'UTC'), ",
    "etag = coalesce(?, etag), modification = coalesce(?, modification) WHERE jeu = ?"),
    params = list(etag, modification, jeu))
}

# Completer uniquement une empreinte dont le MD5 historique est verifie.
.sha256_verifie = function(fichier, md5, sha256 = NA_character_) {
  if (!file.exists(fichier) || is.na(md5)) return(sha256)
  if (!identical(.empreinte_archive(fichier), md5))
    stop("MD5 du cache different de l archive importee : ", fichier)
  if (is.na(sha256)) return(.sha256_archive(fichier))
  sha256
}
.suivi_sha256 = function(db, jeu, fichier) {
  x = DBI::dbGetQuery(db, "SELECT md5, sha256 FROM suivi.actualisations WHERE jeu = ?",
                      params = list(jeu))
  if (nrow(x) && is.na(x$sha256[1])) {
    sha = .sha256_verifie(fichier, x$md5[1])
    if (!is.na(sha)) DBI::dbExecute(db,
      "UPDATE suivi.actualisations SET sha256 = ? WHERE jeu = ?", params = list(sha, jeu))
  }
}

# HTTP IMF-fixdate : mois anglais, independamment de LC_TIME.
.date_http = function(x) {
  x = trimws(x)
  valide = !is.na(x) & grepl("^[A-Za-z]{3}, [0-9]{2} [A-Za-z]{3} [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT$", x)
  x[!valide] = NA_character_
  x = sub("^[A-Za-z]{3}, ", "", x)
  for (i in seq_along(month.abb))
    x = sub(paste0(" ", month.abb[i], " "), sprintf(" %02d ", i), x, fixed = TRUE)
  suppressWarnings(as.POSIXct(x, format = "%d %m %Y %H:%M:%S GMT", tz = "UTC"))
}

# Rattrapage strictement local : aucun appel aux importeurs ou au reseau.
.completer_sha256 = function(db = NULL, cache = tools::R_user_dir("parlementR", "cache")) {
  db = if (is.null(db)) connexion() else .connexion_valide(db)
  existe = function(schema, table) DBI::dbExistsTable(db, DBI::Id(schema = schema, table = table))
  if (!existe("suivi", "actualisations")) return(invisible(NULL))
  DBI::dbExecute(db, "ALTER TABLE suivi.actualisations ADD COLUMN IF NOT EXISTS sha256 VARCHAR")
  ordinaires = setdiff(.jeux_actualisables, c("photos_an", "photos_senat", "rne", "questions_an", "dole", "legi"))
  fichiers = vapply(ordinaires, function(jeu) .source_archive(jeu, cache)$chemin, character(1))
  names(fichiers) = ordinaires
  fichiers = c(fichiers,
    rne_deputes = file.path(cache, "rne", "deputes.csv"),
    rne_senateurs = file.path(cache, "rne", "senateurs.csv"),
    questions_an_ecrites = file.path(cache, "assemblee_nationale", "questions", "Questions_ecrites.json.zip"),
    questions_an_orales = file.path(cache, "assemblee_nationale", "questions", "Questions_orales_sans_debat.json.zip"),
    questions_an_gouvernement = file.path(cache, "assemblee_nationale", "questions", "Questions_gouvernement.json.zip"))
  for (jeu in names(fichiers)) .suivi_sha256(db, jeu, fichiers[[jeu]])
  if (existe("dila_dole", "archives")) {
    DBI::dbExecute(db, "ALTER TABLE dila_dole.archives ADD COLUMN IF NOT EXISTS sha256 VARCHAR")
    .completer_dole_sha256(db, file.path(cache, "dila_dole"))
  }
  if (existe("suivi", "archives_legi")) {
    DBI::dbExecute(db, "ALTER TABLE suivi.archives_legi ADD COLUMN IF NOT EXISTS sha256 VARCHAR")
    x = DBI::dbGetQuery(db, "SELECT archive, md5 FROM suivi.archives_legi WHERE sha256 IS NULL")
    for (i in seq_len(nrow(x))) {
      sha = .sha256_verifie(file.path(cache, "legi", x$archive[i]), x$md5[i])
      if (!is.na(sha)) DBI::dbExecute(db,
        "UPDATE suivi.archives_legi SET sha256 = ? WHERE archive = ?", params = list(sha, x$archive[i]))
    }
    DBI::dbExecute(db, paste0("UPDATE suivi.actualisations SET sha256 = a.sha256 ",
      "FROM suivi.archives_legi a WHERE jeu = 'legi' AND suivi.actualisations.md5 = a.md5 ",
      "AND suivi.actualisations.sha256 IS NULL AND a.sha256 IS NOT NULL"))
  }
  invisible(DBI::dbGetQuery(db, "SELECT jeu, md5 IS NOT NULL AS md5_disponible, sha256 IS NOT NULL AS sha256_disponible FROM suivi.actualisations ORDER BY jeu"))
}

.initialiser_telechargements = function(db) {
  DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS suivi")
  DBI::dbExecute(db, paste0("CREATE TABLE IF NOT EXISTS suivi.telechargements (",
    "jeu VARCHAR, url VARCHAR, nom_fichier VARCHAR, date_telechargement TIMESTAMP, ",
    "taille BIGINT, md5 VARCHAR, sha256 VARCHAR, etag VARCHAR, modification VARCHAR, ",
    "PRIMARY KEY (jeu, url, sha256))"))
  invisible(NULL)
}

# Journal des fichiers recus, independant des imports.
.memoriser_telechargement = function(db, jeu, url, fichier,
                                      meta = list(etag = NA_character_, modification = NA_character_)) {
  meta$md5 = .empreinte_archive(fichier)
  meta$sha256 = .sha256_archive(fichier)
  if (is.na(meta$md5)) stop("MD5 indisponible : ", fichier)
  .initialiser_telechargements(db)
  DBI::dbExecute(db, paste0("INSERT INTO suivi.telechargements VALUES ",
    "(?, ?, ?, (current_timestamp AT TIME ZONE 'UTC'), ?, ?, ?, ?, ?) ",
    "ON CONFLICT (jeu, url, sha256) DO UPDATE SET ",
    "date_telechargement = excluded.date_telechargement, etag = excluded.etag, ",
    "modification = excluded.modification"),
    params = list(jeu, url, basename(fichier), file.info(fichier)$size,
                  meta$md5, meta$sha256, meta$etag, meta$modification))
  meta
}

.cache_importe = function(fichier, md5) {
  file.exists(fichier) && length(md5) == 1L && !is.na(md5) &&
    identical(.empreinte_archive(fichier), md5)
}

.telechargement_cache = function(db, jeu, url, fichier) {
  if (!file.exists(fichier) || !DBI::dbExistsTable(db,
      DBI::Id(schema = "suivi", table = "telechargements"))) return(NULL)
  x = DBI::dbGetQuery(db, paste0("SELECT md5, sha256, etag, modification ",
    "FROM suivi.telechargements WHERE jeu = ? AND url = ? AND md5 = ? ",
    "ORDER BY date_telechargement DESC LIMIT 1"),
    params = list(jeu, url, .empreinte_archive(fichier)))
  if (!nrow(x)) return(NULL)
  as.list(x[1, , drop = FALSE])
}
