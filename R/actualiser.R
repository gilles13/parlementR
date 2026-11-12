#' Actualiser les donnees parlementaires
#'
#' @param db Connexion DBI facultative ; NULL initialise la base au besoin.
#' @param jeux Un ou plusieurs jeux : "deputes", "historique", "scrutins", "dossiers", "amendements", "senateurs", "dosleg", "dole", "questions", "debats".
#' @param cache Repertoire de cache persistant.
#' @param archive Chemin facultatif vers une archive ZIP locale (un seul jeu).
#' @param forcer Ignorer le delai de 24 heures et verifier le serveur ; ne force pas l'import si l'archive est inchangee.
#' @param verbose Afficher les etapes (par defaut TRUE).
#' @export
actualiser = function(db = NULL, jeux = "deputes",
                      cache = tools::R_user_dir("parlementR", "cache"),
                      archive = NULL, verbose = TRUE, forcer = FALSE) {
  disponibles = c("deputes", "historique", "scrutins", "dossiers", "amendements", "senateurs", "dosleg", "dole", "questions", "debats")
  if (!length(jeux) || anyNA(jeux) || any(!jeux %in% disponibles) ||
      anyDuplicated(jeux))
    stop("Jeux attendus, sans doublon : ", paste(disponibles, collapse = ", "))
  if (!is.null(archive) && length(jeux) != 1L)
    stop("Le parametre 'archive' exige un seul jeu.")
  if (!is.logical(forcer) || length(forcer) != 1L || is.na(forcer))
    stop("'forcer' doit etre TRUE ou FALSE")
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("'verbose' doit etre TRUE ou FALSE")
  db = if (is.null(db)) connexion(creer = TRUE) else .connexion_valide(db)
  DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS suivi")
  DBI::dbExecute(db, paste0(
    "CREATE TABLE IF NOT EXISTS suivi.actualisations (",
    "jeu VARCHAR PRIMARY KEY, date_import TIMESTAMP NOT NULL)"))
  colonnes = .db_get_tibble(db, "DESCRIBE suivi.actualisations")$column_name
  for (colonne in c("date_controle", "md5", "etag", "modification")) {
    if (!colonne %in% colonnes) {
      type = if (colonne == "date_controle") "TIMESTAMP" else "VARCHAR"
      DBI::dbExecute(db, paste("ALTER TABLE suivi.actualisations ADD COLUMN", colonne, type))
    }
  }
  tables = list(
    deputes = c("an_deputes.acteurs", "an_deputes.organes", "an_deputes.deports"),
    historique = c("an_historique.acteurs", "an_historique.organes", "an_historique.deports"),
    scrutins = "an_votes.scrutins",
    debats = c("an_debats.debats_seances", "an_debats.debats_points",
               "an_debats.debats_paragraphes"),
    dossiers = c("an_dossiers.dossiers", "an_dossiers.documents"),
    amendements = "an_amendements.amendements",
    senateurs = "senateurs",
    dosleg = "sen_dosleg",
    questions = "sen_questions"
  )
  for (jeu in jeux) {
    journal = function(...) if (isTRUE(verbose)) message(
      "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [", toupper(jeu), "] ", ...)
    journal("Debut de l'actualisation")
    if (jeu == "dole") {
      if (!is.null(archive))
        stop("Pour DOLE, utiliser importer_dole(archives = ...) pour une liste locale.")
      precedent_dole = .db_get_tibble(db,
        "SELECT coalesce(date_controle, date_import) AS dernier FROM suivi.actualisations WHERE jeu = 'dole'")
      heures_dole = if (nrow(precedent_dole)) as.numeric(difftime(
        Sys.time(), as.POSIXct(precedent_dole$dernier[1], tz = "UTC"),
        units = "hours")) else Inf
      if (!forcer && nrow(precedent_dole) &&
          DBI::dbExistsTable(db, DBI::Id(schema = "dila_dole", table = "archives")) &&
          DBI::dbExistsTable(db, DBI::Id(schema = "dila_dole", table = "dossiers")) &&
          DBI::dbExistsTable(db, DBI::Id(schema = "dila_dole", table = "liens")) &&
          is.finite(heures_dole) && heures_dole >= 0 && heures_dole < 24) {
        journal("Controle recent (< 24 h) : aucun acces reseau")
        next
      }
      importer_dole(db = db, cache = file.path(cache, "dila_dole"), verbose = verbose)
      DBI::dbExecute(db, paste0(
        "INSERT INTO suivi.actualisations (jeu, date_import, date_controle) ",
        "VALUES ('dole', current_timestamp, current_timestamp) ",
        "ON CONFLICT (jeu) DO UPDATE SET date_import = excluded.date_import, ",
        "date_controle = excluded.date_controle"))
      journal("Actualisation terminee")
      next
    }
    precedent = .db_get_tibble(db, paste0(
      "SELECT coalesce(date_controle, date_import) AS dernier, md5, etag, modification ",
      "FROM suivi.actualisations WHERE jeu = ?"), params = list(jeu))
    presentes = if (jeu %in% c("senateurs", "dosleg", "dole", "questions", "debats")) {
      n = .db_get_tibble(db, paste0(
        "SELECT count(*) AS n FROM information_schema.tables ",
        "WHERE table_schema = ? AND table_type = 'BASE TABLE'"),
        params = list(tables[[jeu]]))$n
      identical(as.integer(n), if (jeu == "senateurs") 95L else if (jeu == "dosleg") 53L else 8L)
    } else all(vapply(tables[[jeu]], function(nom) {
      parties = strsplit(nom, ".", fixed = TRUE)[[1]]
      DBI::dbExistsTable(db, DBI::Id(schema = parties[1], table = parties[2]))
    }, logical(1)))
    heures = if (nrow(precedent)) as.numeric(difftime(Sys.time(),
      as.POSIXct(precedent$dernier[1], tz = "UTC"), units = "hours")) else Inf
    if (!forcer && is.null(archive) && presentes && is.finite(heures) &&
        heures >= 0 && heures < 24) {
      journal("Controle recent (< 24 h) : aucun acces reseau, aucun import")
      next
    }
    chemin_archive = archive
    etag = NA_character_
    modification = NA_character_
    if (is.null(archive)) {
      info = .source_archive(jeu, cache)
      if (nrow(precedent) && presentes) {
        etag = precedent$etag[1]
        modification = precedent$modification[1]

      }
      controle = .verifier_archive(info$url, etag, modification,
                                   verbose = verbose, jeu = jeu)
      if (presentes && isTRUE(controle$inchange)) {
        journal("Archive inchangee (HTTP ", controle$code,
                ") : aucun telechargement, aucune extraction, aucun import")
        DBI::dbExecute(db, paste0(
          "UPDATE suivi.actualisations SET date_controle = current_timestamp, ",
          "etag = coalesce(?, etag), modification = coalesce(?, modification) WHERE jeu = ?"),
          params = list(controle$etag, controle$modification, jeu))
        next
      }
      journal(if (presentes) "Nouvelle version ou controle non concluant" else
                "Tables absentes : import initial necessaire")
      meta = .telecharger_archive(info$url, info$chemin, verbose = verbose, jeu = jeu)
      etag = meta$etag
      modification = meta$modification
      chemin_archive = info$chemin
    } else {
      if (!file.exists(archive)) stop("Archive locale introuvable : ", archive)
      journal("Archive locale fournie : ", archive)
    }
    journal("Debut de l'extraction et de l'import dans DuckDB")
    if (jeu == "deputes") {
      .importer_deputes(db = db, cache = cache, archive = chemin_archive,
                       verbose = verbose)
    } else if (jeu == "historique") {
      .importer_historique(db = db, archive = chemin_archive, verbose = verbose)
    } else if (jeu == "scrutins") {
      .importer_scrutins(db = db, archive = chemin_archive, verbose = verbose)
    } else if (jeu == "debats") {
      .importer_debats(con = db, fichier = chemin_archive,
                       remplacer = presentes, verbose = verbose)
    } else if (jeu == "dossiers") {
      .importer_dossiers(db = db, cache = cache, archive = chemin_archive,
                        verbose = verbose)
    } else if (jeu == "amendements") {
      .importer_amendements(db = db, cache = cache, archive = chemin_archive,
                           verbose = verbose)
    } else if (jeu == "senateurs") {
      importer_senateurs(db = db, archive = chemin_archive, verbose = verbose)
    } else if (jeu == "questions") {
      importer_questions(db = db, archive = chemin_archive, remplacer = presentes,
                         verbose = verbose)
    } else {
      importer_dosleg(db = db, archive = chemin_archive, remplacer = presentes,
                      verbose = verbose)
    }
    journal("Import termine ; enregistrement du suivi DuckDB")
    DBI::dbExecute(db, paste0(
      "INSERT INTO suivi.actualisations ",
      "(jeu, date_import, date_controle, md5, etag, modification) ",
      "VALUES (?, current_timestamp, current_timestamp, NULL, ?, ?) ",
      "ON CONFLICT (jeu) DO UPDATE SET date_import = excluded.date_import, ",
      "date_controle = excluded.date_controle, md5 = NULL, ",
      "etag = excluded.etag, modification = excluded.modification"),
      params = list(jeu, etag, modification))
    journal("Actualisation terminee")
  }
  invisible(TRUE)
}
