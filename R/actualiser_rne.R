# Import local des deux CSV RNE valides par importer_rne().
# Le manifeste constitue la source des metadonnees, non la base DuckDB.
.actualiser_rne = function(db, cache, forcer = FALSE, verbose = TRUE,
                           autoriser_regression = FALSE) {
  journal = function(...) if (isTRUE(verbose)) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] RNE : ", ...)
  meta = importer_rne(cache = cache, forcer = forcer, verbose = verbose)
  attendus = c(deputes = "rne_deputes", senateurs = "rne_senateurs")
  if (!setequal(meta$jeu, unname(attendus)) || anyDuplicated(meta$jeu))
    stop("Manifeste RNE incomplet ou duplique")
  DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS rne")
  for (nom in names(attendus)) {
    jeu = unname(attendus[[nom]])
    ligne = meta[meta$jeu == jeu, , drop = FALSE]
    fichier = ligne$fichier[[1L]]
    if (!file.exists(fichier) || !identical(unname(tools::md5sum(fichier)), ligne$md5[[1L]]))
      stop("Fichier RNE absent ou different du manifeste : ", jeu)
    precedent = DBI::dbGetQuery(db,
      "SELECT md5, modification FROM suivi.actualisations WHERE jeu = ?",
      params = list(jeu))
    table_id = DBI::Id(schema = "rne", table = nom)
    presente = DBI::dbExistsTable(db, table_id)
    if (presente && nrow(precedent) && identical(precedent$md5[[1L]], ligne$md5[[1L]])) {
      DBI::dbExecute(db,
        "UPDATE suivi.actualisations SET date_controle = current_timestamp WHERE jeu = ?",
        params = list(jeu))
      journal(toupper(nom), " : MD5 identique, aucun import")
      next
    }
    # Ne jamais remplacer une version deja importee par un fichier d'anciennete
    # inconnue ou anterieure sans autorisation explicite.
    if (presente && !autoriser_regression) {
      ancien = if (nrow(precedent)) precedent$modification[[1L]] else NA_character_
      nouveau = ligne$modification[[1L]]
      a = suppressWarnings(as.POSIXct(ancien, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT"))
      b = suppressWarnings(as.POSIXct(nouveau, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT"))
      if (!nrow(precedent) || is.na(a) || is.na(b) || b < a)
        stop("Chronologie RNE non verifiable ou regression pour ", jeu,
             "; utiliser autoriser_regression = TRUE si voulu")
    }
    donnees = utils::read.csv2(fichier, check.names = FALSE,
      fileEncoding = "UTF-8-BOM", colClasses = "character")
    colonnes = c("Nom de l'\u00e9lu", "Pr\u00e9nom de l'\u00e9lu", "Date de naissance",
                 "Code de la cat\u00e9gorie socio-professionnelle", "Date de d\u00e9but du mandat")
    if (!all(colonnes %in% names(donnees)) || !nrow(donnees) ||
        nrow(donnees) != ligne$nombre_lignes[[1L]])
      stop("Structure ou nombre de lignes RNE inattendu : ", jeu)
    for (champ in c("Date de naissance", "Date de d\u00e9but du mandat")) {
      valeurs = donnees[[champ]]
      valides = is.na(valeurs) | valeurs == "" | grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", valeurs)
      if (!all(valides)) stop("Date RNE invalide : ", champ)
    }
    # Ecriture et suivi atomiques : une erreur ne detruit pas la table precedente.
    DBI::dbBegin(db)
    tryCatch({
      DBI::dbWriteTable(db, table_id, donnees, overwrite = TRUE)
      DBI::dbExecute(db, paste0(
        "INSERT INTO suivi.actualisations ",
        "(jeu, date_import, date_controle, md5, etag, modification) ",
        "VALUES (?, current_timestamp, current_timestamp, ?, ?, ?) ",
        "ON CONFLICT (jeu) DO UPDATE SET date_import = excluded.date_import, ",
        "date_controle = excluded.date_controle, md5 = excluded.md5, ",
        "etag = excluded.etag, modification = excluded.modification"),
        params = list(jeu, ligne$md5[[1L]], ligne$etag[[1L]], ligne$modification[[1L]]))
      DBI::dbCommit(db)
    }, error = function(e) {
      DBI::dbRollback(db)
      stop(e)
    })
    journal(toupper(nom), " : ", nrow(donnees), " lignes importees")
  }
  invisible(TRUE)
}
