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
    if (!DBI::dbExistsTable(db, DBI::Id(schema = "suivi", table = "tables_attendues")))
      return(FALSE)
    manifeste = DBI::dbGetQuery(db,
      "SELECT table_name FROM suivi.tables_attendues WHERE jeu = ? ORDER BY table_name",
      params = list(jeu))$table_name
    return(length(manifeste) > 0L && identical(tables, manifeste))
  }
  all(vapply(attendu, function(nom) {
    morceaux = strsplit(nom, ".", fixed = TRUE)[[1L]]
    length(morceaux) == 2L && DBI::dbExistsTable(db,
      DBI::Id(schema = morceaux[[1L]], table = morceaux[[2L]]))
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
    "UPDATE suivi.actualisations SET date_controle = current_timestamp, ",
    "etag = coalesce(?, etag), modification = coalesce(?, modification) WHERE jeu = ?"),
    params = list(etag, modification, jeu))
}
