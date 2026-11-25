
# Migration locale des anciens schemas AMO, sans telechargement.
.migrer_schemas_amo = function(db) {
  correspondances = c(an_deputes = "an_amo10",
                      an_historique = "an_amo30")
  schemas = DBI::dbGetQuery(db,
    "SELECT schema_name FROM information_schema.schemata")$schema_name
  anciens = names(correspondances)[names(correspondances) %in% schemas]
  if (!length(anciens)) return(invisible(FALSE))

  tables = DBI::dbGetQuery(db, "
    SELECT table_schema, table_name
    FROM information_schema.tables
    WHERE table_type = 'BASE TABLE'
      AND table_schema IN ('an_deputes', 'an_historique')
    ORDER BY table_schema, table_name")
  if (!nrow(tables)) return(invisible(FALSE))
  if (any(!tables$table_name %in% c("acteurs", "organes", "deports")))
    stop("Migration AMO interrompue : tables inattendues dans les anciens schemas.",
         call. = FALSE)

  DBI::dbBegin(db)
  tryCatch({
    for (ancien in unique(tables$table_schema)) {
      nouveau = unname(correspondances[[ancien]])
      DBI::dbExecute(db, paste("CREATE SCHEMA IF NOT EXISTS", nouveau))
      for (table in tables$table_name[tables$table_schema == ancien]) {
        destination = paste0(nouveau, ".", table)
        source = paste0(ancien, ".", table)
        if (DBI::dbExistsTable(db, DBI::Id(schema = nouveau, table = table)))
          stop("Migration AMO interrompue : destination deja presente : ",
               destination, call. = FALSE)
        DBI::dbExecute(db, paste("CREATE TABLE", destination,
                                 "AS SELECT * FROM", source))
        nombres = DBI::dbGetQuery(db, paste0(
          "SELECT (SELECT count(*) FROM ", source, ") AS avant, ",
          "(SELECT count(*) FROM ", destination, ") AS apres"))
        if (!identical(nombres$avant, nombres$apres))
          stop("Migration AMO : effectifs differents pour ", source,
               call. = FALSE)
        DBI::dbExecute(db, paste("DROP TABLE", source))
      }
      DBI::dbExecute(db, paste("DROP SCHEMA", ancien))
    }
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  invisible(TRUE)
}

# Connexion DuckDB geree par parlementR, ouverte a la demande.
.connexions = new.env(parent = emptyenv())

#' Ouvrir ou reutiliser la connexion DuckDB
#'
#' @param chemin Chemin de la base ; NULL utilise la base par defaut.
#' @param creer Creer la base si elle est absente (reserve a l'initialisation).
#' @return Une connexion DuckDB valide.
#' @export
connexion = function(chemin = NULL, creer = FALSE) {
  if (is.null(chemin)) {
    chemin = file.path(tools::R_user_dir("parlementR", "data"),
                       "parlementR.duckdb")
  }
  if (!is.logical(creer) || length(creer) != 1L || is.na(creer))
    stop("'creer' doit etre TRUE ou FALSE")
  if (!is.character(chemin) || length(chemin) != 1L ||
      is.na(chemin) || !nzchar(chemin)) stop("Chemin de base invalide")
  chemin = normalizePath(path.expand(chemin), mustWork = FALSE)

  if (exists("db", envir = .connexions, inherits = FALSE)) {
    db = .connexions$db
    if (isTRUE(tryCatch(DBI::dbIsValid(db), error = function(e) FALSE))) {
      if (identical(.connexions$chemin, chemin)) {
        .migrer_schemas_amo(db)
        .initialiser_telechargements(db)
        return(db)
      }
      stop("Une connexion vers une autre base est deja ouverte. ",
           "Appeler fermer_base() avant de changer de base.")
    }
    rm(list = intersect(c("db", "chemin"), ls(.connexions)),
       envir = .connexions)
  }
  if (!file.exists(chemin)) {
    if (!creer)
      stop("Base DuckDB introuvable : ", chemin,
           "
Executer actualiser() pour initialiser la base.")
    dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(dirname(chemin)))
      stop("Impossible de creer le repertoire de la base : ", dirname(chemin))
  }
  db = DBI::dbConnect(duckdb::duckdb(), dbdir = chemin)
  # Une extension DuckDB est installee pour une version precise du moteur.
  # Apres une mise a jour, une installation ponctuelle peut etre necessaire.
  erreur_icu = tryCatch({
    DBI::dbExecute(db, "LOAD icu")
    NULL
  }, error = function(e) e)

  if (!is.null(erreur_icu)) {
    erreur_icu = tryCatch({
      DBI::dbExecute(db, "INSTALL icu")
      DBI::dbExecute(db, "LOAD icu")
      NULL
    }, error = function(e) e)
  }

  if (!is.null(erreur_icu)) {
    DBI::dbDisconnect(db, shutdown = TRUE)
    stop("Impossible de charger ou d'installer l'extension DuckDB 'icu'. ",
         "Verifier la connexion reseau, puis reessayer connexion(). ",
         "Detail : ", conditionMessage(erreur_icu), call. = FALSE)
  }
  .migrer_schemas_amo(db)
  .initialiser_telechargements(db)
  .connexions$db = db
  .connexions$chemin = chemin
  db
}

#' Fermer la connexion geree par parlementR
#' @export
fermer_base = function() {
  if (exists("db", envir = .connexions, inherits = FALSE)) {
    db = .connexions$db
    if (isTRUE(tryCatch(DBI::dbIsValid(db), error = function(e) FALSE)))
      DBI::dbDisconnect(db, shutdown = TRUE)
  }
  rm(list = intersect(c("db", "chemin"), ls(.connexions)),
     envir = .connexions)
  invisible(TRUE)
}

# Verifie aussi les connexions explicites, sans les prendre en charge.
.connexion_valide = function(db = NULL) {
  if (is.null(db)) return(connexion())
  if (!inherits(db, "DBIConnection") ||
      !isTRUE(tryCatch(DBI::dbIsValid(db), error = function(e) FALSE)))
    stop("Connexion DBI invalide ou fermee")
  db
}
