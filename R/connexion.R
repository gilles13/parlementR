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
      if (identical(.connexions$chemin, chemin)) return(db)
      stop("Une connexion vers une autre base est deja ouverte. ",
           "Appeler fermer_base() avant de changer de base.")
    }
    rm(list = intersect(c("db", "chemin"), ls(.connexions)),
       envir = .connexions)
  }
  if (!file.exists(chemin)) {
    if (!creer)
      stop("Base DuckDB introuvable : ", chemin,
           "\nExecuter actualiser() pour initialiser la base.")
    dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(dirname(chemin)))
      stop("Impossible de creer le repertoire de la base : ", dirname(chemin))
  }
  db = DBI::dbConnect(duckdb::duckdb(), dbdir = chemin)
  tryCatch(
    DBI::dbExecute(db, "LOAD icu"),
    error = function(e) {
      DBI::dbDisconnect(db, shutdown = TRUE)
      stop("Extension DuckDB 'icu' indisponible pour cette version de DuckDB.\n",
           "Installer une fois l'extension avec une connexion DuckDB :\n",
           "  DBI::dbExecute(db, 'INSTALL icu')\n",
           "Puis relancer connexion().\n",
           "Detail : ", conditionMessage(e), call. = FALSE)
    }
  )
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
