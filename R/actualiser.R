#' Actualiser les donnees parlementaires
#'
#' @param db Connexion DBI vers DuckDB, deja ouverte.
#' @param jeux Jeux a importer ; actuellement uniquement "deputes".
#' @param cache Repertoire de cache persistant.
#' @param archive Chemin facultatif vers une archive AMO10 locale.
#' @param verbose Afficher les etapes (par defaut TRUE).
#' @export
actualiser = function(db, jeux = "deputes",
                      cache = tools::R_user_dir("parlementR", "cache"),
                      archive = NULL, verbose = TRUE) {
  if (!identical(jeux, "deputes")) {
    stop("Jeu non pris en charge : seul 'deputes' est disponible.")
  }
  .importer_deputes(db = db, cache = cache, archive = archive,
                    verbose = verbose)
  invisible(TRUE)
}
