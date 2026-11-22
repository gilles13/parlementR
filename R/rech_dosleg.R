#' Lister tous les dossiers legislatifs de l'Assemblee nationale
#'
#' Inventaire exhaustif des dossiers presents dans la base DuckDB locale.
#' Pour rechercher un sujet, utiliser `rech_dosleg_an()`.
#' @param db Connexion DuckDB facultative ; NULL utilise `connexion()`.
#' @return Tibble avec uid, titre, procedure et legislature.
#' @export
rech_dosleg = function(db = NULL) {
  db = .connexion_valide(db)
  if (!DBI::dbExistsTable(db, DBI::Id(schema = "an_dossiers", table = "dossiers")))
    stop("Importer les dossiers de l'Assemblee nationale")
  .db_get_tibble(db, "
    SELECT
      uid,
      json_extract_string(contenu, '$.dossierParlementaire.titreDossier.titre') AS titre,
      json_extract_string(contenu, '$.dossierParlementaire.procedureParlementaire.libelle') AS procedure,
      json_extract_string(contenu, '$.dossierParlementaire.legislature') AS legislature
    FROM an_dossiers.dossiers
    ORDER BY legislature DESC NULLS LAST, titre, uid
  ")
}
