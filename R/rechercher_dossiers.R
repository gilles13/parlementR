#' Rechercher des dossiers parlementaires dans la base locale
#'
#' @param sujet Expression recherchee dans les titres des dossiers et documents.
#' @param db Connexion DuckDB ; NULL utilise `connexion()`.
#' @param limite Nombre maximum de dossiers renvoyes.
#' @return Un tibble de dossiers, avec la date du premier document rattache et l'origine de la correspondance.
#' @export
rechercher_dossiers = function(sujet, db = NULL, limite = 50L) {
  db = .connexion_valide(db)
  if (!is.character(sujet) || length(sujet) != 1L || is.na(sujet) ||
      !nzchar(trimws(sujet))) stop("'sujet' doit etre une expression non vide")
  if (length(limite) != 1L || is.na(limite) || !is.numeric(limite) ||
      limite < 1 || limite != as.integer(limite))
    stop("'limite' doit etre un entier positif")
  tables = c("dossiers", "documents")
  if (!all(vapply(tables, function(x)
    DBI::dbExistsTable(db, DBI::Id(schema = "an_dossiers", table = x)), logical(1))))
    stop("Importer les dossiers et documents de l'Assemblee nationale")
  motif = paste0("%", trimws(sujet), "%")
  sql = "
    WITH d AS (
      SELECT uid,
        json_extract_string(contenu, '$.dossierParlementaire.titreDossier.titre') AS titre,
        json_extract_string(contenu, '$.dossierParlementaire.procedureParlementaire.libelle') AS procedure,
        json_extract_string(contenu, '$.dossierParlementaire.legislature') AS legislature
      FROM an_dossiers.dossiers
    ), r AS (
      SELECT d.*,
        CASE WHEN d.titre ILIKE ? THEN 'Titre du dossier'
             ELSE 'Titre d\u2019un document' END AS correspondance,
        CASE WHEN d.titre ILIKE ? THEN 0 ELSE 1 END AS rang
      FROM d
      WHERE d.titre ILIKE ? OR EXISTS (
        SELECT 1 FROM an_dossiers.documents x
        WHERE json_extract_string(x.contenu, '$.document.dossierRef') = d.uid
          AND json_extract_string(x.contenu, '$.document.titres.titrePrincipal') ILIKE ?
      )
    )
    , dates AS (
      SELECT json_extract_string(x.contenu, '$.document.dossierRef') AS dossier_uid,
        MIN(COALESCE(
          NULLIF(substr(json_extract_string(x.contenu,
            '$.document.cycleDeVie.chrono.dateDepot'), 1, 10), ''),
          NULLIF(substr(json_extract_string(x.contenu,
            '$.document.cycleDeVie.chrono.datePublication'), 1, 10), ''),
          NULLIF(substr(json_extract_string(x.contenu,
            '$.document.cycleDeVie.chrono.dateCreation'), 1, 10), '')
        )) AS date
      FROM an_dossiers.documents x
      JOIN r s ON s.uid = json_extract_string(x.contenu, '$.document.dossierRef')
      GROUP BY 1
    )
    SELECT s.uid, s.titre, s.procedure, s.legislature,
      s.correspondance, dates.date
    FROM r s LEFT JOIN dates ON dates.dossier_uid = s.uid
    ORDER BY dates.date DESC NULLS LAST, s.rang, s.titre, s.uid
    LIMIT ?"
  .db_get_tibble(db, sql, params = list(motif, motif, motif, motif, as.integer(limite)))
}
