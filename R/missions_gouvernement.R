#' Catalogue des missions parlementaires aupres du Gouvernement
#'
#' Chaque ligne represente une relation entre un mandat de mission et un
#' organisme de rattachement. Un meme mandat peut donc apparaitre plusieurs
#' fois. L'objet de la mission provient du champ libelle du mandat AMO30.
#'
#' @param date Date AAAA-MM-JJ facultative ; NULL conserve tout l'historique.
#' @param db Connexion DuckDB facultative ; NULL utilise connexion().
#' @return Tibble des missions et de leurs organismes de rattachement.
#' @export
cat_missions_gvt = function(date = NULL, db = NULL) {
  if (!is.null(date)) {
    if (length(date) != 1L || is.na(date)) stop("Date invalide.", call. = FALSE)
    date = tryCatch(as.Date(date), error = function(e) NA_Date_)
    if (is.na(date)) stop("Date invalide.", call. = FALSE)
  }
  db = .connexion_valide(db)
  if (!DBI::dbExistsTable(db, DBI::Id(schema = "an_amo30", table = "acteurs")) ||
      !DBI::dbExistsTable(db, DBI::Id(schema = "an_amo30", table = "organes")))
    stop("Tables an_amo30.acteurs/organes absentes : actualiser(jeux = 'historique').",
         call. = FALSE)

  # organeRef est tantot une chaine, tantot un tableau JSON : les deux
  # formes sont normalisees en tableau avant de deplier les rattachements.
  sql = "
    WITH missions AS (
      SELECT a.uid AS acteur_uid,
             a.contenu ->> '$.acteur.etatCivil.ident.prenom' AS prenom,
             a.contenu ->> '$.acteur.etatCivil.ident.nom' AS nom,
             m.value ->> '$.uid' AS mandat_uid,
             m.value ->> '$.libelle' AS objet,
             m.value ->> '$.missionPrecedenteRef' AS mission_precedente,
             m.value ->> '$.missionSuivanteRef' AS mission_suivante,
             TRY_CAST(m.value ->> '$.dateDebut' AS DATE) AS debut,
             TRY_CAST(m.value ->> '$.dateFin' AS DATE) AS fin,
             CASE
               WHEN json_type(m.value, '$.organes.organeRef') = 'ARRAY'
                 THEN json_extract(m.value, '$.organes.organeRef')
               ELSE json_array(m.value ->> '$.organes.organeRef')
             END AS organismes
      FROM an_amo30.acteurs a,
           json_each(a.contenu, '$.acteur.mandats.mandat') m
      WHERE (m.value ->> '$.@xsi:type') = 'MandatMission_Type'
        AND (m.value ->> '$.typeOrgane') = 'MINISTERE'
    )
    SELECT m.mandat_uid, m.acteur_uid, m.prenom, m.nom,
           m.objet, m.debut, m.fin,
           m.mission_precedente, m.mission_suivante,
           json_extract_string(r.value, '$') AS ministere_uid,
           o.contenu ->> '$.organe.libelleAbrege' AS ministere_abrege,
           o.contenu ->> '$.organe.libelle' AS ministere
    FROM missions m
    LEFT JOIN LATERAL json_each(m.organismes) r ON TRUE
    LEFT JOIN an_amo30.organes o ON o.uid = json_extract_string(r.value, '$')
    ORDER BY m.debut DESC, m.mandat_uid, ministere_uid"
  donnees = .db_get_tibble(db, sql)
  if (!is.null(date)) {
    actif = !is.na(donnees$debut) & donnees$debut <= date &
      (is.na(donnees$fin) | donnees$fin >= date)
    donnees = donnees[actif, , drop = FALSE]
  }
  donnees
}
