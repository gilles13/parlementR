# Extraction locale des mandats gouvernementaux AMO30.
# Une ligne par mandat ministeriel et gouvernement de rattachement.
.mandats_gouvernement = function(db) {
  if (!DBI::dbExistsTable(db, DBI::Id(schema = "an_historique", table = "acteurs")) ||
      !DBI::dbExistsTable(db, DBI::Id(schema = "an_historique", table = "organes"))) {
    stop("Tables an_historique.acteurs/organes absentes : actualiser(jeux = 'historique').",
         call. = FALSE)
  }
  sql = "
    WITH mandats AS (
      SELECT a.uid AS acteur_uid,
             a.contenu ->> '$.acteur.etatCivil.ident.prenom' AS prenom,
             a.contenu ->> '$.acteur.etatCivil.ident.nom' AS nom,
             m.value ->> '$.uid' AS mandat_uid,
             m.value ->> '$.typeOrgane' AS type_organe,
             m.value ->> '$.@xsi:type' AS type_mandat,
             m.value ->> '$.infosQualite.libQualite' AS fonction,
             TRY_CAST(m.value ->> '$.preseance' AS INTEGER) AS preseance,
             m.value ->> '$.organes.organeRef' AS organe_uid,
             TRY_CAST(m.value ->> '$.dateDebut' AS DATE) AS debut,
             TRY_CAST(m.value ->> '$.dateFin' AS DATE) AS fin
      FROM an_historique.acteurs a,
           json_each(a.contenu, '$.acteur.mandats.mandat') m
      WHERE (m.value ->> '$.typeOrgane') IN ('GOUVERNEMENT', 'MINISTERE')
        AND (m.value ->> '$.@xsi:type') = 'MandatSimple_Type'
    ),
    gouvernements AS (
      SELECT m.acteur_uid, m.organe_uid AS gouvernement_uid,
             m.debut AS debut_mandat_gouvernement,
             m.fin AS fin_mandat_gouvernement,
             TRY_CAST(o.contenu ->> '$.organe.viMoDe.dateDebut' AS DATE) AS debut_officiel,
             TRY_CAST(o.contenu ->> '$.organe.viMoDe.dateFin' AS DATE) AS fin_officielle
      FROM mandats m
      JOIN an_historique.organes o ON o.uid = m.organe_uid
      WHERE m.type_organe = 'GOUVERNEMENT'
    ),
    ministeres AS (
      SELECT m.*, o.contenu ->> '$.organe.libelle' AS portefeuille,
             o.contenu ->> '$.organe.libelleAbrege' AS portefeuille_abrege,
             TRY_CAST(o.contenu ->> '$.organe.preseance' AS INTEGER) AS preseance_ministere
      FROM mandats m
      LEFT JOIN an_historique.organes o ON o.uid = m.organe_uid
      WHERE m.type_organe = 'MINISTERE'
    )
    , candidats AS (
      SELECT m.*, g.gouvernement_uid,
             g.debut_officiel AS debut_gouvernement,
             g.fin_officielle AS fin_gouvernement,
             ROW_NUMBER() OVER (
               PARTITION BY m.mandat_uid
               ORDER BY
                 CASE WHEN m.debut >= g.debut_officiel
                            AND m.debut <= COALESCE(g.fin_officielle, DATE '9999-12-31')
                      THEN 0 ELSE 1 END,
                 LEAST(COALESCE(m.fin, DATE '9999-12-31'),
                       COALESCE(g.fin_officielle, DATE '9999-12-31'))
                   - GREATEST(m.debut, g.debut_officiel) DESC,
                 g.debut_officiel DESC, g.gouvernement_uid
             ) AS rang
      FROM ministeres m
      LEFT JOIN gouvernements g
        ON g.acteur_uid = m.acteur_uid
       AND m.debut <= COALESCE(g.fin_mandat_gouvernement, DATE '9999-12-31')
       AND g.debut_mandat_gouvernement <= COALESCE(m.fin, DATE '9999-12-31')
       AND m.debut <= COALESCE(g.fin_officielle, DATE '9999-12-31')
       AND g.debut_officiel <= COALESCE(m.fin, DATE '9999-12-31')
    )
    SELECT c.acteur_uid, c.prenom, c.nom, c.mandat_uid,
           c.fonction, c.preseance AS preseance_qualite,
           c.preseance_ministere AS preseance, c.organe_uid AS ministere_uid,
           c.portefeuille, c.portefeuille_abrege,
           c.debut, c.fin, c.gouvernement_uid,
           o.contenu ->> '$.organe.libelleAbrege' AS gouvernement,
           c.debut_gouvernement, c.fin_gouvernement
    FROM candidats c
    LEFT JOIN an_historique.organes o ON o.uid = c.gouvernement_uid
    WHERE c.rang = 1
    ORDER BY c.debut, c.nom, c.prenom, c.mandat_uid"
  donnees = .db_get_tibble(db, sql)
  # Priorite au gouvernement officiellement actif au debut du mandat.
  # En cas de chevauchement, retenir la plus longue intersection des periodes.
  # Un mandat sans rattachement reste une erreur explicite.
  if (anyNA(donnees$gouvernement_uid))
    stop("Mandats ministeriels sans gouvernement correspondant.", call. = FALSE)
  donnees
}

#' Catalogue historique des gouvernements
#'
#' Utilise les organismes et mandats AMO30 deja presents dans DuckDB.
#' Les effectifs comptent les personnes distinctes ayant appartenu au gouvernement
#' pendant sa duree, et non les seuls membres presents a une date donnee.
#' @param db Connexion DuckDB facultative ; NULL utilise connexion().
#' @return Tibble : identifiant, gouvernement, dates, Premier ministre et effectif.
#' @export
cat_gvts = function(db = NULL) {
  db = .connexion_valide(db)
  if (!DBI::dbExistsTable(db, DBI::Id(schema = "an_historique", table = "organes")))
    stop("Table an_historique.organes absente.", call. = FALSE)
  gouvernements = .db_get_tibble(db, "
    SELECT uid AS gouvernement_uid,
           contenu ->> '$.organe.libelleAbrege' AS gouvernement,
           TRY_CAST(contenu ->> '$.organe.viMoDe.dateDebut' AS DATE) AS debut,
           TRY_CAST(contenu ->> '$.organe.viMoDe.dateFin' AS DATE) AS fin
    FROM an_historique.organes
    WHERE (contenu ->> '$.organe.codeType') = 'GOUVERNEMENT'
    ORDER BY debut")
  membres = .mandats_gouvernement(db)
  stats = dplyr::summarise(
    dplyr::group_by(membres, gouvernement_uid),
    effectif = dplyr::n_distinct(acteur_uid),
    premier_ministre = paste(sort(unique(paste(prenom[fonction == 'Premier ministre'],
                                               nom[fonction == 'Premier ministre']))),
                            collapse = ', '), .groups = 'drop')
  dplyr::left_join(gouvernements, stats, by = 'gouvernement_uid')
}

#' Composition historique ou datee d'un gouvernement
#'
#' @param gvt Identifiant PO ou libelle abrege du gouvernement.
#' @param date Date au format AAAA-MM-JJ ; NULL retourne tous les mandats du gouvernement.
#' @param db Connexion DuckDB facultative ; NULL utilise connexion().
#' @return Tibble des membres et de leurs fonctions ministerielles.
#'   `preseance` est le rang de l'organisme ministeriel (eventuellement NA) ;
#'   `preseance_qualite` est la categorie du mandat.
#' @export
cat_gvt = function(gvt, date = NULL, db = NULL) {
  if (!is.character(gvt) || length(gvt) != 1L ||
      is.na(gvt) || !nzchar(trimws(gvt)))
    stop("'gvt' doit etre un identifiant ou un libelle non vide.")
  if (!is.null(date)) {
    if (length(date) != 1L || is.na(date)) stop("Date invalide.")
    date = as.Date(date)
    if (is.na(date)) stop("Date invalide.")
  }
  db = .connexion_valide(db)
  donnees = .mandats_gouvernement(db)
  selection = donnees$gouvernement_uid == gvt |
    toupper(donnees$gouvernement) == toupper(gvt)
  selection[is.na(selection)] = FALSE
  if (!any(selection)) stop("Gouvernement inconnu : ", gvt, call. = FALSE)
  donnees = donnees[selection, , drop = FALSE]
  if (!is.null(date)) {
    dans_mandat = donnees$debut <= date & (is.na(donnees$fin) | donnees$fin >= date)
    dans_gouvernement = donnees$debut_gouvernement <= date &
      (is.na(donnees$fin_gouvernement) | donnees$fin_gouvernement >= date)
    selection = dans_mandat & dans_gouvernement
    selection[is.na(selection)] = FALSE
    donnees = donnees[selection, , drop = FALSE]
  }
  donnees = dplyr::select(donnees, gouvernement_uid, gouvernement,
                          acteur_uid, prenom, nom, fonction, preseance, preseance_qualite,
                          portefeuille_abrege, portefeuille,
                          debut, fin, mandat_uid, ministere_uid)
  # Ordre des organismes ministeriels lorsqu'il est disponible.
  # Sinon : categories de mandats, puis alphabetique (pas protocolaire).
  dplyr::arrange(donnees, is.na(preseance), preseance,
                 preseance_qualite, nom, prenom, debut)
}
