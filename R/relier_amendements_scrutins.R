#' Relier des amendements aux scrutins publics de l'Assemblee nationale
#'
#' Liaison locale exclusivement par les UID officiels declares dans
#' objet.referenceLegislative.amendementsRefs.amendementRef du scrutin.
#' Les scrutins sans reference directe ne sont pas attribues par conjecture.
#' @param amendements Tibble retourne par rechercher_amendements(), ou vecteur
#'   d'UID officiels d'amendements.
#' @param db Connexion DuckDB locale.
#' @param groupes Si TRUE, ne retourner que les scrutins effectivement lies,
#'   avec une ligne par groupe. Les amendements sans scrutin sont exclus.
#'   Par defaut FALSE : conserver tous les amendements et leur statut.
#' @return Tibble contenant uid_amendement, uid_scrutin, statut_liaison et,
#'   si demande, les positions des groupes.
#' @export
relier_amendements_scrutins = function(amendements, db = NULL, groupes = FALSE) {
  db = .connexion_valide(db)
  ids = if (is.data.frame(amendements)) amendements$uid else amendements
  if (!is.character(ids) || anyNA(ids) || any(!grepl("^AMANR", ids)))
    stop("'amendements' doit contenir des UID officiels d'amendements")
  if (!is.logical(groupes) || length(groupes) != 1L || is.na(groupes))
    stop("'groupes' doit etre TRUE ou FALSE")
  ids = unique(ids)
  if (!length(ids)) return(tibble::tibble(uid_amendement = character(),
    uid_scrutin = character(), statut_liaison = character()))
  requis = c("an_amendements.amendements", "an_votes.scrutins")
  if (groupes) requis = c(requis, "an_historique.organes")
  ok = vapply(strsplit(requis, ".", fixed = TRUE), function(x)
    DBI::dbExistsTable(db, DBI::Id(schema = x[1L], table = x[2L])), logical(1))
  if (!all(ok)) stop("Tables absentes : ", paste(requis[!ok], collapse = ", "))
  if (groupes && !DBI::dbExistsTable(db, DBI::Id(schema = "an_votes", table = "votes_groupes")))
    stop("Vue an_votes.votes_groupes absente : reimporter les scrutins")
  # La seule liaison validee ici est la reference d'UID portee par le scrutin.
  # Aucun rapprochement par numero, seance, auteur ou titre n'est accepte.
  valeurs = paste(as.character(DBI::dbQuoteString(db, ids)), collapse = ", ")
  sql = paste0("WITH a AS (
    SELECT uid AS uid_amendement
    FROM an_amendements.amendements WHERE uid IN (", valeurs, ")
  ), s AS (
    SELECT uid AS uid_scrutin,
      json_extract_string(contenu, '$.scrutin.numero') AS numero_scrutin,
      json_extract_string(contenu, '$.scrutin.dateScrutin') AS date_scrutin,
      json_extract_string(contenu, '$.scrutin.titre') AS objet,
      json_extract(contenu,
        '$.scrutin.objet.referenceLegislative.amendementsRefs.amendementRef') AS refs
    FROM an_votes.scrutins
  ), refs AS (
    SELECT s.uid_scrutin, s.numero_scrutin, s.date_scrutin, s.objet,
      CASE WHEN json_type(j.value) = 'VARCHAR'
        THEN json_extract_string(j.value, '$')
        ELSE NULL END AS uid_amendement
    FROM s, LATERAL json_each(
      CASE WHEN json_type(s.refs) = 'ARRAY' THEN s.refs
        WHEN json_type(s.refs) = 'VARCHAR' THEN json_array(s.refs)
        ELSE '[]'::JSON END
    ) AS j
  ), liens AS (
    SELECT DISTINCT a.uid_amendement, r.uid_scrutin, r.numero_scrutin,
      r.date_scrutin, r.objet
    FROM a LEFT JOIN refs r ON a.uid_amendement = r.uid_amendement
  ) SELECT uid_amendement, uid_scrutin, numero_scrutin, date_scrutin, objet,
    CASE WHEN uid_scrutin IS NULL THEN 'reference_directe_absente'
      ELSE 'reference_directe' END AS statut_liaison
    FROM liens ORDER BY uid_amendement, date_scrutin, uid_scrutin")
  d = tibble::as_tibble(DBI::dbGetQuery(db, sql))
  # Rendre visibles les UID demandes qui ne sont pas dans la table locale.
  manquants = setdiff(ids, d$uid_amendement)
  if (length(manquants)) d = dplyr::bind_rows(d, tibble::tibble(
    uid_amendement = manquants, uid_scrutin = NA_character_,
    numero_scrutin = NA_character_, date_scrutin = NA_character_,
    objet = NA_character_, statut_liaison = "amendement_absent"))
  if (!groupes) return(d)
  # Une absence de scrutin n'est pas une observation de vote.
  # Ne pas joindre les groupes aux lignes sans scrutin : cela produirait
  # des lignes avec des positions et des effectifs manquants.
  d = d[!is.na(d$uid_scrutin), , drop = FALSE]
  if (!nrow(d)) {
    d$groupe_uid = character()
    d$groupe = character()
    d$position = character()
    d$pour = integer()
    d$contre = integer()
    d$abstentions = integer()
    d$non_votants = integer()
    return(d)
  }
  g = .db_get_tibble(db, "SELECT v.scrutin_uid AS uid_scrutin,
    v.groupe_uid,
    coalesce(json_extract_string(o.contenu, '$.organe.libelleAbrege'),
             json_extract_string(o.contenu, '$.organe.libelleAbrev'),
             json_extract_string(o.contenu, '$.organe.libelle')) AS groupe,
    v.position, v.pour, v.contre, v.abstentions, v.non_votants
    FROM an_votes.votes_groupes v
    LEFT JOIN an_historique.organes o ON o.uid = v.groupe_uid")
  dplyr::left_join(d, g, by = "uid_scrutin")
}
