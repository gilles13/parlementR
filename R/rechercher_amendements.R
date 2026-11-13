#' Rechercher les amendements de l'Assemblee nationale en local
#'
#' Les recherches interrogent uniquement DuckDB. Les noms d'auteurs et les
#' groupes sont des informations de presentation, jamais des cles de jointure.
#' @param db Connexion DuckDB locale.
#' @param sujet Expression recherchee dans le dispositif ou l'expose.
#' @param auteur Expression recherchee dans le nom de l'auteur.
#' @param groupe Expression recherchee dans le libelle ou l'abreviation du groupe.
#' @param texte Expression recherchee dans le titre du texte legislatif.
#' @param date_debut,date_fin Dates inclusives au format YYYY-MM-DD.
#' @param etat,sort Expressions recherchees dans les etats et sorts officiels.
#' @param limite Nombre maximal de resultats.
#' @param colonnes `"complet"` (defaut) ou `"identification"`.
#' @return Un tibble avec l'UID officiel de chaque amendement.
#' @export
rechercher_amendements = function(db = NULL, sujet = NULL, auteur = NULL,
                                  groupe = NULL, texte = NULL,
                                  date_debut = NULL, date_fin = NULL,
                                  etat = NULL, sort = NULL, limite = 100L,
                                  colonnes = c("complet", "identification")) {
  db = .connexion_valide(db)
  colonnes = match.arg(colonnes)
  if (length(limite) != 1L || is.na(limite) || !is.finite(limite) ||
      limite < 1 || limite != as.integer(limite)) stop("'limite' doit etre un entier positif")
  filtres = list(sujet = sujet, auteur = auteur, groupe = groupe,
                 texte = texte, etat = etat, sort = sort)
  for (x in filtres)
    if (!is.null(x) && (!is.character(x) || length(x) != 1L ||
                        is.na(x) || !nzchar(trimws(x))))
      stop("Les filtres doivent etre des chaines non vides")
  for (x in list(date_debut, date_fin))
    if (!is.null(x) && (length(x) != 1L || is.na(x) ||
                        is.na(as.Date(as.character(x), format = "%Y-%m-%d"))))
      stop("Date invalide (YYYY-MM-DD)")
  sql = paste0(
    "WITH a AS (SELECT uid, ",
    "json_extract_string(contenu, '$.amendement.identification.numeroLong') numero, ",
    "json_extract_string(contenu, '$.amendement.signataires.auteur.acteurRef') auteur_ref, ",
    "json_extract_string(contenu, '$.amendement.signataires.auteur.groupePolitiqueRef') groupe_ref, ",
    "json_extract_string(contenu, '$.amendement.texteLegislatifRef') texte_ref, ",
    "json_extract_string(contenu, '$.amendement.pointeurFragmentTexte.division.articleDesignation') article, ",
    "json_extract_string(contenu, '$.amendement.cycleDeVie.dateDepot') date_depot, ",
    "json_extract_string(contenu, '$.amendement.cycleDeVie.etatDesTraitements.etat.libelle') etat, ",
    "json_extract_string(contenu, '$.amendement.cycleDeVie.sort') sort, ",
    "json_extract_string(contenu, '$.amendement.corps.contenuAuteur.dispositif') dispositif, ",
    "json_extract_string(contenu, '$.amendement.corps.contenuAuteur.exposeSommaire') expose ",
    "FROM an_amendements.amendements), ",
    "acteurs AS (SELECT uid, ",
    "trim(concat_ws(' ', json_extract_string(contenu, '$.acteur.etatCivil.ident.prenom'), ",
    "json_extract_string(contenu, '$.acteur.etatCivil.ident.nom'))) auteur ",
    "FROM an_historique.acteurs), ",
    "groupes AS (SELECT uid, ",
    "json_extract_string(contenu, '$.organe.libelle') groupe_libelle, ",
    "coalesce(json_extract_string(contenu, '$.organe.libelleAbrege'), ",
    "json_extract_string(contenu, '$.organe.libelleAbrev')) groupe ",
    "FROM an_historique.organes), ",
    "textes AS (SELECT uid, coalesce(",
    "json_extract_string(contenu, '$.document.titres.titrePrincipalCourt'), ",
    "json_extract_string(contenu, '$.document.titres.titrePrincipal')) texte ",
    "FROM an_dossiers.documents) ",
    "SELECT a.uid, a.numero, ac.auteur, a.auteur_ref, g.groupe, ",
    "g.groupe_libelle, a.groupe_ref, a.date_depot, a.etat, a.sort, ",
    "a.texte_ref, t.texte, a.article, a.dispositif, a.expose ",
    "FROM a LEFT JOIN acteurs ac ON ac.uid = a.auteur_ref ",
    "LEFT JOIN groupes g ON g.uid = a.groupe_ref ",
    "LEFT JOIN textes t ON t.uid = a.texte_ref WHERE TRUE")
  params = list()
  ajouter = function(condition, valeur) {
    sql <<- paste0(sql, " AND ", condition)
    params <<- c(params, list(valeur))
  }
  if (!is.null(sujet)) {
    sql = paste0(sql, " AND (a.dispositif ILIKE ? OR a.expose ILIKE ?)")
    params = c(params, rep(list(paste0("%", sujet, "%")), 2L))
  }
  if (!is.null(auteur)) ajouter("ac.auteur ILIKE ?", paste0("%", auteur, "%"))
  if (!is.null(groupe)) {
    sql = paste0(sql, " AND (g.groupe ILIKE ? OR g.groupe_libelle ILIKE ?)")
    params = c(params, rep(list(paste0("%", groupe, "%")), 2L))
  }
  if (!is.null(texte)) ajouter("t.texte ILIKE ?", paste0("%", texte, "%"))
  if (!is.null(date_debut)) ajouter("substr(a.date_depot, 1, 10) >= ?", as.character(date_debut))
  if (!is.null(date_fin)) ajouter("substr(a.date_depot, 1, 10) <= ?", as.character(date_fin))
  if (!is.null(etat)) ajouter("a.etat ILIKE ?", paste0("%", etat, "%"))
  if (!is.null(sort)) ajouter("a.sort ILIKE ?", paste0("%", sort, "%"))
  sql = paste0(sql, " ORDER BY a.date_depot DESC NULLS LAST, a.uid LIMIT ?")
  d = tibble::as_tibble(DBI::dbGetQuery(db, sql,
                    params = c(params, list(as.integer(limite)))))
  if (colonnes == "identification")
    d = dplyr::select(d, dplyr::any_of(c("uid", "numero", "auteur", "auteur_ref",
                         "texte", "texte_ref", "article", "date_depot", "sort")))
  d
}
