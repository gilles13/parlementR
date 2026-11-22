# Reperes Wikidata structures : aucune donnee Wikidata n'est certifiee officielle.
# Les donnees officielles AMO30/JORF restent independantes.

.wd_requete = function(sparql, dossier, nom, actualiser) {
  dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  fichier = file.path(dossier, paste0(nom, '.json'))
  if (isTRUE(actualiser) || !file.exists(fichier)) {
    temporaire = tempfile(tmpdir = dossier, fileext = '.json')
    on.exit(unlink(temporaire), add = TRUE)
    url = paste0('https://query.wikidata.org/sparql?format=json&query=',
      utils::URLencode(sparql, reserved = TRUE))
    statut = system2('curl', c('-fL', '--retry', '3', '--connect-timeout', '30',
      '--max-time', '180', '--output', shQuote(temporaire), shQuote(url)))
    if (!identical(as.integer(statut), 0L)) stop('Telechargement Wikidata impossible : ', nom)
    invisible(.wd_lire(temporaire))
    if (!file.rename(temporaire, fichier)) stop('Impossible de conserver : ', fichier)
  }
  list(fichier = fichier, md5 = unname(tools::md5sum(fichier)), donnees = .wd_lire(fichier))
}

.wd_lire = function(fichier) {
  brut = jsonlite::fromJSON(fichier, simplifyVector = FALSE)
  if (is.null(brut$results$bindings) || is.null(brut$head$vars))
    stop('Reponse SPARQL invalide : ', fichier)
  champs = brut$head$vars
  lignes = lapply(brut$results$bindings, function(x) {
    stats::setNames(vapply(champs, function(n) {
      if (is.null(x[[n]]$value)) NA_character_ else as.character(x[[n]]$value)
    }, character(1)), champs)
  })
  if (!length(lignes)) return(tibble::as_tibble(stats::setNames(
    replicate(length(champs), character(), simplify = FALSE), champs)))
  tibble::as_tibble(as.data.frame(do.call(rbind, lignes), stringsAsFactors = FALSE))
}

#' Importer des reperes historiques structures depuis Wikidata
#'
#' Les postes et leurs titulaires sont lus dans Wikidata, sans extraction HTML.
#' La couverture n'est pas garantie exhaustive : les resultats sont des candidats
#' a rapprocher des actes officiels. Les JSON sont caches avec leur empreinte MD5.
#' @param db Connexion DuckDB ; NULL utilise connexion().
#' @param actualiser Reinterroger Wikidata ; FALSE relit le cache.
#' @param dossier Dossier de cache des reponses SPARQL.
#' @return Liste des tables importees et des controles.
#' @export
importer_referentiel_historique = function(db = NULL, actualiser = FALSE,
    dossier = file.path(tools::R_user_dir('parlementR', 'cache'), 'wikidata')) {
  db = if (is.null(db)) connexion() else db
  if (!DBI::dbIsValid(db)) stop('Connexion DuckDB invalide')
  if (!is.logical(actualiser) || length(actualiser) != 1L || is.na(actualiser))
    stop('actualiser doit etre TRUE ou FALSE')

  # Q15135541 est la classe des chefs de gouvernement francais,
  # utilisee dans les exemples documentes du service SPARQL Wikidata.
  # Les fonctions presidentielles sont reperees separement par libelle.
  postes = paste0(
    'SELECT DISTINCT ?poste ?posteLabel WHERE { ',
    '{ ?poste (wdt:P31|wdt:P279*) wd:Q15135541. } UNION ',
    '{ ?poste rdfs:label "pr\u00e9sident de la R\u00e9publique fran\u00e7aise"@fr. } ',
    'SERVICE wikibase:label { bd:serviceParam wikibase:language "fr,en". } ',
    'FILTER(STRSTARTS(STR(?poste), "http://www.wikidata.org/entity/Q")) }')
  p = .wd_requete(postes, dossier, 'postes', actualiser)
  if (!nrow(p$donnees)) stop('Aucun poste historique trouve dans Wikidata')
  qids = unique(sub('^.*/', '', p$donnees$poste))
  if (any(!grepl('^Q[0-9]+$', qids))) stop('Identifiant Wikidata invalide')
  valeurs = paste(paste0('wd:', qids), collapse = ' ')
  mandats = paste0(
    'SELECT ?personne ?personneLabel ?poste ?debut ?fin ?declaration WHERE { ',
    'VALUES ?poste { ', valeurs, ' } ',
    '?personne p:P39 ?declaration. ?declaration ps:P39 ?poste. ',
    'OPTIONAL { ?declaration pq:P580 ?debut. } ',
    'OPTIONAL { ?declaration pq:P582 ?fin. } ',
    'SERVICE wikibase:label { bd:serviceParam wikibase:language "fr,en". } ',
    '}')
  m = .wd_requete(mandats, dossier, 'mandats', actualiser)
  donnees = m$donnees
  if (!nrow(donnees)) stop('Aucun mandat retourne par Wikidata')
  donnees = dplyr::mutate(donnees,
    personne_qid = sub('^.*/', '', .data$personne),
    poste_qid = sub('^.*/', '', .data$poste),
    source = 'Wikidata', statut = 'a_verifier_officiellement')
  # Ne jamais convertir les dates Wikidata en dates R : valeurs atypiques
  # et precision historique doivent rester visibles dans les donnees brutes.
  sources = tibble::tibble(
    jeu = c('postes', 'mandats'),
    url = 'https://query.wikidata.org/sparql',
    fichier = c(p$fichier, m$fichier),
    md5 = c(p$md5, m$md5),
    statut = 'reperage_non_officiel')

  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, 'CREATE SCHEMA IF NOT EXISTS hist_reperes')
    DBI::dbWriteTable(db, DBI::Id(schema = 'hist_reperes', table = 'postes'),
                      p$donnees, overwrite = TRUE)
    DBI::dbWriteTable(db, DBI::Id(schema = 'hist_reperes', table = 'mandats'),
                      donnees, overwrite = TRUE)
    DBI::dbWriteTable(db, DBI::Id(schema = 'hist_reperes', table = 'sources'),
                      sources, overwrite = TRUE)
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  controles = tibble::tibble(
    indicateur = c('postes', 'mandats', 'personnes', 'mandats_sans_debut'),
    valeur = c(nrow(p$donnees), nrow(donnees),
      dplyr::n_distinct(donnees$personne_qid), sum(is.na(donnees$debut))))
  list(postes = p$donnees, mandats = donnees, sources = sources,
       controles = controles)
}
