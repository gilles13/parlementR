#' Rechercher des documents parlementaires dans les sources locales
#'
#' La recherche est litterale, insensible a la casse. Chaque resultat conserve
#' sa source, son type et son identifiant officiel. Les sources absentes sont
#' signalees dans l'attribut `sources_absentes`.
#' @param terme Expression a rechercher.
#' @param sources Sources parmi `ameli`, `dosleg`, `questions_senat`, `amendements_an`, `scrutins_an`.
#'   NULL interroge toutes les sources disponibles.
#' @param db Connexion DuckDB ; NULL utilise connexion().
#' @param limite Nombre maximum de resultats par source.
#' Le resultat conserve le sujet recherche dans l'attribut `sujet`.
#' @return Tibble de documents, de classe `parlementR_documents`.
#' @export
rech_docs = function(terme, sources = NULL, db = NULL, limite = 100L) {
  if (!is.character(terme) || length(terme) != 1L || is.na(terme) ||
      !nzchar(trimws(terme))) stop("'terme' doit etre une expression non vide")
  if (length(limite) != 1L || is.na(limite) || !is.numeric(limite) ||
      !is.finite(limite) || limite < 1 || limite != as.integer(limite))
    stop("'limite' doit etre un entier positif")
  db = .connexion_valide(db)
  disponibles = c("ameli", "dosleg", "questions_senat", "amendements_an", "scrutins_an")
  if (is.null(sources)) sources = disponibles
  if (!is.character(sources) || anyNA(sources) ||
      any(!sources %in% disponibles))
    stop("Sources possibles : ", paste(disponibles, collapse = ", "))
  sources = unique(sources)
  tables = list(ameli = c("sen_amendements.amd", "sen_amendements.txt_ameli"),
    dosleg = "sen_dosleg.loi", questions_senat = "sen_questions.tam_questions",
    amendements_an = "an_amendements.amendements",
    scrutins_an = "an_votes.scrutins")
  presente = function(nom) {
    x = strsplit(nom, ".", fixed = TRUE)[[1L]]
    DBI::dbExistsTable(db, DBI::Id(schema = x[1L], table = x[2L]))
  }
  absentes = sources[!vapply(sources, function(s)
    all(vapply(tables[[s]], presente, logical(1))), logical(1))]
  sources = setdiff(sources, absentes)
  vide = tibble::tibble(source = character(), type_document = character(),
    identifiant = character(), titre = character(), date = character(),
    contenu = character(), url = character(), dossier_id = character())
  if (!length(sources)) {
    attr(vide, "sujet") = terme
    attr(vide, "sources_interrogees") = character()
    attr(vide, "metadonnees_sources") = .metadonnees_documentaires(db, character())
    attr(vide, "sources_absentes") = absentes
    class(vide) = c("parlementR_documents", class(vide))
    return(vide)
  }
  fonctions = list(ameli = .rechercher_documents_ameli,
    dosleg = .rechercher_documents_dosleg,
    questions_senat = .rechercher_documents_questions_senat,
    amendements_an = .rechercher_documents_amendements_an,
    scrutins_an = .rechercher_documents_scrutins_an)
  resultats = lapply(sources, function(s) fonctions[[s]](db, terme, as.integer(limite)))
  resultat = dplyr::bind_rows(resultats)
  if (!nrow(resultat)) resultat = vide
  attr(resultat, "sujet") = terme
  attr(resultat, "sources_interrogees") = sources
  attr(resultat, "metadonnees_sources") = .metadonnees_documentaires(db, unique(resultat$source))
  attr(resultat, "sources_absentes") = absentes
  class(resultat) = c("parlementR_documents", class(resultat))
  if (nrow(resultat)) {
    resultat = resultat[order(is.na(resultat$date), resultat$date, resultat$source, resultat$identifiant), , drop = FALSE]
  }
  attr(resultat, "sujet") = terme
  resultat
}

# Le tri et le LIMIT portent uniquement sur les identifiants et dates.
# Les textes longs ne sont extraits qu'apres selection des documents.
.rechercher_documents_ameli = function(db, terme, limite) {
  tibble::as_tibble(DBI::dbGetQuery(db, "
    WITH selection AS (
      SELECT id, datdep
      FROM sen_amendements.amd
      WHERE strpos(lower(coalesce(obj, '')), lower(?)) > 0
         OR strpos(lower(coalesce(dis, '')), lower(?)) > 0
      ORDER BY datdep DESC NULLS LAST, id
      LIMIT ?
    )
    SELECT 'ameli' AS source, 'amendement' AS type_document,
      CAST(a.id AS VARCHAR) AS identifiant,
      t.int AS titre, CAST(a.datdep AS VARCHAR) AS date,
      concat_ws('\n', a.obj, a.dis) AS contenu,
      CAST(NULL AS VARCHAR) AS url, trim(t.doslegsignet) AS dossier_id
    FROM selection s
    JOIN sen_amendements.amd a ON a.id = s.id
    LEFT JOIN sen_amendements.txt_ameli t ON t.id = a.txtid",
    params = list(terme, terme, limite)))
}

.rechercher_documents_dosleg = function(db, terme, limite) {
  tibble::as_tibble(DBI::dbGetQuery(db, "
    WITH selection AS (
      SELECT loicod, proaccdat
      FROM sen_dosleg.loi
      WHERE strpos(lower(coalesce(loient, '')), lower(?)) > 0
         OR strpos(lower(coalesce(loitit, '')), lower(?)) > 0
         OR strpos(lower(coalesce(loiint, '')), lower(?)) > 0
         OR strpos(lower(coalesce(loiintori, '')), lower(?)) > 0
         OR strpos(lower(coalesce(objet, '')), lower(?)) > 0
      ORDER BY proaccdat DESC NULLS LAST, loicod
      LIMIT ?
    )
    SELECT 'dosleg' AS source, 'dossier legislatif' AS type_document,
      trim(l.loicod) AS identifiant, l.loient AS titre,
      CAST(l.proaccdat AS VARCHAR) AS date,
      concat_ws('\n', l.loitit, l.loiint, l.loiintori, l.objet) AS contenu,
      l.url_an AS url, trim(l.signet) AS dossier_id
    FROM selection s JOIN sen_dosleg.loi l ON l.loicod = s.loicod",
    params = c(rep(list(terme), 5L), list(limite))))
}

.rechercher_documents_questions_senat = function(db, terme, limite) {
  tibble::as_tibble(DBI::dbGetQuery(db, "
    WITH selection AS (
      SELECT id, datejodepot
      FROM sen_questions.tam_questions
      WHERE strpos(lower(coalesce(titre, '')), lower(?)) > 0
         OR strpos(lower(coalesce(rubrique, '')), lower(?)) > 0
         OR strpos(lower(coalesce(txtque, '')), lower(?)) > 0
      ORDER BY datejodepot DESC NULLS LAST, id
      LIMIT ?
    )
    SELECT 'questions_senat' AS source, 'question' AS type_document,
      CAST(q.id AS VARCHAR) AS identifiant, q.titre,
      CAST(q.datejodepot AS VARCHAR) AS date, q.txtque AS contenu,
      CAST(NULL AS VARCHAR) AS url, CAST(NULL AS VARCHAR) AS dossier_id
    FROM selection s JOIN sen_questions.tam_questions q ON q.id = s.id",
    params = list(terme, terme, terme, limite)))
}

.rechercher_documents_amendements_an = function(db, terme, limite) {
  tibble::as_tibble(DBI::dbGetQuery(db, "
    WITH selection AS (
      SELECT uid,
        json_extract_string(contenu, '$.amendement.cycleDeVie.dateDepot') AS date
      FROM an_amendements.amendements
      WHERE strpos(lower(coalesce(json_extract_string(contenu,
        '$.amendement.corps.contenuAuteur.exposeSommaire'), '')), lower(?)) > 0
         OR strpos(lower(coalesce(json_extract_string(contenu,
        '$.amendement.corps.contenuAuteur.dispositif'), '')), lower(?)) > 0
      ORDER BY date DESC NULLS LAST, uid
      LIMIT ?
    )
    SELECT 'amendements_an' AS source, 'amendement' AS type_document,
      a.uid AS identifiant,
      concat('Amendement ', coalesce(json_extract_string(a.contenu,
        '$.amendement.identification.numeroLong'), a.uid)) AS titre,
      s.date,
      concat_ws('\n',
        json_extract_string(a.contenu, '$.amendement.corps.contenuAuteur.exposeSommaire'),
        json_extract_string(a.contenu, '$.amendement.corps.contenuAuteur.dispositif')) AS contenu,
      CAST(NULL AS VARCHAR) AS url,
      json_extract_string(a.contenu, '$.amendement.texteLegislatifRef') AS dossier_id
    FROM selection s JOIN an_amendements.amendements a ON a.uid = s.uid",
    params = list(terme, terme, limite)))
}

# Les scrutins lies sont trouves via la table locale de correspondance
# des UID officiels, sans sous-requete correlee ni json_each par scrutin.
.rechercher_documents_scrutins_an = function(db, terme, limite) {
  avec_amendements = DBI::dbExistsTable(db,
    DBI::Id(schema = 'an_amendements', table = 'amendements')) &&
    DBI::dbExistsTable(db,
      DBI::Id(schema = 'an_votes', table = 'candidats_amendements'))
  jointure = if (avec_amendements) "
    UNION
    SELECT c.scrutin_uid AS uid
    FROM an_votes.candidats_amendements c
    JOIN an_amendements.amendements a ON a.uid = c.amendement_uid
    WHERE strpos(lower(coalesce(json_extract_string(a.contenu,
      '$.amendement.corps.contenuAuteur.exposeSommaire'), '')), lower(?)) > 0
       OR strpos(lower(coalesce(json_extract_string(a.contenu,
      '$.amendement.corps.contenuAuteur.dispositif'), '')), lower(?)) > 0" else ""
  sql = paste0("
    WITH correspondants AS (
      SELECT s.uid
      FROM an_votes.scrutins s
      WHERE strpos(lower(coalesce(json_extract_string(s.contenu,
        '$.scrutin.titre'), '')), lower(?)) > 0
         OR strpos(lower(coalesce(json_extract_string(s.contenu,
        '$.scrutin.objet.libelle'), '')), lower(?)) > 0",
    jointure, "
    ), selection AS (
      SELECT s.uid,
        json_extract_string(s.contenu, '$.scrutin.dateScrutin') AS date
      FROM an_votes.scrutins s
      JOIN correspondants c ON c.uid = s.uid
      ORDER BY date DESC NULLS LAST, s.uid
      LIMIT ?
    )
    SELECT 'scrutins_an' AS source, 'scrutin' AS type_document,
      s.uid AS identifiant,
      json_extract_string(s.contenu, '$.scrutin.titre') AS titre,
      x.date,
      concat_ws('\n',
        json_extract_string(s.contenu, '$.scrutin.titre'),
        json_extract_string(s.contenu, '$.scrutin.objet.libelle')) AS contenu,
      CAST(NULL AS VARCHAR) AS url, CAST(NULL AS VARCHAR) AS dossier_id
    FROM selection x JOIN an_votes.scrutins s ON s.uid = x.uid")
  params = c(list(terme, terme), if (avec_amendements) list(terme, terme), list(limite))
  tibble::as_tibble(DBI::dbGetQuery(db, sql, params = params))
}

# Dates de fraicheur issues du suivi des imports, sans les confondre avec
# les dates de publication par les producteurs.
.metadonnees_documentaires = function(db, sources) {
  correspondance = c(ameli = 'ameli', dosleg = 'dosleg',
    questions_senat = 'questions_senat', amendements_an = 'amendements',
    scrutins_an = 'scrutins')
  origine = c(ameli = 'Senat', dosleg = 'Senat', questions_senat = 'Senat',
    amendements_an = 'Assemblee nationale', scrutins_an = 'Assemblee nationale')
  url = c(ameli = 'https://data.senat.fr/ameli/',
    dosleg = 'https://data.senat.fr/dosleg/',
    questions_senat = 'https://data.senat.fr/',
    amendements_an = 'https://data.assemblee-nationale.fr/',
    scrutins_an = 'https://data.assemblee-nationale.fr/')
  resultat = tibble::tibble(source = sources, producteur = unname(origine[sources]),
    origine = unname(url[sources]), date_import = NA_character_,
    date_controle = NA_character_, modification = NA_character_)
  if (!length(sources) || !DBI::dbExistsTable(db,
      DBI::Id(schema = 'suivi', table = 'actualisations'))) return(resultat)
  suivi = DBI::dbGetQuery(db, 'SELECT jeu, CAST(date_import AS VARCHAR) AS date_import,
    CAST(date_controle AS VARCHAR) AS date_controle, modification FROM suivi.actualisations')
  indice = match(unname(correspondance[sources]), suivi$jeu)
  for (nom in c('date_import', 'date_controle', 'modification'))
    resultat[[nom]] = suivi[[nom]][indice]
  resultat
}
