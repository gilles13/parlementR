# Extraction locale des articles JORF depuis les XML officiels.
# Aucun acces a DuckDB, aucun telechargement, aucun remplacement des sources.
#' Extraire les articles de textes JORF depuis une archive locale
#' @param ids Identifiants JORFTEXT demandes.
#' @param archive Chemin vers l archive tar.gz JORF locale.
#' @param dossier Repertoire du cache des XML.
#' @param index Index local des chemins de l archive.
#' @param limite Nombre maximal de textes par appel.
#' @return Data frame des relations texte/article et contenus XML officiels.
#' @export
extraire_articles_jorf = function(ids, archive, dossier = "~/.cache/R/parlementR/jorf/gouvernements", index = file.path(dossier, "index_archive.txt"), limite = 1000L) {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Le paquet xml2 est requis")
  ids = unique(as.character(ids))
  if (!length(ids) || anyNA(ids) || any(!grepl("^JORFTEXT[0-9]{12}$", ids))) stop("Identifiants JORFTEXT invalides")
  archive = path.expand(archive)
  dossier = path.expand(dossier)
  index = path.expand(index)
  if (!file.exists(archive) || !file.exists(index)) stop("Archive ou index local introuvable")
  if (length(ids) > limite) stop("Nombre de textes superieur a limite")
  journal = function(...) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"), "] [JORF] ", ...)
  chemins = readLines(index, warn = FALSE)
  chemins = chemins[grepl("\\.xml$", chemins, ignore.case = TRUE)]
  # Une archive peut contenir plusieurs representations du meme identifiant.
  # Le chemin est selectionne par sa branche documentaire, puis controle par la racine XML.
  trouver = function(ids, branche) {
    if (!length(ids)) return(character())
    x = chemins[grepl(branche, chemins, ignore.case = TRUE, perl = TRUE)]
    x = x[tools::file_path_sans_ext(basename(x)) %in% ids]
    noms = tools::file_path_sans_ext(basename(x))
    absents = setdiff(ids, noms)
    if (length(absents)) stop("XML absents de l'index (", branche, ") : ", paste(absents, collapse = ", "))
    if (anyDuplicated(noms)) stop("Chemins multiples pour : ", paste(unique(noms[duplicated(noms)]), collapse = ", "))
    stats::setNames(x, noms)
  }
  cache = file.path(dossier, "xml_sources")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  charger = function(selection, racine_attendue) {
    if (!length(selection)) return(list())
    destinations = file.path(cache, basename(selection))
    a_extraire = !file.exists(destinations)
    if (any(a_extraire)) {
      # Les chemins de l'index sont transmis tels quels a tar, en une extraction groupee.
      temporaire = tempfile("jorf_selection_")
      dir.create(temporaire)
      on.exit(unlink(temporaire, recursive = TRUE), add = TRUE)
      liste = tempfile("jorf_chemins_")
      on.exit(unlink(liste), add = TRUE)
      writeLines(unname(selection[a_extraire]), liste)
      journal("Extraction de ", sum(a_extraire), " XML ", racine_attendue)
      statut = system2("tar", c("-xzf", shQuote(archive), "-C", shQuote(temporaire), "-T", shQuote(liste)))
      if (statut != 0L) stop("Echec tar pour ", racine_attendue)
      sources = file.path(temporaire, unname(selection[a_extraire]))
      if (!all(file.exists(sources))) stop("Extraction incomplete pour ", racine_attendue)
      # Valider avant de copier dans le cache.
      docs = lapply(sources, function(f) xml2::read_xml(f, options = "NONET"))
      racines = vapply(docs, function(x) xml2::xml_name(xml2::xml_root(x)), character(1))
      if (!all(racines == racine_attendue)) stop("Racine XML inattendue : ", paste(unique(racines), collapse = ", "))
      if (!all(file.copy(sources, destinations[a_extraire], overwrite = FALSE))) stop("Copie cache incomplete")
    }
    documents = lapply(destinations, function(f) xml2::read_xml(f, options = "NONET"))
    racines = vapply(documents, function(x) xml2::xml_name(xml2::xml_root(x)), character(1))
    if (!all(racines == racine_attendue)) stop("Racine XML inattendue dans le cache")
    noms = vapply(documents, function(x) xml2::xml_text(xml2::xml_find_first(x, "/*/META/META_COMMUN/ID")), character(1))
    if (!identical(noms, names(selection))) stop("Identifiants XML differents des chemins demandes")
    stats::setNames(documents, names(selection))
  }
  # Le TEXTELR est la structure du texte, et non sa version documentaire.
  structures = charger(trouver(ids, "/texte/struct/"), "TEXTELR")
  # Recursion sur les sections (LIEN_SECTION), y compris les sous-sections.
  sections = list()
  liens = list()
  niveau = stats::setNames(rep(0L, length(structures)), names(structures))
  attente = lapply(names(structures), function(id) list(id_texte = id, id_parent = id, document = structures[[id]], profondeur = 0L))
  vus = character()
  n_liens = 0L
  while (length(attente)) {
    suivant = list()
    for (element in attente) {
      noeuds = xml2::xml_find_all(element$document, "/*/STRUCT/LIEN_ART | /*/STRUCT/LIEN_SECTION")
      if (!length(noeuds)) next
      for (i in seq_along(noeuds)) {
        noeud = noeuds[[i]]
        cible = xml2::xml_attr(noeud, "id")
        type = xml2::xml_name(noeud)
        if (is.na(cible) || !nzchar(cible)) stop("Lien structurel sans identifiant")
        n_liens = n_liens + 1L
        if (n_liens > 100000L) stop("Limite de liens atteinte")
        liens[[n_liens]] = data.frame(id_texte = element$id_texte, id_parent = element$id_parent, id_cible = cible, type_lien = type, ordre = i, profondeur = element$profondeur, debut = xml2::xml_attr(noeud, "debut"), fin = xml2::xml_attr(noeud, "fin"), etat = xml2::xml_attr(noeud, "etat"), stringsAsFactors = FALSE)
        if (type == "LIEN_SECTION" && !cible %in% c(vus, names(sections))) {
          vus = c(vus, cible)
          suivant[[length(suivant) + 1L]] = list(id_texte = element$id_texte, id_parent = cible, profondeur = element$profondeur + 1L)
        }
      }
    }
    if (!length(suivant)) break
    ids_sections = unique(vapply(suivant, function(x) x$id_parent, character(1)))
    nouvelles = charger(trouver(ids_sections, "/section_ta/"), "SECTION_TA")
    sections = c(sections, nouvelles)
    attente = lapply(suivant, function(x) { x$document = sections[[x$id_parent]]; x })
  }
  if (!length(liens)) stop("Aucun lien structurel dans les textes demandes")
  liens = do.call(rbind, liens)
  articles_ids = unique(liens$id_cible[liens$type_lien == "LIEN_ART"])
  if (!length(articles_ids)) stop("Aucun LIEN_ART dans les structures selectionnees")
  if (any(!grepl("^JORFARTI[0-9]{12}$", articles_ids))) stop("Identifiants d'articles inattendus")
  articles_xml = charger(trouver(articles_ids, "/article/"), "ARTICLE")
  # Conserver le contenu XML officiel, sans nettoyage ni normalisation.
  articles = do.call(rbind, lapply(names(articles_xml), function(id) {
    doc = articles_xml[[id]]
    noeud = xml2::xml_find_first(doc, "/ARTICLE/BLOC_TEXTUEL/CONTENU")
    numero = xml2::xml_find_first(doc, "/ARTICLE/META/META_SPEC/META_ARTICLE/NUM")
    contexte = xml2::xml_find_first(doc, "/ARTICLE/CONTEXTE/TEXTE")
    data.frame(id_article = id, numero = if (inherits(numero, "xml_missing")) NA_character_ else xml2::xml_text(numero), contenu_xml = if (inherits(noeud, "xml_missing")) NA_character_ else as.character(noeud), contenu_texte = if (inherits(noeud, "xml_missing")) NA_character_ else xml2::xml_text(noeud), id_contexte = if (inherits(contexte, "xml_missing")) NA_character_ else xml2::xml_attr(contexte, "cid"), fichier_xml = file.path(cache, paste0(id, ".xml")), stringsAsFactors = FALSE)
  }))
  relations = liens[liens$type_lien == "LIEN_ART", , drop = FALSE]
  resultat = merge(relations, articles, by.x = "id_cible", by.y = "id_article", all.x = TRUE, sort = FALSE)
  resultat = resultat[order(match(resultat$id_texte, ids), resultat$profondeur, resultat$id_parent, resultat$ordre), , drop = FALSE]
  rownames(resultat) = NULL
  journal(length(ids), " textes ; ", length(sections), " sections ; ", length(articles_ids), " articles ; ", nrow(resultat), " relations")
  attr(resultat, "liens_structurels") = liens
  resultat
}
