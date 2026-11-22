# Fonctions XML partagees par l extraction ciblee JORF.
.jorf_texte = function(noeud, xpath) {
  x = xml2::xml_find_first(noeud, xpath)
  if (inherits(x, "xml_missing")) return(NA_character_)
  y = trimws(xml2::xml_text(x))
  if (nzchar(y)) y else NA_character_
}

.jorf_attr = function(noeud, nom) {
  if (inherits(noeud, "xml_missing")) return(NA_character_)
  x = xml2::xml_attr(noeud, nom)
  x[is.na(x) | !nzchar(x)] = NA_character_
  x
}

.jorf_lignes = function(noeuds, parent, origine) {
  if (!length(noeuds)) return(NULL)
  data.frame(
    parent = rep(parent, length(noeuds)),
    cible = .jorf_attr(noeuds, "id"),
    type = xml2::xml_name(noeuds),
    ordre = seq_along(noeuds),
    debut = .jorf_attr(noeuds, "debut"),
    fin = .jorf_attr(noeuds, "fin"),
    etat = .jorf_attr(noeuds, "etat"),
    numero = .jorf_attr(noeuds, "num"),
    origine = rep(origine, length(noeuds)),
    stringsAsFactors = FALSE
  )
}

# Lit un flux de XML concatenes, sans extraction sur disque.
# Les declarations XML sont les separateurs des documents DILA.
.jorf_flux_xml = function(con, traiter, limite = NULL) {
  signature = charToRaw("<?xml")
  tampon = raw()
  n = 0L
  repeat {
    bloc = readBin(con, what = "raw", n = 1024L * 1024L)
    fini = !length(bloc)
    if (!fini) tampon = c(tampon, bloc)
    positions = grepRaw(signature, tampon, fixed = TRUE, all = TRUE)
    if (length(positions) && positions[1] != 1L)
      stop("JORF : donnees inattendues avant une declaration XML")
    if (length(positions) >= 2L) {
      for (i in seq_len(length(positions) - 1L)) {
        debut = positions[i]
        fin = positions[i + 1L] - 1L
        traiter(xml2::read_xml(tampon[debut:fin], options = c("NONET", "NOBLANKS")))
        n = n + 1L
        if (!is.null(limite) && n >= limite) return(n)
      }
      tampon = tampon[positions[length(positions)]:length(tampon)]
    }
    if (fini) {
      if (length(tampon)) {
        if (!length(positions)) stop("JORF : document XML sans declaration")
        traiter(xml2::read_xml(tampon, options = c("NONET", "NOBLANKS")))
        n = n + 1L
      }
      break
    }
  }
  n
}


# Extraction JORF ciblee : deux lectures de l'archive, sans import global.
# Les documents non demandes ne sont ni parses ni stockes.

.jorf_cible_extraire = function(archive, categorie, ids) {
  ids = unique(as.character(ids))
  if (!length(ids)) return(list())
  if (anyNA(ids) || any(!grepl('^JORF(TEXT|ARTI)[0-9]{12}$', ids)))
    stop('JORF : identifiants invalides')
  repertoire = switch(categorie,
    struct = 'texte/struct', article = 'article',
    stop('JORF : categorie non prise en charge'))
  # GNU tar cherche les chemins, mais n'envoie sur stdout que les XML demandes.
  motifs = paste0('jorf/global/', repertoire, '/*/', ids, '.xml')
  cmd = paste('tar -xOzf', shQuote(normalizePath(archive)),
              '--wildcards --wildcards-match-slash',
              paste(shQuote(motifs), collapse = ' '))
  resultat = list()
  recevoir = function(doc) {
    racine = xml2::xml_name(xml2::xml_root(doc))
    if (!identical(racine, if (categorie == 'struct') 'TEXTELR' else 'ARTICLE'))
      stop('JORF : racine inattendue : ', racine)
    id = .jorf_texte(doc, paste0('/', racine, '/META/META_COMMUN/ID'))
    if (is.na(id) || !id %in% ids || !is.null(resultat[[id]]))
      stop('JORF : document absent de la selection ou duplique : ', id)
    resultat[[id]] <<- doc
  }
  con = pipe(cmd, open = 'rb')
  ouvert = TRUE
  on.exit(if (ouvert) try(close(con), silent = TRUE), add = TRUE)
  .jorf_flux_xml(con, recevoir)
  statut = close(con)
  ouvert = FALSE
  manquants = setdiff(ids, names(resultat))
  if (length(manquants))
    stop('JORF : XML introuvables (', categorie, ') : ',
         paste(utils::head(manquants, 10L), collapse = ', '),
         if (length(manquants) > 10L) ' ...' else '')
  if (length(statut) && is.numeric(statut) && !is.na(statut) && statut != 0)
    stop('JORF : tar a echoue (statut ', statut, ')')
  resultat
}

#' Rechercher les decrets JORF relatifs aux gouvernements
#'
#' @param db Connexion DuckDB ; NULL utilise connexion().
#' @param motif Expression reguliere DuckDB sur le titre.
#' @param limite Nombre maximal de resultats.
#' @export
rechercher_jorf_gvt = function(db = NULL,
  motif = 'composition du gouvernement|relatif a la composition du gouvernement',
  limite = 1000L) {
  db = if (is.null(db)) connexion() else .connexion_valide(db)
  if (!DBI::dbExistsTable(db, DBI::Id(schema = 'jorf', table = 'textes')))
    stop('JORF : index jorf.textes absent')
  if (length(motif) != 1L || is.na(motif) || !nzchar(motif))
    stop('JORF : motif invalide')
  if (length(limite) != 1L || is.na(limite) || limite < 1L)
    stop('JORF : limite invalide')
  DBI::dbGetQuery(db, paste0(
    'SELECT id, date_texte, date_publi, titre_complet ',
    'FROM jorf.textes WHERE nature = ', DBI::dbQuoteString(db, 'DECRET'),
    ' AND regexp_matches(lower(coalesce(titre_complet, titre, ',
    DBI::dbQuoteString(db, ''), ')), ', DBI::dbQuoteString(db, motif), ') ',
    'ORDER BY date_texte, id LIMIT ', as.integer(limite)))
}

#' Importer seulement les articles de textes JORF selectionnes
#'
#' Deux lectures de l'archive globale (structures puis articles). Ne touche
#' pas aux tables JORF historiques et refuse les sections non resolues.
#'
#' @param ids Identifiants JORFTEXT des textes cibles.
#' @param archive Archive JORF tar.gz deja presente localement.
#' @param db Connexion DuckDB facultative.
#' @param limite Nombre maximal de textes par appel (securite).
#' @export
importer_jorf_cible = function(ids, archive, db = NULL, limite = 100L) {
  ids = unique(as.character(ids))
  if (!length(ids) || anyNA(ids) ||
      any(!grepl('^JORFTEXT[0-9]{12}$', ids)))
    stop('JORF : fournir des identifiants JORFTEXT valides')
  if (length(limite) != 1L || is.na(limite) || limite < 1L ||
      length(ids) > limite) stop('JORF : limite de textes depassee')
  if (length(archive) != 1L || is.na(archive) || !file.exists(archive))
    stop('JORF : archive locale introuvable')
  db = if (is.null(db)) connexion() else .connexion_valide(db)
  if (!DBI::dbExistsTable(db, DBI::Id(schema = 'jorf', table = 'textes')))
    stop('JORF : index jorf.textes absent')
  titres = DBI::dbGetQuery(db, paste0(
    'SELECT id, date_texte, titre_complet FROM jorf.textes WHERE id IN (',
    paste(DBI::dbQuoteString(db, ids), collapse = ', '), ')'))
  if (length(setdiff(ids, titres$id))) stop('JORF : textes absents de textes : ',
    paste(setdiff(ids, titres$id), collapse = ', '))
  journal = function(...) message('[', format(Sys.time(), '%Y-%m-%d %H:%M:%S'),
                                  '] [JORF] ', ...)
  journal('Extraction des structures de ', length(ids), ' textes')
  structures = .jorf_cible_extraire(archive, 'struct', ids)
  liens = lapply(names(structures), function(id) {
    noeuds = xml2::xml_find_all(structures[[id]], '/TEXTELR/STRUCT/*')
    if (!length(noeuds)) return(NULL)
    data.frame(id_texte = id, id_article = xml2::xml_attr(noeuds, 'id'),
               type_lien = xml2::xml_name(noeuds),
               ordre = seq_along(noeuds), stringsAsFactors = FALSE)
  })
  liens = Filter(Negate(is.null), liens)
  if (!length(liens)) stop('JORF : aucun article dans les structures selectionnees')
  liens = do.call(rbind, liens)
  if (any(liens$type_lien != 'LIEN_ART'))
    stop('JORF : structure contenant des sections : extraction recursive necessaire ; aucune ecriture')
  articles_ids = unique(liens$id_article)
  if (anyNA(articles_ids) || any(!grepl('^JORFARTI[0-9]{12}$', articles_ids)))
    stop('JORF : liens articles invalides')
  journal('Extraction de ', length(articles_ids), ' articles references')
  documents = .jorf_cible_extraire(archive, 'article', articles_ids)
  articles = do.call(rbind, lapply(names(documents), function(id) {
    doc = documents[[id]]
    meta = '/ARTICLE/META/META_SPEC/META_ARTICLE/'
    data.frame(id_article = id,
      id_texte = .jorf_attr(xml2::xml_find_first(doc, '/ARTICLE/CONTEXTE/TEXTE'), 'cid'),
      numero = .jorf_texte(doc, paste0(meta, 'NUM')),
      contenu = .jorf_texte(doc, '/ARTICLE/BLOC_TEXTUEL/CONTENU'),
      stringsAsFactors = FALSE)
  }))
  if (anyNA(articles$id_texte) || any(!articles$id_texte %in% ids))
    stop('JORF : contexte des articles incompatible avec les textes selectionnes')
  if (!setequal(articles$id_article, articles_ids))
    stop('JORF : articles manquants')
  journal('Extraction cible terminee sans ecriture en base')
  invisible(list(textes = titres, liens = liens, articles = articles))
}
