# Synchronisation des archives LEGI de la DILA.
.legi_journal = function(verbose, ...) if (isTRUE(verbose))
  message('[', format(Sys.time(), '%Y-%m-%d %H:%M:%S', tz = 'Europe/Paris'), '] [LEGI] ', ...)

.legi_archives = function() {
  url = 'https://echanges.dila.gouv.fr/OPENDATA/LEGI/'
  sortie = tempfile('index_legi_')
  on.exit(unlink(sortie), add = TRUE)
  statut = system2('curl', c('-fL', '--retry', '3', '--connect-timeout', '30',
                              '-o', shQuote(sortie), shQuote(url)))
  if (!identical(as.integer(statut), 0L)) stop('LEGI : index DILA inaccessible')
  html = paste(readLines(sortie, warn = FALSE), collapse = '\n')
  noms = unique(regmatches(html, gregexpr(
    '(Freemium_legi_global_|LEGI_)[0-9]{8}-[0-9]{6}\\.tar\\.gz',
    html, perl = TRUE))[[1]])
  if (!length(noms)) stop('LEGI : aucune archive sur le serveur DILA')
  data.frame(nom = noms, globale = startsWith(noms, 'Freemium_'),
             horodatage = sub('.*?([0-9]{8}-[0-9]{6})\\.tar\\.gz', '\\1', noms),
             url = paste0(url, noms), stringsAsFactors = FALSE)
}

.legi_telecharger = function(url, cache, verbose = TRUE, db) {
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  destination = file.path(cache, basename(url))
  # Ne pas confondre fichier partiel et archive valide.
  if (file.exists(destination) &&
      identical(as.integer(system2('gzip', c('-t', shQuote(destination)),
                                   stdout = FALSE, stderr = FALSE)), 0L))
    return(destination)
  .legi_journal(verbose, 'Telechargement : ', basename(url))
  statut = system2('curl', c('-fL', '--retry', '3', '--connect-timeout', '30',
                             '-C', '-', '-o', shQuote(destination), shQuote(url)))
  if (!identical(as.integer(statut), 0L))
    stop('LEGI : telechargement interrompu (reprise possible) : ', destination)
  if (!identical(as.integer(system2('gzip', c('-t', shQuote(destination)),
                                    stdout = FALSE, stderr = FALSE)), 0L))
    stop('LEGI : archive corrompue : ', destination)
  .memoriser_telechargement(db, "legi", url, destination)
  destination
}

.legi_suivi = function(db) {
  DBI::dbExecute(db, 'CREATE SCHEMA IF NOT EXISTS suivi')
  DBI::dbExecute(db, paste0(
    'CREATE TABLE IF NOT EXISTS suivi.archives_legi (',
    'archive VARCHAR PRIMARY KEY, type VARCHAR NOT NULL, ',
    'horodatage VARCHAR NOT NULL, md5 VARCHAR NOT NULL, ',
    'date_import TIMESTAMP NOT NULL DEFAULT (current_timestamp AT TIME ZONE \'UTC\'), ',
    'nb_xml BIGINT NOT NULL, nb_suppressions BIGINT NOT NULL)'))
  DBI::dbExecute(db, 'ALTER TABLE suivi.archives_legi ADD COLUMN IF NOT EXISTS sha256 VARCHAR')
}

.legi_enregistrer = function(db, archive, nb_xml, nb_suppressions = 0L) {
  md5 = .empreinte_archive(archive)
  sha256 = .sha256_archive(archive)
  nom = basename(archive)
  horodatage = sub('.*?([0-9]{8}-[0-9]{6})\\.tar\\.gz', '\\1', nom)
  if (!grepl('^[0-9]{8}-[0-9]{6}$', horodatage)) stop('Nom LEGI inattendu')
  DBI::dbExecute(db, paste0(
    'INSERT INTO suivi.archives_legi ',
    '(archive, type, horodatage, md5, sha256, nb_xml, nb_suppressions) ',
    'VALUES (?, ?, ?, ?, ?, ?, ?)'),
    params = list(nom, if (startsWith(nom, 'Freemium_')) 'globale' else 'quotidienne',
                  horodatage, md5, sha256,
                  as.numeric(nb_xml), as.numeric(nb_suppressions)))
  DBI::dbExecute(db, paste0(
    'INSERT INTO suivi.actualisations (jeu, date_import, date_controle, md5, sha256) ',
    "VALUES ('legi', (current_timestamp AT TIME ZONE 'UTC'), (current_timestamp AT TIME ZONE 'UTC'), ?, ?) ",
    'ON CONFLICT (jeu) DO UPDATE SET date_import = excluded.date_import, ',
    'date_controle = excluded.date_controle, md5 = excluded.md5, sha256 = excluded.sha256'),
    params = list(md5, sha256))
}

.legi_valeur = function(doc, xpath) {
  x = xml2::xml_find_first(doc, xpath)
  if (inherits(x, 'xml_missing')) return(NA_character_)
  v = trimws(xml2::xml_text(x))
  if (!nzchar(v)) NA_character_ else v
}

.legi_liens = function(doc, id_texte, id_parent = NULL) {
  if (is.null(id_parent)) {
    noeuds = xml2::xml_find_all(doc, '/TEXTELR/STRUCT/*')
  } else {
    noeuds = xml2::xml_find_all(doc,
      '/*/*[self::STRUCT or self::STRUCTURE_TA]/*[self::LIEN_SECTION_TA or self::LIEN_ART]')
  }
  if (!length(noeuds)) return(NULL)
  attrs = xml2::xml_attrs(noeuds)
  a = function(nom) vapply(attrs, function(x)
    if (nom %in% names(x)) unname(x[[nom]]) else NA_character_, character(1))
  commun = data.frame(id_texte = id_texte, type = xml2::xml_name(noeuds),
                      niveau = suppressWarnings(as.integer(a('niv'))),
                      debut = a('debut'), fin = a('fin'), etat = a('etat'),
                      ordre = seq_along(noeuds), stringsAsFactors = FALSE)
  if (is.null(id_parent)) {
    data.frame(id_texte = commun$id_texte, type = commun$type,
               id_cible = a('id'), cid = a('cid'), niveau = commun$niveau,
               debut = commun$debut, fin = commun$fin, etat = commun$etat,
               url = a('url'), ordre = commun$ordre)
  } else {
    data.frame(id_texte = commun$id_texte, id_parent = id_parent,
               type_parent = 'section', id_cible = a('id'), cid = a('cid'),
               type = commun$type, niveau = commun$niveau,
               debut = commun$debut, fin = commun$fin, etat = commun$etat,
               ordre = commun$ordre)
  }
}

# Une archive quotidienne remplace les objets presents ; elle ne remplace
# jamais les objets absents. Les suppressions sont traitees explicitement.
.legi_quotidienne = function(db, archive, verbose = TRUE) {
  nom = basename(archive)
  if (!grepl('^LEGI_[0-9]{8}-[0-9]{6}\\.tar\\.gz$', nom))
    stop('Archive quotidienne LEGI attendue')
  temp = tempfile('legi_delta_')
  dir.create(temp)
  on.exit(unlink(temp, recursive = TRUE), add = TRUE)
  liste = utils::untar(archive, list = TRUE)
  prefixe = 'legi/global/code_et_TNC_en_vigueur/code_en_vigueur/'
  chemins = liste[endsWith(liste, '.xml') & grepl(prefixe, liste, fixed = TRUE)]
  chemins = chemins[grepl('/(article|section_ta|texte/(struct|version))/', chemins)]
  suppressions = liste[grepl('/liste_suppression_legi(_dossier)?\\.dat$', liste)]
  # Une archive DILA peut ne contenir que des XML hors du perimetre des codes.
  # Ce cas est valide, mais une archive sans structure LEGI ne l'est pas.
  if (!any(grepl('(^|/)legi/global/code_et_TNC_en_vigueur/',
                 liste)))
    stop('Archive LEGI : arborescence officielle absente')
  if (!length(chemins) && !length(suppressions))
    .legi_journal(verbose, 'Archive sans modification des codes en vigueur : ', nom)
  # Refuser toute extraction hors du repertoire de travail.
  if (any(startsWith(liste, '/') | grepl('(^|/)\\.\\.(/|$)', liste)))
    stop('Chemin dangereux dans l archive LEGI')
  statut = system2('tar', c('-xzf', shQuote(archive), '-C', shQuote(temp)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L)) stop('Extraction LEGI echouee')
  # Les listes de suppression peuvent viser aussi des objets hors perimetre.
  supprimer = character()
  for (f in suppressions) {
    lignes = readLines(file.path(temp, f), warn = FALSE)
    lignes = trimws(sub('D$', '', lignes))
    supprimer = c(supprimer, lignes[grepl(prefixe, lignes, fixed = TRUE)])
  }
  supprimer = unique(supprimer)
  # Les suppressions de dossiers doivent etre resolues sur les chemins indexes.
  # Un chemin inconnu est ignore uniquement s'il est hors du perimetre suivi.
  DBI::dbBegin(db)
  tryCatch({
    for (chemin in chemins) {
      doc = xml2::read_xml(file.path(temp, chemin), options = c('NONET', 'NOBLANKS'))
      chemin_canonique = sub('^[^/]+/', '', chemin)
      id_texte = regmatches(chemin_canonique, regexpr('LEGITEXT[0-9]+', chemin_canonique))
      if (!length(id_texte)) stop('Texte LEGI absent du chemin : ', chemin)
      id = sub('\\.xml$', '', basename(chemin))
      if (grepl('/article/', chemin, fixed = TRUE)) {
        if (!grepl('^LEGIARTI[0-9]+$', id) ||
            !identical(.legi_valeur(doc, '/ARTICLE/META/META_COMMUN/ID'), id))
          stop('Article LEGI invalide : ', chemin)
        ligne = data.frame(id = id, id_texte = id_texte,
          numero = .legi_valeur(doc, '/*/META/META_SPEC/META_ARTICLE/NUM'),
          etat = .legi_valeur(doc, '/*/META/META_SPEC/META_ARTICLE/ETAT'),
          debut = .legi_valeur(doc, '/*/META/META_SPEC/META_ARTICLE/DATE_DEBUT'),
          fin = .legi_valeur(doc, '/*/META/META_SPEC/META_ARTICLE/DATE_FIN'),
          fichier = chemin_canonique)
        DBI::dbExecute(db, 'DELETE FROM legi.articles WHERE id = ?', list(id))
        DBI::dbAppendTable(db, DBI::Id(schema = 'legi', table = 'articles'), ligne)
        DBI::dbExecute(db, 'DELETE FROM legi.articles_contenu WHERE id = ?', list(id))
        DBI::dbAppendTable(db, DBI::Id(schema = 'legi', table = 'articles_contenu'),
          data.frame(id = id, fichier = chemin_canonique,
                     contenu = paste(readLines(file.path(temp, chemin), warn = FALSE),
                                     collapse = '\n')))
      } else if (grepl('/section_ta/', chemin, fixed = TRUE)) {
        if (!grepl('^LEGISCTA[0-9]+$', id) ||
            !identical(.legi_valeur(doc, '/SECTION_TA/ID'), id))
          stop('Section LEGI invalide : ', chemin)
        DBI::dbExecute(db, 'DELETE FROM legi.sections WHERE id = ?', list(id))
        DBI::dbAppendTable(db, DBI::Id(schema = 'legi', table = 'sections'),
          data.frame(id = id, id_texte = id_texte,
                     titre = .legi_valeur(doc, '/SECTION_TA/TITRE_TA'),
                     niveau = NA_character_, fichier = chemin_canonique))
        DBI::dbExecute(db, 'DELETE FROM legi.liens WHERE id_parent = ?', list(id))
        liens = .legi_liens(doc, id_texte, id)
        if (!is.null(liens)) DBI::dbAppendTable(db,
          DBI::Id(schema = 'legi', table = 'liens'), liens)
      } else if (grepl('/texte/struct/', chemin, fixed = TRUE)) {
        if (!identical(.legi_valeur(doc, '/TEXTELR/META/META_COMMUN/ID'), id))
          stop('Structure LEGI invalide : ', chemin)
        DBI::dbExecute(db, 'DELETE FROM legi.liens_racine WHERE id_texte = ?', list(id))
        liens = .legi_liens(doc, id)
        if (!is.null(liens)) DBI::dbAppendTable(db,
          DBI::Id(schema = 'legi', table = 'liens_racine'), liens)
      } else if (grepl('/texte/version/', chemin, fixed = TRUE)) {
        if (!identical(.legi_valeur(doc, '/TEXTE_VERSION/META/META_COMMUN/ID'), id))
          stop('Version LEGI invalide : ', chemin)
        DBI::dbExecute(db, 'DELETE FROM legi.textes WHERE id = ?', list(id))
        DBI::dbAppendTable(db, DBI::Id(schema = 'legi', table = 'textes'),
          data.frame(id = id,
            titre = .legi_valeur(doc, '/TEXTE_VERSION/META/META_SPEC/META_TEXTE_VERSION/TITRE'),
            titre_long = .legi_valeur(doc, '/TEXTE_VERSION/META/META_SPEC/META_TEXTE_VERSION/TITREFULL'),
            nature = .legi_valeur(doc, '/TEXTE_VERSION/META/META_SPEC/META_TEXTE_CHRONICLE/NATURE'),
            fichier_version = chemin_canonique))
      }
    }
    # Suppressions individuelles ou de dossiers : ne supprimer que les chemins
    # explicitement references, sans toucher aux autres versions historiques.
    for (chemin in supprimer) {
      for (tab in c('articles', 'sections')) {
        ids = DBI::dbGetQuery(db,
          paste0('SELECT id FROM legi.', tab, ' WHERE fichier = ? OR starts_with(fichier, ?)'),
          params = list(chemin, paste0(sub('/$', '', chemin), '/')))$id
        if (length(ids)) for (id in ids) {
          DBI::dbExecute(db, paste0('DELETE FROM legi.', tab, ' WHERE id = ?'), list(id))
          if (tab == 'articles') DBI::dbExecute(db,
            'DELETE FROM legi.articles_contenu WHERE id = ?', list(id))
          if (tab == 'sections') DBI::dbExecute(db,
            'DELETE FROM legi.liens WHERE id_parent = ?', list(id))
        }
      }
    }
    controle = DBI::dbGetQuery(db, paste0(
      'SELECT (SELECT count(*) FROM legi.articles a LEFT JOIN ',
      'legi.articles_contenu c ON a.id = c.id ',
      'WHERE c.id IS NULL) AS sans_contenu, ',
      '(SELECT count(*) FROM legi.articles_contenu c LEFT JOIN ',
      'legi.articles a ON a.id = c.id WHERE a.id IS NULL) AS sans_article'))
    if (any(unlist(controle) != 0))
      stop('LEGI : incoherence articles/contenus apres mise a jour')
    .legi_enregistrer(db, archive, length(chemins), length(supprimer))
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  .legi_journal(verbose, nom, ' : ', length(chemins), ' XML integres')
  invisible(length(chemins))
}

.actualiser_legi_impl = function(db, archive, cache, verbose, autoriser_regression) {
  .legi_suivi(db)
  repertoire = file.path(cache, 'legi')
  presentes = .jeu_present(db, 'legi', c('legi.textes', 'legi.liens_racine',
    'legi.sections', 'legi.articles', 'legi.liens', 'legi.articles_contenu'))
  suivi = DBI::dbGetQuery(db, 'SELECT * FROM suivi.archives_legi ORDER BY horodatage')
  # Migration uniquement si l'empreinte de l'archive globale est verifiee.
  if (presentes && !nrow(suivi)) {
    precedent = DBI::dbGetQuery(db,
      "SELECT md5 FROM suivi.actualisations WHERE jeu = 'legi'")
    fichiers = list.files(repertoire, '^Freemium_legi_global_.*\\.tar\\.gz$',
                          full.names = TRUE)
    if (!is.null(archive) && startsWith(basename(archive), 'Freemium_'))
      fichiers = unique(c(fichiers, archive))
    correspond = if (nrow(precedent) && !is.na(precedent$md5[1]))
      fichiers[unname(tools::md5sum(fichiers)) == precedent$md5[1]] else character()
    if (length(correspond) != 1L)
      stop('LEGI : archive globale de reference non identifiable dans le cache')
    reference = basename(correspond)
    DBI::dbExecute(db, paste0(
      'INSERT INTO suivi.archives_legi ',
      '(archive, type, horodatage, md5, sha256, nb_xml, nb_suppressions) ',
      'VALUES (?, ?, ?, ?, ?, 0, 0)'),
      params = list(reference, 'globale',
        sub('.*?([0-9]{8}-[0-9]{6})\\.tar\\.gz', '\\1', reference),
        unname(tools::md5sum(correspond)), .sha256_archive(correspond)))
    suivi = DBI::dbGetQuery(db, 'SELECT * FROM suivi.archives_legi')
  }
  for (i in which(is.na(suivi$sha256))) {
    fichier = file.path(repertoire, suivi$archive[i])
    if (!is.null(archive) && basename(archive) == suivi$archive[i]) fichier = archive
    sha = .sha256_verifie(fichier, suivi$md5[i])
    if (!is.na(sha)) DBI::dbExecute(db,
      'UPDATE suivi.archives_legi SET sha256 = ? WHERE archive = ?',
      params = list(sha, suivi$archive[i]))
  }
  DBI::dbExecute(db, paste0("UPDATE suivi.actualisations SET sha256 = a.sha256 ",
    "FROM suivi.archives_legi a WHERE jeu = 'legi' AND suivi.actualisations.md5 = a.md5 ",
    "AND suivi.actualisations.sha256 IS NULL AND a.sha256 IS NOT NULL"))
  if (!is.null(archive)) {
    if (!file.exists(archive)) stop('Archive LEGI introuvable : ', archive)
    nom = basename(archive)
    if (nom %in% suivi$archive) {
      md5 = unname(tools::md5sum(archive))
      if (!identical(md5, suivi$md5[match(nom, suivi$archive)]))
        stop('Archive LEGI deja enregistree avec une autre empreinte')
      .legi_journal(verbose, 'Archive deja appliquee : ', nom)
      return(invisible(NULL))
    }
    if (startsWith(nom, 'Freemium_')) {
      if (presentes && !autoriser_regression)
        stop('Reconstruction globale refusee sans autoriser_regression = TRUE')
      importer_legi(archive, db, verbose)
      DBI::dbWithTransaction(db, {
        DBI::dbExecute(db, 'DELETE FROM suivi.archives_legi')
        .legi_enregistrer(db, archive, 0L)
      })
    } else {
      if (!presentes || !nrow(suivi)) stop('LEGI : archive globale requise')
      dernier = utils::tail(suivi$horodatage, 1)
      horodatage = sub('^LEGI_([0-9]{8}-[0-9]{6})\\.tar\\.gz$', '\\1', nom)
      if (horodatage <= dernier) stop('LEGI : archive quotidienne anterieure')
      # Une archive locale isolee ne garantit pas la continuite : comparer
      # obligatoirement avec l'inventaire officiel.
      inventaire = .legi_archives()
      attendues = inventaire$nom[!inventaire$globale &
        inventaire$horodatage > dernier & inventaire$horodatage <= horodatage]
      if (!length(attendues) || !identical(utils::tail(attendues, 1L), nom))
        stop('LEGI : archive absente de l inventaire DILA')
      for (precedente in utils::head(attendues, -1L)) {
        adresse = inventaire$url[match(precedente, inventaire$nom)]
        fichier = .legi_telecharger(adresse, repertoire, verbose, db)
        .legi_quotidienne(db, fichier, verbose)
      }
      .legi_quotidienne(db, archive, verbose)
    }
    return(invisible(NULL))
  }
  inventaire = .legi_archives()
  if (!presentes) {
    globales = inventaire[inventaire$globale, , drop = FALSE]
    if (!nrow(globales)) stop('LEGI : aucune archive globale disponible')
    globale = globales[which.max(globales$horodatage), ]
    fichier = .legi_telecharger(globale$url, repertoire, verbose, db)
    importer_legi(fichier, db, verbose)
    DBI::dbWithTransaction(db, .legi_enregistrer(db, fichier, 0L))
    suivi = DBI::dbGetQuery(db, 'SELECT * FROM suivi.archives_legi')
  }
  dernier = utils::tail(suivi$horodatage, 1)
  quotidiennes = inventaire[!inventaire$globale &
    inventaire$horodatage > dernier, , drop = FALSE]
  quotidiennes = quotidiennes[order(quotidiennes$horodatage), , drop = FALSE]
  for (i in seq_len(nrow(quotidiennes))) {
    fichier = .legi_telecharger(quotidiennes$url[i], repertoire, verbose, db)
    .legi_quotidienne(db, fichier, verbose)
  }
  invisible(NULL)
}

# Journal de synchronisation : les chiffres portent uniquement sur cette execution.
.actualiser_legi = function(db, archive, cache, verbose, autoriser_regression) {
  .legi_suivi(db)
  etat = function() DBI::dbGetQuery(db,
    'SELECT archive, type, horodatage, nb_xml, nb_suppressions FROM suivi.archives_legi ORDER BY horodatage')
  avant = etat()
  afficher_etat = function(x) {
    globale = x[x$type == 'globale', , drop = FALSE]
    quotidiennes = x[x$type == 'quotidienne', , drop = FALSE]
    if (!nrow(globale)) {
      .legi_journal(verbose, 'Situation actuelle : aucune archive globale referencee')
    } else {
      reference = globale[order(globale$horodatage, decreasing = TRUE)[1L], , drop = FALSE]
      date = as.POSIXct(reference$horodatage, format = '%Y%m%d-%H%M%S', tz = 'UTC')
      .legi_journal(verbose, 'Situation actuelle : archive globale du ',
                    format(date, '%Y/%m/%d', tz = 'UTC'), ' (', reference$archive, ')')
      if (nrow(quotidiennes)) {
        derniere = quotidiennes[order(quotidiennes$horodatage, decreasing = TRUE)[1L], , drop = FALSE]
        date_fin = as.POSIXct(derniere$horodatage, format = '%Y%m%d-%H%M%S', tz = 'UTC')
        .legi_journal(verbose, 'Situation actuelle : ', nrow(quotidiennes),
                      ' archives quotidiennes appliquees, derniere du ',
                      format(date_fin, '%Y/%m/%d %H:%M:%S', tz = 'UTC'))
      } else {
        .legi_journal(verbose, 'Situation actuelle : aucune archive quotidienne appliquee')
      }
    }
  }
  .legi_journal(verbose, 'Actualisation de LEGI - donnees juridiques officielles de la DILA')
  .legi_journal(verbose, 'Source : https://echanges.dila.gouv.fr/OPENDATA/LEGI/')
  afficher_etat(avant)
  .legi_journal(verbose, if (is.null(archive))
    'Recherche et telechargement des nouvelles archives DILA...'
    else paste0('Import depuis une archive locale : ', basename(archive)))
  debut = Sys.time()
  erreur = NULL
  tryCatch(
    .actualiser_legi_impl(db, archive, cache, verbose, autoriser_regression),
    error = function(e) { erreur <<- e }
  )
  apres = etat()
  nouvelles = apres[!apres$archive %in% avant$archive, , drop = FALSE]
  n = nrow(nouvelles)
  xml = sum(nouvelles$nb_xml)
  suppressions = sum(nouvelles$nb_suppressions)
  .legi_journal(verbose, 'Bilan : ', n, ' archive(s) appliquee(s), ',
                format(xml, big.mark = ' ', scientific = FALSE), ' XML integres, ',
                format(suppressions, big.mark = ' ', scientific = FALSE),
                ' suppression(s) ; duree ', round(as.numeric(difftime(Sys.time(), debut, units = 'secs')), 1), ' s')
  if (!is.null(erreur)) {
    .legi_journal(verbose, 'ECHEC : ', conditionMessage(erreur),
                  ' ; archives deja validees conservees, reprise possible')
    stop(erreur)
  }
  .legi_journal(verbose, if (n) 'Mise a jour LEGI terminee avec succes'
                else 'LEGI deja a jour : aucune nouvelle archive appliquee')
  invisible(NULL)
}
