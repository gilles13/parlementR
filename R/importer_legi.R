#' Indexer les codes LEGI depuis une archive globale locale
#'
#' Importe les metadonnees, la hierarchie et les contenus XML des articles.
#' Les contenus sont conserves dans legi.articles_contenu. Les titres des
#' subdivisions proviennent directement de SECTION_TA/TITRE_TA. Le niveau
#' numerique officiel est conserve sur les liens (attribut niv).
#'
#' @param archive Archive Freemium_legi_global_*.tar.gz deja telechargee.
#' @param db Connexion DuckDB facultative.
#' @param verbose Afficher les messages de suivi.
#' @return Invisiblement, le nombre de codes indexes.
#' @export
importer_legi = function(archive, db = NULL, verbose = TRUE) {
  if (length(archive) != 1L || is.na(archive) || !file.exists(archive))
    stop("Archive LEGI locale introuvable")
  if (!grepl("^Freemium_legi_global_.*\\.tar\\.gz$", basename(archive)))
    stop("Cette premiere version exige l'archive globale LEGI")
  db = if (is.null(db)) connexion(creer = TRUE) else .connexion_valide(db)
  journal = function(...) if (isTRUE(verbose)) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [LEGI] ", ...)
  if (!identical(as.integer(system2("gzip", c("-t", shQuote(archive)),
                                    stdout = FALSE, stderr = FALSE)), 0L))
    stop("Archive LEGI invalide")

  temporaire = tempfile("parlementR_legi_")
  dir.create(temporaire)
  on.exit(unlink(temporaire, recursive = TRUE), add = TRUE)
  prefixe = "legi/global/code_et_TNC_en_vigueur/code_en_vigueur/"
  motifs = paste0(prefixe, c("*/texte/struct/*.xml", "*/texte/version/*.xml"))
  journal("Extraction ciblee des seuls XML de texte (une lecture de l'archive)")
  statut = system2("tar", c("-xzf", shQuote(archive), "-C", shQuote(temporaire),
                            "--wildcards", shQuote(motifs)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L)) stop("Extraction LEGI echouee")
  fichiers = list.files(file.path(temporaire, prefixe), pattern = "\\.xml$",
                        recursive = TRUE, full.names = TRUE)
  structures = fichiers[grepl("/texte/struct/", fichiers, fixed = TRUE)]
  versions = fichiers[grepl("/texte/version/", fichiers, fixed = TRUE)]
  if (!length(structures) || length(structures) != length(versions))
    stop("Paires structure/version LEGI incompletes")

  valeur = function(noeud, chemin) {
    v = xml2::xml_text(xml2::xml_find_first(noeud, chemin))
    if (!length(v) || is.na(v) || !nzchar(trimws(v))) NA_character_ else trimws(v)
  }
  textes = lapply(versions, function(f) {
    doc = xml2::read_xml(f)
    data.frame(
      id = valeur(doc, "/TEXTE_VERSION/META/META_COMMUN/ID"),
      titre = valeur(doc, "/TEXTE_VERSION/META/META_SPEC/META_TEXTE_VERSION/TITRE"),
      titre_long = valeur(doc, "/TEXTE_VERSION/META/META_SPEC/META_TEXTE_VERSION/TITREFULL"),
      nature = valeur(doc, "/TEXTE_VERSION/META/META_SPEC/META_TEXTE_CHRONICLE/NATURE"),
      fichier_version = sub(paste0("^", temporaire, "/"), "", f),
      stringsAsFactors = FALSE
    )
  })
  textes = do.call(rbind, textes)
  if (anyNA(textes$id) || anyDuplicated(textes$id))
    stop("Identifiants des textes LEGI manquants ou dupliques")
  liens = lapply(structures, function(f) {
    doc = xml2::read_xml(f)
    id = valeur(doc, "/TEXTELR/META/META_COMMUN/ID")
    noeuds = xml2::xml_find_all(doc, "/TEXTELR/STRUCT/*")
    if (!length(noeuds)) return(NULL)
    attrs = xml2::xml_attrs(noeuds)
    champ = function(nom) vapply(attrs, function(a)
      if (nom %in% names(a)) unname(a[[nom]]) else NA_character_, character(1))
    data.frame(id_texte = id, type = xml2::xml_name(noeuds),
               id_cible = champ("id"), cid = champ("cid"),
               niveau = suppressWarnings(as.integer(champ("niv"))),
               debut = champ("debut"), fin = champ("fin"),
               etat = champ("etat"), url = champ("url"),
               ordre = seq_along(noeuds), stringsAsFactors = FALSE)
  })
  liens = Filter(Negate(is.null), liens)
  liens = if (length(liens)) do.call(rbind, liens) else
    stop("Aucun lien structurel LEGI trouve")
  if (anyNA(liens$id_texte) || any(!liens$id_texte %in% textes$id))
    stop("Liens LEGI sans texte parent connu")

  journal(nrow(textes), " codes ; ", nrow(liens), " liens de premier niveau")
  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS legi")
    DBI::dbWriteTable(db, DBI::Id(schema = "legi", table = "textes_nouveau"),
                      textes, overwrite = TRUE)
    DBI::dbWriteTable(db, DBI::Id(schema = "legi", table = "liens_racine_nouveau"),
                      liens, overwrite = TRUE)
    DBI::dbExecute(db, "DROP TABLE IF EXISTS legi.textes")
    DBI::dbExecute(db, "DROP TABLE IF EXISTS legi.liens_racine")
    DBI::dbExecute(db, "ALTER TABLE legi.textes_nouveau RENAME TO textes")
    DBI::dbExecute(db, "ALTER TABLE legi.liens_racine_nouveau RENAME TO liens_racine")
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  .completer_legi(archive, db, verbose)
  .importer_contenu_legi(archive, db, verbose)
  invisible(nrow(textes))
}

# Controle distinct pour les archives LEGI au format tar.gz.
.controle_archive_legi = function(db, archive, presentes, autoriser_regression) {
  if (length(archive) != 1L || is.na(archive) || !file.exists(archive))
    stop("Archive LEGI introuvable")
  if (!presentes) return("importer")
  precedent = DBI::dbGetQuery(db,
    "SELECT md5 FROM suivi.actualisations WHERE jeu = 'legi'")
  md5 = .empreinte_archive(archive)
  if (nrow(precedent) && !is.na(precedent$md5[1]) &&
      identical(precedent$md5[1], md5)) return("identique")
  if (!isTRUE(autoriser_regression))
    stop("Archive LEGI differente : remplacement refuse sans autoriser_regression = TRUE")
  "importer"
}

# Index des subdivisions et des articles : aucune transcription des contenus.
# L'ordre et les periodes sont ceux des liens officiels, pas ceux des chemins.
.completer_legi = function(archive, db, verbose = TRUE) {
  journal = function(...) if (isTRUE(verbose)) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [LEGI] ", ...)
  racine = "legi/global/code_et_TNC_en_vigueur/code_en_vigueur/"
  temporaire = tempfile("parlementR_legi_hierarchie_")
  dir.create(temporaire)
  on.exit(unlink(temporaire, recursive = TRUE), add = TRUE)
  motifs = paste0(racine, c("*/section_ta/*.xml", "*/article/*.xml"))
  journal("Extraction des subdivisions et articles (un passage de l'archive)")
  statut = system2("tar", c("-xzf", shQuote(archive), "-C", shQuote(temporaire),
                             "--wildcards", shQuote(motifs)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L)) stop("Extraction LEGI incomplete")
  fichiers = list.files(file.path(temporaire, racine), pattern = "\\.xml$",
                        recursive = TRUE, full.names = TRUE)
  fichiers = fichiers[grepl("/section_ta/|/article/", fichiers)]
  if (!length(fichiers)) stop("Aucune subdivision ni article LEGI")

  DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS legi")
  DBI::dbExecute(db, paste0(
    "CREATE TABLE IF NOT EXISTS legi.sections_nouveau (",
    "id VARCHAR, id_texte VARCHAR, titre VARCHAR, niveau VARCHAR, ",
    "fichier VARCHAR)"))
  DBI::dbExecute(db, paste0(
    "CREATE TABLE IF NOT EXISTS legi.articles_nouveau (",
    "id VARCHAR, id_texte VARCHAR, numero VARCHAR, etat VARCHAR, ",
    "debut VARCHAR, fin VARCHAR, fichier VARCHAR)"))
  DBI::dbExecute(db, paste0(
    "CREATE TABLE IF NOT EXISTS legi.liens_nouveau (",
    "id_texte VARCHAR, id_parent VARCHAR, type_parent VARCHAR, ",
    "id_cible VARCHAR, cid VARCHAR, type VARCHAR, niveau INTEGER, ",
    "debut VARCHAR, fin VARCHAR, etat VARCHAR, ordre INTEGER)"))
  # Les tables de travail sont recrées pour ne pas reprendre des donnees incompletes.
  for (nom in c("sections", "articles", "liens"))
    DBI::dbExecute(db, paste0("DELETE FROM legi.", nom, "_nouveau"))

  valeur = function(noeud, chemin) {
    x = xml2::xml_find_first(noeud, chemin)
    if (inherits(x, "xml_missing")) return(NA_character_)
    v = trimws(xml2::xml_text(x))
    if (!nzchar(v)) NA_character_ else v
  }
  attr = function(a, nom) if (nom %in% names(a)) unname(a[[nom]]) else NA_character_
  sections = list(); articles = list(); liens = list()
  compte = c(section = 0L, article = 0L, lien = 0L)
  vider = function() {
    for (nom in c("sections", "articles", "liens")) {
      lignes = get(nom, inherits = TRUE)
      if (length(lignes)) DBI::dbWriteTable(
        db, DBI::Id(schema = "legi", table = paste0(nom, "_nouveau")),
        do.call(rbind, lignes), append = TRUE)
    }
  }
  for (i in seq_along(fichiers)) {
    f = fichiers[[i]]
    doc = xml2::read_xml(f, options = c("NONET", "NOBLANKS"))
    chemin = gsub("/+", "/", substring(f, nchar(temporaire) + 2L))
    # L'identifiant du code figure dans le chemin, meme si TAR contient //.
    id_texte = regmatches(chemin, regexpr("LEGITEXT[0-9]+", chemin))
    if (!grepl("^LEGITEXT", id_texte)) stop("Chemin LEGI inattendu : ", chemin)
    # Les identifiants LEGI sont egalement portes par les noms des fichiers.
    # La position de META differe selon le type de document XML.
    id_fichier = sub("\\.xml$", "", basename(f))
    prefixe_id = if (grepl("/section_ta/", chemin, fixed = TRUE))
      "LEGISCTA" else "LEGIARTI"
    if (!grepl(paste0("^", prefixe_id, "[0-9]+$"), id_fichier))
      stop("Nom de fichier LEGI inattendu : ", chemin)
    id_xml = if (identical(prefixe_id, "LEGISCTA"))
      valeur(doc, "/SECTION_TA/ID") else valeur(doc, "/ARTICLE/META/META_COMMUN/ID")
    if (!is.na(id_xml) && !identical(id_xml, id_fichier))
      stop("Identifiants XML et fichier differents : ", chemin)
    id = id_fichier
    if (grepl("/section_ta/", chemin, fixed = TRUE)) {
      sections[[length(sections) + 1L]] = data.frame(
        id = id, id_texte = id_texte,
        titre = valeur(doc, "/SECTION_TA/TITRE_TA"),
        niveau = NA_character_,
        fichier = chemin)
      compte["section"] = compte["section"] + 1L
      noeuds = xml2::xml_find_all(doc, "/*/*[self::STRUCT or self::STRUCTURE_TA]/*[self::LIEN_SECTION_TA or self::LIEN_ART]")
      if (length(noeuds)) {
        attrs = xml2::xml_attrs(noeuds)
        liens[[length(liens) + 1L]] = data.frame(
          id_texte = id_texte, id_parent = id, type_parent = "section",
          id_cible = vapply(attrs, attr, character(1), "id"),
          cid = vapply(attrs, attr, character(1), "cid"),
          type = xml2::xml_name(noeuds),
          niveau = suppressWarnings(as.integer(vapply(attrs, attr, character(1), "niv"))),
          debut = vapply(attrs, attr, character(1), "debut"),
          fin = vapply(attrs, attr, character(1), "fin"),
          etat = vapply(attrs, attr, character(1), "etat"),
          ordre = seq_along(noeuds))
        compte["lien"] = compte["lien"] + length(noeuds)
      }
    } else {
      articles[[length(articles) + 1L]] = data.frame(
        id = id, id_texte = id_texte,
        numero = valeur(doc, "/*/META/META_SPEC/META_ARTICLE/NUM"),
        etat = valeur(doc, "/*/META/META_SPEC/META_ARTICLE/ETAT"),
        debut = valeur(doc, "/*/META/META_SPEC/META_ARTICLE/DATE_DEBUT"),
        fin = valeur(doc, "/*/META/META_SPEC/META_ARTICLE/DATE_FIN"),
        fichier = chemin)
      compte["article"] = compte["article"] + 1L
    }
    if (i %% 5000L == 0L || i == length(fichiers)) {
      vider()
      sections = list(); articles = list(); liens = list()
      if (i %% 50000L == 0L || i == length(fichiers))
        journal(i, "/", length(fichiers), " XML traites")
    }
  }
  if (compte["section"] < 1L || compte["article"] < 1L)
    stop("Index LEGI incomplet : subdivisions ou articles absents")
  qualite = DBI::dbGetQuery(db, paste0(
    "SELECT count(*) AS total, count(titre) AS titres, ",
    "count(DISTINCT fichier) AS fichiers FROM legi.sections_nouveau"))
  if (qualite$total != compte["section"] || qualite$titres != qualite$total ||
      qualite$fichiers != qualite$total)
    stop("Index LEGI invalide : titres manquants ou fichiers dupliques")
  DBI::dbBegin(db)
  tryCatch({
    for (nom in c("sections", "articles", "liens")) {
      DBI::dbExecute(db, paste0("DROP TABLE IF EXISTS legi.", nom))
      DBI::dbExecute(db, paste0("ALTER TABLE legi.", nom,
                                "_nouveau RENAME TO ", nom))
    }
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  journal(compte["section"], " subdivisions ; ", compte["article"],
          " articles ; ", compte["lien"], " liens internes")
  invisible(compte)
}


# Import en flux des contenus XML : pas de fichiers XML temporaires.
# Les metadonnees des articles sont conservees dans legi.articles.
.importer_contenu_legi = function(archive, db, verbose = TRUE, lot = 1000L) {
  if (!file.exists(archive)) stop("Archive LEGI introuvable : ", archive)
  if (!is.numeric(lot) || length(lot) != 1L || is.na(lot) ||
      lot < 1 || lot != as.integer(lot)) stop("Taille de lot invalide")
  if (!nzchar(Sys.which("pigz")) || !nzchar(Sys.which("tar")) ||
      !nzchar(Sys.which("bash"))) stop("Import LEGI : pigz, tar et bash requis")
  journal = function(...) if (isTRUE(verbose)) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [LEGI] ", ...)
  index = DBI::dbGetQuery(db, "SELECT id, fichier FROM legi.articles")
  n = nrow(index)
  if (!n || anyNA(index$id) || anyNA(index$fichier) ||
      anyDuplicated(index$id) || anyDuplicated(index$fichier) ||
      any(!grepl("^LEGIARTI[0-9]+$", index$id)) ||
      any(sub("\\.xml$", "", basename(index$fichier)) != index$id))
    stop("Index des articles LEGI invalide")
  attendus = new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_len(n)) assign(index$id[i], index$fichier[i], envir = attendus)
  table = DBI::Id(schema = "legi", table = "articles_contenu_nouveau")
  DBI::dbExecute(db, paste0("CREATE TABLE IF NOT EXISTS legi.articles_contenu_nouveau (",
                            "id VARCHAR PRIMARY KEY, fichier VARCHAR, contenu VARCHAR)"))
  DBI::dbExecute(db, "DELETE FROM legi.articles_contenu_nouveau")
  commande = sprintf("set -o pipefail; pigz -dc %s | tar -xOf - --wildcards --no-anchored '*/code_en_vigueur/*/article/*.xml'", shQuote(archive))
  con = NULL
  on.exit({
    if (!is.null(con)) try(close(con), silent = TRUE)
  }, add = TRUE)
  con = pipe(sprintf("bash -c %s", shQuote(commande)), open = "rb")
  separateur = charToRaw("</ARTICLE>")
  longueur = length(separateur)
  tampon = raw()
  ids = character(lot)
  noms = character(lot)
  textes = character(lot)
  remplissage = 0L
  recus = 0L
  examines = 0L
  octets = 0
  debut = proc.time()[["elapsed"]]
  enregistrer = function() {
    if (!remplissage) return(invisible(NULL))
    donnees = data.frame(id = ids[seq_len(remplissage)],
                         fichier = noms[seq_len(remplissage)],
                         contenu = textes[seq_len(remplissage)])
    DBI::dbAppendTable(db, table, donnees)
    remplissage <<- 0L
    if (recus %% 10000L == 0L || recus == n)
      journal(recus, "/", n, " contenus importes")
    invisible(NULL)
  }
  repeat {
    bloc = readBin(con, "raw", n = 1048576L)
    if (!length(bloc)) break
    if (recus == n) next
    tampon = c(tampon, bloc)
    positions = grepRaw(separateur, tampon, fixed = TRUE, all = TRUE)
    if (!length(positions)) next
    debut_fragment = 1L
    for (position in positions) {
      fin_fragment = position + longueur - 1L
      fragment = tampon[debut_fragment:fin_fragment]
      debut_fragment = fin_fragment + 1L
      examines = examines + 1L
      xml = trimws(rawToChar(fragment))
      candidat = regmatches(xml, regexpr("LEGIARTI[0-9]+", xml))
      if (length(candidat) == 1L &&
          exists(candidat, envir = attendus, inherits = FALSE)) {
        document = xml2::read_xml(xml, options = "NONET")
        id = xml2::xml_text(xml2::xml_find_first(
          document, "/ARTICLE/META/META_COMMUN/ID"))
        if (!identical(unname(id), unname(candidat)))
          stop("Identifiant XML incoherent : ", candidat)
        remplissage = remplissage + 1L
        ids[remplissage] = id
        noms[remplissage] = get(id, envir = attendus, inherits = FALSE)
        textes[remplissage] = xml
        octets = octets + nchar(xml, type = "bytes")
        recus = recus + 1L
        rm(list = id, envir = attendus)
        if (remplissage == lot) enregistrer()
      }
      if (recus == n) break
    }
    if (recus == n) {
      tampon = raw()
    } else if (debut_fragment <= length(tampon)) {
      tampon = tampon[debut_fragment:length(tampon)]
    } else {
      tampon = raw()
    }
  }
  enregistrer()
  statut = close(con)
  con = NULL
  if (length(statut) && !is.na(statut) && statut != 0L)
    stop("Extraction TAR des articles LEGI echouee : ", statut)
  if (recus != n)
    stop("Articles LEGI sans contenu : ", n - recus, " sur ", n,
         " ; exemples : ", paste(utils::head(ls(attendus), 10L), collapse = ", "))
  controles = DBI::dbGetQuery(db, paste0(
    "SELECT (SELECT count(*) FROM legi.articles_contenu_nouveau) AS total, ",
    "(SELECT count(*) FROM legi.articles a LEFT JOIN legi.articles_contenu_nouveau c ",
    "ON a.id = c.id AND a.fichier = c.fichier WHERE c.id IS NULL) AS absents, ",
    "(SELECT count(*) FROM legi.articles_contenu_nouveau c LEFT JOIN legi.articles a ",
    "ON a.id = c.id AND a.fichier = c.fichier WHERE a.id IS NULL) AS inconnus, ",
    "(SELECT count(*) FROM legi.articles_contenu_nouveau WHERE contenu IS NULL ",
    "OR length(contenu) = 0) AS vides"))
  if (controles$total != n || controles$absents != 0 ||
      controles$inconnus != 0 || controles$vides != 0)
    stop("Controle d'integrite des contenus LEGI echoue : ",
         paste(names(controles), controles[1, ], collapse = "; "))
  # Controle XML final sans identical() sur des vecteurs nommes.
  echantillon = DBI::dbGetQuery(db, paste0(
    "SELECT id, contenu FROM legi.articles_contenu_nouveau ",
    "ORDER BY id LIMIT 100"))
  ids_xml = vapply(echantillon$contenu, function(x) {
    document = xml2::read_xml(x, options = "NONET")
    xml2::xml_text(xml2::xml_find_first(
      document, "/ARTICLE/META/META_COMMUN/ID"))
  }, character(1), USE.NAMES = FALSE)
  if (!all(ids_xml == echantillon$id))
    stop("Controle XML final : identifiants divergents")
  DBI::dbWithTransaction(db, {
    DBI::dbExecute(db, "DROP TABLE IF EXISTS legi.articles_contenu")
    DBI::dbExecute(db, "ALTER TABLE legi.articles_contenu_nouveau RENAME TO articles_contenu")
  })
  journal(n, " contenus valides ; ", examines, " XML examines ; ",
          format(octets, scientific = FALSE), " octets ; ",
          round(proc.time()[["elapsed"]] - debut, 1), " secondes")
  invisible(n)
}
