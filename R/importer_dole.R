# Donnees DILA / DOLE : stock global et mises a jour incrementales.
.dole_journal = function(...) {
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [DOLE] ", ...)
}

# Corpus officiel DOLE (DILA) : stock global puis archives incrementales.
.dole_repertoire = "https://echanges.dila.gouv.fr/OPENDATA/DOLE/"

.dole_telecharger = function(url, destination, verbose = TRUE) {
  if (verbose) .dole_journal("Archive : ", basename(destination))
  if (file.exists(destination) && isTRUE(file.info(destination)$size > 0)) {
    if (verbose) .dole_journal("Archive deja en cache")
    return(invisible(destination))
  }
  if (verbose) .dole_journal("Telechargement depuis ", url)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  tmp = paste0(destination, ".part")
  on.exit(unlink(tmp), add = TRUE)
  statut = system2("curl", c("-fL", "--retry", "3", "--connect-timeout", "30",
                              "-o", shQuote(tmp), shQuote(url)))
  if (!identical(as.integer(statut), 0L) || !file.exists(tmp) ||
      file.info(tmp)$size == 0 || !file.rename(tmp, destination))
    stop("Telechargement DOLE impossible : ", url, call. = FALSE)
  if (verbose) .dole_journal("Telechargement termine : ", round(file.info(destination)$size / 1024^2, 1), " Mo")
  invisible(destination)
}

.dole_catalogue = function(cache, verbose = TRUE) {
  if (verbose) .dole_journal("Lecture du catalogue officiel")
  page = file.path(cache, "index.html")
  # Relecture du catalogue a chaque appel pour detecter les nouvelles livraisons.
  if (file.exists(page)) unlink(page)
  .dole_telecharger(.dole_repertoire, page, verbose = FALSE)
  doc = xml2::read_html(page)
  liens = xml2::xml_attr(xml2::xml_find_all(doc, "//a[@href]"), "href")
  liens = basename(sub("\\?.*$", "", liens))
  globaux = grep("^Freemium_dole_global_[0-9]{8}-[0-9]{6}\\.tar\\.gz$", liens, value = TRUE)
  mises = grep("^DOLE_[0-9]{8}-[0-9]{6}\\.tar\\.gz$", liens, value = TRUE)
  if (!length(globaux)) stop("Archive globale DOLE absente du catalogue officiel.")
  global = sort(unique(globaux), decreasing = TRUE)[[1L]]
  # Les mises a jour anterieures au stock global sont deja comprises dans celui-ci.
  origine = sub("^Freemium_dole_global_([0-9]{8}).*$", "\\1", global)
  mises = sort(unique(mises[substr(mises, 6L, 13L) >= origine &
                             substr(mises, 6L, 13L) <= format(Sys.Date(), "%Y%m%d")]))
  if (verbose) .dole_journal("Stock global : ", global, " ; mises a jour : ", length(mises))
  c(global, mises)
}

.dole_importer_archive = function(con, archive, nom_archive, verbose = TRUE, progression = 500L) {
  if (verbose) .dole_journal("Inventaire XML : ", nom_archive)
  fichiers = utils::untar(archive, list = TRUE)
  fichiers = fichiers[grepl("\\.xml$", fichiers, ignore.case = TRUE)]
  if (!length(fichiers)) stop("Aucun XML dans ", nom_archive)
  if (verbose) .dole_journal("", length(fichiers), " fichiers XML trouves ; extraction en cours")
  dossier = tempfile("dole_xml_")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  # Refuser les chemins absolus ou traversants avant extraction.
  if (any(grepl("^/|(^|/)\\.\\.(/|$)", fichiers)))
    stop("Chemin dangereux dans ", nom_archive)
  utils::untar(archive, exdir = dossier)
  if (verbose) .dole_journal("Extraction terminee ; importation DuckDB en cours")
  compteur = 0L
  for (f in fichiers) {
    chemin = file.path(dossier, f)
    if (!file.exists(chemin)) stop("XML manquant apres extraction : ", f)
    doc = xml2::read_xml(chemin, options = "HUGE")
    racine = xml2::xml_name(xml2::xml_root(doc))
    if (racine != "DOSSIER_LEGISLATIF")
      stop("Racine XML inattendue : ", racine, " dans ", f)
    identifiant = xml2::xml_text(xml2::xml_find_first(doc, "./META/META_COMMUN/ID"))
    if (is.na(identifiant) || !nzchar(identifiant))
      stop("Identifiant DOLE absent : ", f)
    xml = paste(readLines(chemin, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # L'archive la plus recente remplace la version precedente du meme dossier.
    DBI::dbExecute(con, "DELETE FROM dila_dole.dossiers WHERE uid = ?", params = list(identifiant))
    DBI::dbExecute(con, "DELETE FROM dila_dole.liens WHERE dossier_uid = ?", params = list(identifiant))
    DBI::dbExecute(con, paste(
      "INSERT INTO dila_dole.dossiers (uid, fichier, archive, xml) VALUES (?, ?, ?, ?)"),
      params = list(identifiant, f, nom_archive, xml))
    liens = xml2::xml_find_all(doc, ".//LIEN")
    if (length(liens)) {
      valeurs = tibble::tibble(
        dossier_uid = rep(identifiant, length(liens)),
        libelle = xml2::xml_attr(liens, "libelle"),
        url = xml2::xml_attr(liens, "lien"),
        archive = rep(nom_archive, length(liens))
      )
      DBI::dbAppendTable(con, DBI::Id(schema = "dila_dole", table = "liens"), valeurs)
    }
    compteur = compteur + 1L
    if (verbose && compteur %% progression == 0L)
      .dole_journal("", compteur, "/", length(fichiers), " XML traites (", round(100 * compteur / length(fichiers), 1), " %)")
  }
  if (verbose) .dole_journal("", compteur, "/", length(fichiers), " XML traites (100 %)")
  compteur
}

#' Importer le corpus complet DOLE de la DILA
#'
#' Recupere le stock global officiel et applique toutes les mises a jour
#' posterieures dans l'ordre chronologique. Les XML originaux sont conserves
#' dans DuckDB, ainsi que tous leurs liens documentaires. Les archives sont
#' conservees dans le cache et les mises a jour deja importees sont ignorees.
#' @param db Connexion DuckDB ; NULL utilise `connexion()`.
#' @param cache Repertoire local des archives DOLE.
#' @param archives Noms d'archives a traiter ; NULL pour le catalogue officiel
#'   complet (stock global et mises a jour).
#' @param reimporter Reinitialiser les trois tables DOLE depuis le stock global.
#' @param verbose Afficher les etapes et la progression (TRUE par defaut).
#' @param progression Frequence des messages de progression, en fichiers XML.
#' @return Effectifs des archives traitees, invisiblement.
#' @export
importer_dole = function(db = NULL, cache = "/tmp/dila_dole",
                         archives = NULL, reimporter = FALSE,
                         verbose = TRUE, progression = 500L) {
  stopifnot(is.logical(verbose), length(verbose) == 1L, !is.na(verbose),
            length(progression) == 1L, is.numeric(progression),
            !is.na(progression), progression >= 1)
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  if (is.null(archives)) archives = .dole_catalogue(cache, verbose = verbose)
  if (verbose) .dole_journal("", length(archives), " archive(s) a examiner")
  if (!length(archives) || any(!grepl(
    "^(Freemium_dole_global|DOLE)_[0-9]{8}-[0-9]{6}\\.tar\\.gz$", archives)))
    stop("Liste d'archives DOLE invalide.")
  if (verbose) .dole_journal("Ouverture de DuckDB et verification du schema")
  con = if (is.null(db)) connexion(creer = TRUE) else .connexion_valide(db)
  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS dila_dole")
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS dila_dole.dossiers",
    "(uid VARCHAR PRIMARY KEY, fichier VARCHAR, archive VARCHAR, xml VARCHAR)"))
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS dila_dole.liens",
    "(dossier_uid VARCHAR, libelle VARCHAR, url VARCHAR, archive VARCHAR)"))
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS dila_dole.archives",
    "(nom VARCHAR PRIMARY KEY, md5 VARCHAR, nombre INTEGER, date_import TIMESTAMP)"))
  globaux = grep("^Freemium_dole_global_", archives, value = TRUE)
  if (length(globaux) && !reimporter) {
    precedents = .db_get_tibble(con,
      "SELECT nom FROM dila_dole.archives WHERE nom LIKE 'Freemium_dole_global_%'")$nom
    if (length(precedents) && !all(globaux %in% precedents))
      stop("Un autre stock global est deja importe : utiliser reimporter = TRUE")
  }
  if (reimporter && !length(globaux))
    stop("Reimportation complete : fournir une archive globale.")
  if (reimporter) {
    if (verbose) .dole_journal("Reinitialisation des tables DOLE")
    DBI::dbBegin(con)
    DBI::dbExecute(con, "DELETE FROM dila_dole.liens")
    DBI::dbExecute(con, "DELETE FROM dila_dole.dossiers")
    DBI::dbExecute(con, "DELETE FROM dila_dole.archives")
    DBI::dbCommit(con)
  }
  bilan = tibble::tibble(archive = character(), dossiers = integer())
  for (nom in archives) {
    chemin = file.path(cache, nom)
    if (verbose) .dole_journal("Traitement : ", nom)
    .dole_telecharger(paste0(.dole_repertoire, nom), chemin, verbose = verbose)
    empreinte = unname(tools::md5sum(chemin))
    ancien = .db_get_tibble(con, "SELECT md5 FROM dila_dole.archives WHERE nom = ?", params = list(nom))
    if (nrow(ancien) && identical(ancien$md5[[1L]], empreinte)) {
      if (verbose) .dole_journal("Archive deja importee, ignoree : ", nom)
      next
    }
    if (verbose) .dole_journal("Debut de transaction : ", nom)
    DBI::dbBegin(con)
    valide = FALSE
    tryCatch({
      n = .dole_importer_archive(con, chemin, nom, verbose = verbose, progression = progression)
      DBI::dbExecute(con, "DELETE FROM dila_dole.archives WHERE nom = ?", params = list(nom))
      DBI::dbExecute(con, paste(
        "INSERT INTO dila_dole.archives VALUES (?, ?, ?, CURRENT_TIMESTAMP)"),
        params = list(nom, empreinte, n))
      DBI::dbCommit(con)
      valide = TRUE
      if (verbose) .dole_journal("Transaction validee : ", nom)
    }, error = function(e) {
      if (!valide) DBI::dbRollback(con)
      stop(e)
    })
    bilan = rbind(bilan, tibble::tibble(archive = nom, dossiers = n))
    .dole_journal("", nom, " (", n, " dossiers)")
  }
  if (verbose) .dole_journal("Termine : ", nrow(bilan), " archive(s) importee(s), ",
                       sum(bilan$dossiers), " dossier(s) traite(s)")
  invisible(bilan)
}
