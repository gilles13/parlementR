#' Telecharger et controler les fichiers du Repertoire national des elus
#'
#' Prepare les CSV et leur manifeste local, sans ouvrir ni modifier DuckDB.
#' Les fichiers deja valides restent disponibles si une mise a jour echoue.
#'
#' @param cache Repertoire racine du cache parlementR.
#' @param forcer Verifier le serveur meme si le dernier controle date de moins de 24 heures.
#' @param verbose Afficher les messages horodates et la progression native de curl.
#' @return Un data.frame de metadonnees (invisiblement).
#' @export
importer_rne = function(cache = tools::R_user_dir("parlementR", "cache"),
                         forcer = FALSE, verbose = TRUE) {
  if (!is.logical(forcer) || length(forcer) != 1L || is.na(forcer) ||
      !is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("'forcer' et 'verbose' doivent etre TRUE ou FALSE")
  repertoire = file.path(cache, "rne")
  dir.create(repertoire, recursive = TRUE, showWarnings = FALSE)
  manifeste = file.path(repertoire, "manifest.rds")
  sources = c(
    deputes = "https://www.data.gouv.fr/api/1/datasets/r/1ac42ff4-1336-44f8-a221-832039dbc142",
    senateurs = "https://www.data.gouv.fr/api/1/datasets/r/b78f8945-509f-4609-a4a7-3048b8370479"
  )
  precedent = if (file.exists(manifeste)) readRDS(manifeste) else data.frame()
  journal = function(jeu, ...) if (isTRUE(verbose)) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] RNE ",
    toupper(jeu), " : ", ...)
  resultat = vector("list", length(sources))
  names(resultat) = names(sources)
  for (jeu in names(sources)) {
    url = unname(sources[[jeu]])
    destination = file.path(repertoire, paste0(jeu, ".csv"))
    partiel = paste0(destination, ".part")
    ancien = if (nrow(precedent) && "jeu" %in% names(precedent))
      precedent[precedent$jeu == paste0("rne_", jeu), , drop = FALSE] else data.frame()
    if (nrow(ancien) > 1L) ancien = utils::tail(ancien, 1L)
    valeur = function(champ) if (nrow(ancien) && champ %in% names(ancien))
      ancien[[champ]][[1L]] else NA
    valide = file.exists(destination) && nrow(ancien) &&
      isTRUE(as.numeric(file.info(destination)$size) == as.numeric(valeur("taille"))) &&
      !is.na(valeur("md5")) &&
      identical(unname(tools::md5sum(destination)), as.character(valeur("md5")))
    dernier = suppressWarnings(as.POSIXct(valeur("date_controle"), tz = "UTC"))
    recent = length(dernier) == 1L && !is.na(dernier) &&
      as.numeric(difftime(Sys.time(), dernier, units = "hours")) < 24 &&
      as.numeric(difftime(Sys.time(), dernier, units = "hours")) >= 0
    etag = as.character(valeur("etag"))
    modification = as.character(valeur("modification"))
    telecharger = !valide
    if (valide && !forcer && recent) {
      journal(jeu, "Controle recent : cache valide, aucun acces reseau")
    } else if (valide) {
      controle = tryCatch(.verifier_archive(url, etag, modification,
                                           verbose = FALSE, jeu = paste0("RNE_", jeu)),
                          error = function(e) NULL)
      if (!is.null(controle) && isTRUE(controle$inchange)) {
        journal(jeu, "Ressource distante inchangee")
        etag = controle$etag
        modification = controle$modification
      } else {
        telecharger = TRUE
      }
    }
    if (telecharger) {
      journal(jeu, "Telechargement : ", url)
      entetes = tempfile("rne-http-")
      # curl -C - reprend le fichier .part si le serveur accepte Range.
      # En cas de refus, on repart de zero sans toucher au CSV valide.
      lancer = function(reprise) {
        args = c("-fL", "--retry", "3", "--connect-timeout", "30",
                 "--speed-time", "60", "--speed-limit", "1024",
                 if (reprise) c("-C", "-"),
                 "-D", shQuote(entetes), "-o", shQuote(partiel), shQuote(url))
        suppressWarnings(system2("curl", args,
          stdout = if (verbose) "" else FALSE,
          stderr = if (verbose) "" else FALSE))
      }
      statut = lancer(file.exists(partiel) && file.info(partiel)$size > 0)
      if (!identical(as.integer(statut), 0L) && file.exists(partiel)) {
        journal(jeu, "Reprise impossible : nouvel essai complet")
        unlink(partiel)
        statut = lancer(FALSE)
      }
      if (!identical(as.integer(statut), 0L))
        stop("Echec du telechargement RNE ", jeu, "; cache precedent conserve")
      controle_csv = tryCatch({
        if (!file.exists(partiel) || file.info(partiel)$size == 0)
          stop("Fichier vide")
        donnees = utils::read.csv2(partiel, check.names = FALSE,
          fileEncoding = "UTF-8-BOM", colClasses = "character")
        obligatoires = c("Nom de l'elu", "Prenom de l'elu", "Date de naissance",
                         "Code de la categorie socio-professionnelle")
        noms = iconv(names(donnees), to = "ASCII//TRANSLIT")
        if (!all(obligatoires %in% noms) || !nrow(donnees))
          stop("En-tete inattendu ou CSV vide")
        nrow(donnees)
      }, error = function(e) stop("CSV RNE ", jeu, " invalide : ",
                                  conditionMessage(e), call. = FALSE))
      meta_http = tryCatch(.entetes_archive(entetes), error = function(e)
        list(etag = NA_character_, modification = NA_character_))
      unlink(entetes)
      nouveau_md5 = unname(tools::md5sum(partiel))
      identique = valide && identical(nouveau_md5, as.character(valeur("md5")))
      if (!identique) {
        sauvegarde = paste0(destination, ".bak")
        if (file.exists(sauvegarde)) stop("Sauvegarde deja presente : ", sauvegarde)
        if (file.exists(destination) && !file.rename(destination, sauvegarde))
          stop("Impossible de sauvegarder le cache RNE : ", destination)
        if (!file.rename(partiel, destination)) {
          if (file.exists(sauvegarde)) file.rename(sauvegarde, destination)
          stop("Impossible de finaliser le CSV RNE : ", destination)
        }
        if (file.exists(sauvegarde)) unlink(sauvegarde)
      } else unlink(partiel)
      etag = meta_http$etag
      modification = meta_http$modification
      date_telechargement = if (identique) valeur("date_telechargement") else Sys.time()
      nombre_lignes = controle_csv
      journal(jeu, if (identique) "Contenu identique (MD5)" else "CSV valide et conserve")
    } else {
      date_telechargement = valeur("date_telechargement")
      nombre_lignes = valeur("nombre_lignes")
    }
    resultat[[jeu]] = data.frame(
      jeu = paste0("rne_", jeu), url = url, fichier = destination,
      taille = as.numeric(file.info(destination)$size),
      md5 = unname(tools::md5sum(destination)),
      date_source = as.POSIXct(NA, tz = "UTC"),
      date_telechargement = as.POSIXct(date_telechargement, tz = "UTC"),
      date_controle = Sys.time(), etag = etag, modification = modification,
      nombre_lignes = as.integer(nombre_lignes),
      statut = if (telecharger && !identique) "valide" else "inchange",
      stringsAsFactors = FALSE)
    # Persistance progressive : le premier fichier reste suivi si le second echoue.
    autres = if (nrow(precedent) && "jeu" %in% names(precedent))
      precedent[precedent$jeu != paste0("rne_", jeu), , drop = FALSE] else data.frame()
    precedent = if (nrow(autres)) rbind(autres, resultat[[jeu]]) else resultat[[jeu]]
    temporaire = tempfile("manifest-", tmpdir = repertoire, fileext = ".rds")
    saveRDS(precedent, temporaire)
    if (!file.rename(temporaire, manifeste))
      stop("Impossible d'enregistrer le manifeste RNE")
  }
  invisible(do.call(rbind, resultat))
}
