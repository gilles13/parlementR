# URLs officielles, identiques a celles des importeurs existants.
.source_archive = function(jeu, cache) {
  racine = "https://data.assemblee-nationale.fr/static/openData/repository/17/"
  if (identical(jeu, "senateurs")) {
    return(list(url = "https://data.senat.fr/data/senateurs/export_sens.zip",
                chemin = file.path(cache, "senat", "senateurs", "export_sens.zip")))
  }
  if (identical(jeu, "dosleg")) {
    return(list(url = "https://data.senat.fr/data/dosleg/dosleg.zip",
                chemin = file.path(cache, "senat", "dosleg", "dosleg.zip")))
  }
  if (identical(jeu, "questions")) {
    return(list(url = "https://data.senat.fr/data/questions/questions.zip",
                chemin = file.path(cache, "senat", "questions", "questions.zip")))
  }
  specification = switch(jeu,
    deputes = c("amo/deputes_actifs_mandats_actifs_organes/",
                "AMO10_deputes_actifs_mandats_actifs_organes.json.zip", "amo10"),
    dossiers = c("loi/dossiers_legislatifs/", "Dossiers_Legislatifs.json.zip", "dossiers_legislatifs"),
    amendements = c("loi/amendements_div_legis/", "Amendements.json.zip", "amendements"))
  list(url = paste0(racine, specification[1], specification[2]),
       chemin = file.path(cache, "assemblee_nationale", specification[3], specification[2]))
}

# Conserver uniquement les en-tetes de la derniere reponse HTTP (apres redirections).
.entetes_archive = function(fichier) {
  lignes = readLines(fichier, warn = FALSE)
  debuts = grep("^HTTP/[0-9.]+ [0-9]{3}", lignes)
  if (!length(debuts)) stop("Reponse HTTP sans en-tetes valides")
  lignes = lignes[utils::tail(debuts, 1L):length(lignes)]
  champ = function(nom) {
    valeur = grep(paste0("^", nom, ":"), lignes, ignore.case = TRUE, value = TRUE)
    if (!length(valeur)) return(NA_character_)
    trimws(sub("^[^:]+:", "", utils::tail(valeur, 1L)))
  }
  list(etag = champ("etag"), modification = champ("last-modified"))
}

# Controle HEAD uniquement : jamais de transfert du corps de l'archive.
.verifier_archive = function(url, etag = NA_character_, modification = NA_character_,
                              verbose = TRUE, jeu = "ARCHIVE") {
  journal = function(...) if (isTRUE(verbose)) message("[", toupper(jeu), "] ", ...)
  entetes = tempfile("parlementR-head-")
  on.exit(unlink(entetes), add = TRUE)
  args = c("--http1.1", "-sS", "-L", "-I", "--connect-timeout", "8",
           "--max-time", "25", "-D", shQuote(entetes), "-o", "/dev/null",
           "-w", shQuote("%{http_code}"))
  if (!is.na(etag) && nzchar(etag)) {
    args = c(args, "-H", shQuote(paste0("If-None-Match: ", etag)))
  } else if (!is.na(modification) && nzchar(modification)) {
    args = c(args, "-H", shQuote(paste0("If-Modified-Since: ", modification)))
  }
  journal("Controle HTTP HEAD (sans telechargement)")
  code = suppressWarnings(system2("curl", c(args, shQuote(url)),
                                   stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(code, "status")) || length(code) != 1L ||
      !code %in% c("200", "304")) stop("Echec du controle HTTP : ", url)
  meta = .entetes_archive(entetes)
  inchange = identical(code, "304") ||
    (identical(code, "200") && !is.na(etag) && !is.na(meta$etag) &&
       identical(etag, meta$etag)) ||
    (identical(code, "200") && is.na(etag) && is.na(meta$etag) &&
       !is.na(modification) && !is.na(meta$modification) &&
       identical(modification, meta$modification))
  list(inchange = inchange, etag = meta$etag, modification = meta$modification,
       code = code)
}

# Le GET n'est appele qu'apres une verification HEAD non concluante.
.telecharger_archive = function(url, chemin, verbose = TRUE, jeu = "ARCHIVE") {
  journal = function(...) if (isTRUE(verbose)) message("[", toupper(jeu), "] ", ...)
  dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
  temporaire = tempfile("parlementR-archive-", tmpdir = dirname(chemin), fileext = ".zip")
  entetes = tempfile("parlementR-get-")
  on.exit(unlink(c(temporaire, entetes)), add = TRUE)
  journal("Telechargement du ZIP : ", url)
  statut = system2("curl", c("--http1.1", "-fL", "--connect-timeout", "8",
     "--speed-time", "60", "--speed-limit", "1024", "--retry", "1",
     "-D", shQuote(entetes), "-o", shQuote(temporaire), shQuote(url)),
     stdout = if (isTRUE(verbose)) "" else FALSE,
     stderr = if (isTRUE(verbose)) "" else FALSE)
  if (!identical(as.integer(statut), 0L) || !file.exists(temporaire) ||
      is.na(file.info(temporaire)$size) || file.info(temporaire)$size == 0)
    stop("Echec du telechargement : ", url)
  journal("Verification de l'integrite du ZIP")
  if (!identical(as.integer(system2("unzip", c("-tq", shQuote(temporaire)),
                                  stdout = FALSE, stderr = FALSE)), 0L))
    stop("Archive ZIP invalide : ", url)
  meta = .entetes_archive(entetes)
  # L'ancien cache reste intact tant que le nouveau ZIP n'est pas valide.
  sauvegarde = paste0(chemin, ".bak")
  if (file.exists(sauvegarde)) stop("Sauvegarde de cache deja presente : ", sauvegarde)
  if (file.exists(chemin) && !file.rename(chemin, sauvegarde))
    stop("Impossible de sauvegarder l'ancien ZIP : ", chemin)
  if (!file.rename(temporaire, chemin)) {
    if (file.exists(sauvegarde)) file.rename(sauvegarde, chemin)
    stop("Impossible de conserver le ZIP : ", chemin)
  }
  if (file.exists(sauvegarde)) unlink(sauvegarde)
  journal("ZIP conserve dans le cache : ", chemin)
  meta
}
