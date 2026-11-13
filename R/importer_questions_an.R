# Import des questions de l'Assemblee nationale : QE, QOSD et QG.
.importer_questions_an = function(db, archives, verbose = TRUE) {
  types = c("ecrites", "orales", "gouvernement")
  if (!is.character(archives) || length(archives) != length(types) ||
      anyNA(archives) || any(!nzchar(archives)) ||
      !identical(sort(names(archives)), sort(types)) ||
      any(!file.exists(archives))) stop("Trois archives de questions requises")
  archives = archives[types]
  dossier = tempfile("parlementR-questions-an-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  nombres = integer(length(types))
  for (i in seq_along(types)) {
    type = types[i]
    a = archives[[type]]
    if (system2("unzip", c("-tq", shQuote(a)), stdout = FALSE,
                stderr = FALSE) != 0L) stop("ZIP invalide : ", type)
    fichiers = system2("unzip", c("-Z1", shQuote(a)), stdout = TRUE)
    if (!is.null(attr(fichiers, "status"))) stop("Inventaire ZIP impossible : ", type)
    json = fichiers[grepl("\\.json$", fichiers, ignore.case = TRUE)]
    if (!length(json) || anyDuplicated(json) ||
        any(grepl("(^/|(^|/)\\.\\.(/|$))", fichiers)) ||
        any(grepl("\\", fichiers, fixed = TRUE)))
      stop("Organisation ZIP invalide : ", type)
    destination = file.path(dossier, type)
    dir.create(destination)
    if (system2("unzip", c("-oq", shQuote(a), "-d", shQuote(destination)),
                stdout = FALSE, stderr = FALSE) != 0L ||
        !all(file.exists(file.path(destination, json))))
      stop("Extraction incomplete : ", type)
    nombres[i] = length(json)
  }
  motif = as.character(DBI::dbQuoteString(db,
    file.path(dossier, "*", "**", "*.json")))
  prefixe = as.character(DBI::dbQuoteString(db,
    paste0(normalizePath(dossier, winslash = "/"), "/")))
  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS an_questions")
    DBI::dbExecute(db, paste0(
      "CREATE OR REPLACE TABLE an_questions.questions AS SELECT ",
      "json_extract_string(content, '$.question.uid') AS uid, ",
      "json_extract_string(content, '$.question.type') AS type, ",
      "replace(filename, ", prefixe, ", '') AS chemin, ",
      "content::JSON AS contenu FROM read_text(", motif, ")"))
    controle = DBI::dbGetQuery(db, paste0(
      "SELECT count(*) AS n, count(DISTINCT uid) AS distincts, ",
      "count(*) FILTER (WHERE uid IS NULL OR uid = '') AS absents, ",
      "count(*) FILTER (WHERE type IS NULL OR type NOT IN ('QE','QOSD','QG')) AS invalides, ",
      "count(*) FILTER (WHERE type = 'QE') AS qe, ",
      "count(*) FILTER (WHERE type = 'QOSD') AS qosd, ",
      "count(*) FILTER (WHERE type = 'QG') AS qg ",
      "FROM an_questions.questions"))
    if (controle$n != sum(nombres) || controle$distincts != controle$n ||
        controle$absents != 0 || controle$invalides != 0 ||
        any(unlist(controle[c("qe", "qosd", "qg")]) != nombres))
      stop("Controle d'integrite des questions AN echoue")
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  if (verbose) message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       "] [QUESTIONS_AN] Questions importees : ", controle$n)
  invisible(controle$n)
}

# Trois archives independantes, avec un suivi par categorie.
.actualiser_questions_an = function(db, cache, archive = NULL,
                                    forcer = FALSE, verbose = TRUE,
                                    autoriser_regression = FALSE) {
  journal = function(...) if (verbose) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [QUESTIONS_AN] ", ...)
  racine = paste0("https://data.assemblee-nationale.fr/",
                  "static/openData/repository/17/questions/")
  chemins = c(ecrites = "questions_ecrites/Questions_ecrites.json.zip",
              orales = "questions_orales_sans_debat/Questions_orales_sans_debat.json.zip",
              gouvernement = "questions_gouvernement/Questions_gouvernement.json.zip")
  destination = file.path(cache, "assemblee_nationale", "questions", basename(chemins))
  names(destination) = names(chemins)
  if (!is.null(archive)) {
    if (length(archive) == 1L && dir.exists(archive)) {
      destination = file.path(archive, basename(chemins))
      names(destination) = names(chemins)
    } else {
      if (!identical(sort(names(archive)), sort(names(chemins))))
        stop("'archive' doit etre un dossier ou un vecteur nomme ecrites/orales/gouvernement")
      destination = archive[names(chemins)]
    }
    if (any(!file.exists(destination))) stop("Archives locales manquantes")
  }
  presente = DBI::dbExistsTable(db, DBI::Id(schema = "an_questions", table = "questions"))
  if (!is.null(archive)) {
    for (type in names(destination)) {
      if (!.zip_valide(destination[[type]])) stop("ZIP invalide : ", type)
    }
    if (presente) {
      identiques = vapply(names(destination), function(type) {
        ancien = DBI::dbGetQuery(db,
          "SELECT md5 FROM suivi.actualisations WHERE jeu = ?",
          params = list(paste0("questions_an_", type)))
        nrow(ancien) && !is.na(ancien$md5[[1L]]) &&
          identical(ancien$md5[[1L]], .empreinte_archive(destination[[type]]))
      }, logical(1))
      if (all(identiques)) {
        journal("Archives locales identiques : aucun import")
        return(invisible(TRUE))
      }
      if (!autoriser_regression) stop(
        "Archives locales differentes ou chronologie inconnue : remplacement refuse. ",
        "Utiliser autoriser_regression = TRUE pour restaurer explicitement.")
    }
  }
  precedent = .db_get_tibble(db, paste0(
    "SELECT jeu, coalesce(date_controle, date_import) AS dernier, etag, modification ",
    "FROM suivi.actualisations WHERE jeu IN ",
    "('questions_an_ecrites','questions_an_orales','questions_an_gouvernement')"))
  metas = vector("list", length(chemins))
  names(metas) = names(chemins)
  change = !presente || !is.null(archive)
  modifies = stats::setNames(rep(!presente || !is.null(archive), length(chemins)), names(chemins))
  for (type in names(chemins)) {
    jeu = paste0("questions_an_", type)
    ligne = precedent[precedent$jeu == jeu, , drop = FALSE]
    etag = if (nrow(ligne)) ligne$etag[1] else NA_character_
    modification = if (nrow(ligne)) ligne$modification[1] else NA_character_
    metas[[type]] = list(etag = etag, modification = modification)
    if (!is.null(archive)) next
    heures = if (nrow(ligne)) as.numeric(difftime(
      Sys.time(), as.POSIXct(ligne$dernier[1], tz = "UTC"), units = "hours")) else Inf
    if (!forcer && presente && file.exists(destination[[type]]) &&
        is.finite(heures) && heures >= 0 && heures < 24) next
    url = paste0(racine, chemins[[type]])
    controle = .verifier_archive(url, etag, modification,
                                 verbose = verbose, jeu = jeu)
    metas[[type]] = list(etag = controle$etag,
                          modification = controle$modification)
    if (presente && file.exists(destination[[type]]) && isTRUE(controle$inchange)) next
    journal("Telechargement : ", type)
    .telecharger_questions_an(url, destination[[type]])
    modifies[[type]] = TRUE
    change = TRUE
  }
  if (!change) {
    journal("Archives inchangees : aucun import")
  } else {
    .importer_questions_an(db, destination, verbose = verbose)
  }
  for (type in names(chemins)) {
    jeu = paste0("questions_an_", type)
    meta = metas[[type]]
    empreinte = if (file.exists(destination[[type]])) .empreinte_archive(destination[[type]]) else NA_character_
    DBI::dbExecute(db, paste0(
      "INSERT INTO suivi.actualisations ",
      "(jeu, date_import, date_controle, md5, etag, modification) ",
      "VALUES (?, current_timestamp, current_timestamp, ?, ?, ?) ",
      "ON CONFLICT (jeu) DO UPDATE SET ",
      "date_import = CASE WHEN ? THEN excluded.date_import ELSE suivi.actualisations.date_import END, ",
      "date_controle = excluded.date_controle, ",
      "md5 = CASE WHEN ? THEN excluded.md5 ELSE suivi.actualisations.md5 END, ",
      "etag = coalesce(excluded.etag, suivi.actualisations.etag), ",
      "modification = coalesce(excluded.modification, suivi.actualisations.modification)"),
      params = list(jeu, empreinte, meta$etag, meta$modification,
                    modifies[[type]], modifies[[type]]))
  }
  journal("Actualisation terminee")
  invisible(TRUE)
}

# Conserve les fragments .part entre tentatives, valide avant remplacement.
.telecharger_questions_an = function(url, chemin) {
  dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
  partiel = paste0(chemin, ".part")
  statut = system2("curl", c(
    "-fL", "-C", "-", "--retry", "10", "--retry-all-errors",
    "--retry-delay", "5", "--connect-timeout", "30",
    "-o", shQuote(partiel), shQuote(url)), stdout = "", stderr = "")
  if (statut != 0L) stop("Telechargement interrompu : ", url)
  if (system2("unzip", c("-tq", shQuote(partiel)),
              stdout = FALSE, stderr = FALSE) != 0L)
    stop("ZIP invalide : ", url)
  sauvegarde = paste0(chemin, ".bak")
  if (file.exists(sauvegarde)) stop("Sauvegarde deja presente : ", sauvegarde)
  if (file.exists(chemin) && !file.rename(chemin, sauvegarde))
    stop("Impossible de sauvegarder : ", chemin)
  if (!file.rename(partiel, chemin)) {
    if (file.exists(sauvegarde)) file.rename(sauvegarde, chemin)
    stop("Impossible de conserver : ", chemin)
  }
  if (file.exists(sauvegarde)) unlink(sauvegarde)
  invisible(chemin)
}

#' Importer les questions de l'Assemblee nationale depuis trois archives locales
#'
#' Conserve les JSON complets des questions ecrites (QE), orales sans debat
#' (QOSD) et au gouvernement (QG) dans `an_questions.questions`.
#' Aucune requete reseau n'est effectuee par cette fonction.
#' @param archives Vecteur de trois chemins ZIP nommes `ecrites`, `orales`
#'   et `gouvernement`.
#' @param db Connexion DuckDB ; `NULL` utilise la connexion geree.
#' @param verbose Afficher le bilan d'importation.
#' @return Invisiblement, le nombre de questions importees.
#' @export
importer_questions_an = function(archives, db = NULL, verbose = TRUE) {
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("'verbose' doit etre TRUE ou FALSE")
  db = if (is.null(db)) connexion(creer = TRUE) else .connexion_valide(db)
  .importer_questions_an(db = db, archives = archives, verbose = verbose)
}
