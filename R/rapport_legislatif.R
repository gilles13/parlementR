#' Rapport legislatif thematique, construit exclusivement depuis DuckDB
#'
#' @param db Connexion DBI facultative ; NULL utilise connexion().
#' @param sujet Expression recherchee dans les titres des dossiers.
#' @param fichier PDF de sortie, par defaut dans le repertoire temporaire.
#' @param limite Nombre maximum de dossiers dans le rapport.
#' @param pdf Si FALSE, renvoie les donnees sans produire de PDF.
#' @return Chemin du PDF (invisible) ou liste de tables si pdf = FALSE.
#' @export
rapport_legislatif = function(db = NULL, sujet = NULL, fichier = NULL, limite = 20L, pdf = TRUE) {
  # Accepte rapport_legislatif("logement") et l'ancien appel (db, "logement").
  if (is.character(db) && is.null(sujet)) {
    sujet = db
    db = NULL
  }
  db = .connexion_valide(db)
  if (!is.character(sujet) || length(sujet) != 1L || is.na(sujet) ||
      !nzchar(trimws(sujet))) stop("'sujet' doit etre une expression non vide")
  if (length(limite) != 1L || is.na(limite) || limite < 1 ||
      limite != as.integer(limite)) stop("'limite' doit etre un entier positif")
  requis = c("an_dossiers.dossiers", "an_dossiers.documents",
             "an_amendements.amendements")
  present = vapply(strsplit(requis, ".", fixed = TRUE), function(x)
    DBI::dbExistsTable(db, DBI::Id(schema = x[1], table = x[2])), logical(1))
  if (!all(present)) stop("Tables manquantes : ", paste(requis[!present], collapse = ", "))
  motif = paste0("%", trimws(sujet), "%")
  sql = paste0("SELECT uid, ",
    "json_extract_string(contenu, '$.dossierParlementaire.titreDossier.titre') titre, ",
    "json_extract_string(contenu, '$.dossierParlementaire.procedureParlementaire.libelle') AS \"procedure\", ",
    "json_extract_string(contenu, '$.dossierParlementaire.legislature') legislature ",
    "FROM an_dossiers.dossiers WHERE ",
    "lower(json_extract_string(contenu, '$.dossierParlementaire.titreDossier.titre')) LIKE lower(?) ",
    "ORDER BY uid LIMIT ?")
  dossiers = .db_get_tibble(db, sql, params = list(motif, as.integer(limite)))
  if (!nrow(dossiers)) stop("Aucun dossier trouve pour : ", sujet)
  # Relation fiable dossier -> notices documentaires via dossierRef.
  documents = .db_get_tibble(db, paste0(
    "SELECT d.uid dossier_uid, x.uid document_uid, ",
    "json_extract_string(x.contenu, '$.document.titres.titrePrincipal') titre, ",
    "json_extract_string(x.contenu, '$.document.cycleDeVie.dateDepot') date_depot ",
    "FROM an_dossiers.documents x JOIN an_dossiers.dossiers d ",
    "ON json_extract_string(x.contenu, '$.document.dossierRef') = d.uid ",
    "WHERE d.uid IN (", paste(rep("?", nrow(dossiers)), collapse = ","), ")"),
    params = as.list(dossiers$uid))
  # Un amendement se rapporte a un texte : la reference peut etre distincte
  # de l'UID de la notice. Ne pas fabriquer de jointure approximative.
  amendements = if (nrow(documents)) .db_get_tibble(db, paste0(
    "SELECT uid, ",
    "json_extract_string(contenu, '$.amendement.identification.numeroLong') numero, ",
    "json_extract_string(contenu, '$.amendement.cycleDeVie.sort') sort, ",
    "json_extract_string(contenu, '$.amendement.texteLegislatifRef') texte_ref, ",
    "json_extract_string(contenu, '$.amendement.corps.exposeSommaire') expose ",
    "FROM an_amendements.amendements WHERE ",
    "json_extract_string(contenu, '$.amendement.texteLegislatifRef') IN (",
    paste(rep("?", nrow(documents)), collapse = ","), ")"),
    params = as.list(documents$document_uid)) else
      tibble::tibble(uid = character(), numero = character(), sort = character(),
                 texte_ref = character(), expose = character())
  resultat = list(dossiers = dossiers, documents = documents, amendements = amendements)
  if (!pdf) return(resultat)
  if (is.null(fichier)) fichier = file.path(tempdir(), "parlementR-rapport-legislatif.pdf")
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !requireNamespace("knitr", quietly = TRUE))
    stop("Installer 'rmarkdown' et 'knitr' pour produire un PDF")
  modele = system.file("modeles", "rapport_legislatif.Rmd", package = "parlementR")
  if (!nzchar(modele)) modele = "inst/modeles/rapport_legislatif.Rmd"
  if (!file.exists(modele)) stop("Modele de rapport introuvable")
  dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
  sortie = rmarkdown::render(modele, output_file = basename(fichier),
    output_dir = normalizePath(dirname(fichier)),
    params = c(list(sujet = sujet), resultat), quiet = TRUE,
    envir = new.env(parent = globalenv()))
  invisible(normalizePath(sortie))
}
