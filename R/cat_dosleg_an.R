#' Produire un catalogue PDF de dossiers correspondant a un sujet
#'
#' Recherche exclusivement dans la base DuckDB locale. Les titres officiels
#' sont privilegies ; les references permettent d'appeler rapport_dosleg_an().
#' @param sujet Expression recherchee dans les titres officiels.
#' @param db Connexion DuckDB ; NULL utilise connexion().
#' @param limite Nombre maximum de dossiers (100 par defaut).
#' @param sortie Fichier PDF, dans /tmp par defaut.
#' @param ouvrir Ouvrir le PDF apres production.
#' @param pdf Si FALSE, retourner le tibble des resultats.
#' @return Chemin du PDF (invisible), ou tibble si pdf = FALSE.
#' @export
cat_dosleg_an = function(sujet, db = NULL, limite = 100L,
                           sortie = NULL, ouvrir = FALSE, pdf = TRUE) {
  dossiers = rech_dosleg_an(sujet, db = db, limite = limite)
  if (!isTRUE(pdf)) return(dossiers)
  .verifier_volume_rapport(nrow(dossiers), seuil_alerte = 100L,
                           seuil_max = 500L, unite = "dossiers")
  if (is.null(sortie)) {
    sortie = file.path('/tmp', 'parlementR-sujets.pdf')
  }
  if (!is.character(sortie) || length(sortie) != 1L || is.na(sortie) ||
      !nzchar(sortie) || tolower(tools::file_ext(sortie)) != 'pdf')
    stop("'sortie' doit designer un fichier PDF")
  titre = trimws(sujet)
  chemin = .pdf_questions_senat(
    modele = "dossiers_an.Rmd", donnees = dossiers,
    titre = "Assembl\u00e9e nationale - Recherche de sujets",
    sous_titre = titre, sortie = sortie
  )
  .ouvrir_pdf_senateurs(chemin, ouvrir = ouvrir)
}
