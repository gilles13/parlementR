#' Produire un suivi PDF des amendements selectionnes
#'
#' Utilise directement le resultat de rech_amdt_an(), sans requete
#' supplementaire. Les statistiques portent sur la selection fournie.
#' @param donnees Tableau retourne par rech_amdt_an().
#' @param sujet Libelle facultatif de la recherche.
#' @param sortie Chemin du PDF ; /tmp par defaut.
#' @param ouvrir Ouvrir le PDF apres production.
#' @param pdf Si FALSE, retourner le tableau sans produire de PDF.
#' @return Chemin du PDF (invisible) ou tableau fourni.
#' @export
rapport_amdt_an = function(donnees, sujet = NULL, sortie = NULL,
                                     ouvrir = FALSE, pdf = TRUE) {
  requis = c("uid", "numero", "auteur", "groupe", "date_depot", "etat",
             "sort", "texte", "article", "dispositif", "expose")
  if (!is.data.frame(donnees)) stop("'donnees' doit etre un tableau")
  manque = setdiff(requis, names(donnees))
  if (length(manque)) stop("Colonnes manquantes : ", paste(manque, collapse = ", "))
  if (is.null(sujet)) sujet = .titre_recherche(donnees)
  if (!isTRUE(pdf)) return(donnees)
  .verifier_volume_rapport(length(unique(donnees$uid)))
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Installer 'knitr' pour produire le PDF")
  if (is.null(sortie)) sortie = "/tmp/parlementR-suivi-amendements.pdf"
  if (!is.character(sortie) || length(sortie) != 1L || is.na(sortie) ||
      !nzchar(sortie) || tolower(tools::file_ext(sortie)) != "pdf")
    stop("'sortie' doit designer un fichier PDF")
  if (!is.null(sujet) && (!is.character(sujet) || length(sujet) != 1L || is.na(sujet)))
    stop("'sujet' doit etre une chaine de caracteres")
  entete = if (is.null(sujet) || !nzchar(sujet))
    "S\u00e9lection d'amendements" else sujet
  chemin = .pdf_questions_senat(
    modele = "amendements_an.Rmd", donnees = donnees,
    titre = "Assembl\u00e9e nationale - Suivi des amendements",
    sous_titre = entete, sortie = sortie
  )
  .ouvrir_pdf_senateurs(chemin, ouvrir = ouvrir)
}
