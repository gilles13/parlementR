#' Rapport sur les amendements du Senat
#' @param references Resultat de rech_amdt_senat().
#' @param fichier Fichier PDF de sortie.
#' @param ouvrir Ouvrir le PDF apres creation.
#' @param surligner Surligner le sujet recherche.
#' @param pdf Si FALSE, retourner les donnees structurees.
#' @return Chemin PDF ou donnees de recherche.
#' @export
rapport_amdt_senat = function(references, fichier = NULL, ouvrir = TRUE,
                              surligner = TRUE, pdf = TRUE) {
  if (!inherits(references, "parlementR_sujet"))
    stop("'references' doit provenir de rech_amdt_senat()")
  if (!isTRUE(pdf)) return(references)
  .verifier_volume_rapport(nrow(references$amendements))
  if (is.null(fichier)) {
    ids = unique(as.character(references$amendements$amendement_id))
    ref = if (length(ids) == 1L) ids else paste0("selection_", length(ids))
    fichier = file.path("/tmp", paste0("amdt_senat_", ref, ".pdf"))
  }
  .produire_documents(references, fichier = fichier, ouvrir = ouvrir, surligner = surligner)
}

#' Rapport documentaire multisource
#' @param references Resultat de rech_docs().
#' @param fichier Fichier PDF de sortie.
#' @param ouvrir Ouvrir le PDF apres creation.
#' @param surligner Surligner le sujet recherche.
#' @param pdf Si FALSE, retourner les donnees structurees.
#' @return Chemin PDF ou tableau de documents.
#' @export
rapport_docs = function(references, fichier = NULL, ouvrir = TRUE,
                        surligner = TRUE, pdf = TRUE) {
  if (!inherits(references, "parlementR_documents"))
    stop("'references' doit provenir de rech_docs()")
  if (!isTRUE(pdf)) return(references)
  .verifier_volume_rapport(nrow(references), unite = "documents")
  .produire_documents(references, fichier = fichier, ouvrir = ouvrir, surligner = surligner)
}
