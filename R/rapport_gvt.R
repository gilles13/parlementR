#' Rapport PDF sur la composition d'un gouvernement
#'
#' Sans date, restitue les mandats historiques ; avec une date, une photographie.
#' @param gvt Identifiant PO ou libelle abrege du gouvernement.
#' @param date Date AAAA-MM-JJ facultative.
#' @param sortie Chemin PDF, dans /tmp par defaut.
#' @param ouvrir Ouvrir le PDF apres production.
#' @param db Connexion DuckDB facultative.
#' @param pdf Si FALSE, retourner les donnees sans produire le PDF.
#' @return Chemin PDF invisible, ou liste des donnees si pdf = FALSE.
#' @export
rapport_gvt = function(gvt, date = NULL, sortie = NULL, ouvrir = TRUE,
                       db = NULL, pdf = TRUE) {
  db = .connexion_valide(db)
  catalogue = cat_gvts(db = db)
  cible = which(catalogue$gouvernement_uid == gvt |
                  toupper(catalogue$gouvernement) == toupper(gvt))
  if (length(cible) != 1L) stop("Gouvernement inconnu ou ambigu : ", gvt, call. = FALSE)
  info = catalogue[cible, , drop = FALSE]
  membres = cat_gvt(gvt = info$gouvernement_uid[[1]], date = date, db = db)
  resultat = list(gouvernement = info, membres = membres, date = date)
  if (!isTRUE(pdf)) return(resultat)
  if (is.null(sortie)) {
    suffixe = if (is.null(date)) "historique" else as.character(as.Date(date))
    sortie = file.path("/tmp", paste0("parlementR-gvt-", info$gouvernement_uid[[1]],
                                        "-", suffixe, ".pdf"))
  }
  if (!is.character(sortie) || length(sortie) != 1L || is.na(sortie) ||
      !nzchar(sortie) || tolower(tools::file_ext(sortie)) != "pdf")
    stop("'sortie' doit designer un fichier PDF.", call. = FALSE)
  .verifier_volume_rapport(nrow(membres), unite = "mandats")
  chemin = .pdf_questions_senat(
    modele = "gouvernement.Rmd", donnees = resultat,
    titre = paste("Gouvernement", info$gouvernement[[1]]),
    sous_titre = if (is.null(date)) "Historique des mandats" else
      paste("Composition au", format(as.Date(date), "%d/%m/%Y")),
    sortie = sortie)
  .ouvrir_pdf(chemin, ouvrir = ouvrir)
}
