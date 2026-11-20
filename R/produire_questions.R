#' Produire un catalogue PDF des questions du Senat
#' @param sujet Expression recherchee ; NULL si filtre par theme seul.
#' @param db Connexion DuckDB.
#' @param limite Nombre de questions.
#' @param theme Code ou libelle d'un theme officiel du Senat.
#' @param sortie Chemin du PDF, dans /tmp par defaut.
#' @param ouvrir Ouvrir le PDF.
#' @param pdf Si FALSE, retourner le tibble.
#' @return Chemin PDF invisible ou tibble.
#' @export
cat_quest_senat = function(sujet = NULL, db = NULL, limite = 100L,
                              sortie = NULL, ouvrir = TRUE, pdf = TRUE,
                              theme = NULL) {
  x = rech_quest_senat(sujet, db = db, limite = limite, theme = theme)
  if (!isTRUE(pdf)) return(x)
  .verifier_volume_rapport(nrow(x), seuil_alerte = 100L,
                           seuil_max = 500L, unite = "questions")
  if (is.null(sortie)) sortie = "/tmp/parlementR-questions.pdf"
  if (length(sortie) != 1L || is.na(sortie) ||
      tolower(tools::file_ext(sortie)) != "pdf") stop("Sortie PDF invalide")
  etiquette = attr(x, "sujet")
  if (is.null(etiquette) || !length(etiquette) || is.na(etiquette[[1]]))
    etiquette = ""
  chemin = .pdf_questions_senat(
    modele = "questions_senat.Rmd", donnees = x,
    titre = "S\u00e9nat - Questions parlementaires",
    sous_titre = etiquette, sortie = sortie
  )
  .ouvrir_pdf(chemin, ouvrir = ouvrir)
}

#' Produire une fiche PDF de question et reponses
#' @param id Identifiant officiel interne.
#' @param db Connexion DuckDB.
#' @param sortie Chemin PDF.
#' @param ouvrir Ouvrir le PDF.
#' @param pdf Si FALSE, retourner les tibbles sources.
#' @return Chemin PDF invisible ou liste de tibbles.
#' @export
rapport_quest_senat = function(id, db = NULL, sortie = NULL,
                             ouvrir = TRUE, pdf = TRUE) {
  x = fiche_quest_senat(id, db = db)
  if (!isTRUE(pdf)) return(x)
  if (is.null(sortie)) sortie = file.path("/tmp", paste0("parlementR-question-", id, ".pdf"))
  if (length(sortie) != 1L || is.na(sortie) ||
      tolower(tools::file_ext(sortie)) != "pdf") stop("Sortie PDF invalide")
  q = x$question[1, ]
  titre = q$titre[[1]]
  if (is.na(titre) || !nzchar(titre)) titre = "Question parlementaire"
  chemin = .pdf_questions_senat(
    modele = "question_senat.Rmd", donnees = x,
    titre = "S\u00e9nat - Question parlementaire",
    sous_titre = titre, sortie = sortie
  )
  .ouvrir_pdf(chemin, ouvrir = ouvrir)
}
