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
produire_questions = function(sujet = NULL, db = NULL, limite = 100L,
                              sortie = NULL, ouvrir = FALSE, pdf = TRUE,
                              theme = NULL) {
  x = rechercher_questions(sujet, db = db, limite = limite, theme = theme)
  if (!isTRUE(pdf)) return(x)
  if (is.null(sortie)) sortie = "/tmp/parlementR-questions.pdf"
  if (length(sortie) != 1L || is.na(sortie) ||
      tolower(tools::file_ext(sortie)) != "pdf") stop("Sortie PDF invalide")
  esc = .echapper_latex_senateurs
  if (nrow(x)) {
    lignes = vapply(seq_len(nrow(x)), function(i) {
      titre = x$titre[[i]]
      if (is.na(titre) || !nzchar(titre)) titre = "Titre non renseign\u00e9"
      date = x$date[[i]]
      if (is.na(date) || !nzchar(date)) date = "---"
      ref = x$reference[[i]]
      if (is.na(ref) || !nzchar(ref)) ref = x$id[[i]]
      paste0("{\\footnotesize ", esc(date), "} & ",
             "\\textbf{", esc(titre), "}\\par",
             "{\\footnotesize\\texttt{", esc(ref),
             "} \\quad ID : ", esc(x$id[[i]]), "} \\\\")
    }, character(1))
    lignes = ifelse(seq_along(lignes) %% 2L == 0L,
                    paste0("\\rowcolor{gray!12} ", lignes), lignes)
    tableau = paste0("\\begin{longtable}{@{}p{2.0cm}p{13.4cm}@{}}\n",
      "\\toprule\nDate & Sujet et r\u00e9f\u00e9rence \\\\ \\midrule\n\\endhead\n",
      paste(lignes, collapse = "\n\\addlinespace[5pt]\n\\midrule\n"),
      "\n\\bottomrule\n\\end{longtable}")
  } else tableau = "Aucune question trouv\u00e9e dans la base locale."
  etiquette = if (is.null(sujet)) paste0("Th\u00e8me : ", theme) else if (is.null(theme)) sujet else paste0(sujet, " (th\u00e8me : ", theme, ")")
  contenu = c(paste0("\\noindent\\colorbox{blue!9}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep\\relax}{",
                      "\\textbf{Sujet recherch\u00e9 :} ", esc(etiquette), "}}\\par\\medskip"),
              paste0(nrow(x), " question(s) trouv\u00e9e(s).\\par"), tableau)
  chemin = .pdf_senateurs("S\u00e9nat - Questions parlementaires", etiquette,
                          paste(contenu, collapse = "\n\n"), sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir = ouvrir)
}

#' Produire une fiche PDF de question et reponses
#' @param id Identifiant officiel interne.
#' @param db Connexion DuckDB.
#' @param sortie Chemin PDF.
#' @param ouvrir Ouvrir le PDF.
#' @param pdf Si FALSE, retourner les tibbles sources.
#' @return Chemin PDF invisible ou liste de tibbles.
#' @export
produire_question = function(id, db = NULL, sortie = NULL,
                             ouvrir = FALSE, pdf = TRUE) {
  x = consulter_question(id, db = db)
  if (!isTRUE(pdf)) return(x)
  if (is.null(sortie)) sortie = file.path("/tmp", paste0("parlementR-question-", id, ".pdf"))
  if (length(sortie) != 1L || is.na(sortie) ||
      tolower(tools::file_ext(sortie)) != "pdf") stop("Sortie PDF invalide")
  esc = .echapper_latex_senateurs
  q = x$question[1, ]
  valeur = function(v) if (is.na(v) || !nzchar(v)) "Non renseign\u00e9" else v
  texte = function(v) {
    if (is.na(v) || !nzchar(trimws(v)))
      return("Texte non disponible dans la source officielle")
    gsub("\n", "\\\\par ", esc(v), fixed = TRUE)
  }
  contenu = c(paste0("\\textbf{R\u00e9f\u00e9rence :} ", esc(valeur(q$reference[[1]])), "\\par"),
              paste0("\\textbf{Date :} ", esc(substr(valeur(q$datejodepot[[1]]), 1, 10)), "\\par"),
              paste0("\\textbf{S\u00e9nateur :} ", esc(valeur(q$prenom[[1]])), " ", esc(valeur(q$nom[[1]])), "\\par"),
              "\\section*{Question}", texte(q$txtque[[1]]))
  if (nrow(x$reponses)) for (i in seq_len(nrow(x$reponses))) {
    r = x$reponses[i, ]
    contenu = c(contenu, paste0("\\section*{R\u00e9ponse du ",
      esc(substr(valeur(r$datejorep[[1]]), 1, 10)), "}"), texte(r$txtrep[[1]]))
  } else contenu = c(contenu, "\\section*{R\u00e9ponse}", "Aucune r\u00e9ponse dans la base locale.")
  chemin = .pdf_senateurs("S\u00e9nat - Question parlementaire", valeur(q$titre[[1]]),
                          paste(contenu, collapse = "\n\n"), sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir = ouvrir)
}
