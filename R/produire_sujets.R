#' Produire un catalogue PDF de dossiers correspondant a un sujet
#'
#' Recherche exclusivement dans la base DuckDB locale. Les titres officiels
#' sont privilegies ; les references permettent d'appeler produire_dossier().
#' @param sujet Expression recherchee dans les titres officiels.
#' @param db Connexion DuckDB ; NULL utilise connexion().
#' @param limite Nombre maximum de dossiers (100 par defaut).
#' @param sortie Fichier PDF, dans /tmp par defaut.
#' @param ouvrir Ouvrir le PDF apres production.
#' @param pdf Si FALSE, retourner le tibble des resultats.
#' @return Chemin du PDF (invisible), ou tibble si pdf = FALSE.
#' @export
produire_sujets = function(sujet, db = NULL, limite = 100L,
                           sortie = NULL, ouvrir = FALSE, pdf = TRUE) {
  dossiers = rechercher_dossiers(sujet, db = db, limite = limite)
  if (!isTRUE(pdf)) return(dossiers)
  if (is.null(sortie)) {
    sortie = file.path('/tmp', 'parlementR-sujets.pdf')
  }
  if (!is.character(sortie) || length(sortie) != 1L || is.na(sortie) ||
      !nzchar(sortie) || tolower(tools::file_ext(sortie)) != 'pdf')
    stop("'sortie' doit designer un fichier PDF")
  echapper = .echapper_latex_senateurs
  titre = trimws(sujet)
  # Date courte en premiere colonne ; le sujet conserve la largeur utile.
  if (nrow(dossiers)) {
    lignes = vapply(seq_len(nrow(dossiers)), function(i) {
      d = dossiers[i, ]
      nom = d$titre[[1]]
      if (is.na(nom) || !nzchar(trimws(nom))) nom = 'Titre non renseigne'
      procedure = d$procedure[[1]]
      if (is.na(procedure) || !nzchar(procedure)) procedure = 'Procedure non renseignee'
      legislature = d$legislature[[1]]
      if (is.na(legislature) || !nzchar(legislature)) legislature = '?'
      date = d$date[[1]]
      if (is.na(date) || !nzchar(date)) date = '---'
      paste0('{\\footnotesize ', echapper(date), '} & ',
             '\\textbf{', echapper(nom), '}',
             '\\par{\\footnotesize\\texttt{', echapper(d$uid[[1]]),
             '} \\quad Legislature ', echapper(legislature),
             ' \\quad ', echapper(procedure),
             ' }',
             ' \\\\')
    }, character(1))
    tableau = paste0('\\begin{longtable}{@{}p{2.0cm}p{13.4cm}@{}}\n',
                     '\\toprule\nDate & Sujet et reference \\\\ \\midrule\n\\endhead\n',
                     paste(lignes, collapse = '\n\\addlinespace[5pt]\n\\midrule\n'),
                     '\n\\bottomrule\n\\end{longtable}')
  } else {
    tableau = 'Aucun dossier correspondant dans la base locale.'
  }
  contenu = c(
    paste0('\\noindent\\colorbox{blue!9}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep\\relax}{',
           '\\textbf{Sujet recherche :} ', echapper(titre), '}}\\par\\medskip'),
    paste0('\\noindent ', nrow(dossiers), ' dossier(s) trouve(s).\\par'),
    '\\noindent{\\footnotesize Les identifiants officiels permettent de consulter chaque dossier avec ',
    '\\texttt{produire\\_dossier(uid, db = db)}.}\\par\\medskip',
    tableau
  )
  chemin = .pdf_senateurs('Assemblee nationale - Recherche de sujets', titre,
                          paste(contenu, collapse = '\n\n'), sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir = ouvrir)
}
