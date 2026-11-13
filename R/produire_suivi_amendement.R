#' Produire un suivi PDF des amendements selectionnes
#'
#' Utilise directement le resultat de rechercher_amendements(), sans requete
#' supplementaire. Les statistiques portent sur la selection fournie.
#' @param donnees Tableau retourne par rechercher_amendements().
#' @param sujet Libelle facultatif de la recherche.
#' @param sortie Chemin du PDF ; /tmp par defaut.
#' @param ouvrir Ouvrir le PDF apres production.
#' @param pdf Si FALSE, retourner le tableau sans produire de PDF.
#' @return Chemin du PDF (invisible) ou tableau fourni.
#' @export
produire_suivi_amendement = function(donnees, sujet = NULL, sortie = NULL,
                                     ouvrir = FALSE, pdf = TRUE) {
  requis = c("uid", "numero", "auteur", "groupe", "date_depot", "etat",
             "sort", "texte", "article", "dispositif", "expose")
  if (!is.data.frame(donnees)) stop("'donnees' doit etre un tableau")
  manque = setdiff(requis, names(donnees))
  if (length(manque)) stop("Colonnes manquantes : ", paste(manque, collapse = ", "))
  if (!isTRUE(pdf)) return(donnees)
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Installer 'knitr' pour produire le PDF")
  if (is.null(sortie)) sortie = "/tmp/parlementR-suivi-amendements.pdf"
  if (!is.character(sortie) || length(sortie) != 1L || is.na(sortie) ||
      !nzchar(sortie) || tolower(tools::file_ext(sortie)) != "pdf")
    stop("'sortie' doit designer un fichier PDF")
  if (!is.null(sujet) && (!is.character(sujet) || length(sujet) != 1L || is.na(sujet)))
    stop("'sujet' doit etre une chaine de caracteres")
  esc = .echapper_latex_senateurs
  valeur = function(x) {
    x = .valeur_parlementaire(x)
    x[is.na(x) | !nzchar(trimws(x))] = "Non renseign\u00e9"
    x
  }
  # Le HTML officiel contient notamment des entites numeriques (&#x00E9;).
  # xml2 est deja une dependance du package ; decoder avant l'echappement LaTeX.
  extrait = function(x, n = 180L) {
    x = as.character(x)
    x[is.na(x)] = ""
    x = vapply(x, function(v) {
      if (!nzchar(v)) return("")
      doc = xml2::read_html(paste0("<html><body>", v, "</body></html>"))
      xml2::xml_text(xml2::xml_find_first(doc, "//body"))
    }, character(1))
    x = trimws(gsub("[[:space:]]+", " ", x))
    x = ifelse(nchar(x) > n, paste0(substr(x, 1L, n), "\u2026"), x)
    x[!nzchar(x)] = "Non renseign\u00e9"
    x
  }
  x = donnees[!duplicated(donnees$uid), , drop = FALSE]
  x$sort = valeur(x$sort)
  x$etat = valeur(x$etat)
  x$groupe = valeur(x$groupe)
  x$auteur = valeur(x$auteur)
  x$texte = valeur(x$texte)
  x$article = valeur(x$article)
  x$numero = valeur(x$numero)
  x$date_depot = valeur(x$date_depot)
  comptes = function(v, nom) {
    t = sort(table(v), decreasing = TRUE)
    data.frame(Categorie = names(t), Nombre = as.integer(t), check.names = FALSE)
  }
  tableau = function(d) {
    if (!nrow(d)) return("Aucune donn\u00e9e disponible.")
    .zebrer_tableau_pdf(knitr::kable(d, format = "latex", booktabs = TRUE,
                                     row.names = FALSE, escape = TRUE))
  }
  entete = if (is.null(sujet) || !nzchar(sujet)) "S\u00e9lection d'amendements" else sujet
  contenu = c(
    paste0("\\noindent\\textbf{Recherche :} ", esc(entete), "\\par"),
    paste0("\\noindent\\textbf{S\u00e9lection :} ", nrow(x), " amendement(s)\\par"),
    paste0("\\noindent\\textbf{\u00c9dition :} ", format(Sys.Date(), "%d/%m/%Y"), "\\par"),
    "\\smallskip\\noindent\\footnotesize Source : Assembl\u00e9e nationale, donn\u00e9es officielles import\u00e9es localement. Les effectifs portent uniquement sur la s\u00e9lection.\\normalsize",
    "\\section*{\u00c9tats de traitement}", tableau(comptes(x$etat)),
    "\\section*{Sorts}", tableau(comptes(x$sort)),
    "\\section*{Groupes politiques}", tableau(comptes(x$groupe)),
    "\\section*{Amendements}"
  )
  if (!nrow(x)) contenu = c(contenu, "Aucun amendement dans la s\u00e9lection.")
  for (i in seq_len(nrow(x))) {
    a = x[i, , drop = FALSE]
    ligne = paste0("\\noindent\\textbf{Amendement ", esc(a$numero), "} ",
                   "\\hfill\\textit{", esc(a$date_depot), "}\\par\n",
                   "\\noindent ", esc(a$auteur), " (", esc(a$groupe), ")",
                   " \\quad \u00c9tat : ", esc(a$etat), " \\quad Sort : ", esc(a$sort), "\\par\n",
                   "\\noindent\\textit{", esc(extrait(a$texte, 125L)), "}",
                   " \\quad Article : ", esc(a$article), "\\par\n",
                   "\\noindent\\textbf{Dispositif :} ", esc(extrait(a$dispositif)), "\\par\n",
                   "\\noindent\\textbf{Expos\u00e9 sommaire :} ", esc(extrait(a$expose)), "\\par\n",
                   "\\begingroup\\scriptsize UID : \\texttt{", esc(as.character(a$uid)), "}\\endgroup\\par\\medskip\n",
                   "\\noindent\\rule{\\linewidth}{0.2pt}\\par")
    contenu = c(contenu, paste0("\\begin{minipage}{\\linewidth}\n", ligne, "\n\\end{minipage}\n\\par\\medskip"))
  }
  chemin = .pdf_senateurs("Assembl\u00e9e nationale - Suivi des amendements", entete,
                          paste(contenu, collapse = "\n\n"), sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir = ouvrir)
}
