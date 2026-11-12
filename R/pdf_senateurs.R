# PDF local commun aux deux productions senateurs.
.echapper_latex_senateurs = function(x) {
  x = as.character(x)
  x[is.na(x)] = ""
  for (i in seq_along(x)) {
    v = x[i]
    # Echapper les caracteres speciaux sans retraiter les antislashs inseres.
    morceaux = strsplit(v, "", fixed = TRUE)[[1]]
    table = c("\\" = "\\textbackslash{}", "{" = "\\{", "}" = "\\}",
              "#" = "\\#", "$" = "\\$", "%" = "\\%", "&" = "\\&",
              "_" = "\\_", "^" = "\\textasciicircum{}", "~" = "\\textasciitilde{}")
    x[i] = paste0(ifelse(morceaux %in% names(table), unname(table[morceaux]), morceaux), collapse = "")
  }
  x
}

.logo_pdf_parlementR = function() {
  logo = system.file("assets", "logo_hexa.png", package = "parlementR")
  if (!nzchar(logo)) logo = file.path("inst", "assets", "logo_hexa.png")
  if (!file.exists(logo)) stop("Logo hexagonal introuvable.", call. = FALSE)
  normalizePath(logo, winslash = "/", mustWork = TRUE)
}

.pdf_senateurs = function(titre, sous_titre, contenu, sortie, modele = NULL) {
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Le package 'rmarkdown' est necessaire.")
  if (is.null(modele)) modele = system.file("modeles", "senateurs.Rmd", package = "parlementR")
  if (!nzchar(modele) || !file.exists(modele)) stop("Modele PDF senateurs introuvable.")
  logo = .logo_pdf_parlementR()
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  cible = normalizePath(dirname(sortie), mustWork = TRUE)
  resultat = rmarkdown::render(input = modele, output_file = basename(sortie),
    output_dir = cible, params = list(titre = .echapper_latex_senateurs(titre),
    sous_titre = .echapper_latex_senateurs(sous_titre),
    logo = normalizePath(logo, winslash = "/", mustWork = TRUE),
    contenu = contenu), envir = new.env(parent = globalenv()), quiet = TRUE)
  normalizePath(resultat, mustWork = TRUE)
}

.ouvrir_pdf_senateurs = function(chemin, ouvrir = TRUE) {
  message("PDF produit : ", chemin)
  if (isTRUE(ouvrir)) {
    commande = Sys.which("xdg-open")
    if (nzchar(commande)) system2(commande, shQuote(chemin), wait = FALSE)
  }
  invisible(chemin)
}
