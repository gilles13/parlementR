# Rendu R Markdown des questions du Senat, sans assemblage LaTeX manuel.
.pdf_questions_senat = function(modele, donnees, titre, sous_titre, sortie) {
  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Le package 'rmarkdown' est necessaire.")
  fichier = system.file("modeles", modele, package = "parlementR")
  if (!nzchar(fichier)) fichier = file.path("inst", "modeles", modele)
  if (!file.exists(fichier)) stop("Modele Rmd introuvable : ", modele)
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  resultat = rmarkdown::render(
    input = fichier, output_file = basename(sortie),
    output_dir = normalizePath(dirname(sortie), mustWork = TRUE),
    params = list(titre = .echapper_latex_senateurs(titre),
                  sous_titre = .echapper_latex_senateurs(sous_titre),
                  logo = .logo_pdf_parlementR(), donnees = donnees),
    envir = new.env(parent = globalenv()), quiet = TRUE
  )
  normalizePath(resultat, mustWork = TRUE)
}
