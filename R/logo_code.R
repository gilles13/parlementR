# Source unique du logo historique pour les sorties grid et LaTeX.
.logo_parlementR = local({
  image = NULL
  octets = NULL
  function(fichier = FALSE) {
    if (is.null(image)) {
      chemin = system.file("assets", "01_hemicycle_tricolore.svg", package = "parlementR")
      if (!nzchar(chemin)) chemin = file.path("inst", "assets", "01_hemicycle_tricolore.svg")
      if (!file.exists(chemin))
        stop("Logo officiel absent : inst/assets/01_hemicycle_tricolore.svg", call. = FALSE)
      if (!requireNamespace("rsvg", quietly = TRUE))
        stop("Le paquet rsvg est necessaire pour le logo SVG.", call. = FALSE)
      octets <<- rsvg::rsvg_png(chemin, width = 180)
      image <<- png::readPNG(octets)
    }
    if (isTRUE(fichier)) {
      sortie = tempfile("parlementR-logo-", fileext = ".png")
      writeBin(octets, sortie)
      return(sortie)
    }
    image
  }
})

# Compatibilite avec les anciens appels internes ; le choix numerique est ignore.
.logo_code = function(logo = 1L) .logo_parlementR()
