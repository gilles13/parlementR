# Chargement commun des logos pour les documents LEGI.
.logo_code = function(logo = 1L) {
  if (length(logo) != 1L || !is.numeric(logo) || is.na(logo) ||
      !is.finite(logo) || logo != as.integer(logo) || !logo %in% 1:6)
    stop("logo doit etre un entier entre 1 et 6")
  dossier = system.file("assets", package = "parlementR")
  if (!nzchar(dossier)) dossier = file.path("inst", "assets")
  if (!dir.exists(dossier)) stop("Dossier des logos introuvable : ", dossier)
  fichiers = list.files(dossier, full.names = TRUE)
  noms = basename(fichiers)
  candidats = which(startsWith(noms, sprintf("%02d_", logo)) &
                     grepl("\\.svg$", noms, ignore.case = TRUE))
  if (length(candidats) != 1L)
    stop("Logo SVG ", logo, " introuvable ou ambigu dans ", dossier)
  fichier = fichiers[candidats[[1L]]]
  if (grepl("\\.png$", fichier, ignore.case = TRUE)) return(png::readPNG(fichier))
  if (!requireNamespace("rsvg", quietly = TRUE))
    stop("Le paquet rsvg est necessaire pour les logos SVG")
  png::readPNG(rsvg::rsvg_png(fichier, width = 180))
}
