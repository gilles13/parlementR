#' En-tete commun des documents parlementR
#'
#' Dessine le logo, le titre et une ligne de separation sur la page grid
#' courante. Ne cree pas de nouvelle page et ne ferme pas le peripherique.
#' @keywords internal
#' @param titre Titre du document.
#' @param sous_titre Texte facultatif sous le titre.
#' @param logo Afficher le logo du package.
#' @return Invisible NULL.
.entete = function(titre, sous_titre = NULL, logo = TRUE) {
  stopifnot(is.character(titre), length(titre) == 1L, !is.na(titre))
  if (!is.null(sous_titre)) stopifnot(is.character(sous_titre), length(sous_titre) == 1L)
  if (isTRUE(logo)) {
    chemin = system.file("assets", "logo_hexa.png", package = "parlementR")
    # En developpement, load_all() fournit normalement le chemin inst/.
    if (!nzchar(chemin)) {
      chemin = file.path("inst", "assets", "logo_hexa.png")
    }
    if (file.exists(chemin)) {
      image = png::readPNG(chemin)
      # Dimensions physiques : le ratio de l'image est conserve sur une page A4.
      largeur = grid::unit(.76, "in")
      hauteur = grid::unit(.76 * dim(image)[1L] / dim(image)[2L], "in")
      grid::grid.raster(image, x = .875, y = .916,
                        width = largeur, height = hauteur, interpolate = TRUE)
    }
  }
  grid::grid.text(titre, x = .08, y = .942, just = "left",
                  gp = grid::gpar(fontsize = 16, fontface = "bold", col = "#163D5C"))
  if (!is.null(sous_titre)) {
    grid::grid.text(sous_titre, x = .08, y = .909, just = "left",
                    gp = grid::gpar(fontsize = 9, col = "#555555"))
  }
  grid::grid.lines(x = c(.08, .92), y = c(.872, .872),
                   gp = grid::gpar(col = "#163D5C", lwd = 1))
  invisible(NULL)
}
