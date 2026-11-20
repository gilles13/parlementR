#' En-tete commun des documents parlementR
#'
#' Dessine le logo, le titre et une ligne de separation sur la page grid
#' courante. Ne cree pas de nouvelle page et ne ferme pas le peripherique.
#' @keywords internal
#' @param titre Titre du document.
#' @param sous_titre Texte facultatif sous le titre.
#' @param logo Entier de 1 a 6, ou booleen pour compatibilite.
#' @return Invisible NULL.
.entete = function(titre, sous_titre = NULL, logo = TRUE) {
  stopifnot(is.character(titre), length(titre) == 1L, !is.na(titre))
  if (!is.null(sous_titre)) stopifnot(is.character(sous_titre), length(sous_titre) == 1L)
  if (!identical(logo, FALSE)) {
    if (identical(logo, TRUE)) logo = 1L
    image = .logo_code(logo)
    hauteur = grid::unit(1.2, "cm")
    largeur = grid::unit(1.2 * dim(image)[2L] / dim(image)[1L], "cm")
    grid::grid.raster(image, x = .875, y = .925,
                      width = largeur, height = hauteur, interpolate = TRUE)
  }
  grid::grid.text(titre, x = .08, y = .942, just = "left",
                  gp = grid::gpar(fontsize = 16, fontface = "bold", col = "#163D5C"))
  if (!is.null(sous_titre)) {
    grid::grid.text(sous_titre, x = .08, y = .909, just = "left",
                    gp = grid::gpar(fontsize = 9, col = "#555555"))
  }
  grid::grid.lines(x = c(.08, .92), y = c(.858, .858),
                   gp = grid::gpar(col = "#163D5C", lwd = 1))
  invisible(NULL)
}
