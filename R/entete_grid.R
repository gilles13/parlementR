# En-tete commun des rapports parlementaires en grid.
.entete_grid = function(assemblee, sous_titre, logo = 1L) {
  image = .logo_code(logo)
  grid::grid.raster(image,
    x = grid::unit(185, "mm"), y = grid::unit(285, "mm"),
    width = grid::unit(15, "mm"), height = grid::unit(15, "mm"),
    interpolate = TRUE)
  grid::grid.text("parlementR", x = grid::unit(17, "mm"),
    y = grid::unit(288, "mm"), just = "left",
    gp = grid::gpar(fontsize = 11, fontface = "bold", col = "#183149"))
  grid::grid.text(paste(assemblee, "/", sous_titre),
    x = grid::unit(66, "mm"), y = grid::unit(288, "mm"),
    just = "left", gp = grid::gpar(fontsize = 8, col = "#607387"))
  invisible(NULL)
}
