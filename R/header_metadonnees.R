# Cartouche commun de provenance des documents PDF grid.
# Les donnees sont fournies par le document appelant ; aucun acces a la base.
.header_metadonnees = function(metadonnees, source_url = NULL, y = 34,
                                page = 1L, hauteur_page = 297) {
  H = hauteur_page
  bleu = "#183149"
  gris = "#53616D"
  fond = "#D9DEE3"
  rect = function(x, yy, w, h, fill) grid::grid.rect(
    x = grid::unit(x, "mm"), y = grid::unit(H-yy-h, "mm"),
    width = grid::unit(w, "mm"), height = grid::unit(h, "mm"),
    just = c("left", "bottom"), gp = grid::gpar(fill = fill, col = NA))
  texte = function(s, x, yy, taille = 4.3, col = bleu, gras = FALSE) grid::grid.text(
    as.character(s), x = grid::unit(x, "mm"), y = grid::unit(H-yy, "mm"),
    just = c("left", "top"), gp = grid::gpar(fontfamily = "sans",
      fontsize = taille, col = col, fontface = if (gras) "bold" else "plain"))
  archive_disponible = length(source_url) == 1L && !is.na(source_url) &&
    nzchar(source_url) && grepl("^https://", source_url)
  pas = 3.05
  h = 7.5 + length(metadonnees) * pas + 3.5 +
    if (archive_disponible) 3.5 else 0
  h = h + 7.5
  rect(17, y, 176, h, fond)
  texte("PROVENANCE ET INT\u00c9GRIT\u00c9", 21, y + 2, 6.2, bleu, TRUE)
  texte("Source identifi\u00e9e \u00b7 Empreintes conserv\u00e9es",
    103, y + 2.4, 4.5, gris)
  # Embleme discret aligne a droite : bouclier ouvert en pointe, coche.
  sx = 187; sy = y + 1.45
  grid::grid.polygon(
    x = grid::unit(sx + c(0, 2.2, 4.4, 4.2, 3.5, 2.2, .9, .2), "mm"),
    y = grid::unit(H-sy-c(.45, 0, .45, 2.7, 4.0, 4.7, 4.0, 2.7), "mm"),
    gp = grid::gpar(fill = NA, col = bleu, lwd = .85, ljoin = "round"))
  grid::grid.lines(
    x = grid::unit(sx + c(1.05, 1.85, 3.4), "mm"),
    y = grid::unit(H-sy-c(2.25, 3.05, 1.65), "mm"),
    gp = grid::gpar(col = "#287B69", lwd = 1.15,
      lineend = "round", linejoin = "round"))
  yy = y + 8.2
  for (i in seq_along(metadonnees)) {
    ligne = yy + (i-1) * pas
    texte(names(metadonnees)[i], 21, ligne, 4.3, gris, TRUE)
    taille = if (grepl("SHA-256", names(metadonnees)[i], fixed = TRUE)) 4 else 4.3
    texte(unname(metadonnees[i]), 75, ligne, taille, bleu)
  }
  yy = yy + length(metadonnees) * pas
  texte("Source officielle", 21, yy, 4.3, gris, TRUE)
  texte("data.assemblee-nationale.fr", 75, yy, 4.3, "#235E9B")
  liens = list(list(page = page, x = 75, haut = yy,
    largeur = 90, url = "https://data.assemblee-nationale.fr/"))
  yy = yy + 3.5
  if (archive_disponible) {
    texte("Archive source", 21, yy, 4.3, gris, TRUE)
    texte("Acc\u00e9der au fichier ZIP", 75, yy, 4.3, "#235E9B")
    liens[[length(liens)+1L]] = list(page = page, x = 75,
      haut = yy, largeur = 90, url = source_url)
    yy = yy + 3.5
  }
  texte("Archive ZIP contenant des fichiers JSON ; traitement local par parlementR.",
    21, yy + .4, 4.3, gris)
  texte("Rapport non officiel de l'Assembl\u00e9e nationale.",
    21, yy + 3.4, 4.3, gris)
  list(hauteur = h, liens = liens)
}
