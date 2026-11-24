# Catalogue A4 produit directement avec grid et Cairo.
.pdf_catalogue_deputes_grid = function(deputes, metadonnees, source_url, fichier, enrichie) {
  if (!capabilities("cairo")) stop("Le support Cairo est necessaire.")
  W = 210; H = 297
  bleu = "#183149"; gris = "#667686"; trait = "#DCE4EB"; fond = "#E5E9ED"
  rect = function(x, y, w, h, fill) grid::grid.rect(
    x = grid::unit(x, "mm"), y = grid::unit(H-y-h, "mm"),
    width = grid::unit(w, "mm"), height = grid::unit(h, "mm"),
    just = c("left", "bottom"), gp = grid::gpar(fill = fill, col = NA))
  texte = function(s, x, y, taille = 9, col = bleu, gras = FALSE) grid::grid.text(
    as.character(s), x = grid::unit(x, "mm"), y = grid::unit(H-y, "mm"),
    just = c("left", "top"), gp = grid::gpar(fontfamily = "sans", fontsize = taille,
      col = col, fontface = if (gras) "bold" else "plain"))
  couper = function(s, largeur) {
    s = as.character(s)
    if (is.na(s) || !nzchar(s)) return(character())
    unlist(lapply(strsplit(s, "\\n", fixed = TRUE)[[1]], function(z) {
      a = strwrap(z, width = largeur)
      if (length(a)) a else ""
    }), use.names = FALSE)
  }
  liens = list()
  page = 0L; y = 0
  nouvelle_page = function() {
    grid::grid.newpage()
    page <<- page + 1L
    texte("Assembl\u00e9e nationale : composition", 17, 13, 11, bleu, TRUE)
    image_logo = .logo_parlementR()
    grid::grid.raster(image_logo,
      x = grid::unit(185, "mm"), y = grid::unit(H - 16, "mm"),
      width = grid::unit(13, "mm"), height = grid::unit(13, "mm"),
      interpolate = TRUE)
    rect(17, 24, 176, .4, trait)
    rect(17, 280, 176, .3, trait)
    texte(paste("parlementR  \u00b7  \u00c9dition du", format(Sys.Date(), "%d/%m/%Y")), 17, 284, 7.5, gris)
    texte(paste("Page", page), 177, 284, 7.5, gris)
    y <<- 34
  }
  assurer = function(h) if (y + h > 272) nouvelle_page()
  groupe_ligne = function(nom, sigle, n) {
    assurer(16)
    texte(paste0(nom, " (", sigle, ")"), 17, y + 1, 10, bleu, TRUE)
    texte(paste(n, "d\u00e9put\u00e9s"), 17, y + 7, 8, gris)
    rect(17, y + 13, 176, .35, trait)
    y <<- y + 17
  }
  dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
  grDevices::cairo_pdf(fichier, width = W / 25.4, height = H / 25.4,
    onefile = TRUE, family = "sans")
  ferme = TRUE
  on.exit(if (ferme) grDevices::dev.off(), add = TRUE)
  nouvelle_page()
  # Premiere page : provenance, puis repartition compacte.
  effectifs = stats::aggregate(uid ~ sigle + groupe, deputes, length)
  names(effectifs)[3] = "n"
  effectifs = effectifs[order(-effectifs$n, effectifs$groupe), , drop = FALSE]
  bloc = .header_metadonnees(
    metadonnees = metadonnees, source_url = source_url,
    y = y, page = page, hauteur_page = H)
  liens = c(liens, bloc$liens)
  bloc_y = y
  bloc_h = bloc$hauteur
  y = bloc_y + bloc_h + 10
  texte("R\u00e9partition par groupe", 17, y, 10, bleu, TRUE)
  y = y + 8
  texte(paste(nrow(deputes), "d\u00e9put\u00e9s"), 17, y, 11, bleu, TRUE)
  texte(paste(nrow(effectifs), "groupes ou cat\u00e9gories"), 82, y + .5, 8, gris)
  y = y + 10
  # Trois colonnes, avec alignement numerique a droite.
  x_n = 168
  x_part = 192
  texte("Groupe politique", 18, y, 7.2, gris, TRUE)
  grid::grid.text("Effectif", x = grid::unit(x_n, "mm"),
    y = grid::unit(H-y, "mm"), just = c("right", "top"),
    gp = grid::gpar(fontfamily = "sans", fontsize = 7.2, col = gris, fontface = "bold"))
  grid::grid.text("Part", x = grid::unit(x_part, "mm"),
    y = grid::unit(H-y, "mm"), just = c("right", "top"),
    gp = grid::gpar(fontfamily = "sans", fontsize = 7.2, col = gris, fontface = "bold"))
  y = y + 5.5
  rect(17, y - .8, 176, .3, trait)
  for (i in seq_len(nrow(effectifs))) {
    texte(paste0(effectifs$groupe[i], " (", effectifs$sigle[i], ")"),
      18, y, 8, bleu)
    grid::grid.text(as.character(effectifs$n[i]),
      x = grid::unit(x_n, "mm"), y = grid::unit(H-y, "mm"),
      just = c("right", "top"),
      gp = grid::gpar(fontfamily = "sans", fontsize = 8.2,
        col = bleu, fontface = "bold"))
    part = sprintf("%.1f %%", 100 * effectifs$n[i] / nrow(deputes))
    part = sub("\\.", ",", part)
    grid::grid.text(part, x = grid::unit(x_part, "mm"),
      y = grid::unit(H-y, "mm"), just = c("right", "top"),
      gp = grid::gpar(fontfamily = "sans", fontsize = 8, col = bleu))
    rect(17, y + 4.2, 176, .13, trait)
    y = y + 4.9
  }
  texte("Ensemble", 18, y + .8, 8, bleu, TRUE)
  grid::grid.text(as.character(nrow(deputes)),
    x = grid::unit(x_n, "mm"), y = grid::unit(H-y-.8, "mm"),
    just = c("right", "top"),
    gp = grid::gpar(fontfamily = "sans", fontsize = 8.2, col = bleu, fontface = "bold"))
  grid::grid.text("100,0 %", x = grid::unit(x_part, "mm"),
    y = grid::unit(H-y-.8, "mm"), just = c("right", "top"),
    gp = grid::gpar(fontfamily = "sans", fontsize = 8, col = bleu, fontface = "bold"))
  # Le sommaire occupe seul la premiere page.
  blocs = split(seq_len(nrow(deputes)), factor(deputes$sigle, levels = unique(deputes$sigle)))
  for (k in seq_along(blocs)) {
    idx = blocs[[k]]
    nouvelle_page()
    groupe_ligne(deputes$groupe[idx[1]], deputes$sigle[idx[1]], length(idx))
    for (i in idx) {
      nom = paste0(deputes$prenom[i], " ", deputes$nom[i])
      if (isTRUE(deputes$president[i])) nom = paste0(nom, "  \u00b7  Pr\u00e9sidence du groupe")
      circo = deputes$circonscription[i]
      fonction = if (enrichie) couper(deputes$fonction[i], 92) else character()
      h = 4.55 + length(fonction) * 3.3
      assurer(h)
      texte(nom, 17, y, 8.4, bleu, isTRUE(deputes$president[i]))
      if (nzchar(circo)) texte(circo, 136, y, 7.7, gris)
      if (length(fonction)) for (j in seq_along(fonction))
        texte(fonction[j], 26, y + 3.7 + (j-1) * 3.3, 7.2, gris)
      rect(17, y + h - .35, 176, .12, trait)
      y = y + h
    }
    y = y + 2
  }
  grDevices::dev.off()
  ferme = FALSE
  if (length(liens)) .liens_pdf_grid(fichier, liens, hauteur = H)
  normalizePath(fichier, mustWork = TRUE)
}
