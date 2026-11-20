# Catalogue A4 produit directement avec grid et Cairo.
.pdf_catalogue_deputes_grid = function(deputes, metadonnees, source_url, fichier, enrichie) {
  if (!capabilities("cairo")) stop("Le support Cairo est necessaire.")
  W = 210; H = 297
  bleu = "#183149"; gris = "#667686"; trait = "#DCE4EB"; fond = "#F3F6F9"
  couleurs = .couleurs_groupes()
  couleur = function(sigle) {
    z = unname(couleurs[sigle])
    if (is.na(z)) "#8996A3" else z
  }
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
  page = 0L; y = 0
  nouvelle_page = function() {
    grid::grid.newpage()
    page <<- page + 1L
    texte("Assemblée nationale : composition", 17, 13, 11, bleu, TRUE)
    image_logo = tryCatch(.logo_code(1L), error = function(e) NULL)
    if (!is.null(image_logo)) grid::grid.raster(image_logo,
      x = grid::unit(185, "mm"), y = grid::unit(H - 16, "mm"),
      width = grid::unit(13, "mm"), height = grid::unit(13, "mm"),
      interpolate = TRUE)
    rect(17, 24, 176, .4, trait)
    rect(17, 280, 176, .3, trait)
    texte(paste("parlementR  ·  Édition du", format(Sys.Date(), "%d/%m/%Y")), 17, 284, 7.5, gris)
    texte(paste("Page", page), 177, 284, 7.5, gris)
    y <<- 34
  }
  assurer = function(h) if (y + h > 272) nouvelle_page()
  groupe_ligne = function(nom, sigle, n) {
    assurer(16)
    rect(17, y, 3, 10, couleur(sigle))
    texte(paste0(nom, " (", sigle, ")"), 24, y + 1, 10, bleu, TRUE)
    texte(paste(n, "députés"), 24, y + 7, 8, gris)
    y <<- y + 16
  }
  dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
  grDevices::cairo_pdf(fichier, width = W / 25.4, height = H / 25.4,
    onefile = TRUE, family = "sans")
  ferme = TRUE
  on.exit(if (ferme) grDevices::dev.off(), add = TRUE)
  nouvelle_page()
  texte("Composition de l'Assemblée nationale", 17, y, 15, bleu, TRUE)
  y = y + 11
  texte(paste(nrow(deputes), "députés ·", length(unique(deputes$sigle)), "groupes ou catégories"),
    17, y, 10, gris)
  y = y + 12
  effectifs = aggregate(uid ~ sigle + groupe, deputes, length)
  names(effectifs)[3] = "n"
  effectifs = effectifs[order(-effectifs$n, effectifs$groupe), , drop = FALSE]
  for (i in seq_len(nrow(effectifs))) {
    assurer(8)
    rect(17, y + 1.0, 3, 3, couleur(effectifs$sigle[i]))
    texte(paste0(effectifs$groupe[i], " (", effectifs$sigle[i], ")"), 24, y, 9.5)
    texte(effectifs$n[i], 182, y, 9.5, bleu, TRUE)
    y = y + 7.5
  }
  y = y + 10
  texte("Députés par groupe politique", 17, y, 13, bleu, TRUE)
  y = y + 11
  blocs = split(seq_len(nrow(deputes)), factor(deputes$sigle, levels = unique(deputes$sigle)))
  for (k in seq_along(blocs)) {
    idx = blocs[[k]]
    if (k == 2L) nouvelle_page()
    groupe_ligne(deputes$groupe[idx[1]], deputes$sigle[idx[1]], length(idx))
    for (i in idx) {
      nom = paste0(deputes$prenom[i], " ", deputes$nom[i])
      if (isTRUE(deputes$president[i])) nom = paste0(nom, "  ·  Présidence du groupe")
      circo = deputes$circonscription[i]
      fonction = if (enrichie) couper(deputes$fonction[i], 92) else character()
      h = 7.1 + length(fonction) * 4.3
      assurer(h)
      texte(nom, 24, y, 8.8, bleu, isTRUE(deputes$president[i]))
      if (nzchar(circo)) texte(circo, 136, y, 8, gris)
      if (length(fonction)) for (j in seq_along(fonction))
        texte(fonction[j], 26, y + 5 + (j-1) * 4.3, 7.5, gris)
      rect(24, y + h - 2, 169, .2, trait)
      y = y + h
    }
    y = y + 2
  }
  # Traçabilité placée après la totalité du catalogue.
  assurer(24)
  texte("Sources et métadonnées", 17, y, 13, bleu, TRUE)
  y = y + 12
  for (i in seq_along(metadonnees)) {
    label = names(metadonnees)[i]
    valeur = unname(metadonnees[i])
    lignes = couper(valeur, 96)
    h = 6 + max(1L, length(lignes)) * 4.3
    assurer(h)
    rect(17, y - 1, 176, h, "#E8EBEF")
    texte(label, 20, y, 8, gris, TRUE)
    if (length(lignes)) for (j in seq_along(lignes))
      texte(lignes[j], 68, y + (j-1) * 4.3, 8)
    y = y + h
  }
  if (length(source_url) && !is.na(source_url) && nzchar(source_url)) {
    lignes = couper(source_url, 95)
    assurer(6 + length(lignes) * 4.3)
    texte("Adresse officielle", 17, y, 8, gris, TRUE)
    for (j in seq_along(lignes)) texte(lignes[j], 68, y + (j-1) * 4.3, 8, "#235E9B")
  }
  grDevices::dev.off()
  ferme = FALSE
  normalizePath(fichier, mustWork = TRUE)
}
