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
  liens = list()
  page = 0L; y = 0
  nouvelle_page = function() {
    grid::grid.newpage()
    page <<- page + 1L
    texte("Assembl\u00e9e nationale : composition", 17, 13, 11, bleu, TRUE)
    image_logo = tryCatch(.logo_code(1L), error = function(e) NULL)
    if (!is.null(image_logo)) grid::grid.raster(image_logo,
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
    rect(17, y, 3, 10, couleur(sigle))
    texte(paste0(nom, " (", sigle, ")"), 24, y + 1, 10, bleu, TRUE)
    texte(paste(n, "d\u00e9put\u00e9s"), 24, y + 7, 8, gris)
    y <<- y + 16
  }
  dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
  grDevices::cairo_pdf(fichier, width = W / 25.4, height = H / 25.4,
    onefile = TRUE, family = "sans")
  ferme = TRUE
  on.exit(if (ferme) grDevices::dev.off(), add = TRUE)
  nouvelle_page()
  # M\u00e9tadonn\u00e9es de provenance : une seule fois, en haut de la premi\u00e8re page.
  h_meta = 25
  rect(17, y, 176, h_meta, "#E8EBEF")
  for (i in seq_along(metadonnees)) {
    yy = y + 2.1 + (i - 1) * 4.1
    texte(names(metadonnees)[i], 21, yy, 7, gris, TRUE)
    texte(unname(metadonnees[i]), 74, yy, 7, bleu)
  }
  if (length(source_url) == 1L && !is.na(source_url) && nzchar(source_url)) {
    yy = y + 2.1 + length(metadonnees) * 4.1
    texte("Document officiel", 21, yy, 7, gris, TRUE)
    texte("Lien document officiel", 74, yy, 7, "#235E9B")
    liens[[length(liens) + 1L]] = list(page = page, x = 74, haut = yy,
      largeur = 62, url = source_url)
  }
  y = y + h_meta + 6
  texte("Composition de l'Assembl\u00e9e nationale", 17, y, 15, bleu, TRUE)
  y = y + 11
  texte(paste(nrow(deputes), "d\u00e9put\u00e9s \u00b7", length(unique(deputes$sigle)), "groupes ou cat\u00e9gories"),
    17, y, 10, gris)
  y = y + 9
  effectifs = stats::aggregate(uid ~ sigle + groupe, deputes, length)
  names(effectifs)[3] = "n"
  effectifs = effectifs[order(-effectifs$n, effectifs$groupe), , drop = FALSE]
  for (i in seq_len(nrow(effectifs))) {
    assurer(7.5)
    rect(17, y, 176, 6.7, couleur(effectifs$sigle[i]))
    texte(paste0(effectifs$groupe[i], " (", effectifs$sigle[i], ")"), 21, y + 0.6, 9.3, "white", TRUE)
    texte(effectifs$n[i], 182, y + 0.6, 9.3, "white", TRUE)
    y = y + 7.5
  }
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
      h = 5.6 + length(fonction) * 3.6
      assurer(h)
      texte(nom, 24, y, 8.4, bleu, isTRUE(deputes$president[i]))
      if (nzchar(circo)) texte(circo, 136, y, 7.7, gris)
      if (length(fonction)) for (j in seq_along(fonction))
        texte(fonction[j], 26, y + 4.2 + (j-1) * 3.6, 7.2, gris)
      rect(24, y + h - .7, 169, .15, trait)
      y = y + h
    }
    y = y + 2
  }
  grDevices::dev.off()
  ferme = FALSE
  if (length(liens)) .liens_pdf_grid(fichier, liens, hauteur = H)
  normalizePath(fichier, mustWork = TRUE)
}
