.pdf_fiche_depute_grid = function(nom, identite, sections, photo, sortie,
                                    assemblee = "Assemblee nationale",
                                    source = "Assemblee nationale") {
  W = 210; H = 297
  encre = '#183149'; gris = '#607387'; trait = '#dce4eb'
  accent = '#183149'; fond = '#f2f6f9'
  x0 = 17; x1 = 193; y = 0; page = 0L
  liens = list()
  rectangle = function(x, haut, w, h, couleur, bord = NA, rayon = 0) {
    grid::grid.roundrect(x = grid::unit(x, 'mm'),
      y = grid::unit(H - haut - h, 'mm'),
      width = grid::unit(w, 'mm'), height = grid::unit(h, 'mm'),
      just = c('left', 'bottom'), r = grid::unit(rayon, 'mm'),
      gp = grid::gpar(fill = couleur, col = bord, lwd = 0.6))
  }
  texte = function(s, x, haut, taille = 9, couleur = encre, gras = FALSE) {
    url = regmatches(s, regexpr("https?://[^[:space:];)]+", s))
    if (length(url) && nzchar(url)) {
      liens[[length(liens) + 1L]] <<- list(page = page, x = x,
        haut = haut, url = url, largeur = min(180, max(20, nchar(s) * taille * 0.19)))
    }
    grid::grid.text(s, x = grid::unit(x, 'mm'),
      y = grid::unit(H - haut, 'mm'), just = c('left', 'top'),
      gp = grid::gpar(fontfamily = 'sans', fontsize = taille,
                      fontface = if (gras) 'bold' else 'plain', col = couleur))
  }
  ligne = function(haut) rectangle(x0, haut, x1 - x0, 0.35, trait)
  # Largeurs de retour a la ligne conservatrices, en caracteres.
  couper = function(x, largeur) {
    x = as.character(x)
    x[is.na(x)] = ''
    z = unlist(lapply(strsplit(x, '\n', fixed = TRUE), function(s)
      unlist(lapply(s, function(t) {
        v = strwrap(t, width = largeur)
        if (length(v)) v else ''
      }))), use.names = FALSE)
    if (length(z)) z else ''
  }
  nouvelle_page = function() {
    grid::grid.newpage()
    page <<- page + 1L
    .entete_grid(assemblee, "Fiche individuelle", logo = 1L, taille_logo = 9)
    ligne(19)
    texte(paste('Source :', source, '| Extraction locale :',
                format(Sys.Date(), '%d/%m/%Y')), x0, 284, 7.4, gris)
    texte(paste('Page', page), 173, 284, 7.4, gris)
    y <<- 26
  }
  assurer = function(h) if (y + h > 274) nouvelle_page()
  section = function(titre) {
    assurer(11)
    texte(titre, x0, y - 0.5, 11, encre, TRUE)
    y <<- y + 7
  }
  cellule = function(label, value, fond_cellule = FALSE) {
    a = couper(label, 22)
    b = couper(value, 78)
    h = max(length(a), length(b)) * 3.5 + 1.5
    assurer(h)
    if (fond_cellule) rectangle(x0, y, x1 - x0, h, fond)
    for (i in seq_along(a)) texte(a[i], x0 + 3, y + 0.5 + (i - 1) * 3.5, 8, gris, TRUE)
    for (i in seq_along(b)) texte(b[i], x0 + 51, y + 0.5 + (i - 1) * 3.5, 8.4)
    y <<- y + h
    invisible(NULL)
  }
  entree = function(x) {
    morceaux = couper(x, 105)
    h = length(morceaux) * 3.7 + 1.1
    assurer(h)
    for (i in seq_along(morceaux))
      texte(morceaux[i], x0 + 3, y + 0.3 + (i - 1) * 3.7, 8.6)
    y <<- y + h
    invisible(NULL)
  }
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  grDevices::cairo_pdf(sortie, width = W / 25.4, height = H / 25.4,
                       onefile = TRUE, family = 'sans')
  on.exit(grDevices::dev.off(), add = TRUE)
  nouvelle_page()
  # Cartouche : colonne de texte independante de la photographie.
  valeurs = stats::setNames(as.character(identite$Valeur), identite$Information)
  champ = function(cle) {
    x = valeurs[cle]
    if (!length(x) || is.na(x)) "" else unname(x)
  }
  lieu = champ("Lieu de naissance")
  pays = champ("Pays de naissance")
  if (nzchar(pays)) lieu = paste0(lieu, " (", pays, ")")
  # Nom et groupe constituent l'en-tete, sans libelle pour le groupe.
  lignes_nom = couper(nom, 30)
  lignes_groupe = couper(champ("Groupe politique"), 43)
  if (!nzchar(champ("Groupe politique")) || champ("Groupe politique") == "Inconnu") lignes_groupe = character()
  # La declaration HATVP est une information d'identite, pas une rubrique.
  hatvp = sections[["D\u00e9claration HATVP"]]
  if (!is.null(hatvp)) sections[["D\u00e9claration HATVP"]] = NULL
  hatvp = if (length(hatvp)) as.character(hatvp)[1] else ""
  if (is.na(hatvp) || !grepl("^https?://", hatvp)) hatvp = ""
  elements = list(
    c("Civilit\u00e9", champ("Civilit\u00e9")),
    c("Date de naissance", champ("Date de naissance")),
    c("Lieu de naissance", lieu),
    c("Profession", champ("Profession"))
  )
  # Taille ajustee a la largeur disponible, pour garder chaque champ sur une ligne.
  taille_ajustee = function(s, taille = 8.5, largeur = 121) {
    if (!nzchar(s)) return(taille)
    grid::pushViewport(grid::viewport(gp = grid::gpar(fontfamily = "sans")))
    on.exit(grid::popViewport())
    while (taille > 6.2) {
      w = grid::convertWidth(grid::grobWidth(grid::textGrob(s, gp = grid::gpar(fontsize = taille))),
                             "mm", valueOnly = TRUE)
      if (is.finite(w) && w <= largeur) break
      taille = taille - 0.25
    }
    taille
  }
  position = 7 + 6.5 * length(lignes_nom) + 1
  groupe_haut = position
  position = position + max(1, length(lignes_groupe)) * 4.4 + 2
  hauteur = max(56, position + (length(elements) + as.integer(nzchar(hatvp))) * 5.0 + 3)
  rectangle(x0, y, 176, hauteur, fond, rayon = 2.5)
  for (i in seq_along(lignes_nom))
    texte(lignes_nom[i], x0 + 6, y + 7 + (i - 1) * 6.5, 16, encre, TRUE)
  for (i in seq_along(lignes_groupe))
    texte(lignes_groupe[i], x0 + 6, y + groupe_haut + (i - 1) * 4.4,
          10.5, encre, TRUE)
  for (i in seq_along(elements)) {
    ligne_texte = paste0(elements[[i]][1], " : ",
                         if (nzchar(elements[[i]][2])) elements[[i]][2] else "Inconnu")
    texte(ligne_texte, x0 + 6, y + position + (i - 1) * 5.0,
          taille_ajustee(ligne_texte), encre)
  }
  if (nzchar(hatvp)) {
    haut_hatvp = y + position + length(elements) * 5.0
    texte("HATVP", x0 + 6, haut_hatvp, 8.5, encre, TRUE)
    # Lien PDF sur le libelle uniquement, sans afficher l'URL brute.
    liens[[length(liens) + 1L]] = list(page = page, x = x0 + 6,
      haut = haut_hatvp, url = hatvp, largeur = 14)
  }
  if (!is.null(photo) && file.exists(photo) && requireNamespace('jpeg', quietly = TRUE)) {
    img = tryCatch(jpeg::readJPEG(photo), error = function(e) NULL)
    if (!is.null(img)) {
      # Le cadre et la photographie restent integralement dans le cartouche.
      cadre_h = min(46, hauteur - 10)
      cadre_y = y + (hauteur - cadre_h) / 2
      rectangle(153, cadre_y, 36, cadre_h, '#ffffff', trait, 1.5)
      ratio = dim(img)[2] / dim(img)[1]
      ph = min(cadre_h - 4, 32 / ratio)
      pw = ph * ratio
      grid::grid.raster(img, x = grid::unit(171, 'mm'),
        y = grid::unit(H - cadre_y - cadre_h / 2, 'mm'),
        width = grid::unit(pw, 'mm'), height = grid::unit(ph, 'mm'),
        interpolate = TRUE)
    }
  }
  y = y + hauteur + 3
  for (titre in names(sections)) {
    d = sections[[titre]]
    if (is.null(d) || (is.data.frame(d) && !nrow(d)) || !length(d)) next
    section(titre)
    if (is.data.frame(d)) {
      if (all(c('Information', 'Valeur') %in% names(d))) {
        for (i in seq_len(nrow(d))) cellule(d$Information[i], d$Valeur[i], i %% 2 == 0)
      } else if (all(c('Categorie', 'Organisme', 'Fonction', 'Debut', 'Fin') %in% names(d))) {
        for (i in seq_len(nrow(d))) {
          lib = paste(d$Categorie[i], ' - ', d$Organisme[i])
          detail = paste(d$Fonction[i], ' | ', d$Debut[i], ' - ', d$Fin[i])
          lignes_lib = couper(lib, 102)
          lignes_detail = couper(detail, 110)
          h = (length(lignes_lib) + length(lignes_detail)) * 3.7 + 1.5
          assurer(h)
          if (i %% 2 == 0) rectangle(x0, y, 176, h, fond)
          for (j in seq_along(lignes_lib))
            texte(lignes_lib[j], x0 + 3, y + 0.5 + (j - 1) * 3.7,
                  8.8, encre, TRUE)
          for (j in seq_along(lignes_detail))
            texte(lignes_detail[j], x0 + 3,
                  y + 0.5 + (length(lignes_lib) + j - 1) * 3.7,
                  8, gris)
          y = y + h
          invisible(NULL)
        }
      } else {
        for (i in seq_len(nrow(d))) entree(paste(d[i, ], collapse = '  |  '))
      }
    } else for (v in as.character(d)) entree(v)
    y = y + 1.5
  }
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)
  if (length(liens)) .liens_pdf_grid(sortie, liens, hauteur = H)
  normalizePath(sortie, winslash = "/", mustWork = TRUE)
}
