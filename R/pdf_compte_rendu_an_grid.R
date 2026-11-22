.pdf_compte_rendu_an_grid = function(cr, sortie) {
  H = 297; y = 26; page = 0L
  afficher = function(s, haut, taille = 9, gras = FALSE, x = 17, couleur = '#183149')
    grid::grid.text(s, x = grid::unit(x, 'mm'), y = grid::unit(H - haut, 'mm'),
      just = c('left', 'top'), gp = grid::gpar(fontfamily = 'sans', fontsize = taille,
        fontface = if (gras) 'bold' else 'plain', col = couleur))
  nouvelle_page = function() {
    grid::grid.newpage()
    page <<- page + 1L
    .entete_grid('Assemblee nationale', 'Compte rendu de commission', logo = 1L, taille_logo = 9)
    grid::grid.lines(x = grid::unit(c(17, 193), 'mm'), y = grid::unit(278, 'mm'),
      gp = grid::gpar(col = '#dce4eb'))
    afficher(paste('Source : Assembl\u00e9e nationale |', cr$reference), 284, 7.2, couleur = '#607387')
    afficher(paste('Page', page), 284, 7.2, x = 177, couleur = '#607387')
    y <<- 26
  }
  ecrire = function(s, taille = 9, gras = FALSE, largeur = 100, pas = 4.5, espace = 2) {
    if (length(s) != 1L || is.na(s) || !nzchar(trimws(s))) return(invisible(NULL))
    lignes = strwrap(gsub('[[:space:]]+', ' ', s), width = largeur)
    for (l in lignes) {
      if (y + pas > 274) nouvelle_page()
      afficher(l, y, taille, gras)
      y <<- y + pas
    }
    y <<- y + espace
    invisible(NULL)
  }
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  grDevices::cairo_pdf(sortie, width = 210 / 25.4, height = H / 25.4, family = 'sans')
  on.exit(grDevices::dev.off(), add = TRUE)
  nouvelle_page()
  ecrire('COMPTE RENDU DE COMMISSION', 8, TRUE, espace = 4)
  ecrire(cr$titre, 14, TRUE, largeur = 53, pas = 7, espace = 4)
  if (!is.null(cr$date) && !is.na(cr$date))
    ecrire(paste('Date :', format(as.Date(cr$date), '%d/%m/%Y')))
  if (!is.null(cr$numero)) ecrire(cr$numero)
  ecrire(paste('R\u00e9f\u00e9rence :', cr$reference), 8, espace = 5)
  ecrire('Compte rendu int\u00e9gral', 12, TRUE, espace = 4)
  paragraphes = trimws(strsplit(cr$texte, '\n', fixed = TRUE)[[1]])
  for (p in paragraphes[nzchar(paragraphes)])
    ecrire(p, taille = 8.6, largeur = 107, pas = 4.25, espace = 1.5)
  invisible(sortie)
}
