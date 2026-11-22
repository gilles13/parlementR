.pdf_legislatif_an_grid = function(donnees, sortie) {
  W = 210; H = 297
  encre = "#183149"; gris = "#607387"; trait = "#dce4eb"
  x0 = 17; x1 = 193; y = 0; page = 0L
  valeur = function(x) {
    if (!length(x) || is.na(x[[1L]]) || !nzchar(trimws(as.character(x[[1L]]))))
      return("")
    as.character(x[[1L]])
  }
  texte = function(s, x, haut, taille = 9, gras = FALSE, couleur = encre) {
    grid::grid.text(s, x = grid::unit(x, "mm"),
      y = grid::unit(H - haut, "mm"), just = c("left", "top"),
      gp = grid::gpar(fontfamily = "sans", fontsize = taille,
        fontface = if (gras) "bold" else "plain", col = couleur))
  }
  ligne = function(haut) grid::grid.lines(
    x = grid::unit(c(x0, x1), "mm"), y = grid::unit(rep(H - haut, 2), "mm"),
    gp = grid::gpar(col = trait, lwd = 0.8))
  couper = function(x, largeur = 90L) {
    if (!nzchar(x)) return(character())
    z = strwrap(x, width = largeur)
    if (length(z)) z else x
  }
  nouvelle_page = function() {
    grid::grid.newpage()
    page <<- page + 1L
    .entete_grid("Assemblee nationale", "Dossier legislatif", logo = 1L, taille_logo = 9)
    ligne(19)
    texte(paste("Source : Assemblee nationale | Extraction locale :",
                format(Sys.Date(), "%d/%m/%Y")), x0, 284, 7.4, couleur = gris)
    texte(paste("Page", page), 173, 284, 7.4, couleur = gris)
    y <<- 27
  }
  assurer = function(h) if (y + h > 275) nouvelle_page()
  paragraphe = function(s, indent = 0, taille = 9, couleur = encre, gras = FALSE) {
    s = valeur(s)
    if (!nzchar(s)) return(invisible(NULL))
    largeur = max(24L, as.integer(104 - indent * 1.6))
    lignes = couper(s, largeur)
    h = length(lignes) * 4.2 + 1.8
    assurer(h)
    for (i in seq_along(lignes))
      texte(lignes[[i]], x0 + indent, y + (i - 1L) * 4.2,
            taille = taille, gras = gras, couleur = couleur)
    y <<- y + h
    invisible(NULL)
  }
  section = function(s) {
    assurer(13)
    y <<- y + 4
    texte(s, x0, y, 11, TRUE)
    y <<- y + 8
    ligne(y - 1)
  }
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  grDevices::cairo_pdf(sortie, width = W / 25.4, height = H / 25.4,
                       onefile = TRUE, family = "sans")
  on.exit(grDevices::dev.off(), add = TRUE)
  nouvelle_page()
  d = donnees$dossiers[1L, , drop = FALSE]
  paragraphe(valeur(d$titre), taille = 14, gras = TRUE)
  y = y + 2
  paragraphe(paste("Reference :", valeur(d$uid)), couleur = gris)
  paragraphe(paste("Legislature :", valeur(d$legislature), "| Procedure :",
                    valeur(d$procedure)), couleur = gris)
  section("Parcours legislatif")
  a = donnees$actes
  if (!nrow(a)) paragraphe("Aucun acte disponible dans la base locale.")
  for (i in seq_len(nrow(a))) {
    x = a[i, , drop = FALSE]
    niveau = min(5L, as.integer(x$niveau[[1L]]))
    date = valeur(x$date)
    if (nzchar(date)) date = substr(date, 1L, 10L)
    lib = valeur(x$libelle)
    if (!nzchar(lib)) lib = valeur(x$code)
    paragraphe(paste0(if (nzchar(date)) paste0(date, "  |  ") else "", lib),
               indent = niveau * 3, gras = niveau <= 1L)
    details = c(valeur(x$uid), if (nzchar(valeur(x$organe_ref)))
      paste("Organe :", valeur(x$organe_ref)), if (nzchar(valeur(x$texte_associe)))
      paste("Document :", valeur(x$texte_associe)))
    paragraphe(paste(details[nzchar(details)], collapse = "  |  "),
               indent = niveau * 3 + 2, taille = 7.6, couleur = gris)
  }
  section("Documents associes")
  docs = donnees$documents
  if (!nrow(docs)) paragraphe("Aucun document lie par dossierRef dans la base locale.")
  for (i in seq_len(nrow(docs))) {
    x = docs[i, , drop = FALSE]
    paragraphe(valeur(x$document_uid), gras = TRUE)
    paragraphe(valeur(x$titre), indent = 3)
    date = valeur(x$date_depot)
    if (!nzchar(date)) date = valeur(x$date_publication)
    if (nzchar(date)) paragraphe(paste("Date :", substr(date, 1L, 10L)),
                                 indent = 3, taille = 8, couleur = gris)
    y = y + 2
  }
  invisible(sortie)
}
