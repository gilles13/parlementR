# Ajoute de vraies annotations URI au PDF produit par Cairo.
.liens_pdf_grid = function(fichier, liens, hauteur = 297) {
  gs = Sys.which("gs")
  if (!nzchar(gs)) stop("Ghostscript (gs) est necessaire pour les liens PDF")
  temporaire = tempfile(fileext = ".pdf")
  marques = tempfile(fileext = ".ps")
  on.exit(unlink(c(temporaire, marques)), add = TRUE)
  echapper = function(x) {
    x = gsub("\\", "\\\\", x, fixed = TRUE)
    x = gsub("(", "\\(", x, fixed = TRUE)
    gsub(")", "\\)", x, fixed = TRUE)
  }
  instructions = vapply(liens, function(lien) {
    x = lien$x * 72 / 25.4
    y = (hauteur - lien$haut) * 72 / 25.4
    largeur = lien$largeur * 72 / 25.4
    paste0("[ /Page ", lien$page, " /Rect [",
      sprintf("%.2f %.2f %.2f %.2f", x, y - 11, x + largeur, y + 3),
      "] /Border [0 0 0] /Action << /Subtype /URI /URI (",
      echapper(lien$url), ") >> /Subtype /Link /ANN pdfmark")
  }, character(1))
  writeLines(instructions, marques, useBytes = TRUE)
  statut = suppressWarnings(system2(gs, c("-q", "-dBATCH", "-dNOPAUSE",
    "-sDEVICE=pdfwrite", paste0("-sOutputFile=", shQuote(temporaire)),
    shQuote(marques), shQuote(fichier)), stdout = FALSE, stderr = FALSE))
  if (!identical(as.integer(statut), 0L) || !file.exists(temporaire))
    stop("Impossible d'ajouter les liens cliquables au PDF")
  if (!file.copy(temporaire, fichier, overwrite = TRUE))
    stop("Impossible de remplacer le PDF par sa version avec liens")
  invisible(fichier)
}
