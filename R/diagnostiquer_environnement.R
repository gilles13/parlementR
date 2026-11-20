#' Diagnostiquer les capacites de l'environnement local
#'
#' Controle la disponibilite des logiciels et bibliotheques utiles aux rapports,
#' sans installation ni acces reseau.
#' @param tester Executer les tests locaux HTML, PDF et ecriture (TRUE par defaut).
#' @return Objet de classe `diagnostic_parlementR` contenant les capacites,
#'   bibliotheques, logiciels, tests et strategie de sortie.
#' @export
diagnostiquer_environnement = function(tester = TRUE) {
  systeme = unname(Sys.info()["sysname"])
  if (is.na(systeme) || !nzchar(systeme)) systeme = .Platform$OS.type

  outils = c("qpdf", "pdfinfo", "pdftotext", "pandoc", "xelatex",
             "pdflatex", "chromium", "google-chrome", "firefox")
  chemins = Sys.which(outils)
  logiciels = data.frame(
    outil = outils,
    present = nzchar(unname(chemins)),
    chemin = unname(chemins),
    stringsAsFactors = FALSE
  )

  paquets = c("grid", "grDevices", "jpeg", "png", "base64enc",
              "ragg", "svglite", "pdftools", "qpdf", "rmarkdown")
  bibliotheques = data.frame(
    paquet = paquets,
    installe = vapply(paquets, requireNamespace, logical(1), quietly = TRUE),
    stringsAsFactors = FALSE
  )

  capacites_r = capabilities()
  capacite = function(nom) {
    x = capacites_r[nom]
    length(x) == 1L && !is.na(x) && isTRUE(unname(x))
  }
  ecriture = function() {
    fichier = tempfile("parlementR-test-")
    ok = tryCatch({
      writeLines("test", fichier)
      identical(readLines(fichier, warn = FALSE), "test")
    }, error = function(e) FALSE)
    unlink(fichier)
    ok
  }
  test_html = function() {
    fichier = tempfile("parlementR-test-", fileext = ".html")
    on.exit(unlink(fichier), add = TRUE)
    tryCatch({
      writeLines('<!doctype html><html><body><a href="https://www.hatvp.fr/">HATVP</a></body></html>',
                 fichier, useBytes = TRUE)
      txt = paste(readLines(fichier, warn = FALSE), collapse = "")
      file.exists(fichier) && grepl('href="https://www.hatvp.fr/"', txt, fixed = TRUE)
    }, error = function(e) FALSE)
  }
  test_pdf = function() {
    fichier = tempfile("parlementR-test-", fileext = ".pdf")
    on.exit(unlink(fichier), add = TRUE)
    ouvert = FALSE
    tryCatch({
      grDevices::cairo_pdf(fichier, width = 2, height = 2)
      ouvert = TRUE
      grid::grid.newpage()
      grid::grid.text("parlementR")
      grDevices::dev.off()
      ouvert = FALSE
      file.exists(fichier) && is.finite(file.info(fichier)$size) &&
        file.info(fichier)$size > 100L &&
        identical(readBin(fichier, "raw", n = 5L), charToRaw("%PDF-"))
    }, error = function(e) {
      if (ouvert) try(grDevices::dev.off(), silent = TRUE)
      FALSE
    })
  }

  ecriture_temp = if (tester) ecriture() else NA
  html = if (tester) test_html() else NA
  cairo = capacite("cairo")
  pdf = if (tester && cairo) test_pdf() else NA
  if (tester && !cairo) pdf = FALSE

  # Un PDF valide n'implique pas la presence d'annotations URI.
  # La capacite liens_pdf ne sera vraie qu'apres un test de bout en bout.
  liens_pdf = NA
  strategie = if (isTRUE(pdf) && isTRUE(liens_pdf)) "pdf" else
    if (isTRUE(html)) "html" else "indeterminee"

  structure(list(
    systeme = list(nom = systeme, version = unname(Sys.info()["release"]),
                   architecture = R.version$arch, plateforme = R.version$platform,
                   r = as.character(getRversion()), interactif = interactive()),
    chemins = .libPaths(),
    capacites_r = capacites_r,
    bibliotheques = bibliotheques,
    logiciels = logiciels,
    tests = list(ecriture_temp = ecriture_temp, html_autonome = html,
                 pdf_cairo = pdf, liens_pdf = liens_pdf),
    strategie = strategie,
    installation = "non verifiee (aucun telechargement effectue)"
  ), class = "diagnostic_parlementR")
}

#' Afficher un diagnostic de l'environnement
#'
#' @param x Objet de classe `diagnostic_parlementR`.
#' @param ... Arguments supplementaires (non utilises).
#' @return L'objet `x`, invisiblement.
#' @export
print.diagnostic_parlementR = function(x, ...) {
  cat("Environnement :", x$systeme$nom, x$systeme$architecture,
      "| R", x$systeme$r, "\n")
  cat("Tests : HTML =", if (is.na(x$tests$html_autonome)) "non teste" else
        if (x$tests$html_autonome) "OK" else "echec",
      "| PDF Cairo =", if (is.na(x$tests$pdf_cairo)) "non teste" else
        if (x$tests$pdf_cairo) "OK" else "echec",
      "| Liens PDF = non verifies\n")
  cat("Strategie conforme verifiee :", x$strategie, "\n")
  invisible(x)
}
