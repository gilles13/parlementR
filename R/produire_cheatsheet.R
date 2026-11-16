#' Produire la cheatsheet PDF de parlementR
#'
#' Genere une fiche A4 paysage, en trois colonnes, selon le modele
#' de la cheatsheet assembleer. Necessite Chromium.
#' @param sortie Chemin du PDF produit.
#' @param ouvrir Ouvrir le PDF apres generation.
#' @return Chemin du PDF, invisiblement.
#' @export
produire_cheatsheet = function(sortie = "/tmp/parlementR-cheatsheet.pdf", ouvrir = TRUE) {
  navigateur = Sys.which("chromium")
  if (!nzchar(navigateur)) navigateur = Sys.which("chromium-browser")
  if (!nzchar(navigateur)) {
    stop("Chromium est necessaire pour imprimer la cheatsheet en PDF.", call. = FALSE)
  }
  modele = system.file("modeles", "cheatsheet.html", package = "parlementR")
  logo = system.file("assets", "logo_hexa.png", package = "parlementR")
  fig01 = system.file("assets", "fig01_small.png", package = "parlementR")
  fig02 = system.file("assets", "fig02_small.png", package = "parlementR")
  if (!nzchar(modele) || !nzchar(logo)) {
    stop("Modele ou logo parlementR introuvable.", call. = FALSE)
  }
  if (!nzchar(fig01) || !nzchar(fig02)) {
    stop("Illustrations manquantes dans inst/assets : fig01_small.png et/ou fig02_small.png.", call. = FALSE)
  }
  sortie = normalizePath(sortie, mustWork = FALSE)
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  html = tempfile("parlementR-cheatsheet-", fileext = ".html")
  on.exit(unlink(html), add = TRUE)
  contenu = paste(readLines(modele, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  contenu = sub("{{HEXA}}", paste0("file://", normalizePath(logo)), contenu, fixed = TRUE)
  contenu = gsub("{{FIG01}}", paste0("file://", normalizePath(fig01)), contenu, fixed = TRUE)
  contenu = gsub("{{FIG02}}", paste0("file://", normalizePath(fig02)), contenu, fixed = TRUE)
  writeLines(contenu, html, useBytes = TRUE)
  args = c("--headless", "--disable-gpu", "--no-sandbox",
           "--allow-file-access-from-files", "--no-pdf-header-footer",
           paste0("--print-to-pdf=", sortie),
           paste0("file://", normalizePath(html)))
  sortie_chromium = suppressWarnings(system2(navigateur, args, stdout = TRUE, stderr = TRUE))
  if (!file.exists(sortie) || file.info(sortie)$size == 0) {
    stop("Echec de la production PDF : ", paste(sortie_chromium, collapse = "\n"), call. = FALSE)
  }
  if (isTRUE(ouvrir)) utils::browseURL(sortie)
  invisible(sortie)
}
