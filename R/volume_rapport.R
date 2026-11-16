# Controle du volume avant toute preparation d’un rapport PDF.
.verifier_volume_rapport = function(n, seuil_alerte = 100L, seuil_max = 500L, unite = "amendements") {
  if (length(n) != 1L || is.na(n) || !is.finite(n) || n < 0)
    stop("Volume de rapport invalide", call. = FALSE)
  if (n > seuil_max)
    stop(sprintf(
      "Rapport non produit : %s %s (maximum : %s). Affinez votre recherche.",
      n, unite, seuil_max), call. = FALSE)
  if (n > seuil_alerte)
    warning(sprintf(
      "Rapport volumineux : %s %s. La generation du PDF peut etre longue.",
      n, unite), call. = FALSE)
  invisible(TRUE)
}
