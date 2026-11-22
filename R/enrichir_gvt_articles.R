# Enrichissement documentaire : toutes les valeurs extraites sont distinctes.
.extraire_pm_article_jorf = function(texte) {
  if (length(texte) != 1L || is.na(texte) || !nzchar(trimws(texte))) return(NA_character_)
  x = gsub("[[:space:]]+", " ", texte)
  # Capture uniquement une nomination explicite ; pas de deduction historique.
  motifs = c(
    "(?i)(?:M\\.|Mme\\.?|Monsieur|Madame)\\s+([\\p{L}][\\p{L}'\u2019 -]*?)\\s+est\\s+nomm[\u00e9e]+e?\\s+(?:en\\s+qualit[\u00e9e]\\s+de\\s+)?premier\\s+ministre\\b",
    "(?i)(?:est\\s+nomm[\u00e9e]+e?\\s+premier\\s+ministre\\s*:\\s*)(?:M\\.|Mme\\.?|Monsieur|Madame)?\\s*([\\p{L}][\\p{L}'\u2019 -]*?)(?:[.;,]|$)"
  )
  for (motif in motifs) {
    m = regexec(motif, x, perl = TRUE)
    y = regmatches(x, m)[[1]]
    if (length(y) >= 2L && nzchar(trimws(y[2]))) return(trimws(y[2]))
  }
  NA_character_
}

#' Rattacher les articles officiels aux actes gouvernementaux
#' @param historique_gvt Tableau des actes avec id_jorf et type_acte.
#' @param articles Resultat de extraire_articles_jorf().
#' @param sortie Dossier facultatif pour exporter les controles CSV.
#' @return Liste actes, articles et controles.
#' @export
enrichir_gvt_articles = function(historique_gvt, articles, sortie = NULL) {
  colonnes_actes = c("id_jorf", "type_acte", "premier_ministre")
  colonnes_articles = c("id_texte", "id_cible", "contenu_xml", "contenu_texte", "fichier_xml")
  if (!all(colonnes_actes %in% names(historique_gvt))) stop("Colonnes manquantes dans historique_gvt : ", paste(setdiff(colonnes_actes, names(historique_gvt)), collapse = ", "))
  if (!all(colonnes_articles %in% names(articles))) stop("Colonnes manquantes dans articles : ", paste(setdiff(colonnes_articles, names(articles)), collapse = ", "))
  if (anyNA(historique_gvt$id_jorf) || anyDuplicated(historique_gvt$id_jorf)) stop("id_jorf absent ou duplique dans les actes")
  a = tibble::as_tibble(articles)
  a$pm_article_extrait = vapply(a$contenu_texte, .extraire_pm_article_jorf, character(1))
  # Conserver chaque relation article-texte, sans dedoublonnage ni reecriture du XML.
  actes = tibble::as_tibble(historique_gvt)
  ids = actes$id_jorf
  nombres = vapply(ids, function(id) sum(a$id_texte %in% id), integer(1))
  noms = lapply(ids, function(id) unique(stats::na.omit(a$pm_article_extrait[a$id_texte %in% id])))
  actes$nombre_articles_extraits = nombres
  actes$pm_article_extrait = vapply(noms, function(x) if (length(x) == 1L) x else NA_character_, character(1))
  actes$pm_article_ambigu = vapply(noms, function(x) length(x) > 1L, logical(1))
  actes$pm_article_source = vapply(ids, function(id) {
    x = a$id_cible[a$id_texte %in% id & !is.na(a$pm_article_extrait)]
    if (!length(x)) NA_character_ else paste(unique(x), collapse = ";")
  }, character(1))
  # La comparaison est un indicateur documentaire, pas une correction de la source.
  actes$pm_a_comparer = !is.na(actes$premier_ministre) & !is.na(actes$pm_article_extrait)
  actes$pm_comparaison_brute = ifelse(actes$pm_a_comparer, actes$premier_ministre == actes$pm_article_extrait, NA)
  controles = tibble::tibble(
    indicateur = c("actes", "relations_articles", "textes_avec_articles", "nominations", "nominations_avec_articles", "nominations_sans_nom_metadonnees", "nominations_avec_nom_article", "nominations_sans_nom_ni_article", "articles_avec_nom_explicite", "nominations_avec_plusieurs_noms_articles", "comparaisons_brutes_differentes"),
    valeur = c(nrow(actes), nrow(a), sum(actes$nombre_articles_extraits > 0L), sum(actes$type_acte == "nomination_pm", na.rm = TRUE), sum(actes$type_acte == "nomination_pm" & actes$nombre_articles_extraits > 0L, na.rm = TRUE), sum(actes$type_acte == "nomination_pm" & is.na(actes$premier_ministre), na.rm = TRUE), sum(actes$type_acte == "nomination_pm" & !is.na(actes$pm_article_extrait), na.rm = TRUE), sum(actes$type_acte == "nomination_pm" & is.na(actes$premier_ministre) & is.na(actes$pm_article_extrait), na.rm = TRUE), sum(!is.na(a$pm_article_extrait)), sum(actes$type_acte == "nomination_pm" & actes$pm_article_ambigu, na.rm = TRUE), sum(actes$pm_a_comparer & !actes$pm_comparaison_brute, na.rm = TRUE))
  )
  if (!is.null(sortie)) {
    dir.create(sortie, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv2(actes, file.path(sortie, "gvt_actes_enrichis.csv"), row.names = FALSE, na = "")
    utils::write.csv2(a, file.path(sortie, "gvt_articles.csv"), row.names = FALSE, na = "")
    utils::write.csv2(controles, file.path(sortie, "gvt_controles_articles.csv"), row.names = FALSE)
  }
  list(actes = actes, articles = a, controles = controles)
}
