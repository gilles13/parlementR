# Comptes rendus des commissions de l'Assemblee nationale.
#' Construire l'URL d'un compte rendu de commission
#' @param ref Reference CRCANR du compte rendu (vectorisable).
#' @return URL(s) HTML.
#' @export
url_compte_rendu_an = function(ref) {
  if (!is.character(ref) || anyNA(ref) || any(!grepl("^CRCANR[[:alnum:]]+$", ref))) stop("Reference de compte rendu invalide")
  paste0("https://www.assemblee-nationale.fr/dyn/opendata/", ref, ".html")
}
#' Rechercher les reunions de commission et leurs comptes rendus
#' @param sujet,intervenant Texte recherche dans l'ordre du jour.
#' @param date_debut,date_fin Dates limites incluses.
#' @param avec_compte_rendu Exclure les reunions sans reference de compte rendu.
#' @param db Connexion DuckDB facultative.
#' @return Tibble de reunions, avec URL et intervenants mentionnes dans le sujet.
#' @export
chercher_commissions_an = function(sujet = NULL, intervenant = NULL, date_debut = NULL, date_fin = NULL, avec_compte_rendu = TRUE, db = NULL) {
  db = .connexion_valide(db)
  if (!DBI::dbExistsTable(db, DBI::Id(schema = "an_reunions", table = "reunions"))) stop("Table an_reunions.reunions absente : actualiser les reunions")
  donnees = DBI::dbGetQuery(db, paste0(
    "SELECT uid, substr(json_extract_string(contenu, '$.reunion.timeStampDebut'), 1, 10) AS date, ",
    "json_extract_string(contenu, '$.reunion.organeReuniRef') AS commission_ref, ",
    "json_extract_string(contenu, '$.reunion.ODJ.resumeODJ.item') AS sujet, ",
    "json_extract_string(contenu, '$.reunion.compteRenduRef') AS compte_rendu, ",
    "json_extract_string(contenu, '$.reunion.typeReunion') AS type_reunion ",
    "FROM an_reunions.reunions")) |>
    dplyr::mutate(date = as.Date(date))
  # Le type de reunion est code dans le JSON de l'agenda.
  donnees = donnees |> dplyr::filter(!is.na(commission_ref), nzchar(commission_ref))
  if (!is.null(date_debut)) donnees = donnees |> dplyr::filter(date >= as.Date(date_debut))
  if (!is.null(date_fin)) donnees = donnees |> dplyr::filter(date <= as.Date(date_fin))
  if (!is.null(sujet)) donnees = donnees |> dplyr::filter(grepl(sujet, .data$sujet, ignore.case = TRUE))
  if (!is.null(intervenant)) donnees = donnees |> dplyr::filter(grepl(intervenant, .data$sujet, ignore.case = TRUE))
  if (avec_compte_rendu) donnees = donnees |> dplyr::filter(!is.na(compte_rendu), grepl("^CRCANR[[:alnum:]]+$", compte_rendu))
  donnees$intervenants_ordre_du_jour = vapply(donnees$sujet, function(x) {
    if (is.na(x)) return(NA_character_)
    z = regmatches(x, gregexpr("(?:M\\.|Mme)\\s+[[:upper:]][[:alpha:]\u00c0-\u00ff'\u2019 -]{2,65}", x, perl = TRUE))[[1]]
    if (!length(z)) NA_character_ else paste(unique(trimws(z)), collapse = ", ")
  }, character(1))
  donnees$url = rep(NA_character_, nrow(donnees))
  ok = !is.na(donnees$compte_rendu) & grepl("^CRCANR[[:alnum:]]+$", donnees$compte_rendu)
  donnees$url[ok] = url_compte_rendu_an(donnees$compte_rendu[ok])
  donnees |> dplyr::select(uid, date, commission_ref, sujet, intervenants_ordre_du_jour, compte_rendu, url) |>
    dplyr::arrange(dplyr::desc(date)) |>
    tibble::as_tibble()
}
#' Telecharger et lire un compte rendu de commission
#' @param ref Reference CRCANR ou URL HTML issue de url_compte_rendu_an().
#' @param reunion Ligne facultative de chercher_commissions_an().
#' @return Liste structuree (reference, URL, metadata, HTML et texte).
#' @export
lire_compte_rendu_an = function(ref, reunion = NULL) {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Installer le package 'rvest'")
  if (length(ref) != 1L || is.na(ref)) stop("Une seule reference est attendue")
  url = if (grepl("^CRCANR", ref)) url_compte_rendu_an(ref) else ref
  if (!grepl("^https://www\\.assemblee-nationale\\.fr/dyn/opendata/CRCANR[[:alnum:]]+\\.html$", url)) stop("URL de compte rendu non reconnue")
  page = rvest::read_html(url)
  corps = rvest::html_element(page, "body")
  if (inherits(corps, "xml_missing")) stop("Corps du compte rendu introuvable")
  texte = rvest::html_text2(corps)
  if (!nzchar(trimws(texte))) stop("Compte rendu vide")
  lignes = trimws(strsplit(texte, "\n", fixed = TRUE)[[1]])
  lignes = lignes[nzchar(lignes)]
  # Le titre officiel est extrait du document ; la reference d'organe reste disponible.
  titre = paste(utils::head(lignes, 6L), collapse = " ")
  if (nchar(titre) > 350L) titre = substr(titre, 1L, 350L)
  personnes = unique(unlist(regmatches(texte, gregexpr("(?:M\\.|Mme)\\s+[[:upper:]][[:alpha:]\u00c0-\u00ff'\u2019 -]{2,65}", texte, perl = TRUE))))
  personnes = trimws(personnes)
  personnes = personnes[nzchar(personnes)]
  list(reference = sub("\\.html$", "", basename(url)), url = url,
       date = if (!is.null(reunion) && "date" %in% names(reunion)) as.Date(reunion$date[1]) else as.Date(NA),
       commission_ref = if (!is.null(reunion) && "commission_ref" %in% names(reunion)) reunion$commission_ref[1] else NA_character_,
       sujet = if (!is.null(reunion) && "sujet" %in% names(reunion)) reunion$sujet[1] else NA_character_,
       titre = titre, intervenants_mentions = personnes, html = page, texte = texte)
}
#' Frequences lexicales d'un compte rendu de commission
#' @param cr Objet issu de lire_compte_rendu_an().
#' @param exclusions Mots supplementaires a exclure.
#' @return Tibble mot, n.
#' @export
mots_compte_rendu_an = function(cr, exclusions = character()) {
  if (!requireNamespace("tidytext", quietly = TRUE) || !requireNamespace("stopwords", quietly = TRUE)) stop("Installer 'tidytext' et 'stopwords'")
  mots_vides = unique(c(stopwords::stopwords("fr", source = "snowball"),
    stopwords::stopwords("fr", source = "stopwords-iso"),
    "dit", "car", "vers", "peu", "tous", "doit", "ainsi", "dont", "notamment", "\u00e9galement", "bien", "autres", "faut", "devons", "faire", "fait", "\u00eatre", "tr\u00e8s", "plus", "aussi", "comme", "tout", "mme", "monsieur", "madame", "ministre", "pr\u00e9sident", "pr\u00e9sidente", "commission", "d\u00e9l\u00e9gu\u00e9e", exclusions))
  tibble::tibble(texte = cr$texte) |>
    dplyr::mutate(texte = gsub("['\u2019]", " ", texte)) |>
    tidytext::unnest_tokens(mot, texte) |>
    dplyr::filter(nchar(mot) >= 3L, !mot %in% mots_vides, !grepl("^[0-9]+$", mot)) |>
    dplyr::count(mot, sort = TRUE)
}
#' Nuage de mots d'un compte rendu
#' @param mots Tibble issu de mots_compte_rendu_an().
#' @param n Nombre de mots affiches.
#' @return Graphique ggplot2.
#' @export
nuage_compte_rendu_an = function(mots, n = 100L) {
  if (!requireNamespace("ggwordcloud", quietly = TRUE) || !requireNamespace("viridis", quietly = TRUE)) stop("Installer 'ggwordcloud' et 'viridis'")
  mots |>
    dplyr::slice_head(n = n) |>
    ggplot2::ggplot(ggplot2::aes(label = mot, size = n, colour = n)) +
    ggwordcloud::geom_text_wordcloud(rm_outside = TRUE) +
    ggplot2::scale_size_area(max_size = 18) +
    viridis::scale_color_viridis(option = "plasma", begin = 0.05, end = 0.55, direction = -1) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
#' Produire un PDF du compte rendu avec nuage de mots
#' @param cr Objet issu de lire_compte_rendu_an().
#' @param fichier Chemin du PDF de sortie.
#' @param exclusions Mots supplementaires a exclure.
#' @param n Nombre de mots dans le nuage.
#' @return Chemin du PDF (invisible).
#' @export
rapport_compte_rendu_an = function(cr, fichier = file.path(tempdir(), paste0(cr$reference, ".pdf")), exclusions = character(), n = 100L, ouvrir = TRUE) {
  if (!requireNamespace("rmarkdown", quietly = TRUE) || !requireNamespace("knitr", quietly = TRUE)) stop("Installer 'rmarkdown' et 'knitr'")
  mots = mots_compte_rendu_an(cr, exclusions = exclusions)
  if (!nrow(mots)) stop("Aucun mot apres filtrage")
  modele = system.file("modeles", "compte_rendu_an.Rmd", package = "parlementR")
  if (!nzchar(modele)) modele = "inst/modeles/compte_rendu_an.Rmd"
  if (!file.exists(modele)) stop("Modele PDF compte rendu introuvable")
  dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
  resultat = rmarkdown::render(modele, output_file = basename(fichier),
    output_dir = normalizePath(dirname(fichier), mustWork = TRUE),
    params = list(cr = cr, mots = mots, n = n), quiet = TRUE,
    envir = new.env(parent = globalenv()))
  chemin = normalizePath(resultat, mustWork = TRUE)
  .ouvrir_pdf(chemin, ouvrir)
  invisible(chemin)
}
