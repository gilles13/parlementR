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
rech_comm_an = function(sujet = NULL, intervenant = NULL, date_debut = NULL, date_fin = NULL, avec_compte_rendu = TRUE, db = NULL) {
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
#' @param reunion Ligne facultative de rech_comm_an().
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
  # Les premieres lignes du compte rendu contiennent le nom de la commission,
  # la date et le numero de seance. Ne pas confondre l'intitule et le sommaire.
  debut = paste(utils::head(lignes, 35L), collapse = " ")
  debut = gsub("[[:space:]]+", " ", debut)
  nom = sub("^.*?(Commission (?:d[\u2019']enqu\u00eate|des? |permanente|sp\u00e9ciale)[^\u2026]+?)(?:[\u2026]{2,}|[\u2013-] Audition|[\u2013-] Pr\u00e9sences|Samedi |Lundi |Mardi |Mercredi |Jeudi |Vendredi |Dimanche ).*$", "\\1", debut, perl = TRUE)
  if (identical(nom, debut)) {
    candidats = lignes[grepl("^Commission ", lignes)]
    nom = if (length(candidats)) candidats[1] else "Commission de l'Assembl\u00e9e nationale"
  }
  nom = trimws(gsub("[\u2026]+$", "", nom))
  date_texte = regmatches(debut, regexpr("[0-3]?[0-9] (?:janvier|f\u00e9vrier|mars|avril|mai|juin|juillet|ao\u00fbt|septembre|octobre|novembre|d\u00e9cembre) 20[0-9]{2}", debut, perl = TRUE))
  mois = c("janvier", "f\u00e9vrier", "mars", "avril", "mai", "juin", "juillet", "ao\u00fbt", "septembre", "octobre", "novembre", "d\u00e9cembre")
  date_document = as.Date(NA)
  if (length(date_texte) && nzchar(date_texte)) {
    morceaux = strsplit(date_texte, " ", fixed = TRUE)[[1]]
    date_document = as.Date(sprintf("%s-%02d-%02d", morceaux[3], match(morceaux[2], mois), as.integer(morceaux[1])))
  }
  date_reunion = if (!is.null(reunion) && "date" %in% names(reunion)) as.Date(reunion$date[1]) else as.Date(NA)
  numero = regmatches(debut, regexpr("Compte rendu n[\u00b0o]?[[:space:]]*[0-9]+", debut, perl = TRUE, ignore.case = TRUE))
  objet = regmatches(debut, regexpr("Audition[^\u2026]+", debut, perl = TRUE))
  objet = if (length(objet)) trimws(sub("[\u2026].*$", "", objet)) else NA_character_
  list(reference = sub("\\.html$", "", basename(url)), url = url,
       date = if (!is.na(date_reunion)) date_reunion else date_document,
       commission_ref = if (!is.null(reunion) && "commission_ref" %in% names(reunion)) reunion$commission_ref[1] else NA_character_,
       sujet = if (!is.null(reunion) && "sujet" %in% names(reunion)) reunion$sujet[1] else NA_character_,
       titre = nom, objet = objet,
       numero = if (length(numero)) numero else NA_character_,
       html = page, texte = texte)

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
#' Produire un compte rendu de commission en PDF (grid et Cairo)
#' @param cr Reference CRCANR, ligne de rech_comm_an(), ou objet de lire_compte_rendu_an().
#' @param fichier Chemin facultatif (par defaut /tmp/parlementR-commission-an-REFERENCE.pdf).
#' @param ouvrir Ouvrir le PDF apres generation.
#' @param exclusions,n Arguments historiques conserves pour compatibilite.
#' @return Chemin du PDF (invisible).
#' @export
rapport_comm_an = function(cr, fichier = NULL, exclusions = character(), n = 100L, ouvrir = TRUE) {
  if (is.data.frame(cr)) {
    if (nrow(cr) != 1L || !"compte_rendu" %in% names(cr))
      stop("Fournir une seule ligne avec la colonne compte_rendu")
    cr = lire_compte_rendu_an(cr$compte_rendu[1], reunion = cr)
  } else if (is.character(cr) && length(cr) == 1L) {
    cr = lire_compte_rendu_an(cr)
  }
  if (!is.list(cr) || is.null(cr$reference) || is.null(cr$texte))
    stop("Compte rendu invalide")
  ref = cr$reference
  if (length(ref) != 1L || is.na(ref) || !grepl("^CRCANR[[:alnum:]]+$", ref))
    stop("Reference CRCANR invalide")
  if (is.null(fichier)) fichier = file.path("/tmp", paste0("parlementR-commission-an-", ref, ".pdf"))
  .pdf_compte_rendu_an_grid(cr, fichier)
  .ouvrir_pdf(normalizePath(fichier, mustWork = TRUE), ouvrir)
}
