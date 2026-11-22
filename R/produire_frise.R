#' Produire une frise chronologique des chefs d'Etat et des evenements
#'
#' Les donnees peuvent etre fournies explicitement ou lues depuis les CSV
#' historiques installes avec le package. Le logo hexagonal est place dans
#' l'en-tete, hors de la zone des evenements.
#' @param donnees Table des responsables historiques.
#' @param evenements Table des evenements historiques.
#' @param debut Numero de la Republique de depart, entre 1 et 5.
#' @param ids,noms,categories Filtres facultatifs sur les evenements.
#' @param maximum Nombre maximal d'evenements affiches.
#' @param espacement Espacement minimal entre les etiquettes, en annees.
#' @param logo Afficher le logo hexagonal en haut a droite.
#' @return Un graphique ggplot.
#' @export
produire_frise = function(donnees = NULL, evenements = NULL, debut = 1, ids = NULL, noms = NULL, categories = NULL, maximum = 20, espacement = 1.5, logo = TRUE) {
  stopifnot(length(debut) == 1L, debut %in% 1:5, length(logo) == 1L, !is.na(logo), is.logical(logo), maximum >= 1, espacement > 0)
  lire = function(nom) {
    chemin = system.file("extdata", nom, package = "parlementR")
    if (!nzchar(chemin)) chemin = file.path("inst", "extdata", nom)
    if (!file.exists(chemin)) stop("Referentiel absent : ", nom, ". Fournir la table explicitement.")
    utils::read.csv(chemin, sep = ";", stringsAsFactors = FALSE)
  }
  if (is.null(donnees)) donnees = lire("historique_executif.csv")
  if (is.null(evenements)) evenements = lire("evenements_historiques.csv")
  seuils = as.Date(c("1792-09-21", "1848-02-24", "1870-09-04", "1946-10-27", "1958-10-04"))
  origine = seuils[debut]
  d = donnees |>
    dplyr::filter(.data$categorie == "chef_etat") |>
    dplyr::mutate(date1 = as.Date(.data$date_debut), date2 = dplyr::coalesce(as.Date(.data$date_fin), Sys.Date()), mode_exercice = dplyr::coalesce(.data$mode_exercice, "personnel")) |>
    dplyr::filter(!is.na(.data$date1), .data$date2 >= origine) |>
    dplyr::arrange(.data$date1, .data$date2) |>
    dplyr::mutate(groupe = cumsum(.data$nom != dplyr::lag(.data$nom, default = dplyr::first(.data$nom)) | .data$regime != dplyr::lag(.data$regime, default = dplyr::first(.data$regime)))) |>
    dplyr::group_by(.data$groupe, .data$nom, .data$regime, .data$mode_exercice) |>
    dplyr::summarise(date1 = min(.data$date1), date2 = max(.data$date2), .groups = "drop") |>
    dplyr::arrange(.data$date1) |>
    dplyr::mutate(date1 = pmax(.data$date1, origine), y1 = as.numeric(.data$date1), y2 = as.numeric(.data$date2), milieu = (.data$y1 + .data$y2) / 2, largeur = 0.045 + pmin(as.numeric(.data$date2 - .data$date1) / 365.2425 * 0.008, 0.19), cote = rep(c(-1, 1), length.out = dplyr::n()), x = dplyr::if_else(.data$cote == -1, -0.35, 0.35), h = dplyr::if_else(.data$cote == -1, 1, 0), etiquette = paste0(.data$nom, " (", format(.data$date1, "%Y"), "\u2013", format(.data$date2, "%Y"), ")"), type = dplyr::case_when(grepl("R\u00e9publique", .data$regime, ignore.case = TRUE) ~ "R\u00e9publique", grepl("Empire|Restauration|Monarchie|Cent-Jours", .data$regime, ignore.case = TRUE) ~ "Empire / monarchie", TRUE ~ "Transition / autre"), position = .data$milieu)
  if (!nrow(d)) stop("Aucun chef d'Etat sur cette periode.")
  for (cote in c(-1, 1)) {
    ii = which(d$cote == cote)
    ii = ii[order(d$milieu[ii])]
    if (length(ii) > 1L) for (j in 2:length(ii)) d$position[ii[j]] = max(d$milieu[ii[j]], d$position[ii[j - 1L]] + 0.013 * (as.numeric(Sys.Date()) - as.numeric(origine)))
  }
  regimes = d |>
    dplyr::mutate(changement = .data$regime != dplyr::lag(.data$regime, default = dplyr::first(.data$regime)), changement = replace(.data$changement, 1L, TRUE)) |>
    dplyr::filter(.data$changement) |>
    dplyr::transmute(date = .data$y1, etiquette = .data$regime)
  e = evenements |>
    dplyr::mutate(date1 = as.Date(.data$date_debut), date2 = dplyr::coalesce(as.Date(.data$date_fin), .data$date1), categories = dplyr::coalesce(.data$categories, .data$categorie)) |>
    dplyr::filter(!is.na(.data$date1), .data$date1 <= Sys.Date(), .data$date2 >= origine)
  if (!is.null(ids)) e = dplyr::filter(e, .data$id %in% ids)
  if (!is.null(noms)) e = dplyr::filter(e, .data$nom %in% noms)
  if (!is.null(categories)) e = e[vapply(strsplit(e$categories, ";", fixed = TRUE), function(x) any(trimws(x) %in% categories), logical(1)), , drop = FALSE]
  if (nrow(e) > maximum) stop(nrow(e), " evenements : affiner la selection ou augmenter maximum.")
  e = e |>
    dplyr::arrange(.data$date1, .data$id) |>
    dplyr::mutate(y1 = as.numeric(pmax(.data$date1, origine)), y2 = as.numeric(pmin(.data$date2, Sys.Date())), milieu = (.data$y1 + .data$y2) / 2, position = .data$milieu, durable = .data$y2 > .data$y1, etiquette = paste0(format(.data$date1, "%Y"), " \u00b7 ", .data$nom), couleur = dplyr::case_when(.data$categorie %in% c("guerre", "conflit_interieur", "violence") ~ "Conflits", .data$categorie %in% c("economie", "finance", "famine", "energie") ~ "\u00c9conomie", .data$categorie %in% c("sante", "epidemie") ~ "Sant\u00e9", .data$categorie %in% c("catastrophe", "environnement") ~ "Catastrophes", TRUE ~ "Autres \u00e9v\u00e9nements"))
  if (nrow(e) > 1L) {
    for (i in 2:nrow(e)) e$position[i] = max(e$milieu[i], e$position[i - 1L] + espacement * 365.2425)
    borne = as.numeric(Sys.Date()) + 365
    if (max(e$position) > borne) {
      e$position = e$position - (max(e$position) - borne)
      for (i in rev(seq_len(nrow(e) - 1L))) e$position[i] = min(e$position[i], e$position[i + 1L] - espacement * 365.2425)
    }
  }
  fins = max(c(d$y2, e$y2, as.numeric(Sys.Date())), na.rm = TRUE) + 365
  noms_rep = c("Ire", "IIe", "IIIe", "IVe", "Ve")
  republiques = data.frame(date = as.numeric(seuils))
  republiques = republiques[republiques$date >= as.numeric(origine), , drop = FALSE]
  p = ggplot2::ggplot(d) +
    ggplot2::geom_hline(data = republiques, ggplot2::aes(yintercept = .data$date), colour = "#A99575", linewidth = 0.45, linetype = "dotted") +
    ggplot2::geom_rect(ggplot2::aes(xmin = -.data$largeur, xmax = .data$largeur, ymin = .data$y1, ymax = .data$y2, fill = .data$type), colour = "#F3E5C5", linewidth = 0.3) +
    ggplot2::geom_segment(ggplot2::aes(x = dplyr::if_else(.data$cote == -1, -.data$largeur, .data$largeur), xend = .data$x, y = .data$milieu, yend = .data$position), colour = "#81725D", linewidth = 0.3, linetype = "dotted") +
    ggplot2::geom_text(ggplot2::aes(x = .data$x, y = .data$position, label = .data$etiquette, hjust = .data$h), size = 3.1, family = "serif", colour = "#302A23", show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("R\u00e9publique" = "#3D6389", "Empire / monarchie" = "#A34332", "Transition / autre" = "#827563")) +
    ggplot2::scale_y_reverse(limits = c(fins, as.numeric(origine) - 0.075 * (fins - as.numeric(origine))), breaks = as.numeric(as.Date(paste0(seq(1800, 2040, 10), "-01-01"))), labels = seq(1800, 2040, 10), expand = ggplot2::expansion(mult = 0)) +
    ggplot2::coord_cartesian(xlim = c(-2.45, 2.45), clip = "off") +
    ggplot2::labs(title = "Les chefs d'\u00c9tat de la France", subtitle = paste0("Depuis la ", noms_rep[debut], " R\u00e9publique"), x = NULL, y = NULL, fill = NULL) +
    ggplot2::theme_void(base_family = "serif") +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#F3E5C5", colour = NA), panel.background = ggplot2::element_rect(fill = "#F3E5C5", colour = NA), plot.title = ggplot2::element_text(hjust = 0.5, size = 20, face = "italic"), plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11, margin = ggplot2::margin(b = 18)), axis.text.y = ggplot2::element_text(size = 8, colour = "#81725D"), legend.position = "bottom", plot.margin = ggplot2::margin(25, 100, 25, 35))
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p = p + ggrepel::geom_text_repel(data = regimes, ggplot2::aes(x = -1.65, y = .data$date, label = .data$etiquette), inherit.aes = FALSE, direction = "y", hjust = 0, size = 3, family = "serif", fontface = "italic", colour = "#365F86", box.padding = 0.35, max.overlaps = Inf, seed = 42, show.legend = FALSE)
  } else {
    p = p + ggplot2::geom_text(data = regimes, ggplot2::aes(x = -1.65, y = .data$date, label = .data$etiquette), inherit.aes = FALSE, hjust = 0, family = "serif", colour = "#365F86", size = 3, show.legend = FALSE)
  }
  if (nrow(e)) {
    p = p +
      ggplot2::geom_segment(data = e[e$durable, , drop = FALSE], ggplot2::aes(x = 1.42, xend = 1.42, y = .data$y1, yend = .data$y2, colour = .data$couleur), inherit.aes = FALSE, linewidth = 0.7, show.legend = FALSE) +
      ggplot2::geom_segment(data = e[e$durable, , drop = FALSE], ggplot2::aes(x = 1.42, xend = 1.51, y = .data$y1, yend = .data$y1, colour = .data$couleur), inherit.aes = FALSE, linewidth = 0.7, show.legend = FALSE) +
      ggplot2::geom_segment(data = e[e$durable, , drop = FALSE], ggplot2::aes(x = 1.42, xend = 1.51, y = .data$y2, yend = .data$y2, colour = .data$couleur), inherit.aes = FALSE, linewidth = 0.7, show.legend = FALSE) +
      ggplot2::geom_point(data = e[!e$durable, , drop = FALSE], ggplot2::aes(x = 1.46, y = .data$y1, colour = .data$couleur), inherit.aes = FALSE, size = 2, show.legend = FALSE) +
      ggplot2::geom_segment(data = e, ggplot2::aes(x = 1.53, xend = 1.71, y = .data$milieu, yend = .data$position), inherit.aes = FALSE, colour = "#81725D", linewidth = 0.35, linetype = "dotted", show.legend = FALSE) +
      ggplot2::geom_point(data = e, ggplot2::aes(x = 1.46, y = .data$milieu, colour = .data$couleur), inherit.aes = FALSE, alpha = 0, show.legend = TRUE) +
      ggplot2::geom_text(data = e, ggplot2::aes(x = 1.76, y = .data$position, label = .data$etiquette, colour = .data$couleur), inherit.aes = FALSE, hjust = 0, size = 3, family = "serif", show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = c("Conflits" = "#A34332", "\u00c9conomie" = "#9A722A", "Sant\u00e9" = "#487D6D", "Catastrophes" = "#795D85", "Autres \u00e9v\u00e9nements" = "#565D69"), name = "\u00c9v\u00e9nements") +
      ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1, shape = 16, size = 3)))
  }
  if (logo) {
    chemin = system.file("assets", "02_hemicycle_nuit.svg", package = "parlementR")
    if (!nzchar(chemin)) chemin = file.path("inst", "assets", "02_hemicycle_nuit.svg")
    if (file.exists(chemin) && requireNamespace("rsvg", quietly = TRUE)) {
      dessin = png::readPNG(rsvg::rsvg_png(chemin, width = 240))
      p = p + ggplot2::annotation_custom(grid::rasterGrob(dessin, interpolate = TRUE), xmin = 1.82, xmax = 2.24, ymin = as.numeric(origine) - 0.06 * (fins - as.numeric(origine)), ymax = as.numeric(origine) - 0.005 * (fins - as.numeric(origine)))
    } else warning("Logo SVG absent ou paquet rsvg indisponible : frise produite sans logo.", call. = FALSE)
  }
  p
}
