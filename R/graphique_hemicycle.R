utils::globalVariables(c("n", "groupe", "angle", "rang", "x", "y", "label"))

#' Hemicycle des deputes par groupe
#' @param donnees Tableau avec les colonnes groupe (sigle) et n (effectif).
#' @param titre Titre du graphique.
#' @param rangees Nombre de rangees de sieges.
#' @return Un objet ggplot.
#' @export
graphique_hemicycle = function(donnees, titre = "Assembl\u00e9e nationale", rangees = 10L) {
  ordre = c("LFI-NFP", "GDR", "SOC", "EcoS", "EPR", "Dem", "HOR", "LIOT", "NI", "DR", "UDR", "RN")
  couleurs = c("LFI-NFP" = "#C62828", "GDR" = "#9B1C31", "SOC" = "#E57399", "EcoS" = "#43A047", "EPR" = "#F4B400", "Dem" = "#F28E2B", "HOR" = "#5DADE2", "LIOT" = "#9575CD", "NI" = "#999999", "DR" = "#2874A6", "UDR" = "#174A7E", "RN" = "#142D62")
  noms = c("LFI-NFP" = "La France insoumise - Nouveau Front Populaire", "GDR" = "Gauche d\u00e9mocrate et r\u00e9publicaine", "SOC" = "Socialistes et apparent\u00e9s", "EcoS" = "\u00c9cologiste et Social", "EPR" = "Ensemble pour la R\u00e9publique", "Dem" = "Les D\u00e9mocrates", "HOR" = "Horizons & Ind\u00e9pendants", "LIOT" = "Libert\u00e9s, Ind\u00e9pendants, Outre-mer et Territoires", "NI" = "Non inscrits", "DR" = "Droite R\u00e9publicaine", "UDR" = "Union des droites pour la R\u00e9publique", "RN" = "Rassemblement National")
  if (!all(c("groupe", "n") %in% names(donnees))) stop("Colonnes groupe et n requises.")
  if (anyNA(donnees$groupe) || anyNA(donnees$n) || any(!is.finite(donnees$n)) || any(donnees$n < 0) || any(donnees$n != floor(donnees$n))) stop("Groupes ou effectifs invalides.")
  if (length(rangees) != 1L || is.na(rangees) || !is.finite(rangees) || rangees < 1 || rangees != floor(rangees)) stop("Nombre de rang\u00e9es invalide.")
  d = donnees |>
    dplyr::mutate(groupe = as.character(groupe)) |>
    dplyr::group_by(groupe) |>
    dplyr::summarise(n = sum(n), .groups = "drop") |>
    dplyr::filter(n > 0) |>
    dplyr::mutate(groupe = factor(groupe, levels = ordre)) |>
    dplyr::arrange(groupe)
  if (anyNA(d$groupe)) stop("Groupe inconnu : ", paste(setdiff(as.character(donnees$groupe), ordre), collapse = ", "))
  n = sum(d$n)
  if (!n) stop("Effectif nul.")
  rayons = seq(2, 5, length.out = rangees)
  capacites = floor(n * rayons / sum(rayons))
  manque = n - sum(capacites)
  if (manque > 0) {
    restes = n * rayons / sum(rayons) - capacites
    indices = order(restes, decreasing = TRUE)[seq_len(manque)]
    capacites[indices] = capacites[indices] + 1L
  }
  positions = lapply(seq_along(rayons), function(i) {
    m = capacites[i]
    if (!m) return(NULL)
    angle = if (m == 1) pi / 2 else seq(pi - 0.04, 0.04, length.out = m)
    data.frame(rang = i, angle = angle, x = rayons[i] * cos(angle), y = rayons[i] * sin(angle))
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(dplyr::desc(angle), rang) |>
    dplyr::mutate(groupe = factor(rep(as.character(d$groupe), d$n), levels = ordre))
  enveloppes = positions |>
    dplyr::group_by(groupe) |>
    dplyr::filter(dplyr::n() >= 3) |>
    dplyr::slice(grDevices::chull(x, y)) |>
    dplyr::ungroup()
  etiquettes = positions |>
    dplyr::group_by(groupe) |>
    dplyr::summarise(x = mean(x), y = mean(y), .groups = "drop") |>
    dplyr::mutate(n = d$n[match(as.character(groupe), as.character(d$groupe))], label = paste0(groupe, "\n", n))
  ggplot2::ggplot() +
    ggplot2::geom_polygon(data = enveloppes, ggplot2::aes(x = x, y = y, group = groupe, fill = groupe), alpha = 0.15, colour = NA) +
    ggplot2::geom_point(data = positions, ggplot2::aes(x = x, y = y, colour = groupe), size = 1.6) +
    ggplot2::geom_label(data = etiquettes, ggplot2::aes(x = x, y = y, label = label, colour = groupe), fill = "white", alpha = 0.9, size = 2.2, linewidth = 0, label.padding = grid::unit(0.08, "lines"), show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = couleurs, breaks = ordre, labels = function(x) paste0(x, " \u2014 ", noms[x]), drop = TRUE) +
    ggplot2::scale_fill_manual(values = couleurs, guide = "none") +
    ggplot2::coord_equal(clip = "off") +
    ggplot2::labs(title = titre, caption = paste(n, "d\u00e9put\u00e9s"), colour = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 10), plot.caption = ggplot2::element_text(size = 7, colour = "grey50", hjust = 0), legend.position = "top", legend.justification = "center", legend.box.just = "center", legend.text = ggplot2::element_text(size = 5.5), legend.key.size = grid::unit(2.5, "mm"), legend.spacing.y = grid::unit(0, "mm"), legend.background = ggplot2::element_blank(), plot.margin = ggplot2::margin(2, 3, 2, 3)) +
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = 3, byrow = TRUE, override.aes = list(size = 2.5)))
}
