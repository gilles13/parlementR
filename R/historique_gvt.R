# Catalogue local des decrets gouvernementaux : aucune lecture de XML.
#' Construire le catalogue des actes gouvernementaux
#' @param db Connexion DuckDB facultative.
#' @param depuis Premiere annee retenue.
#' @return Tibble des actes distincts, avec leur provenance.
#' @export
construire_catalogue_gvt = function(db = NULL, depuis = 1959L) {
  db = if (is.null(db)) connexion() else .connexion_valide(db)
  if (!DBI::dbExistsTable(db, DBI::Id(schema = "jorf", table = "textes")))
    stop("Catalogue jorf.textes absent", call. = FALSE)
  if (length(depuis) != 1L || is.na(depuis) || depuis < 1959L)
    stop("Annee 'depuis' invalide", call. = FALSE)
  titres = .db_get_tibble(db, paste0(
    "SELECT id AS id_jorf, date_texte, date_publi, ",
    "coalesce(titre_complet, titre) AS intitule FROM jorf.textes ",
    "WHERE nature = 'DECRET' AND regexp_matches(",
    "lower(coalesce(titre_complet, titre, '')), ",
    "'nomination du premier ministre|composition du gouvernement|cessation des fonctions du gouvernement')"))
  titres |>
    dplyr::mutate(
      date = as.Date(.data$date_texte),
      date_publication = as.Date(.data$date_publi),
      date = dplyr::if_else(is.na(.data$date) | .data$date > Sys.Date(),
                            .data$date_publication, .data$date),
      type_acte = dplyr::case_when(
        grepl("nomination du premier ministre", tolower(.data$intitule)) ~ "nomination_pm",
        grepl("cessation des fonctions du gouvernement", tolower(.data$intitule)) ~ "cessation",
        TRUE ~ "composition"),
      source = "jorf.textes"
    ) |>
    dplyr::filter(!is.na(.data$date), .data$date >= as.Date(sprintf("%04d-01-01", depuis))) |>
    dplyr::distinct(.data$id_jorf, .keep_all = TRUE) |>
    dplyr::select(.data$id_jorf, .data$date, .data$date_publication,
                  .data$intitule, .data$type_acte, .data$source) |>
    dplyr::arrange(.data$date, .data$id_jorf)
}

#' Construire l'historique des actes gouvernementaux
#' @param db Connexion DuckDB facultative.
#' @param catalogue Catalogue construit par construire_catalogue_gvt().
#' @param historique Historique deja qualifie, facultatif ; prioritaire pour les noms officiels.
#' @param mandats Table locale des mandats avec personneLabel, personne_qid, debut et fin,
#'   ou date_debut et date_fin. Aucun acces reseau n'est effectue.
#' @return Tibble du catalogue enrichi ; les noms non verifies restent NA.
#' @export
construire_historique_gvt = function(db = NULL, catalogue = NULL,
                                     historique = NULL, mandats = NULL) {
  if (is.null(catalogue)) catalogue = construire_catalogue_gvt(db)
  requis = c("id_jorf", "date", "type_acte")
  if (!all(requis %in% names(catalogue))) stop("Catalogue incomplet")
  resultat = dplyr::mutate(catalogue, date = as.Date(.data$date))
  if (!is.null(historique)) {
    if (!all(c("id_jorf", "premier_ministre") %in% names(historique)))
      stop("Historique incomplet")
    anciens = historique |>
      dplyr::distinct(.data$id_jorf, .keep_all = TRUE) |>
      dplyr::select(.data$id_jorf, .data$premier_ministre,
                    dplyr::any_of("source_nom"))
    resultat = dplyr::left_join(resultat, anciens, by = "id_jorf")
  } else {
    resultat$premier_ministre = NA_character_
    resultat$source_nom = NA_character_
  }
  if (!"source_nom" %in% names(resultat)) resultat$source_nom = NA_character_
  if (!is.null(mandats)) {
    if (!"poste" %in% names(mandats)) stop("Colonne poste absente des mandats")
    mandats = dplyr::filter(mandats, sub(".*/", "", .data$poste) %in%
      c("Q3409212", "Q1587677", "Q15135541"))
    debut_col = if ("date_debut" %in% names(mandats)) "date_debut" else "debut"
    fin_col = if ("date_fin" %in% names(mandats)) "date_fin" else "fin"
    if (!all(c("personneLabel", debut_col, fin_col) %in% names(mandats)))
      stop("Colonnes des mandats manquantes")
    m = mandats |>
      dplyr::transmute(nom_wikidata = .data$personneLabel,
        qid_wikidata = if ("personne_qid" %in% names(mandats))
          .data$personne_qid else NA_character_,
        date_debut = as.Date(substr(as.character(.data[[debut_col]]), 1L, 10L)),
        date_fin = as.Date(substr(as.character(.data[[fin_col]]), 1L, 10L))) |>
      dplyr::mutate(date_fin = dplyr::coalesce(.data$date_fin, as.Date("9999-12-31"))) |>
      dplyr::filter(!is.na(.data$date_debut), !is.na(.data$nom_wikidata))
    candidats = resultat |>
      dplyr::filter(.data$type_acte == "nomination_pm", is.na(.data$premier_ministre)) |>
      dplyr::select(.data$id_jorf, .data$date) |>
      dplyr::left_join(m, by = dplyr::join_by(x$date >= y$date_debut,
                                               x$date < y$date_fin),
                       relationship = "many-to-many") |>
      dplyr::group_by(.data$id_jorf) |>
      dplyr::mutate(n_candidats = dplyr::n_distinct(.data$nom_wikidata, na.rm = TRUE)) |>
      dplyr::filter(.data$n_candidats == 1L) |>
      dplyr::slice(1L) |>
      dplyr::ungroup() |>
      dplyr::select(.data$id_jorf, .data$nom_wikidata, .data$qid_wikidata)
    resultat = dplyr::left_join(resultat, candidats, by = "id_jorf") |>
      dplyr::mutate(
        source_nom = dplyr::if_else(is.na(.data$premier_ministre) &
          !is.na(.data$nom_wikidata), "wikidata_mandat_en_cours_a_verifier", .data$source_nom),
        premier_ministre = dplyr::coalesce(.data$premier_ministre, .data$nom_wikidata))
  }
  resultat |>
    dplyr::mutate(nom_a_verifier = dplyr::coalesce(
      .data$source_nom == "wikidata_mandat_en_cours_a_verifier", FALSE)) |>
    dplyr::arrange(.data$date, .data$id_jorf)
}


#' Frise des presidences et chefs de gouvernement
#' @param mandats Table des mandats Wikidata deja disponible localement.
#' @param depuis,jusqua Dates de debut et de fin.
#' @return Graphique ggplot2.
#' @export
# parlementR \u2014 prototype de frise historique lisible.
# D\u00e9pendances : dplyr, ggplot2, ggrepel.
# Ne modifie pas les donn\u00e9es d'origine ; aucun t\u00e9l\u00e9chargement.
bonus_gouvernement = function(mandats,
                               depuis = as.Date("1946-01-01"),
                               jusqua = Sys.Date()) {
  requis = c("poste", "personne_qid", "personneLabel", "debut", "fin")
  manquantes = setdiff(requis, names(mandats))
  if (length(manquantes)) stop("Colonnes absentes : ", paste(manquantes, collapse = ", "))
  if (!requireNamespace("ggrepel", quietly = TRUE))
    stop("Le paquet ggrepel est n\u00e9cessaire : install.packages('ggrepel')")
  if (depuis >= jusqua) stop("P\u00e9riode invalide.")

  # Les codes Wikidata sont plus fiables que les libell\u00e9s traduits.
  d = mandats |>
    dplyr::mutate(
      code = sub(".*/", "", .data$poste),
      role = dplyr::case_when(
        .data$code == "Q191954" ~ "Pr\u00e9sidence",
        .data$code %in% c("Q3409212", "Q1587677", "Q15135541") ~ "Gouvernement",
        TRUE ~ NA_character_
      ),
      debut_date = as.Date(substr(as.character(.data$debut), 1, 10)),
      fin_date = as.Date(substr(as.character(.data$fin), 1, 10)),
      nom = dplyr::if_else(.data$personne_qid == "Q2105", "Jacques Chirac",
                           .data$personneLabel),
      nom = dplyr::if_else(is.na(.data$nom) | grepl("^Q[0-9]+$", .data$nom),
                           paste0("Nom \u00e0 v\u00e9rifier (", .data$personne_qid, ")"), .data$nom)
    ) |>
    dplyr::filter(!is.na(.data$role), !is.na(.data$debut_date),
                  .data$debut_date <= jusqua,
                  is.na(.data$fin_date) | .data$fin_date >= depuis) |>
    dplyr::mutate(
      fin_date = dplyr::coalesce(.data$fin_date, jusqua),
      debut_date = pmax(.data$debut_date, depuis),
      fin_date = pmin(.data$fin_date, jusqua)
    ) |>
    dplyr::filter(.data$fin_date > .data$debut_date) |>
    dplyr::distinct(.data$role, .data$personne_qid, .data$debut_date,
                    .data$fin_date, .keep_all = TRUE)
  if (!nrow(d)) stop("Aucun mandat dat\u00e9 pour cette p\u00e9riode.")

  # Quatre fen\u00eatres : les mandats courts de la IVe R\u00e9publique deviennent lisibles.
  bornes = as.Date(c("1946-01-01", "1958-10-04", "1981-05-21",
                     "2002-05-06", "2100-01-01"))
  titres = c("IVe R\u00e9publique", "1958\u20131981", "1981\u20132002", "Depuis 2002")
  morceaux = lapply(seq_len(4), function(i) {
    a = max(depuis, bornes[i]); b = min(jusqua, bornes[i + 1])
    if (a >= b) return(NULL)
    d |>
      dplyr::filter(.data$debut_date < b, .data$fin_date > a) |>
      dplyr::mutate(
        periode = titres[i],
        x0 = pmax(.data$debut_date, a),
        x1 = pmin(.data$fin_date, b),
        milieu = .data$x0 + as.numeric(.data$x1 - .data$x0) / 2,
        y = dplyr::if_else(.data$role == "Pr\u00e9sidence", 2, 1)
      )
  })
  d = dplyr::bind_rows(morceaux)
  if (!nrow(d)) stop("Aucun mandat dans les fen\u00eatres de la frise.")
  d$periode = factor(d$periode, levels = titres)

  # Les noms des pr\u00e9sidents sont dans les bandes ; ceux des chefs de
  # gouvernement sont d\u00e9cal\u00e9s automatiquement, sans masquer les mandats courts.
  presidents = dplyr::filter(d, .data$role == "Pr\u00e9sidence",
                              as.numeric(.data$x1 - .data$x0) >= 365)
  gouvernants = dplyr::filter(d, .data$role == "Gouvernement")

  ggplot2::ggplot(d) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$x0, xmax = .data$x1,
                   ymin = .data$y - 0.14, ymax = .data$y + 0.14,
                   fill = .data$role),
      colour = "white", linewidth = 0.25
    ) +
    ggplot2::geom_text(
      data = presidents,
      ggplot2::aes(x = .data$milieu, y = 2, label = .data$nom),
      colour = "white", size = 2.7, fontface = "bold", check_overlap = TRUE
    ) +
    ggrepel::geom_text_repel(
      data = gouvernants,
      ggplot2::aes(x = .data$milieu, y = 0.58, label = .data$nom),
      direction = "x", seed = 1, max.overlaps = Inf,
      min.segment.length = 0, segment.size = 0.2,
      box.padding = 0.1, point.padding = 0.02,
      size = 2.7, colour = "#245B4D"
    ) +
    ggplot2::facet_wrap(~ periode, ncol = 1, scales = "free_x") +
    ggplot2::scale_fill_manual(values = c(
      "Pr\u00e9sidence" = "#315F83", "Gouvernement" = "#488B79"
    )) +
    ggplot2::scale_y_continuous(
      breaks = c(1, 2), labels = c("Gouvernement", "Pr\u00e9sidence"),
      limits = c(0.1, 2.4), expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                          expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      title = "Pr\u00e9sidences et chefs de gouvernement en France",
      subtitle = "Chaque segment repr\u00e9sente un mandat recens\u00e9",
      x = NULL, y = NULL,
      caption = "Source : Wikidata \u2014 r\u00e9f\u00e9rentiel provisoire, \u00e0 v\u00e9rifier avec les actes officiels."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      axis.text.y = ggplot2::element_text(face = "bold"),
      panel.spacing.y = grid::unit(0.6, "cm"),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

#' Frise simplifiee par type de mandat
#' @inheritParams bonus_gouvernement
#' @param vue Presidences ou gouvernements.
#' @return Graphique ggplot2.
#' @export
# parlementR : deux vues simples, sans modifier bonus_gouvernements() v5.
# D\u00e9pendances : dplyr, ggplot2. Source : h$mandats (Wikidata).
bonus_gouvernement_simple = function(mandats,
                                     vue = c("presidences", "gouvernements"),
                                     depuis = as.Date("1946-01-01"),
                                     jusqua = Sys.Date()) {
  vue = match.arg(vue)
  requis = c("poste", "personne_qid", "personneLabel", "debut", "fin")
  absentes = setdiff(requis, names(mandats))
  if (length(absentes)) stop("Colonnes absentes : ", paste(absentes, collapse = ", "))
  depuis = as.Date(depuis)
  jusqua = as.Date(jusqua)
  if (is.na(depuis) || is.na(jusqua) || depuis >= jusqua) stop("P\u00e9riode invalide")

  d = mandats |>
    dplyr::mutate(
      code = sub(".*/", "", .data$poste),
      role = dplyr::case_when(
        .data$code == "Q191954" ~ "presidences",
        .data$code %in% c("Q3409212", "Q1587677", "Q15135541") ~ "gouvernements",
        TRUE ~ NA_character_
      ),
      date_debut = as.Date(substr(as.character(.data$debut), 1, 10)),
      date_fin = as.Date(substr(as.character(.data$fin), 1, 10)),
      nom = dplyr::if_else(.data$personne_qid == "Q2105", "Jacques Chirac", .data$personneLabel),
      nom = dplyr::if_else(is.na(.data$nom) | grepl("^Q[0-9]+$", .data$nom),
                           paste0("Nom \u00e0 v\u00e9rifier (", .data$personne_qid, ")"), .data$nom)
    ) |>
    dplyr::filter(.data$role == vue, !is.na(.data$date_debut),
                  .data$date_debut <= jusqua,
                  is.na(.data$date_fin) | .data$date_fin >= depuis) |>
    dplyr::mutate(
      date_fin = dplyr::coalesce(.data$date_fin, jusqua),
      date_debut = pmax(.data$date_debut, depuis),
      date_fin = pmin(.data$date_fin, jusqua)
    ) |>
    dplyr::filter(.data$date_fin > .data$date_debut) |>
    dplyr::distinct(.data$personne_qid, .data$date_debut, .data$date_fin, .keep_all = TRUE)
  if (!nrow(d)) stop("Aucun mandat pour cette vue et cette p\u00e9riode")

  # Fen\u00eatres lisibles ; les intervalles traversant une borne sont d\u00e9coup\u00e9s
  # uniquement pour l'affichage, sans alt\u00e9rer les mandats d'origine.
  bornes = as.Date(c("1946-01-01", "1958-10-04", "1981-05-21", "2002-05-06", "2100-01-01"))
  titres = c("IVe R\u00e9publique", "1958\u20131981", "1981\u20132002", "Depuis 2002")
  fragments = lapply(seq_len(4), function(i) {
    a = max(depuis, bornes[i]); b = min(jusqua, bornes[i + 1])
    if (a >= b) return(NULL)
    d |>
      dplyr::filter(.data$date_debut < b, .data$date_fin > a) |>
      dplyr::mutate(periode = titres[i], x0 = pmax(.data$date_debut, a),
                    x1 = pmin(.data$date_fin, b))
  })
  d = dplyr::bind_rows(fragments)
  if (!nrow(d)) stop("Aucun mandat dans les p\u00e9riodes disponibles")
  d$periode = factor(d$periode, levels = titres)

  if (vue == "presidences") {
    d = d |>
      dplyr::mutate(milieu = .data$x0 + as.numeric(.data$x1 - .data$x0) / 2,
                    etiquette = dplyr::if_else(as.numeric(.data$x1 - .data$x0) >= 365,
                                               .data$nom, ""))
    return(
      ggplot2::ggplot(d) +
        ggplot2::geom_rect(ggplot2::aes(xmin = .data$x0, xmax = .data$x1,
                                       ymin = 0, ymax = 1),
                           fill = "#315F83", colour = "white", linewidth = 0.35) +
        ggplot2::geom_text(ggplot2::aes(x = .data$milieu, y = 0.5,
                                       label = .data$etiquette),
                           colour = "white", size = 3, check_overlap = TRUE) +
        ggplot2::facet_wrap(~ periode, ncol = 1, scales = "free_x") +
        ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::labs(title = "Pr\u00e9sidents de la R\u00e9publique", x = NULL, y = NULL,
                      caption = "Source : Wikidata \u2014 \u00e0 v\u00e9rifier avec les sources officielles") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid = ggplot2::element_blank(),
                       strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                       panel.spacing.y = grid::unit(0.5, "cm"))
    )
  }

  # Une ligne par mandat et par p\u00e9riode, sans collisions d'\u00e9tiquettes.
  # La position verticale suit le d\u00e9but du mandat ; les noms r\u00e9p\u00e9t\u00e9s
  # restent visibles si une personne revient au gouvernement.
  d = d |>
    dplyr::arrange(.data$periode, .data$x0, .data$x1, .data$nom) |>
    dplyr::group_by(.data$periode) |>
    dplyr::mutate(ligne = dplyr::row_number()) |>
    dplyr::ungroup()
  ggplot2::ggplot(d) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$x0, xend = .data$x1,
                   y = .data$ligne, yend = .data$ligne),
      linewidth = 2.8, colour = "#488B79", lineend = "butt"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$x0, y = .data$ligne, label = .data$nom),
      hjust = 1.03, size = 2.8
    ) +
    ggplot2::facet_wrap(~ periode, ncol = 1, scales = "free") +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                          expand = ggplot2::expansion(mult = c(0.32, 0.02))) +
    ggplot2::scale_y_reverse(breaks = NULL) +
    ggplot2::labs(title = "Chefs de gouvernement", x = NULL, y = NULL,
                  caption = "Source : Wikidata \u2014 mandats \u00e0 v\u00e9rifier avec les sources officielles") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   panel.spacing.y = grid::unit(0.5, "cm"))
}
