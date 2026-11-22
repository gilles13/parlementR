#' Treemap des deputes par groupe politique
#'
#' Les effectifs et les libelles sont extraits des mandats actifs AMO10.
#' Les couleurs sont celles du catalogue et de l hemicycle.
#' @param db Connexion DBI facultative ; NULL utilise connexion().
#' @param titre Titre du graphique.
#' @param fichier Chemin facultatif pour exporter le graphique (PDF, PNG, etc.).
#' @return Un objet ggplot, invisiblement si fichier est renseigne.
#' @export
produire_treemap = function(db = NULL, titre = "Assembl\u00e9e nationale", fichier = NULL) {
  db = .connexion_valide(db)
  sql = "
    WITH mandats AS (
      SELECT a.uid AS acteur_uid, m.value AS mandat
      FROM an_amo10.acteurs a,
           json_each(a.contenu, '$.acteur.mandats.mandat') m
    ),
    actifs AS (
      SELECT acteur_uid,
             json_extract_string(mandat, '$.typeOrgane') AS type_organe,
             json_extract_string(mandat, '$.organes.organeRef') AS organe_uid,
             json_extract_string(mandat, '$.dateDebut') AS date_debut
      FROM mandats
      WHERE json_extract_string(mandat, '$.dateFin') IS NULL
    ),
    assemblee AS (
      SELECT DISTINCT acteur_uid FROM actifs WHERE type_organe = 'ASSEMBLEE'
    ),
    groupes AS (
      SELECT acteur_uid, organe_uid,
             row_number() OVER (PARTITION BY acteur_uid ORDER BY date_debut DESC) AS rang
      FROM actifs WHERE type_organe = 'GP'
    )
    SELECT a.acteur_uid AS uid,
           coalesce(json_extract_string(o.contenu, '$.organe.libelleAbrege'), '?') AS sigle,
           coalesce(json_extract_string(o.contenu, '$.organe.libelle'), 'Non renseign\u00e9') AS libelle
    FROM assemblee a
    LEFT JOIN groupes g ON g.acteur_uid = a.acteur_uid AND g.rang = 1
    LEFT JOIN an_amo10.organes o ON o.uid = g.organe_uid"
  deputes = .db_get_tibble(db, sql)
  if (!nrow(deputes)) stop("Aucun depute actif dans AMO10.")
  if (anyDuplicated(deputes$uid)) stop("Plusieurs lignes par depute : verifier les mandats GP.")
  d = deputes |>
    dplyr::count(sigle, libelle, name = "effectif") |>
    dplyr::arrange(dplyr::desc(effectif)) |>
    dplyr::mutate(libelle = gsub("\n", " ", libelle),
                  libelle = vapply(libelle, function(x) paste(strwrap(x, width = 18), collapse = "\n"), character(1)),
                  etiquette = paste0(sigle, " ", effectif, "\n", libelle))
  couleurs = c(.couleurs_groupes(), "?" = "#8996A3")
  manquants = setdiff(d$sigle, names(couleurs))
  if (length(manquants)) couleurs = c(couleurs, stats::setNames(rep("#8996A3", length(manquants)), manquants))
  p = ggplot2::ggplot(d, ggplot2::aes(area = effectif, fill = sigle, label = etiquette)) +
    treemapify::geom_treemap(colour = "white", linewidth = 1.2) +
    treemapify::geom_treemap_text(place = "centre", grow = TRUE, reflow = FALSE, size = 8, min.size = 3, colour = "white", fontface = "bold", padding.x = grid::unit(1, "mm"), padding.y = grid::unit(1, "mm")) +
    ggplot2::scale_fill_manual(values = couleurs) +
    ggplot2::guides(fill = "none") +
    ggplot2::labs(title = titre, subtitle = paste0(sum(d$effectif), " d\u00e9put\u00e9s en exercice - r\u00e9partition par groupe politique"), caption = "Source : parlementR / Assembl\u00e9e nationale") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 18, face = "bold"), plot.subtitle = ggplot2::element_text(size = 11))
  if (!is.null(fichier)) {
    ggplot2::ggsave(fichier, p, width = 16, height = 10)
    return(invisible(p))
  }
  p
}
