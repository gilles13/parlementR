#' Produire un annuaire PDF des senateurs
#'
#' Lit exclusivement la table locale `senateurs.sen` issue du dump officiel
#' du Senat. Le PDF utilise le modele `senateurs.Rmd` (sans fancyhdr).
#' @param type Statut a retenir : actif, anciens, autres ou tous.
#' @param sortie Chemin du PDF produit, dans /tmp par defaut.
#' @param db Connexion DuckDB facultative ; NULL utilise connexion().
#' @param ouvrir Ouvrir le PDF apres generation.

#' @return Chemin du PDF, invisiblement.
#' @export
cat_senateurs = function(
    type = c("actif", "anciens", "autres", "tous"),
    sortie = file.path("/tmp", paste0("parlementR-senateurs-", match.arg(type), ".pdf")),
    ouvrir = TRUE,
    db = NULL) {
  type = match.arg(type)
  con = .connexion_valide(db)
  if (!DBI::dbExistsTable(con, DBI::Id(schema = "senateurs", table = "sen")))
    stop("Table senateurs.sen absente : executer actualiser(jeux = 'senateurs').", call. = FALSE)
  statuts = c(actif = "ACTIF", anciens = "ANCIEN", autres = "AUTRE")
  sql = paste0(
    "SELECT senmat AS id, sennomuse AS nom, senprenomuse AS prenom, ",
    "sengrppolcodcou AS code_groupe, sengrppolliccou AS groupe, ",
    "sendespro AS profession, sencircou AS circonscription, ",
    "sencomliccou AS commission, etasencod AS statut ",
    "FROM senateurs.sen",
    if (type == "tous") "" else " WHERE etasencod = ?",
    " ORDER BY sengrppolliccou NULLS LAST, sennomuse, senprenomuse, senmat")
  donnees = if (type == "tous") .db_get_tibble(con, sql) else
    .db_get_tibble(con, sql, params = list(unname(statuts[[type]])))
  # Fonctions particulieres exercees dans le groupe politique actuel.
  # On ne retient que les fonctions en cours et l'appartenance en cours.
  if (type == "actif" && nrow(donnees)) {
    fonctions = .db_get_tibble(con, paste0(
      "SELECT m.senmat AS id, m.grppolcod AS code_groupe, ",
      "string_agg(DISTINCT f.fongrppollib, ', ' ORDER BY f.fongrppollib) AS fonction ",
      "FROM senateurs.fonmemgrppol fm ",
      "JOIN senateurs.memgrppol m ON fm.memgrppolid = m.memgrppolid ",
      "JOIN senateurs.fongrppol f ON fm.fongrppolcod = f.fongrppolcod ",
      "WHERE fm.fongrppolcod <> 'MEMBREPOL' ",
      "AND fm.fonmemgrppoldatdeb <= CURRENT_DATE ",
      "AND (fm.fonmemgrppoldatfin IS NULL OR fm.fonmemgrppoldatfin >= CURRENT_DATE) ",
      "AND m.memgrppoldatdeb <= CURRENT_DATE ",
      "AND (m.memgrppoldatfin IS NULL OR m.memgrppoldatfin >= CURRENT_DATE) ",
      "GROUP BY m.senmat, m.grppolcod"))
    # Le code de groupe, et non son libelle, assure la correspondance.
    cle = paste(donnees$id, donnees$code_groupe)
    donnees$fonction = fonctions$fonction[match(
      cle, paste(fonctions$id, fonctions$code_groupe))]
    presidents = .db_get_tibble(con, paste0(
      "SELECT DISTINCT m.senmat AS id, m.grppolcod AS code_groupe ",
      "FROM senateurs.fonmemgrppol fm ",
      "JOIN senateurs.memgrppol m ON fm.memgrppolid = m.memgrppolid ",
      "WHERE fm.fongrppolcod = 'PDTPOL' ",
      "AND fm.fonmemgrppoldatdeb <= CURRENT_DATE ",
      "AND (fm.fonmemgrppoldatfin IS NULL OR fm.fonmemgrppoldatfin >= CURRENT_DATE) ",
      "AND m.memgrppoldatdeb <= CURRENT_DATE ",
      "AND (m.memgrppoldatfin IS NULL OR m.memgrppoldatfin >= CURRENT_DATE)"))
    donnees$president = cle %in% paste(presidents$id, presidents$code_groupe)
  } else {
    donnees$fonction = NA_character_
    donnees$president = FALSE
  }
  if (!nrow(donnees)) stop("Aucun senateur dans la selection.")
  .verifier_volume_rapport(nrow(donnees), seuil_alerte = 700L,
                           seuil_max = 1500L, unite = "senateurs")
  titres = c(actif = "S\u00E9nateurs en exercice", anciens = "Anciens s\u00E9nateurs",
             autres = "Autres personnes", tous = "Tous les s\u00E9nateurs")
  noms_groupes = c(
    "Les R\u00E9publicains" = "Les R\u00E9publicains",
    "UCI" = "Union Centriste et Ind\u00E9pendants",
    "SER" = "Socialiste, \u00C9cologiste et R\u00E9publicain",
    "CRCE-K" = "Communiste R\u00E9publicain Citoyen et \u00C9cologiste \u2013 Kanaky",
    "RDSE" = "Rassemblement D\u00E9mocratique et Social Europ\u00E9en",
    "RDPI" = "Rassemblement des d\u00E9mocrates, progressistes et ind\u00E9pendants",
    "GEST" = "\u00C9cologiste \u2013 Solidarit\u00E9 et Territoires",
    "Les Ind\u00E9pendants" = "Les Ind\u00E9pendants \u2013 R\u00E9publique et Territoires",
    "UNT" = "Union Nationale pour les Territoires",
    "NI" = "S\u00E9nateurs non inscrits"
  )
  codes = donnees$groupe
  libelles = unname(noms_groupes[codes])
  donnees$groupe = ifelse(!is.na(libelles), paste0(libelles, " (", codes, ")"), codes)
  donnees$groupe[is.na(donnees$groupe) | !nzchar(donnees$groupe)] = "Sans groupe renseign\u00E9"
  donnees$senateur = trimws(paste(donnees$nom, donnees$prenom))
  donnees$senateur[donnees$president] = paste0(
    donnees$senateur[donnees$president], " (pr\u00E9sidence du groupe)")
  donnees = donnees[order(donnees$groupe, !donnees$president,
                          donnees$nom, donnees$prenom), , drop = FALSE]
  effectifs = sort(table(donnees$groupe), decreasing = TRUE)
  recap = data.frame(Groupe = names(effectifs), Effectif = as.integer(effectifs),
                     `Part (%)` = sprintf("%.1f", 100 * as.integer(effectifs) / nrow(donnees)),
                     check.names = FALSE)
  chemin = .pdf_questions_senat("annuaire_senateurs.Rmd",
    list(donnees = donnees, recap = recap, type = type), titres[[type]],
    paste0("Source : S\u00E9nat - extraction locale du ", format(Sys.time(), "%d/%m/%Y")), sortie)
  .ouvrir_pdf(chemin, ouvrir)
  invisible(chemin)
}
