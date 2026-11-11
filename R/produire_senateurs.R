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
produire_senateurs = function(
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
    "sentypappcou AS rattachement, ",
    "sendespro AS profession, sencircou AS circonscription, ",
    "sencomliccou AS commission, etasencod AS statut ",
    "FROM senateurs.sen",
    if (type == "tous") "" else " WHERE etasencod = ?",
    " ORDER BY sengrppolliccou NULLS LAST, sennomuse, senprenomuse, senmat")
  donnees = if (type == "tous") .db_get_tibble(con, sql) else
    .db_get_tibble(con, sql, params = list(unname(statuts[[type]])))
  date_extraction = format(Sys.time(), "%d/%m/%Y a %H:%M")
  esc = function(x) {
    x = as.character(x)
    x[is.na(x)] = ""
    .echapper_latex_senateurs(x)
  }
  titres = c(actif = "Senateurs en exercice", anciens = "Anciens senateurs",
             autres = "Autres personnes", tous = "Tous les senateurs")
  # D\u00E9nominations d\u00E9velopp\u00E9es pour les groupes actuels ; les groupes historiques
  # conservent leur libell\u00E9 source lorsqu'aucune correspondance n'est \u00E9tablie.
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
  code = donnees$groupe
  nom = unname(noms_groupes[code])
  donnees$groupe = ifelse(!is.na(nom), paste0(nom, " (", code, ")"), code)
  # A4 portrait : colonnes limitees aux informations lisibles sur cette largeur.
  groupes = ifelse(is.na(donnees$groupe) | !nzchar(donnees$groupe),
                   "Sans groupe renseigne", donnees$groupe)
  if (!nrow(donnees)) stop("Aucun senateur dans la selection.")
  # Le recapitulatif et l'annuaire reposent sur exactement la meme selection.
  effectifs = sort(table(groupes), decreasing = TRUE)
  recap_lignes = paste0(
    esc(names(effectifs)), " & ", as.integer(effectifs), " & ",
    sprintf("%.1f", 100 * as.integer(effectifs) / nrow(donnees)),
    "\\% \\\\"
  )
  recap = paste0(
    "\\section*{Repartition par groupe ou formation}\n",
    "\\begin{longtable}{p{10cm}rr}\n\\toprule\n",
    "Groupe ou formation & Effectif & Part \\\\ \\midrule\n",
    paste(recap_lignes, collapse = "\n"), "\n\\midrule\n",
    "\\textbf{Total} & \\textbf{", nrow(donnees),
    "} & \\textbf{100\\%} \\\\ \\bottomrule\n",
    "\\end{longtable}\n"
  )
  donnees$senateur = trimws(paste(donnees$nom, donnees$prenom))
  colonnes = c("id", "senateur", "rattachement", "profession", "circonscription")
  libelles = c("ID", "Nom et pr\u00E9nom", "Ratt.", "Profession", "Circonscription")
  # Largeur utile : 16,8 cm en A4 portrait (marges du modele).
  largeurs = c(1.25, 3.6, 1.0, 4.8, 5.4)
  spec = paste0("p{", sprintf("%.2f", largeurs), "cm}", collapse = "")
  ligne = function(x) paste(esc(x), collapse = " & ")
  tableaux = vapply(unique(groupes), function(g) {
    d = donnees[groupes == g, colonnes, drop = FALSE]
    d[is.na(d)] = ""
    lignes = apply(d, 1, function(x) paste0(ligne(x), " \\\\"))
    paste0("\\subsection*{", esc(g), " (", nrow(d), ")}\n",
           "\\begin{longtable}{", spec, "}\n",
           "\\toprule\n", ligne(libelles), " \\\\ \\midrule\n",
           "\\endfirsthead\n",
           "\\toprule\n", ligne(libelles), " \\\\ \\midrule\n",
           "\\endhead\n",
           paste(lignes, collapse = "\n"), "\n\\bottomrule\n",
           "\\end{longtable}\n")
  }, character(1))
  contenu = paste0("\\setlength{\\tabcolsep}{2pt}\n\\footnotesize\n",
                   recap, "\\section*{Annuaire}\n",
                   paste(tableaux, collapse = "\n"))
  chemin = .pdf_senateurs(titres[[type]], paste0("Source : Senat - extraction locale du ", date_extraction),
                           contenu, sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir)
  invisible(chemin)
}
