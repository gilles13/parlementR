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
  date_extraction = format(Sys.time(), "%d/%m/%Y \u00e0 %H:%M")
  esc = function(x) {
    x = as.character(x)
    x[is.na(x)] = ""
    .echapper_latex_senateurs(x)
  }
  titres = c(actif = "S\u00e9nateurs en exercice", anciens = "Anciens s\u00e9nateurs",
             autres = "Autres personnes", tous = "Tous les s\u00e9nateurs")
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
                   "Sans groupe renseign\u00e9", donnees$groupe)
  if (!nrow(donnees)) stop("Aucun senateur dans la selection.")
  # Le recapitulatif et l'annuaire reposent sur exactement la meme selection.
  effectifs = sort(table(groupes), decreasing = TRUE)
  recap_lignes = paste0(
    esc(names(effectifs)), " & ", as.integer(effectifs), " & ",
    sprintf("%.1f", 100 * as.integer(effectifs) / nrow(donnees)),
    "\\% \\\\"
  )
  recap = paste0(
    "\\section*{R\u00e9partition par groupe ou formation}\n",
    "\\begin{longtable}{p{10cm}rr}\n\\toprule\n",
    "Groupe ou formation & Effectif & Part \\\\ \\midrule\n",
    paste(ifelse(seq_along(recap_lignes) %% 2L == 0L,
                 paste0("\\rowcolor{gray!12} ", recap_lignes), recap_lignes), collapse = "\n"), "\n\\midrule\n",
    "\\textbf{Total} & \\textbf{", nrow(donnees),
    "} & \\textbf{100\\%} \\\\ \\bottomrule\n",
    "\\end{longtable}\n"
  )
  donnees$senateur = trimws(paste(donnees$nom, donnees$prenom))
  colonnes = c("id", "senateur", "profession", "circonscription")
  libelles = c("ID", "Nom et pr\u00e9nom", "Profession", "Circonscription")
  # Largeur utile : 16,8 cm en A4 portrait (marges du modele).
  largeurs = c(1.5, 4.2, 5.4, 5.1)
  spec = paste0("p{", sprintf("%.2f", largeurs), "cm}", collapse = "")
  ligne = function(x) paste(esc(x), collapse = " & ")
  tableaux = vapply(seq_along(unique(groupes)), function(i) {
    g = unique(groupes)[[i]]
    indices = which(groupes == g)
    indices = indices[order(!donnees$president[indices],
                            donnees$nom[indices], donnees$prenom[indices])]
    d = donnees[indices, colonnes, drop = FALSE]
    d[is.na(d)] = ""
    lignes = vapply(seq_len(nrow(d)), function(j) {
      cellules = esc(unlist(d[j, ], use.names = FALSE))
      if (isTRUE(donnees$president[indices[j]]))
        cellules[2] = paste0("\\textbf{", cellules[2], "}")
      paste0(if (j %% 2L == 0L) "\\rowcolor{gray!12} " else "",
             paste(cellules, collapse = " & "), " \\\\")
    }, character(1))
    paste0(if (i > 1L) "\\clearpage\n" else "",
           "\\subsection*{", esc(g), " (", nrow(d), ")}\n",
           "\\begin{longtable}{", spec, "}\n",
           "\\toprule\n", ligne(libelles), " \\\\ \\midrule\n",
           "\\endfirsthead\n",
           "\\toprule\n", ligne(libelles), " \\\\ \\midrule\n",
           "\\endhead\n",
           paste(lignes, collapse = "\n"), "\n\\bottomrule\n",
           "\\end{longtable}\n")
  }, character(1))
  note = if (type == "actif")
    "\\noindent\\emph{Le ou la pr\u00e9sidente de chaque groupe politique figure en gras dans les tableaux ci-dessous.}\n\n"
  else ""
  contenu = paste0("\\setlength{\\tabcolsep}{2pt}\n\\footnotesize\n",
                   recap, note, "\\section*{Annuaire}\n\\scriptsize\n",
                   paste(tableaux, collapse = "\n"))
  chemin = .pdf_senateurs(titres[[type]], paste0("Source : S\u00e9nat - extraction locale du ", date_extraction),
                           contenu, sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir)
  invisible(chemin)
}
