#' Produire les syntheses statistiques des senateurs en PDF
#'
#' Une page par theme, a partir de la base DuckDB locale. Les groupes sont
#' presentes sous leur nom complet suivi du sigle lorsqu'il est connu.
#' @param type Population : actif, anciens, autres ou tous.
#' @param groupe Code ou libelle de groupe a retenir, ou NULL pour tous.
#' @param syntheses Themes a produire : groupes, sexe, ages, professions,
#'   territoires, commissions.
#' @param sortie Chemin du PDF, dans /tmp par defaut.
#' @param db Connexion DuckDB facultative ; NULL utilise connexion().
#' @param ouvrir Ouvrir le document apres production.

#' @return Chemin du PDF, invisiblement.
#' @export
senateurs_synthese = function(
    type = c("actif", "anciens", "autres", "tous"),
    groupe = NULL,
    syntheses = c("groupes", "sexe", "ages", "professions", "territoires", "commissions"),
    sortie = file.path("/tmp", paste0("parlementR-senateurs-synthese-", match.arg(type), ".pdf")),
    ouvrir = TRUE,
    db = NULL) {
  type = match.arg(type)
  themes = c("groupes", "sexe", "ages", "professions", "territoires", "commissions")
  if (!length(syntheses) || anyNA(syntheses) || any(!syntheses %in% themes))
    stop("Syntheses attendues : ", paste(themes, collapse = ", "), call. = FALSE)
  syntheses = unique(syntheses)
  if (!is.null(groupe) && (length(groupe) != 1L || is.na(groupe) || !nzchar(groupe)))
    stop("groupe doit etre NULL ou un code/libelle unique.", call. = FALSE)

  con = .connexion_valide(db)
  if (!DBI::dbExistsTable(con, DBI::Id(schema = "senateurs", table = "sen")))
    stop("Table senateurs.sen absente.", call. = FALSE)

  statuts = c(actif = "ACTIF", anciens = "ANCIEN", autres = "AUTRE")
  sql = paste0(
    "SELECT s.senmat AS id, s.sennomuse AS nom, s.senprenomuse AS prenom, ",
    "s.sendatnai AS naissance, s.senfem AS femme, s.sendespro AS profession, ",
    "s.sengrppolcodcou AS code_groupe, s.sengrppolliccou AS groupe, ",
    "s.sentypappcou AS rattachement, s.sencircou AS territoire, ",
    "s.sencomliccou AS commission, s.etasencod AS statut, ",
    "p8.pcs8lil AS categorie_professionnelle ",
    "FROM senateurs.sen s ",
    "LEFT JOIN senateurs.pcs p ON s.pcscod = p.pcscod ",
    "LEFT JOIN senateurs.pcs8 p8 ON p.pcs8cod = p8.pcs8cod",
    if (type == "tous") "" else " WHERE s.etasencod = ?")
  d = if (type == "tous") .db_get_tibble(con, sql) else
    .db_get_tibble(con, sql, params = list(unname(statuts[[type]])))

  noms = c(
    "Les R\u00E9publicains" = "Les R\u00E9publicains",
    "UCI" = "Union Centriste et Ind\u00E9pendants",
    "SER" = "Socialiste, \u00C9cologiste et R\u00E9publicain",
    "CRCE-K" = "Communiste R\u00E9publicain Citoyen et \u00C9cologiste \u2013 Kanaky",
    "RDSE" = "Rassemblement D\u00E9mocratique et Social Europ\u00E9en",
    "RDPI" = "Rassemblement des d\u00E9mocrates, progressistes et ind\u00E9pendants",
    "GEST" = "\u00C9cologiste \u2013 Solidarit\u00E9 et Territoires",
    "Les Ind\u00E9pendants" = "Les Ind\u00E9pendants \u2013 R\u00E9publique et Territoires",
    "UNT" = "Union Nationale pour les Territoires",
    "NI" = "S\u00E9nateurs non inscrits")
  code = d$groupe
  nom = unname(noms[code])
  d$groupe_complet = ifelse(!is.na(nom), paste0(nom, " (", code, ")"), code)
  d$groupe_complet[is.na(d$groupe_complet) | !nzchar(d$groupe_complet)] = "Sans groupe renseign\u00E9"
  if (!is.null(groupe)) {
    choix = d$code_groupe == groupe | d$groupe == groupe | d$groupe_complet == groupe
    choix[is.na(choix)] = FALSE
    d = d[choix, , drop = FALSE]
    if (!nrow(d)) stop("Aucun senateur pour le groupe demande.", call. = FALSE)
  }
  if (!nrow(d)) stop("Aucun senateur dans la selection.", call. = FALSE)

  # Une extraction locale : ne pas la confondre avec la date du dump officiel.
  extraction = Sys.time()
  reference = as.Date(extraction)
  naissance = as.Date(d$naissance)
  d$age = as.integer(format(reference, "%Y")) - as.integer(format(naissance, "%Y")) -
    (format(reference, "%m-%d") < format(naissance, "%m-%d"))
  d$sexe = ifelse(d$femme == "OUI", "Femmes", ifelse(d$femme == "NON", "Hommes", "Non renseign\u00E9"))
  d$rattachement_lib = ifelse(d$rattachement %in% c("A", "-A"), "Apparent\u00E9",
                        ifelse(d$rattachement %in% c("R", "-R"), "Rattach\u00E9",
                        ifelse(d$rattachement == "N", "Membre", "Non pr\u00E9cis\u00E9")))
  d$rattachement_lib[is.na(d$rattachement_lib)] = "Non pr\u00E9cis\u00E9"

  esc = function(x) {
    x = as.character(x)
    x[is.na(x) | !nzchar(trimws(x))] = "Non renseign\u00E9"
    .echapper_latex_senateurs(x)
  }
  # longtable garde les tableaux sur plusieurs pages, sans changer le format portrait.
  tableau = function(x, largeurs, entetes = names(x)) {
    stopifnot(length(largeurs) == ncol(x), length(entetes) == ncol(x))
    spec = paste0("p{", format(largeurs, decimal.mark = ".", trim = TRUE), "cm}", collapse = "")
    lignes = apply(x, 1, function(z) paste0(paste(esc(z), collapse = " & "), " \\\\"))
    paste0("\\begin{longtable}{", spec, "}\n\\toprule\n",
           paste(esc(entetes), collapse = " & "), " \\\\ \\midrule\n",
           "\\endfirsthead\n\\toprule\n", paste(esc(entetes), collapse = " & "),
           " \\\\ \\midrule\n\\endhead\n",
           paste(ifelse(seq_along(lignes) %% 2L == 0L,
                        paste0("\\rowcolor{gray!12} ", lignes), lignes), collapse = "\n"), "\n\\bottomrule\n\\end{longtable}\n")
  }
  compter = function(variable) {
    x = as.character(variable)
    x[is.na(x) | !nzchar(trimws(x))] = "Non renseign\u00E9"
    t = sort(table(x), decreasing = TRUE)
    tibble::tibble(libelle = names(t), effectif = as.integer(t),
               part = sprintf("%.1f %%", 100 * as.integer(t) / length(x)),
               .name_repair = "minimal")
  }
  par_groupe = function(variable) {
    x = as.character(variable)
    x[is.na(x) | !nzchar(trimws(x))] = "Non renseign\u00E9"
    t = tibble::as_tibble(table(d$groupe_complet, x))
    names(t) = c("Groupe", "Modalit\u00E9", "Effectif")
    t = t[t$Effectif > 0, , drop = FALSE]
    t[order(t$Groupe, -t$Effectif), , drop = FALSE]
  }
  titres = c(groupes = "Composition politique", sexe = "Femmes et hommes",
             ages = "\u00C2ges", professions = "Professions", territoires = "Territoires",
             commissions = "Commissions")
  pages = lapply(syntheses, function(theme) {
    debut = paste0("\\section*{", esc(titres[[theme]]), "}\n",
                   "\\noindent Population : ", nrow(d), " s\u00E9nateurs.\\par\n\\medskip\n")
    corps = switch(theme,
      groupes = {
        a = compter(d$groupe_complet)
        r = par_groupe(d$rattachement_lib)
        paste0(tableau(a, c(10, 2, 2.3), c("Groupe ou formation", "Effectif", "Part")),
               "\\subsection*{Rattachements par groupe}\n",
               tableau(r, c(8.5, 3.5, 2.3), c("Groupe", "Rattachement", "Effectif")))
      },
      sexe = {
        paste0(tableau(compter(d$sexe), c(8, 3, 3.3), c("Sexe", "Effectif", "Part")),
               "\\subsection*{Par groupe}\n",
               tableau(par_groupe(d$sexe), c(8.5, 3.5, 2.3),
                      c("Groupe", "Sexe", "Effectif")))
      },
      ages = {
        valides = d$age[!is.na(d$age)]
        resume = if (length(valides)) tibble::tibble(
          Indicateur = c("\u00C2ge moyen", "\u00C2ge m\u00E9dian", "Minimum", "Maximum", "\u00C2ge non renseign\u00E9"),
          Valeur = c(sprintf("%.1f ans", mean(valides)), paste0(stats::median(valides), " ans"),
                     paste0(min(valides), " ans"), paste0(max(valides), " ans"), sum(is.na(d$age))))
        else tibble::tibble(Indicateur = "\u00C2ge non renseign\u00E9", Valeur = nrow(d))
        classes = cut(d$age, breaks = c(-Inf, 39, 49, 59, 69, Inf),
                      labels = c("Moins de 40 ans", "40-49 ans", "50-59 ans", "60-69 ans", "70 ans et plus"))
        classes = as.character(classes)
        paste0(tableau(resume, c(9, 5.3)), "\\subsection*{Classes d'\u00E2ge}\n",
               tableau(compter(classes), c(8, 3, 3.3), c("Classe d'\u00E2ge", "Effectif", "Part")),
               "\\subsection*{\u00C2ge moyen par groupe}\n",
               tableau(tibble::tibble(Groupe = names(split(d$age, d$groupe_complet)),
                 Effectif = vapply(split(d$age, d$groupe_complet), function(x) sum(!is.na(x)), integer(1)),
                 Moyenne = vapply(split(d$age, d$groupe_complet), function(x)
                   if (all(is.na(x))) "NR" else sprintf("%.1f", mean(x, na.rm = TRUE)), character(1))),
               c(9, 2.5, 2.8), c("Groupe", "N. \u00E2ges", "\u00C2ge moyen")))
      },
      professions = {
        p = compter(d$categorie_professionnelle)
        metiers = utils::head(compter(d$profession), 20)
        paste0("\\subsection*{Cat\u00E9gories socioprofessionnelles (PCS)}\n",
               tableau(p, c(9, 2.5, 2.8), c("Cat\u00E9gorie", "Effectif", "Part")),
               "\\subsection*{20 professions d\u00E9clar\u00E9es les plus fr\u00E9quentes}\n",
               tableau(metiers, c(9, 2.5, 2.8), c("Profession", "Effectif", "Part")))
      },
      territoires = {
        t = compter(d$territoire)
        paste0(tableau(t, c(9, 2.5, 2.8), c("Circonscription", "Effectif", "Part")))
      },
      commissions = {
        a = compter(d$commission)
        paste0(tableau(a, c(9, 2.5, 2.8), c("Commission", "Effectif", "Part")),
               "\\subsection*{Par groupe}\n",
               tableau(par_groupe(d$commission), c(7, 5, 2.3),
                      c("Groupe", "Commission", "Effectif")))
      })
    paste0(debut, corps)
  })
  contenu = paste0("\\setlength{\\tabcolsep}{3pt}\n\\footnotesize\n",
                   paste(unlist(pages), collapse = "\n\\clearpage\n"))
  sous_titre = paste0("Source : S\u00E9nat ; extraction locale du ",
                      format(extraction, "%d/%m/%Y \u00E0 %H:%M"),
                      " ; s\u00E9lection : ", type,
                      if (!is.null(groupe)) paste0(" ; groupe : ", groupe) else "")
  chemin = .pdf_senateurs("Synth\u00E8se des s\u00E9nateurs", sous_titre, contenu, sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir)
  invisible(chemin)
}
