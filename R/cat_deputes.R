#' Catalogue PDF des deputes par groupe parlementaire (grid et Cairo)
#'
#' Exploite les mandats et organes AMO10 deja importes dans DuckDB.
#' Les responsabilites institutionnelles sont deduites des mandats actifs.
#'
#' @param db Connexion DBI facultative ; NULL utilise connexion().
#' @param groupe Sigle du groupe (par exemple "LFI-NFP"), ou NULL pour tous.
#' @param enrichie Ajouter les fonctions particulieres (FALSE par defaut).
#' @param fichier Chemin du PDF ; par defaut, /tmp/parlementR-deputes.pdf.
#' @param ouvrir Ouvrir le PDF apres generation.
#' @return Chemin du PDF, invisiblement.
#' @export
cat_deputes = function(db = NULL, groupe = NULL, enrichie = FALSE, fichier = NULL, ouvrir = TRUE) {
  db = .connexion_valide(db)
  if (!is.null(groupe) && (!is.character(groupe) || length(groupe) != 1L ||
                           is.na(groupe) || !nzchar(trimws(groupe)))) {
    stop("'groupe' doit etre un sigle non vide, ou NULL.")
  }
  if (!is.logical(enrichie) || length(enrichie) != 1L || is.na(enrichie)) {
    stop("'enrichie' doit valoir TRUE ou FALSE.")
  }
  if (is.null(fichier)) fichier = "/tmp/parlementR-deputes.pdf"
  if (!is.character(fichier) || length(fichier) != 1L || is.na(fichier) ||
      !nzchar(fichier) || !grepl("\\.pdf$", fichier, ignore.case = TRUE)) {
    stop("'fichier' doit etre un chemin PDF.")
  }

  # json_each gere les mandats imbriques sans modifier les tables sources.
  sql = "
    WITH mandats AS (
      SELECT a.uid AS acteur_uid, m.value AS mandat
      FROM an_deputes.acteurs a,
           json_each(a.contenu, '$.acteur.mandats.mandat') m
    ),
    actifs AS (
      SELECT acteur_uid,
             json_extract_string(mandat, '$.typeOrgane') AS type_organe,
             json_extract_string(mandat, '$.organes.organeRef') AS organe_uid,
             json_extract_string(mandat, '$.infosQualite.libQualite') AS qualite,
             json_extract_string(mandat, '$.infosQualite.codeQualite') AS code_qualite,
             json_extract_string(mandat, '$.dateDebut') AS date_debut,
             json_extract_string(mandat, '$.election.lieu.departement') AS departement,
             json_extract_string(mandat, '$.election.lieu.numCirco') AS circo
      FROM mandats
      WHERE json_extract_string(mandat, '$.dateFin') IS NULL
    )
    SELECT a.uid,
           json_extract_string(a.contenu, '$.acteur.etatCivil.ident.prenom') AS prenom,
           json_extract_string(a.contenu, '$.acteur.etatCivil.ident.nom') AS nom,
           m.type_organe, m.qualite, m.code_qualite, m.date_debut, m.departement, m.circo,
           o.uid AS organe_uid,
           json_extract_string(o.contenu, '$.organe.libelle') AS organe,
           json_extract_string(o.contenu, '$.organe.libelleAbrege') AS sigle
    FROM an_deputes.acteurs a
    JOIN actifs m ON a.uid = m.acteur_uid
    LEFT JOIN an_deputes.organes o ON o.uid = m.organe_uid
    WHERE m.type_organe IN (
      'ASSEMBLEE', 'GP', 'COMPER', 'COMNL', 'DELEG', 'BUREAU',
      'DELEGBUREAU', 'MISINFO', 'MISINFOPRE', 'MISINFOCOM', 'CMP'
    )"
  x = .db_get_tibble(db, sql)
  if (!nrow(x)) stop("Aucun mandat actif dans AMO10.")

  # La circonscription provient du mandat parlementaire, non des organes.
  deputes = x[x$type_organe == "ASSEMBLEE", c("uid", "prenom", "nom", "departement", "circo")]
  if (anyDuplicated(deputes$uid)) stop("Plusieurs mandats parlementaires actifs par depute.")
  groupes = x[x$type_organe == "GP", c("uid", "organe", "sigle", "date_debut", "code_qualite")]
  groupes = groupes[order(groupes$uid, groupes$date_debut, decreasing = TRUE), ]
  groupes = groupes[!duplicated(groupes$uid), ]
  i = match(deputes$uid, groupes$uid)
  deputes$groupe = groupes$organe[i]
  deputes$sigle = groupes$sigle[i]
  deputes$president = !is.na(groupes$code_qualite[i]) & groupes$code_qualite[i] == "Pr\u00e9sident"
  deputes$groupe[is.na(deputes$groupe)] = "Non renseign\u00e9"
  deputes$sigle[is.na(deputes$sigle)] = "?"
  if (!is.null(groupe)) {
    deputes = deputes[toupper(deputes$sigle) == toupper(trimws(groupe)), , drop = FALSE]
    if (!nrow(deputes)) stop("Groupe introuvable : ", groupe)
  }

  .verifier_volume_rapport(nrow(deputes), seuil_alerte = 700L,
                           seuil_max = 1500L, unite = "deputes")
  deputes$fonction = ""
  if (enrichie) {
    f = x[!x$type_organe %in% c("ASSEMBLEE", "GP") &
            !is.na(x$qualite) &
            grepl("pr[e\u00e9]sident|rapporteur|secr[e\u00e9]taire|questeur",
                  x$qualite, ignore.case = TRUE), ]
    # Les fonctions dans les groupes d'etudes et d'amitie sont exclues.
    f = f[!is.na(f$organe) & nzchar(f$organe), ]
    f$libelle = paste0(f$qualite, " \u2014 ", f$organe)
    f = f[!duplicated(f[c("uid", "libelle")]), ]
    fonctions_par_depute = split(f$libelle, f$uid)
    deputes$fonction = vapply(deputes$uid, function(id) {
      valeurs = fonctions_par_depute[[id]]
      if (is.null(valeurs)) "" else paste(valeurs, collapse = " ; ")
    }, character(1))
  }

  deputes$circonscription = ifelse(is.na(deputes$departement), "",
    paste0(deputes$departement, ifelse(is.na(deputes$circo), "",
      paste0(" (", deputes$circo, ")"))))
  effectifs_groupes = table(deputes$sigle)
  deputes = deputes[order(-as.integer(effectifs_groupes[deputes$sigle]),
    deputes$groupe, !deputes$president, deputes$nom, deputes$prenom), , drop = FALSE]
  dir.create(dirname(fichier), recursive = TRUE, showWarnings = FALSE)
  # Provenance du jeu AMO10 utilise par cette fiche.
  source = .source_archive("deputes", tools::R_user_dir("parlementR", "cache"))
  suivi = tryCatch(.db_get_tibble(db, paste0(
    "SELECT date_import, date_controle, modification FROM suivi.actualisations ",
    "WHERE jeu = 'deputes'")), error = function(e) NULL)
  valeur_date = function(x) {
    if (length(x) == 0L || is.na(x[1])) return("Non disponible")
    format(as.POSIXct(x[1]), "%d/%m/%Y %H:%M:%S")
  }
  date_import = if (!is.null(suivi) && nrow(suivi))
    valeur_date(suivi$date_import) else "Non disponible"
  date_controle = if (!is.null(suivi) && nrow(suivi))
    valeur_date(suivi$date_controle) else "Non disponible"
  date_source = if (!is.null(suivi) && nrow(suivi) &&
                     !is.na(suivi$modification[1]) &&
                     nzchar(suivi$modification[1]))
    suivi$modification[1] else "Non disponible"
  metadonnees = c(
    "Producteur" = "Assembl\u00e9e nationale (donn\u00e9es ouvertes)",
    "Jeu" = "AMO10 - d\u00e9put\u00e9s actifs, mandats actifs et organes",
    "Archive ZIP" = basename(source$chemin),
    "Source" = "Archive officielle (lien cliquable)",
    "Derni\u00e8re modification HTTP" = date_source,
    "Dernier import DuckDB" = date_import,
    "Dernier contr\u00f4le" = date_controle,
    "Traitement" = "parlementR - mandats actifs et groupes parlementaires"
  )
  chemin = .pdf_catalogue_deputes_grid(
    deputes = deputes, metadonnees = metadonnees,
    source_url = source$url, fichier = fichier, enrichie = enrichie
  )
  .ouvrir_pdf(chemin, ouvrir)
  invisible(chemin)
}
