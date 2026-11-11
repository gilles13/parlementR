#' Produire la fiche PDF d'un dossier legislatif de l'Assemblee nationale
#'
#' Utilise exclusivement les references officielles de la base locale.
#' Les documents sont relies par dossierRef et les amendements par texteLegislatifRef.
#' @param uid Identifiant officiel du dossier.
#' @param db Connexion DuckDB ; NULL utilise connexion().
#' @param sortie Chemin du PDF, dans /tmp par defaut.
#' @param ouvrir Ouvrir le PDF apres production.
#' @param pdf Si FALSE, renvoyer les tables sans produire de PDF.
#' @return Chemin du PDF (invisible), ou liste des tables.
#' @export
produire_dossier = function(uid, db = NULL, sortie = NULL, ouvrir = FALSE, pdf = TRUE) {
  if (!is.character(uid) || length(uid) != 1L || is.na(uid) ||
      !grepl("^[A-Za-z0-9_-]+$", uid))
    stop("'uid' doit etre un identifiant officiel valide")
  db = .connexion_valide(db)
  requis = c("an_dossiers.dossiers", "an_dossiers.documents",
             "an_amendements.amendements")
  present = vapply(strsplit(requis, ".", fixed = TRUE), function(x)
    DBI::dbExistsTable(db, DBI::Id(schema = x[1], table = x[2])), logical(1))
  if (!all(present)) stop("Tables manquantes : ", paste(requis[!present], collapse = ", "))
  dossier = .db_get_tibble(db, paste0(
    "SELECT uid, ",
    "json_extract_string(contenu, '$.dossierParlementaire.titreDossier.titre') AS titre, ",
    "json_extract_string(contenu, '$.dossierParlementaire.procedureParlementaire.libelle') AS procedure, ",
    "json_extract_string(contenu, '$.dossierParlementaire.legislature') AS legislature ",
    "FROM an_dossiers.dossiers WHERE uid = ?"), params = list(uid))
  if (!nrow(dossier)) stop("Dossier introuvable dans la base locale : ", uid)
  documents = .db_get_tibble(db, paste0(
    "SELECT uid AS document_uid, ",
    "json_extract_string(contenu, '$.document.titres.titrePrincipal') AS titre, ",
    "json_extract_string(contenu, '$.document.cycleDeVie.chrono.dateDepot') AS date_depot, ",
    "json_extract_string(contenu, '$.document.cycleDeVie.chrono.datePublication') AS date_publication, ",
    "json_extract_string(contenu, '$.document.cycleDeVie.chrono.dateCreation') AS date_creation ",
    "FROM an_dossiers.documents ",
    "WHERE json_extract_string(contenu, '$.document.dossierRef') = ? ",
    "ORDER BY coalesce(nullif(date_depot, ''), nullif(date_publication, ''), nullif(date_creation, '')), document_uid"), params = list(uid))
  amendements = if (nrow(documents)) .db_get_tibble(db, paste0(
    "SELECT uid, ",
    "json_extract_string(contenu, '$.amendement.identification.numeroLong') AS numero, ",
    "json_extract_string(contenu, '$.amendement.cycleDeVie.sort') AS sort, ",
    "json_extract_string(contenu, '$.amendement.texteLegislatifRef') AS texte_ref ",
    "FROM an_amendements.amendements WHERE ",
    "json_extract_string(contenu, '$.amendement.texteLegislatifRef') IN (",
    paste(rep("?", nrow(documents)), collapse = ","), ") ",
    "ORDER BY texte_ref, numero, uid"), params = as.list(documents$document_uid)) else
      tibble::tibble(uid = character(), numero = character(), sort = character(),
                 texte_ref = character())
  resultat = list(dossier = dossier, documents = documents, amendements = amendements)
  if (!isTRUE(pdf)) return(resultat)
  if (is.null(sortie)) sortie = file.path("/tmp", paste0("parlementR-dossier-", uid, ".pdf"))
  if (!is.character(sortie) || length(sortie) != 1L || is.na(sortie) ||
      !nzchar(sortie) || tolower(tools::file_ext(sortie)) != "pdf")
    stop("'sortie' doit designer un fichier PDF")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Installer 'knitr' pour produire le PDF")
  echapper = .echapper_latex_senateurs
  titre = dossier$titre[[1L]]
  if (is.na(titre) || !nzchar(titre)) titre = paste("Dossier", uid)
  nettoyer = function(x) {
    x = as.character(x)
    x[is.na(x) | grepl('"@xsi:nil"[[:space:]]*:[[:space:]]*"true"', x)] = ""
    x
  }
  documents$titre = nettoyer(documents$titre)
  amendements$sort = nettoyer(amendements$sort)
  amendements$sort[!nzchar(trimws(amendements$sort))] = "Non renseigne"
  resume = if (nrow(amendements)) {
    comptes = table(amendements$sort)
    x = tibble::tibble(Sort = names(comptes), Nombre = as.integer(comptes))
    x[order(-x$Nombre, x$Sort), , drop = FALSE]
  } else tibble::tibble(Sort = character(), Nombre = integer())
  # La date affichee est qualifiee : ne jamais assimiler publication et depot.
  date_valide = function(x) {
    x = nettoyer(x)
    x[grepl("^\\s*(null|\\{)", x, ignore.case = TRUE)] = ""
    trimws(x)
  }
  documents$date_depot = date_valide(documents$date_depot)
  documents$date_publication = date_valide(documents$date_publication)
  documents$date_creation = date_valide(documents$date_creation)
  # Les horodatages officiels sont ISO 8601 : seule la date civile est affichee.
  date_courte = function(x) ifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x),
                                   substr(x, 1L, 10L), "")
  documents$date_affichee = ifelse(nzchar(date_courte(documents$date_depot)),
    paste0("Depot\\newline ", date_courte(documents$date_depot)),
    ifelse(nzchar(date_courte(documents$date_publication)),
      paste0("Publication\\newline ", date_courte(documents$date_publication)),
      ifelse(nzchar(date_courte(documents$date_creation)),
        paste0("Creation\\newline ", date_courte(documents$date_creation)),
        "Date non renseignee")))
  tableau_documents = if (nrow(documents)) {
    # Le titre occupe toute la largeur utile ; la reference reste accessible
    # en dessous, en caracteres reduits. La date a sa propre colonne.
    entete = paste0("\\begin{longtable}{@{}p{11.9cm}p{3.1cm}@{}}\n",
                    "\\toprule\nDocument & Date \\\\\n",
                    "\\midrule\n\\endhead\n")
    lignes = vapply(seq_len(nrow(documents)), function(i) {
      titre_document = documents$titre[i]
      if (!nzchar(titre_document)) titre_document = "Titre non renseigne"
      paste0(echapper(titre_document),
             "\\newline{\\footnotesize\\texttt{",
             echapper(documents$document_uid[i]), "}}",
             " & {\\footnotesize ", documents$date_affichee[i],
             "} \\\\")
    }, character(1))
    paste0(entete, paste(lignes, collapse = "\n"),
           "\n\\bottomrule\n\\end{longtable}")
  } else "Aucun document rattache dans la base locale."
  tableau_amendements = if (nrow(resume)) {
    knitr::kable(resume, format = "latex", booktabs = TRUE, escape = TRUE,
                 row.names = FALSE)
  } else "Aucun amendement relie par reference de texte dans la base locale."
  contenu = c(
    paste0("\\noindent\\textbf{Identifiant officiel :} ", echapper(uid), "\\par\\smallskip"),
    paste0("\\noindent\\colorbox{blue!9}{\\parbox{\\dimexpr\\linewidth-2\\fboxsep\\relax}{",
           "\\textbf{Sujet :} ", echapper(titre), "}}\\par\\medskip"),
    paste0("\\noindent\\textbf{Procedure :} ", echapper(dossier$procedure[[1L]]), "\\par"),
    paste0("\\noindent\\textbf{Legislature :} ", echapper(dossier$legislature[[1L]]), "\\par"),
    "\\section*{Documents officiellement rattaches}",
    tableau_documents,
    "\\section*{Amendements relies par reference de texte}",
    paste0("\\noindent Total : ", nrow(amendements), " amendement(s).\\par"),
    tableau_amendements
  )
  chemin = .pdf_senateurs("Assemblee nationale - Dossier legislatif", titre,
                          paste(contenu, collapse = "\n\n"), sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir = ouvrir)
}
