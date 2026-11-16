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
rapport_dosleg_an = function(uid, db = NULL, sortie = NULL, ouvrir = FALSE, pdf = TRUE) {
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
  for (colonne in c("titre", "procedure", "legislature"))
    dossier[[colonne]] = .valeur_parlementaire(dossier[[colonne]], colonne)
  for (colonne in c("titre", "date_depot", "date_publication", "date_creation"))
    documents[[colonne]] = .valeur_parlementaire(documents[[colonne]], colonne)
  for (colonne in c("numero", "sort", "texte_ref"))
    amendements[[colonne]] = .valeur_parlementaire(amendements[[colonne]], colonne)
  resultat = .sujet_recherche(list(dossier = dossier, documents = documents, amendements = amendements), dossier$titre[[1L]])
  if (!isTRUE(pdf)) return(resultat)
  .verifier_volume_rapport(nrow(documents), unite = "documents")
  .verifier_volume_rapport(nrow(amendements))
  if (is.null(sortie)) sortie = file.path("/tmp", paste0("parlementR-dossier-", uid, ".pdf"))
  if (!is.character(sortie) || length(sortie) != 1L || is.na(sortie) ||
      !nzchar(sortie) || tolower(tools::file_ext(sortie)) != "pdf")
    stop("'sortie' doit designer un fichier PDF")
  titre = dossier$titre[[1L]]
  if (is.na(titre) || !nzchar(titre)) titre = paste("Dossier", uid)
  chemin = .pdf_questions_senat(
    modele = "dossier_an.Rmd", donnees = resultat,
    titre = "Assemblee nationale - Dossier legislatif",
    sous_titre = titre, sortie = sortie
  )
  .ouvrir_pdf_senateurs(chemin, ouvrir = ouvrir)
}
