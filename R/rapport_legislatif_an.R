# Parcours d'un dossier legislatif AN, exclusivement depuis DuckDB local.
.extraire_actes_legislatifs_an = function(x) {
  lignes = list()
  visiter = function(noeud, niveau = 0L) {
    if (is.null(noeud)) return(invisible(NULL))
    if (is.list(noeud) && is.null(names(noeud))) {
      for (element in noeud) visiter(element, niveau)
      return(invisible(NULL))
    }
    if (!is.list(noeud)) return(invisible(NULL))
    if (!is.null(noeud$codeActe)) {
      valeur = function(y) if (is.null(y) || !length(y)) NA_character_ else as.character(y[[1L]])
      lignes[[length(lignes) + 1L]] <<- data.frame(
        niveau = niveau, uid = valeur(noeud$uid),
        code = valeur(noeud$codeActe),
        libelle = valeur(noeud$libelleActe$libelleCourt),
        type = valeur(noeud[["@xsi:type"]]),
        date = valeur(noeud$dateActe),
        organe_ref = valeur(noeud$organeRef),
        texte_associe = valeur(noeud$texteAssocie),
        stringsAsFactors = FALSE)
      niveau = niveau + 1L
    }
    enfants = noeud$actesLegislatifs$acteLegislatif
    if (!is.null(enfants)) visiter(enfants, niveau)
    invisible(NULL)
  }
  visiter(x)
  if (!length(lignes)) return(data.frame(niveau = integer(), uid = character(),
    code = character(), libelle = character(), type = character(), date = character(),
    organe_ref = character(), texte_associe = character()))
  do.call(rbind, lignes)
}

#' Rapport du parcours legislatif d'un dossier de l'Assemblee nationale
#'
#' Restitue les etapes et evenements hierarchises, puis les documents lies.
#' Les relations sont exclusivement celles de la base locale.
#' @param ref UID officiel du dossier legislatif.
#' @param ouvrir Ouvrir le PDF apres sa production (TRUE par defaut).
#' @param fichier Chemin du PDF, dans /tmp par defaut.
#' @param pdf Si FALSE, renvoyer les donnees sans produire de PDF.
#' @param db Connexion DuckDB facultative ; NULL utilise connexion().
#' @return Chemin du PDF (invisible), ou liste de tables si pdf = FALSE.
#' @export
rapport_legislatif_an = function(ref, ouvrir = TRUE, fichier = NULL, pdf = TRUE, db = NULL) {
  if (!is.character(ref) || length(ref) != 1L || is.na(ref) ||
      !grepl("^[A-Za-z0-9_-]+$", ref)) stop("'ref' doit etre un UID valide")
  db = .connexion_valide(db)
  requis = c("an_dossiers.dossiers", "an_dossiers.documents")
  present = vapply(strsplit(requis, ".", fixed = TRUE), function(x)
    DBI::dbExistsTable(db, DBI::Id(schema = x[1L], table = x[2L])), logical(1))
  if (!all(present)) stop("Tables manquantes : ", paste(requis[!present], collapse = ", "))
  donnees = .db_get_tibble(db, paste0(
    "SELECT uid, contenu::VARCHAR AS contenu, ",
    "json_extract_string(contenu, '$.dossierParlementaire.titreDossier.titre') AS titre, ",
    "json_extract_string(contenu, '$.dossierParlementaire.procedureParlementaire.libelle') AS \"procedure\", ",
    "json_extract_string(contenu, '$.dossierParlementaire.legislature') AS legislature ",
    "FROM an_dossiers.dossiers WHERE uid = ?"), params = list(ref))
  if (!nrow(donnees)) stop("Dossier introuvable : ", ref)
  objet = jsonlite::fromJSON(donnees$contenu[[1L]], simplifyVector = FALSE)
  actes = .extraire_actes_legislatifs_an(objet$dossierParlementaire$actesLegislatifs$acteLegislatif)
  dossiers = donnees[, c("uid", "titre", "procedure", "legislature")]
  documents = .db_get_tibble(db, paste0(
    "SELECT uid AS document_uid, ",
    "json_extract_string(contenu, '$.document.titres.titrePrincipal') AS titre, ",
    "json_extract_string(contenu, '$.document.cycleDeVie.chrono.dateDepot') AS date_depot, ",
    "json_extract_string(contenu, '$.document.cycleDeVie.chrono.datePublication') AS date_publication ",
    "FROM an_dossiers.documents WHERE ",
    "json_extract_string(contenu, '$.document.dossierRef') = ? ",
    "ORDER BY date_depot NULLS LAST, document_uid"), params = list(ref))
  resultat = list(dossiers = dossiers, actes = tibble::as_tibble(actes), documents = documents)
  if (!isTRUE(pdf)) return(resultat)
  .verifier_volume_rapport(nrow(documents), unite = "documents")
  if (is.null(fichier)) fichier = file.path("/tmp", paste0("parlementR-legislatif-an-", ref, ".pdf"))
  if (!is.character(fichier) || length(fichier) != 1L || is.na(fichier) ||
      !nzchar(fichier) || tolower(tools::file_ext(fichier)) != "pdf")
    stop("'fichier' doit designer un PDF")
  .pdf_legislatif_an_grid(resultat, fichier)
  chemin = normalizePath(fichier, mustWork = TRUE)
  .ouvrir_pdf(chemin, ouvrir = ouvrir)
  invisible(chemin)
}
