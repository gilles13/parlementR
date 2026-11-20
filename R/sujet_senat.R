#' Rechercher un sujet dans les amendements du Senat
#'
#' Recherche litterale dans l'objet et le dispositif AMELI. Les dossiers
#' DOSLEG sont raccordes par leur signet officiel, jamais par leur titre.
#' @param sujet Expression a rechercher (sans jokers SQL).
#' @param db Connexion DuckDB ; NULL utilise connexion().
#' Le resultat conserve le sujet recherche dans l'attribut `sujet`.
#' @return Liste de tables : textes, amendements, lectures.
#' @export
rech_amdt_senat = function(sujet, db = NULL) {
  if (!is.character(sujet) || length(sujet) != 1L || is.na(sujet) ||
      !nzchar(trimws(sujet))) stop("'sujet' doit etre une expression non vide")
  db = .connexion_valide(db)
  requis = c("sen_amendements.amd", "sen_amendements.txt_ameli",
    "sen_amendements.amdsen", "sen_amendements.sen_ameli",
    "sen_amendements.sor", "sen_dosleg.loi", "sen_dosleg.lecture")
  present = vapply(strsplit(requis, ".", fixed = TRUE), function(x)
    DBI::dbExistsTable(db, DBI::Id(schema = x[1], table = x[2])), logical(1))
  if (!all(present)) stop("Tables manquantes : ", paste(requis[!present], collapse = ", "))

  amendements = tibble::as_tibble(DBI::dbGetQuery(db, "
    SELECT a.id AS amendement_id, a.num AS numero,
      a.datdep AS date_depot, a.txtid AS texte_id,
      a.subid AS subdivision_id, s.lib AS sort,
      a.obj AS objet, a.dis AS dispositif,
      string_agg(DISTINCT trim(coalesce(p.prenomuse, '') || ' ' ||
        coalesce(p.nomuse, '')), ', ') AS auteurs
    FROM sen_amendements.amd a
    LEFT JOIN sen_amendements.sor s ON a.sorid = s.id
    LEFT JOIN sen_amendements.amdsen x ON x.amdid = a.id
    LEFT JOIN sen_amendements.sen_ameli p ON p.entid = x.senid
    WHERE strpos(lower(coalesce(a.obj, '')), lower(?)) > 0
       OR strpos(lower(coalesce(a.dis, '')), lower(?)) > 0
    GROUP BY a.id, a.num, a.datdep, a.txtid, a.subid,
      s.lib, a.obj, a.dis
    ORDER BY a.datdep, a.id
  ", params = list(sujet, sujet)))

  textes = tibble::as_tibble(DBI::dbGetQuery(db, "
    SELECT DISTINCT t.id AS texte_id, t.num AS numero_texte,
      t.int AS intitule, t.inl AS titre_complet, t.datdep AS date_depot,
      trim(t.doslegsignet) AS signet,
      trim(l.loicod) AS dossier_id, l.loient AS dossier_intitule,
      l.loitit AS dossier_titre, l.url_an
    FROM sen_amendements.txt_ameli t
    LEFT JOIN sen_dosleg.loi l ON trim(l.signet) = trim(t.doslegsignet)
    WHERE t.id IN (
      SELECT DISTINCT txtid FROM sen_amendements.amd
      WHERE strpos(lower(coalesce(obj, '')), lower(?)) > 0
         OR strpos(lower(coalesce(dis, '')), lower(?)) > 0
    )
    ORDER BY t.id
  ", params = list(sujet, sujet)))

  lectures = tibble::as_tibble(DBI::dbGetQuery(db, "
    SELECT DISTINCT trim(r.loicod) AS dossier_id,
      trim(r.lecidt) AS lecture_id, r.typleccod AS type_lecture,
      r.leccom AS commentaire
    FROM sen_dosleg.lecture r
    WHERE trim(r.loicod) IN (
      SELECT trim(l.loicod) FROM sen_dosleg.loi l
      JOIN sen_amendements.txt_ameli t
        ON trim(l.signet) = trim(t.doslegsignet)
      WHERE t.id IN (
        SELECT DISTINCT txtid FROM sen_amendements.amd
        WHERE strpos(lower(coalesce(obj, '')), lower(?)) > 0
           OR strpos(lower(coalesce(dis, '')), lower(?)) > 0
      )
    )
    ORDER BY dossier_id, lecture_id
  ", params = list(sujet, sujet)))
  .sujet_recherche(structure(list(sujet = sujet, textes = textes,
    amendements = amendements, lectures = lectures), class = "parlementR_sujet"), sujet)
}

#' Produire un PDF documentaire a partir des references d'un sujet
#'
#' Aucun resume ni verbatim n'est genere : seuls les champs de la base
#' locale sont reproduits. Les liens vers des debats ne sont pas des debats.
#' @param references Resultat de rech_amdt_senat() ou rech_docs().
#' @param fichier Chemin du PDF, par defaut dans /tmp.
#' @param ouvrir Ouvrir le document apres sa creation.
#' @param surligner Mettre en gras les occurrences du terme recherche dans le contenu.
#' @return Chemin du PDF (invisible).
#' @export
.produire_documents = function(references, fichier = NULL, ouvrir = FALSE, surligner = TRUE) {
  documents = inherits(references, "parlementR_documents")
  if (!documents && !inherits(references, "parlementR_sujet"))
    stop("'references' doit provenir de rech_amdt_senat() ou rech_docs()")
  if (is.null(fichier)) fichier = tempfile(if (documents) "parlementR-documents-" else
    "parlementR-sujet-", tmpdir = "/tmp", fileext = ".pdf")
  if (!is.character(fichier) || length(fichier) != 1L || is.na(fichier) ||
      !nzchar(fichier) || tolower(tools::file_ext(fichier)) != "pdf")
    stop("'fichier' doit designer un PDF")
  nettoyer = function(x) {
    x = as.character(x)
    x[is.na(x)] = ""
    vapply(x, function(y) {
      y = gsub("(?i)<br\\s*/?>|</(p|div|li)\\s*>", "\n", y, perl = TRUE)
      if (grepl("<[^>]+>|&[A-Za-z#0-9]+;", y))
        y = tryCatch(xml2::xml_text(xml2::read_html(paste0("<div>", y, "</div>"))),
                     error = function(e) gsub("<[^>]+>", "", y))
      y
    }, character(1))
  }
  if (documents) {
    terme = attr(references, "sujet")
    donnees = list(type = "documents", sujet = terme,
      sources = attr(references, "metadonnees_sources"),
      absentes = attr(references, "sources_absentes"),
      lignes = references, surligner = isTRUE(surligner))
    if (nrow(references)) donnees$lignes$contenu = nettoyer(references$contenu)
    titre = "Recherche documentaire"
  } else {
    donnees = list(type = "senat", sujet = references$sujet,
      textes = references$textes, lectures = references$lectures,
      amendements = references$amendements, surligner = isTRUE(surligner))
    for (col in intersect(c("intitule", "titre_complet"), names(donnees$textes)))
      donnees$textes[[col]] = nettoyer(donnees$textes[[col]])
    for (col in intersect(c("objet", "dispositif"), names(donnees$amendements)))
      donnees$amendements[[col]] = nettoyer(donnees$amendements[[col]])
    titre = "S\u00E9nat - Recherche documentaire"
  }
  chemin = .pdf_questions_senat("documents_recherche.Rmd", donnees,
    titre, as.character(donnees$sujet), fichier)
  .ouvrir_pdf(chemin, ouvrir = ouvrir)
}
