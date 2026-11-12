# Import des comptes rendus XML, sans extraction du ZIP sur disque.
# L'XML complet sans namespaces garantit la conservation des champs non modelises.

.debats_attributs = function(noeud) {
  a = xml2::xml_attrs(noeud)
  jsonlite::toJSON(as.list(a), auto_unbox = TRUE, null = "null") |> as.character()
}

.debats_valeur = function(noeud, nom) {
  x = xml2::xml_attr(noeud, nom)
  if (length(x) == 0L || is.na(x)) NA_character_ else x
}

.debats_enfants = function(noeud, nom) {
  enfants = xml2::xml_children(noeud)
  enfants[xml2::xml_name(enfants) == nom]
}

.debats_texte = function(noeud, nom) {
  enfants = .debats_enfants(noeud, nom)
  if (!length(enfants)) NA_character_ else xml2::xml_text(enfants[[1L]])
}

# Le titre est le texte directement enfant du point, jamais celui des interventions.
.debats_titre = function(point) {
  titre = .debats_texte(point, "texte")
  if (is.na(titre)) return(NA_character_)
  titre = trimws(titre)
  if (nzchar(titre)) titre else NA_character_
}

# Importer les debats dans la connexion et la transaction d'actualiser().
.importer_debats = function(con, fichier, remplacer = FALSE, verbose = TRUE) {
  journal = function(...) if (isTRUE(verbose)) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [DEBATS] ", ...)
  if (!file.exists(fichier)) stop("Archive Syceron introuvable : ", fichier)
  journal("Debut de l'importation : ", fichier)
  membres = utils::unzip(fichier, list = TRUE)
  noms = membres$Name[grepl("\\.xml$", membres$Name, ignore.case = TRUE)]
  if (!length(noms)) stop("Aucun fichier XML dans l'archive.", call. = FALSE)

  DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS an_debats")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS an_debats.debats_seances (fichier VARCHAR PRIMARY KEY, uid VARCHAR, seance_ref VARCHAR, session_ref VARCHAR, metadonnees_json VARCHAR, xml_sans_namespace VARCHAR)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS an_debats.debats_points (fichier VARCHAR, position INTEGER, id_syceron VARCHAR, parent_id_syceron VARCHAR, attributs_json VARCHAR, titre VARCHAR, PRIMARY KEY(fichier, position))")
  DBI::dbExecute(con, "ALTER TABLE an_debats.debats_points ADD COLUMN IF NOT EXISTS titre VARCHAR")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS an_debats.debats_paragraphes (fichier VARCHAR, position INTEGER, id_syceron VARCHAR, point_id_syceron VARCHAR, id_acteur VARCHAR, ordre_absolu_seance VARCHAR, code_grammaire VARCHAR, attributs_json VARCHAR, orateurs_xml VARCHAR, texte VARCHAR, texte_xml VARCHAR, PRIMARY KEY(fichier, position))")

  deja = DBI::dbGetQuery(con, "SELECT fichier FROM an_debats.debats_seances")$fichier
  if (!remplacer) noms = noms[!noms %in% deja]
  lignes = 0L

  for (i in seq_along(noms)) {
    nom = noms[[i]]
    # read_xml prend en charge la connexion unz, sans extraction sur disque.
    doc = xml2::read_xml(unz(fichier, nom))
    xml2::xml_ns_strip(doc)
    racine = xml2::xml_root(doc)
    meta = .debats_enfants(racine, "metadonnees")
    metadata = if (length(meta)) {
      m = meta[[1L]]
      x = lapply(xml2::xml_children(m), function(n) list(
        nom = xml2::xml_name(n), attributs = as.list(xml2::xml_attrs(n)),
        texte = xml2::xml_text(n), xml = as.character(n)))
      jsonlite::toJSON(x, auto_unbox = TRUE, null = "null") |> as.character()
    } else "[]"
    seance = data.frame(
      fichier = nom, uid = .debats_texte(racine, "uid"),
      seance_ref = .debats_texte(racine, "seanceRef"),
      session_ref = .debats_texte(racine, "sessionRef"),
      metadonnees_json = metadata,
      xml_sans_namespace = as.character(doc))

    points = xml2::xml_find_all(doc, "//*[local-name()='point']")
    tab_points = if (length(points)) do.call(rbind, lapply(seq_along(points), function(j) {
      p = points[[j]]
      parent = xml2::xml_find_first(p, "ancestor::*[local-name()='point'][1]")
      data.frame(fichier = nom, position = j,
        id_syceron = .debats_valeur(p, "id_syceron"),
        parent_id_syceron = if (inherits(parent, "xml_missing")) NA_character_ else .debats_valeur(parent, "id_syceron"),
        attributs_json = .debats_attributs(p),
        titre = .debats_titre(p))
    })) else NULL

    paragraphes = xml2::xml_find_all(doc, "//*[local-name()='paragraphe']")
    tab_paragraphes = if (length(paragraphes)) do.call(rbind, lapply(seq_along(paragraphes), function(j) {
      p = paragraphes[[j]]
      point = xml2::xml_find_first(p, "ancestor::*[local-name()='point'][1]")
      orateurs = .debats_enfants(p, "orateurs")
      texte = .debats_enfants(p, "texte")
      data.frame(fichier = nom, position = j,
        id_syceron = .debats_valeur(p, "id_syceron"),
        point_id_syceron = if (inherits(point, "xml_missing")) NA_character_ else .debats_valeur(point, "id_syceron"),
        id_acteur = .debats_valeur(p, "id_acteur"),
        ordre_absolu_seance = .debats_valeur(p, "ordre_absolu_seance"),
        code_grammaire = .debats_valeur(p, "code_grammaire"),
        attributs_json = .debats_attributs(p),
        orateurs_xml = if (length(orateurs)) as.character(orateurs[[1L]]) else NA_character_,
        texte = if (length(texte)) xml2::xml_text(texte[[1L]]) else NA_character_,
        texte_xml = if (length(texte)) as.character(texte[[1L]]) else NA_character_)
    })) else NULL

    if (remplacer) {
      DBI::dbExecute(con, "DELETE FROM an_debats.debats_paragraphes WHERE fichier = ?", params = list(nom))
      DBI::dbExecute(con, "DELETE FROM an_debats.debats_points WHERE fichier = ?", params = list(nom))
      DBI::dbExecute(con, "DELETE FROM an_debats.debats_seances WHERE fichier = ?", params = list(nom))
    }
    DBI::dbAppendTable(con, DBI::Id(schema = "an_debats", table = "debats_seances"), seance)
    if (!is.null(tab_points)) DBI::dbAppendTable(con, DBI::Id(schema = "an_debats", table = "debats_points"), tab_points)
    if (!is.null(tab_paragraphes)) DBI::dbAppendTable(con, DBI::Id(schema = "an_debats", table = "debats_paragraphes"), tab_paragraphes)
    lignes = lignes + 1L + length(points) + length(paragraphes)
    if (i %% 10L == 0L || i == length(noms)) journal(i, "/", length(noms), " fichiers importes")
    rm(doc, points, paragraphes, tab_points, tab_paragraphes, seance)
  }
  journal("Import termine : ", length(noms), " fichiers, ", lignes, " lignes")
  list(fichiers = length(noms), lignes = lignes)
}

