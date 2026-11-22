# Recherche nominative et fiche historique, exclusivement dans DuckDB local.
.normaliser_nom_senateur = function(x) {
  x = iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  x = tolower(x)
  trimws(gsub("[^a-z0-9]+", " ", x))
}

.resoudre_senateur = function(nom, con) {
  if (!is.character(nom) || length(nom) != 1L || is.na(nom) || !nzchar(trimws(nom)))
    stop("'nom' doit etre un nom de senateur non vide.", call. = FALSE)
  s = .db_get_tibble(con,
    "SELECT senmat, sennomuse, sennomtec, senprenomuse FROM senateurs.sen")
  nom_complet = trimws(paste(s$senprenomuse, s$sennomuse))
  cle = .normaliser_nom_senateur(nom)
  noms = .normaliser_nom_senateur(s$sennomuse)
  noms_tech = .normaliser_nom_senateur(s$sennomtec)
  complets = .normaliser_nom_senateur(nom_complet)
  complets_inverse = .normaliser_nom_senateur(paste(s$sennomuse, s$senprenomuse))
  ok = which(noms == cle | noms_tech == cle | complets == cle | complets_inverse == cle)
  if (!length(ok)) {
    distances = pmin(utils::adist(cle, noms)[1, ],
                     utils::adist(cle, noms_tech)[1, ],
                     utils::adist(cle, complets)[1, ])
    limite = max(1L, floor(nchar(cle) / 4))
    proches = which(distances <= limite)
    proches = proches[order(distances[proches], nom_complet[proches])]
    proposition = if (length(proches)) paste0(
      "\nVouliez-vous dire : ",
      paste(utils::head(unique(nom_complet[proches]), 5L), collapse = " ; "), " ?") else ""
    stop("Aucun senateur trouve pour : '", nom, "'.", proposition, call. = FALSE)
  }
  if (length(ok) != 1L)
    stop("Nom ambigu : '", nom, "'. Precisez le prenom : ",
         paste(nom_complet[ok], collapse = " ; "), ".", call. = FALSE)
  s$senmat[[ok]]
}

#' Fiche historique d'un senateur
#'
#' Recherche par nom ou prenom et nom, sans selection approximative implicite.
#' Les sections restituent les lignes historiques de la base locale, y compris
#' les dates manquantes et les codes non encore traduits.
#' @param nom Nom du senateur (ou prenom et nom).
#' @param db Connexion DuckDB facultative.
#' @return Liste de tableaux de donnees pour un seul senateur.
#' @export
fiche_senateur = function(nom, db = NULL) {
  con = .connexion_valide(db)
  if (!DBI::dbExistsTable(con, DBI::Id(schema = "senateurs", table = "sen")))
    stop("Table senateurs.sen absente : executer actualiser(jeux = 'senateurs').", call. = FALSE)
  id = .resoudre_senateur(nom, con)
  lire = function(sql) .db_get_tibble(con, sql, params = list(id))
  fiche = list(
    identite = lire("SELECT senmat, sennomuse, senprenomuse, sendatnai, sendatdec, etasencod, sendespro, sencircou, sendaiurl FROM senateurs.sen WHERE senmat = ?"),
    mandats = lire("SELECT eludatdeb AS debut, eludatfin AS fin, eludatelu AS election, dptnum AS departement, typmancod AS type_mandat, etadebmancod AS debut_motif, etafinmancod AS fin_motif FROM senateurs.elusen WHERE senmat = ? ORDER BY eludatdeb"),
    groupes_politiques = lire("SELECT m.memgrppoldatdeb AS debut, m.memgrppoldatfin AS fin, m.grppolcod AS code, g.grppolliccou AS sigle, g.grppollibcou AS libelle, m.typapppolcod AS type_appartenance FROM senateurs.memgrppol m LEFT JOIN senateurs.grppol g ON m.grppolcod = g.grppolcod WHERE m.senmat = ? ORDER BY m.memgrppoldatdeb NULLS FIRST"),
    fonctions_groupes = lire("SELECT m.grppolcod AS code_groupe, g.grppollibcou AS groupe, fm.fongrppolcod AS code_fonction, f.fongrppollib AS fonction, fm.fonmemgrppoldatdeb AS debut, fm.fonmemgrppoldatfin AS fin FROM senateurs.fonmemgrppol fm JOIN senateurs.memgrppol m ON fm.memgrppolid = m.memgrppolid LEFT JOIN senateurs.grppol g ON m.grppolcod = g.grppolcod LEFT JOIN senateurs.fongrppol f ON fm.fongrppolcod = f.fongrppolcod WHERE m.senmat = ? AND fm.fongrppolcod <> 'MEMBREPOL' ORDER BY fm.fonmemgrppoldatdeb DESC NULLS LAST"),
    organismes = lire("SELECT m.memgrpsendatent AS debut, m.memgrpsendatsor AS fin, m.orgcod AS code, g.typorgcod AS type_organisme FROM senateurs.memgrpsen m LEFT JOIN senateurs.grpsenami g ON m.orgcod = g.orgcod WHERE m.senmat = ? ORDER BY m.memgrpsendatent NULLS FIRST"),
    commissions = lire("SELECT memcomdatdeb AS debut, memcomdatfin AS fin, orgcod AS code, memcomtitsup AS statut FROM senateurs.memcom WHERE senmat = ? ORDER BY memcomdatdeb NULLS FIRST"),
    delegations = lire("SELECT memdelegadatdeb AS debut, memdelegadatfin AS fin, orgcod AS code, designcod AS designation FROM senateurs.memdelega WHERE senmat = ? ORDER BY memdelegadatdeb NULLS FIRST"),
    mandats_locaux = lire("SELECT eluid, typmancod AS type_mandat, titelecod AS titre, regcod AS region, dptnum AS departement, fonction, organisme FROM senateurs.mandat_local WHERE senmat = ? ORDER BY eluid")
  )
  class(fiche) = c("fiche_senateur", "list")
  fiche
}

#' Produire une fiche biographique lisible pour un senateur
#' @param nom Nom ou prenom et nom du senateur.
#' @param sortie Chemin du PDF dans /tmp par defaut.
#' @param ouvrir Ouvrir le PDF produit.
#' @param db Connexion DuckDB facultative.
#' @param cache Repertoire de cache des photographies.
#' @return Chemin du PDF, invisiblement.
#' @export
rapport_senateur = function(nom, sortie = NULL, ouvrir = TRUE, db = NULL,
                              cache = tools::R_user_dir("parlementR", "cache")) {
  con = .connexion_valide(db)
  fiche = fiche_senateur(nom, db = con)
  id = fiche$identite[1, ]
  nom_affiche = trimws(paste(id$senprenomuse, id$sennomuse))
  if (is.null(sortie))
    sortie = file.path('/tmp', paste0('parlementR-fiche-senateur-', id$senmat, '.pdf'))

  valeur = function(x, vide = "Non renseign\u00E9") {
    if (!length(x) || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))) return(vide)
    as.character(x[1])
  }
  date = function(x) {
    if (!length(x) || is.na(x[1])) return("Date non renseign\u00E9e")
    d = as.Date(x[1], origin = "1970-01-01")
    if (is.na(d)) return("Date non renseign\u00E9e")
    format(d, "%d/%m/%Y")
  }
  periode = function(debut, fin) {
    a = if (is.na(debut)) "D\u00E9but non renseign\u00E9" else paste0("Du ", date(debut))
    b = if (is.na(fin)) "fin non renseign\u00E9e" else paste0("au ", date(fin))
    paste(a, b)
  }
  # Les nomenclatures sont consultees localement ; un code inconnu reste explicite.
  reference = function(table, cle, code, candidats) {
    if (is.na(code) || !nzchar(as.character(code))) return(NA_character_)
    colonnes = DBI::dbListFields(con, DBI::Id(schema = 'senateurs', table = table))
    lib = intersect(candidats, colonnes)
    if (!cle %in% colonnes || !length(lib)) return(NA_character_)
    sql = paste0('SELECT ', paste(DBI::dbQuoteIdentifier(con, lib), collapse = ', '),
                 ' FROM senateurs.', DBI::dbQuoteIdentifier(con, table),
                 ' WHERE ', DBI::dbQuoteIdentifier(con, cle), ' = ? LIMIT 1')
    z = .db_get_tibble(con, sql, params = list(code))
    if (!nrow(z)) return(NA_character_)
    v = unlist(z[1, ], use.names = FALSE)
    v = v[!is.na(v) & nzchar(trimws(as.character(v)))]
    if (!length(v)) NA_character_ else as.character(v[1])
  }
  organisme = function(code, table = 'liborg') {
    reference(table, 'orgcod', code,
              if (table == 'liborg') c('evelil', 'evelib', 'evelic')
              else c('libgrpsenlib', 'libgrpsenlil', 'libgrpsenlic'))
  }
  territoire = function(code) {
    lib = reference('dpt', 'dptnum', code,
                    c('dptlib', 'dptlil', 'dptnom', 'dptlibcou', 'dptlic'))
    if (!is.na(lib)) return(lib)
    # Le territoire figurant dans la fiche du senateur est une information
    # textuelle officielle, utile si la nomenclature n'a pas de libelle reconnu.
    if (!is.na(id$sencircou) && nrow(fiche$mandats) &&
        length(unique(stats::na.omit(fiche$mandats$departement))) == 1L)
      return(id$sencircou)
    paste0('D\u00e9partement (code ', code, ')')
  }
  identite = c(
    paste("Statut :", switch(valeur(id$etasencod), ACTIF = "En exercice",
      ANCIEN = "Ancien s\u00E9nateur", valeur(id$etasencod))),
    paste("N\u00E9(e) le", date(id$sendatnai)),
    if (!is.na(id$sendatdec)) paste("D\u00E9c\u00E9d\u00E9(e) le", date(id$sendatdec)),
    paste("Profession d\u00E9clar\u00E9e :", valeur(id$sendespro)),
    paste("Derni\u00E8re circonscription renseign\u00E9e :", valeur(id$sencircou)))

  m = fiche$mandats
  mandats = if (nrow(m)) vapply(seq_len(nrow(m)), function(i) {
    x = m[i, ]
    paste0(periode(x$debut, x$fin), " \u2014 ", territoire(x$departement),
           " (\u00E9lection : ", date(x$election), ")")
  }, character(1)) else character()
  g = fiche$groupes_politiques
  groupes = if (nrow(g)) vapply(seq_len(nrow(g)), function(i) {
    x = g[i, ]
    paste0(periode(x$debut, x$fin), " \u2014 Groupe ", valeur(x$code),
           if (!is.na(x$libelle)) paste0(" (libell\u00E9 actuel : ", x$libelle, ")") else "",
           if (!is.na(x$type_appartenance)) paste0(" ; appartenance : ",
             switch(x$type_appartenance, N = "membre", A = "apparent\u00E9",
                    R = "rattach\u00E9", paste0("code ", x$type_appartenance))) else "")
  }, character(1)) else character()
  fg = fiche$fonctions_groupes
  fonctions_groupes = if (nrow(fg)) vapply(seq_len(nrow(fg)), function(i) {
    x = fg[i, ]
    paste0(valeur(x$fonction, valeur(x$code_fonction)), " du groupe ",
           valeur(x$code_groupe), " \u2014 ", periode(x$debut, x$fin))
  }, character(1)) else character()
  c = fiche$commissions
  commissions = if (nrow(c)) vapply(seq_len(nrow(c)), function(i) {
    x = c[i, ]
    lib = organisme(x$code)
    if (!is.na(x$code) && x$code == "NEANT") lib = "Aucune commission"
    if (is.na(lib)) lib = paste0("Commission (code ", x$code, ")")
    paste(periode(x$debut, x$fin), "\u2014", lib)
  }, character(1)) else character()
  d = fiche$delegations
  delegations = if (nrow(d)) vapply(seq_len(nrow(d)), function(i) {
    x = d[i, ]
    lib = organisme(x$code)
    if (is.na(lib)) lib = paste0("D\u00E9l\u00E9gation (code ", x$code, ")")
    paste(periode(x$debut, x$fin), "\u2014", lib,
          if (!is.na(x$designation)) paste0("(", tolower(x$designation), ")") else "")
  }, character(1)) else character()
  l = fiche$mandats_locaux
  mandats_locaux = if (nrow(l)) vapply(seq_len(nrow(l)), function(i) {
    x = l[i, ]
    type = switch(valeur(x$type_mandat), CANTON = "Mandat d\u00E9partemental",
                  MUNICIPAL = "Mandat municipal", valeur(x$type_mandat))
    titre = reference("titele", "titelecod", x$titre,
                      c("titelelil", "titelelib", "titelelic", "evelil", "evelib"))
    paste0(type, if (!is.na(titre)) paste0(" \u2014 ", titre) else "",
           if (!is.na(x$organisme)) paste0(" \u2014 ", x$organisme) else "",
           if (!is.na(x$departement)) paste0(" \u2014 ", territoire(x$departement)) else "",
           if (!is.na(x$fonction)) paste0(" \u2014 ", x$fonction) else "")
  }, character(1)) else character()
  o = fiche$organismes
  organismes = if (nrow(o)) vapply(seq_len(nrow(o)), function(i) {
    x = o[i, ]
    lib = organisme(x$code, "libgrpsen")
    if (is.na(lib)) lib = paste0("Organisme (code ", x$code, ")")
    paste(periode(x$debut, x$fin), "\u2014", lib)
  }, character(1)) else character()
  sections = list(
    "Mandats s\u00E9natoriaux" = mandats,
    "Appartenances politiques" = groupes,
    "Fonctions dans les groupes politiques" = fonctions_groupes,
    "Commissions" = commissions,
    "D\u00E9l\u00E9gations" = delegations,
    "Autres mandats d\u00E9clar\u00E9s" = mandats_locaux,
    "Autres organismes s\u00E9natoriaux" = organismes)
  naissance = suppressWarnings(as.Date(id$sendatnai))
  date_naissance = "Inconnu"
  if (!is.na(naissance)) {
    age = as.integer(format(Sys.Date(), "%Y")) -
      as.integer(format(naissance, "%Y"))
    if (format(Sys.Date(), "%m-%d") < format(naissance, "%m-%d"))
      age = age - 1L
    date_naissance = paste0(format(naissance, "%d/%m/%Y"), " (", age, " ans)")
  }
  identite_table = data.frame(
    Information = c("Civilit\u00E9", "Date de naissance", "Lieu de naissance",
                    "Profession", "Groupe politique"),
    Valeur = c("Inconnu", date_naissance, "Inconnu",
               valeur(id$sendespro, "Inconnu"),
               if (nrow(g)) valeur(g$libelle[nrow(g)], "Inconnu") else "Inconnu"),
    stringsAsFactors = FALSE)
  # Le lien HATVP est fourni directement par la source officielle du Senat.
  # Le moteur PDF le place dans le cartouche d'identite.
  hatvp = valeur(id$sendaiurl, "")
  if (grepl("^https?://", hatvp))
    sections[["D\u00e9claration HATVP"]] = hatvp
  photo = file.path(cache, "senat", "photos", paste0(id$senmat, ".jpg"))
  if (!.photo_jpeg(photo)) photo = NULL
  chemin = .pdf_fiche_depute_grid(nom_affiche, identite_table, sections,
    photo, sortie, assemblee = "Senat", source = "Senat")
  .ouvrir_pdf(chemin, ouvrir)
  invisible(chemin)
}
