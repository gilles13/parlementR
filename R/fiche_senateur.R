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
    identite = lire("SELECT senmat, sennomuse, senprenomuse, sendatnai, sendatdec, etasencod, sendespro, sencircou FROM senateurs.sen WHERE senmat = ?"),
    mandats = lire("SELECT eludatdeb AS debut, eludatfin AS fin, eludatelu AS election, dptnum AS departement, typmancod AS type_mandat, etadebmancod AS debut_motif, etafinmancod AS fin_motif FROM senateurs.elusen WHERE senmat = ? ORDER BY eludatdeb"),
    groupes_politiques = lire("SELECT m.memgrppoldatdeb AS debut, m.memgrppoldatfin AS fin, m.grppolcod AS code, g.grppolliccou AS sigle, g.grppollibcou AS libelle, m.typapppolcod AS type_appartenance FROM senateurs.memgrppol m LEFT JOIN senateurs.grppol g ON m.grppolcod = g.grppolcod WHERE m.senmat = ? ORDER BY m.memgrppoldatdeb NULLS FIRST"),
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
#' @return Chemin du PDF, invisiblement.
#' @export
produire_fiche_senateur = function(nom, sortie = NULL, ouvrir = TRUE, db = NULL) {
  con = .connexion_valide(db)
  fiche = fiche_senateur(nom, db = con)
  id = fiche$identite[1, ]
  nom_affiche = trimws(paste(id$senprenomuse, id$sennomuse))
  if (is.null(sortie))
    sortie = file.path('/tmp', paste0('parlementR-fiche-senateur-', id$senmat, '.pdf'))

  esc = .echapper_latex_senateurs
  valeur = function(x, vide = 'Non renseign\u00e9') {
    if (!length(x) || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))) return(vide)
    as.character(x[1])
  }
  date = function(x) {
    if (!length(x) || is.na(x[1])) return('Date non renseign\u00e9e')
    d = as.Date(x[1], origin = '1970-01-01')
    if (is.na(d)) return('Date non renseign\u00e9e')
    format(d, '%d/%m/%Y')
  }
  periode = function(debut, fin) {
    a = if (is.na(debut)) 'D\u00e9but non renseign\u00e9' else paste0('Du ', date(debut))
    b = if (is.na(fin)) 'fin non renseign\u00e9e' else paste0('au ', date(fin))
    paste(a, b)
  }
  ligne = function(texte) paste0('\\noindent ', esc(texte), '\\par\n')
  item = function(texte) paste0('\\item ', esc(texte), '\n')
  liste = function(x) {
    if (!length(x)) return('\\emph{Aucune information renseign\u00e9e.}\\par\n')
    paste0('\\begin{itemize}\\setlength{\\itemsep}{2pt}\n',
           paste(vapply(x, item, character(1)), collapse = ''),
           '\\end{itemize}\n')
  }
  section = function(titre, contenu) paste0('\\section*{', esc(titre), '}\n', contenu)

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
  identite = paste0(
    ligne(paste('Statut :', switch(valeur(id$etasencod),
                                 ACTIF = 'En exercice', ANCIEN = 'Ancien s\u00e9nateur',
                                 valeur(id$etasencod)))),
    ligne(paste('N\u00e9(e) le', date(id$sendatnai))),
    if (!is.na(id$sendatdec)) ligne(paste('D\u00e9c\u00e9d\u00e9(e) le', date(id$sendatdec))) else '',
    ligne(paste('Profession d\u00e9clar\u00e9e :', valeur(id$sendespro))),
    ligne(paste('Derni\u00e8re circonscription renseign\u00e9e :', valeur(id$sencircou))))

  m = fiche$mandats
  mandats = if (nrow(m)) vapply(seq_len(nrow(m)), function(i) {
    x = m[i, ]
    paste0(periode(x$debut, x$fin), ' \u2014 ', territoire(x$departement),
           ' (\u00e9lection : ', date(x$election), ')')
  }, character(1)) else character()

  g = fiche$groupes_politiques
  groupes = if (nrow(g)) vapply(seq_len(nrow(g)), function(i) {
    x = g[i, ]
    # Les denominations de grppol sont actuelles et ne sont pas necessairement
    # celles de la periode concernee : conserver le code historique en premier.
    paste0(periode(x$debut, x$fin), ' \u2014 Groupe ', valeur(x$code),
           if (!is.na(x$libelle)) paste0(' (libell\u00e9 actuel : ', x$libelle, ')') else '',
           if (!is.na(x$type_appartenance)) paste0(' ; appartenance : ',
             switch(x$type_appartenance, N = 'membre', A = 'apparent\u00e9',
                    R = 'rattach\u00e9', paste0('code ', x$type_appartenance))) else '')
  }, character(1)) else character()

  c = fiche$commissions
  commissions = if (nrow(c)) vapply(seq_len(nrow(c)), function(i) {
    x = c[i, ]
    lib = organisme(x$code)
    if (!is.na(x$code) && x$code == 'NEANT') lib = 'Aucune commission'
    if (is.na(lib)) lib = paste0('Commission (code ', x$code, ')')
    paste(periode(x$debut, x$fin), '\u2014', lib)
  }, character(1)) else character()

  d = fiche$delegations
  delegations = if (nrow(d)) vapply(seq_len(nrow(d)), function(i) {
    x = d[i, ]
    lib = organisme(x$code)
    if (is.na(lib)) lib = paste0('D\u00e9l\u00e9gation (code ', x$code, ')')
    paste(periode(x$debut, x$fin), '\u2014', lib,
          if (!is.na(x$designation)) paste0('(', tolower(x$designation), ')') else '')
  }, character(1)) else character()

  l = fiche$mandats_locaux
  mandats_locaux = if (nrow(l)) vapply(seq_len(nrow(l)), function(i) {
    x = l[i, ]
    type = switch(valeur(x$type_mandat),
                  CANTON = 'Mandat d\u00e9partemental', MUNICIPAL = 'Mandat municipal',
                  valeur(x$type_mandat))
    titre = reference('titele', 'titelecod', x$titre,
                      c('titelelil', 'titelelib', 'titelelic', 'evelil', 'evelib'))
    paste0(type, if (!is.na(titre)) paste0(' \u2014 ', titre) else '',
           if (!is.na(x$organisme)) paste0(' \u2014 ', x$organisme) else '',
           if (!is.na(x$departement)) paste0(' \u2014 ', territoire(x$departement)) else '',
           if (!is.na(x$fonction)) paste0(' \u2014 ', x$fonction) else '')
  }, character(1)) else character()

  # Les appartenances aux organismes sénatoriaux sont nombreuses et parfois
  # reduites a des evenements d'une journee : les presenter en annexe.
  o = fiche$organismes
  organismes = if (nrow(o)) vapply(seq_len(nrow(o)), function(i) {
    x = o[i, ]
    lib = organisme(x$code, 'libgrpsen')
    if (is.na(lib)) lib = paste0('Organisme (code ', x$code, ')')
    paste(periode(x$debut, x$fin), '\u2014', lib)
  }, character(1)) else character()

  contenu = paste0('\\small\n',
    section('Identit\u00e9', identite),
    section('Mandats s\u00e9natoriaux', liste(mandats)),
    section('Appartenances politiques', paste0(
      liste(groupes),
      '\\emph{Les codes de groupe sont ceux de la base historique ; les d\u00e9nominations actuelles ne sont pas substitu\u00e9es aux noms anciens.}\\par\n')),
    section('Commissions', liste(commissions)),
    section('D\u00e9l\u00e9gations', liste(delegations)),
    section('Autres mandats d\u00e9clar\u00e9s', liste(mandats_locaux)),
    section('Autres organismes s\u00e9natoriaux', paste0(
      ligne(paste(length(organismes), 'appartenances ou \u00e9v\u00e9nements document\u00e9s.')),
      liste(organismes))))
  chemin = .pdf_senateurs(paste0('Fiche : ', nom_affiche),
    paste0('Source : S\u00e9nat ; extraction locale du ', format(Sys.time(), '%d/%m/%Y')),
    contenu, sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir)
  invisible(chemin)
}
