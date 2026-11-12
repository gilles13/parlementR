# Fiche nominative issue exclusivement des JSON AMO10 locaux.
.normaliser_nom_depute = function(x) {
  x = iconv(x, from = '', to = 'ASCII//TRANSLIT', sub = '')
  trimws(gsub('[^a-z0-9]+', ' ', tolower(x)))
}

.resoudre_depute = function(nom, con) {
  if (!is.character(nom) || length(nom) != 1L || is.na(nom) || !nzchar(trimws(nom)))
    stop("'nom' doit etre un nom de depute non vide.", call. = FALSE)
  x = .db_get_tibble(con, "SELECT uid,
    contenu ->> '$.acteur.etatCivil.ident.prenom' AS prenom,
    contenu ->> '$.acteur.etatCivil.ident.nom' AS nom
    FROM an_deputes.acteurs")
  x = x[!is.na(x$nom) & nzchar(x$nom), , drop = FALSE]
  complet = trimws(paste(x$prenom, x$nom))
  cle = .normaliser_nom_depute(nom)
  noms = .normaliser_nom_depute(x$nom)
  directs = .normaliser_nom_depute(complet)
  inverses = .normaliser_nom_depute(paste(x$nom, x$prenom))
  ok = which(noms == cle | directs == cle | inverses == cle)
  if (!length(ok)) {
    distances = pmin(utils::adist(cle, noms)[1, ],
                     utils::adist(cle, directs)[1, ],
                     utils::adist(cle, inverses)[1, ])
    proches = which(distances <= max(2L, floor(nchar(cle) / 3)))
    proches = proches[order(distances[proches], complet[proches])]
    proposition = if (length(proches)) paste0(
      ' Vouliez-vous dire : ',
      paste(utils::head(unique(complet[proches]), 5L), collapse = ' ; '), ' ?') else ''
    stop("Aucun depute trouve pour : '", nom, "'.", proposition, call. = FALSE)
  }
  if (length(ok) > 1L)
    stop("Nom ambigu : '", nom, "'. Precisez le prenom : ",
         paste(complet[ok], collapse = ' ; '), '.', call. = FALSE)
  x[ok, , drop = FALSE]
}

#' Fiche exhaustive d'un depute dans les donnees locales
#'
#' Recherche par nom, prenom et nom, ou nom et prenom. Les homonymes
#' provoquent une erreur explicite et les noms proches sont proposes.
#' @param nom Nom du depute recherche.
#' @param db Connexion DuckDB facultative.
#' @return Liste contenant identite, acteur, mandats, organes et deports.
#' @export
fiche_depute = function(nom, db = NULL) {
  con = .connexion_valide(db)
  for (table in c('acteurs', 'organes', 'deports')) {
    if (!DBI::dbExistsTable(con, DBI::Id(schema = 'an_deputes', table = table)))
      stop('Table an_deputes.', table, ' absente : actualiser les donnees des deputes.', call. = FALSE)
  }
  personne = .resoudre_depute(nom, con)
  id = personne$uid[[1]]
  acteur = .db_get_tibble(con,
    'SELECT CAST(contenu AS VARCHAR) AS contenu FROM an_deputes.acteurs WHERE uid = ?',
    params = list(id))
  document = jsonlite::fromJSON(acteur$contenu[[1]], simplifyVector = FALSE)$acteur
  mandats = document$mandats$mandat
  if (is.null(mandats)) mandats = list()
  if (!is.null(mandats$uid)) mandats = list(mandats)
  refs = unique(vapply(mandats, function(m) {
    ref = m$organes$organeRef
    if (is.null(ref) || !length(ref)) NA_character_ else as.character(ref[[1]])
  }, character(1)))
  refs = refs[!is.na(refs)]
  organes = list()
  if (length(refs)) {
    q = paste(rep('?', length(refs)), collapse = ',')
    lignes = .db_get_tibble(con, paste0('SELECT uid, CAST(contenu AS VARCHAR) AS contenu ',
      'FROM an_deputes.organes WHERE uid IN (', q, ')'), params = as.list(refs))
    organes = stats::setNames(lapply(lignes$contenu, function(z)
      jsonlite::fromJSON(z, simplifyVector = FALSE)$organe), lignes$uid)
  }
  # Les deports peuvent etre relies a un acteur par des champs imbriques.
  # Le filtre JSON textuel est suivi d'une verification recursive des valeurs.
  candidats = .db_get_tibble(con,
    "SELECT uid, CAST(contenu AS VARCHAR) AS contenu FROM an_deputes.deports
     WHERE contains(CAST(contenu AS VARCHAR), ?)", params = list(id))
  contient = function(x, valeur) {
    if (is.list(x)) return(any(vapply(x, contient, logical(1), valeur = valeur)))
    any(!is.na(x) & as.character(x) == valeur)
  }
  deports = lapply(candidats$contenu, jsonlite::fromJSON, simplifyVector = FALSE)
  deports = deports[vapply(deports, contient, logical(1), valeur = id)]
  structure(list(identite = personne, acteur = document,
                 mandats = mandats, organes = organes, deports = deports),
            class = c('fiche_depute', 'list'))
}

#' Produire la fiche PDF synthetique d'un depute
#' @param nom Nom ou prenom et nom du depute.
#' @param sortie Chemin du PDF, dans /tmp par defaut.
#' @param ouvrir Ouvrir le PDF apres generation.
#' @param db Connexion DuckDB facultative.
#' @return Chemin du PDF, invisiblement.
#' @export
produire_fiche_depute = function(nom, sortie = NULL, ouvrir = TRUE, db = NULL) {
  fiche = fiche_depute(nom, db = db)
  identite = fiche$identite
  nom_affiche = trimws(paste(identite$prenom, identite$nom))
  if (is.null(sortie)) sortie = file.path('/tmp', paste0('parlementR-fiche-depute-', identite$uid, '.pdf'))
  esc = .echapper_latex_senateurs
  valeur = function(x, defaut = '') {
    if (is.null(x) || !length(x)) return(defaut)
    if (is.list(x)) return(defaut)
    y = trimws(paste(as.character(x), collapse = ', '))
    if (is.na(y) || !nzchar(y) || identical(y, 'NA')) defaut else y
  }
  champ = function(x, ...) {
    for (cle in c(...)) {
      if (!is.list(x) || is.null(x[[cle]])) return(NULL)
      x = x[[cle]]
    }
    x
  }
  # Les tables sont construites a partir des informations deja extraites :
  # les references techniques ne sont pas des informations a afficher.
  tableau = function(d, largeurs = NULL) {
    if (!nrow(d)) return('\\emph{Aucune information renseign\u00e9e.}\\par\n')
    d[] = lapply(d, function(z) {
      z = as.character(z)
      z[is.na(z) | !nzchar(trimws(z))] = '\\textemdash{}'
      z
    })
    n = ncol(d)
    if (is.null(largeurs)) largeurs = rep(0.94 / n, n)
    cols = paste(sprintf('p{%.3f\\textwidth}', largeurs), collapse = '')
    entete = paste(esc(names(d)), collapse = ' & ')
    corps = vapply(seq_len(nrow(d)), function(i) {
      cellules = vapply(d[i, , drop = FALSE], identity, character(1))
      cellules = ifelse(cellules == '\\textemdash{}', cellules, esc(cellules))
      paste0(if (i %% 2L == 0L) '\\rowcolor{gray!12}\n' else '',
             paste(cellules, collapse = ' & '), ' \\\\ \n')
    }, character(1))
    paste0('\\begingroup\\footnotesize\n\\begin{longtable}{', cols, '}\n',
           '\\hline ', entete, ' \\\\ \\hline\\endfirsthead\n',
           '\\hline ', entete, ' \\\\ \\hline\\endhead\n',
           paste0(corps, collapse = ''), '\\hline\n\\end{longtable}\\endgroup\n')
  }
  section = function(titre, x, nouvelle_page = TRUE) paste0(
    if (nouvelle_page) '\\clearpage\n' else '',
    '\\section*{', esc(titre), '}\n', x)
  info = function(libelle, x) data.frame(Information = libelle, Valeur = x, stringsAsFactors = FALSE)
  ident = fiche$acteur$etatCivil
  id = ident$ident
  lignes = list()
  ajouter = function(libelle, x) {
    v = valeur(x)
    if (nzchar(v)) lignes[[length(lignes) + 1L]] <<- info(libelle, v)
  }
  # Le groupe politique courant est affiche avant les informations d'etat civil.
  groupes = Filter(function(m) {
    if (!identical(valeur(m$typeOrgane), 'GP')) return(FALSE)
    debut = as.Date(valeur(m$dateDebut))
    fin = as.Date(valeur(m$dateFin))
    !is.na(debut) && debut <= Sys.Date() && (is.na(fin) || fin >= Sys.Date())
  }, fiche$mandats)
  groupes = vapply(groupes, function(m) {
    ref = valeur(champ(m, 'organes', 'organeRef'))
    org = fiche$organes[[ref]]
    valeur(org$libelle, valeur(org$libelleEdition))
  }, character(1))
  groupes = unique(groupes[nzchar(groupes)])
  if (length(groupes)) ajouter('Groupe politique', paste(groupes, collapse = ', '))
  ajouter('Nom', champ(id, 'nom'))
  ajouter('Pr\u00e9nom', champ(id, 'prenom'))
  ajouter('Civilit\u00e9', champ(id, 'civ'))
  naissance = valeur(champ(ident, 'infoNaissance', 'dateNais'))
  date_naissance = suppressWarnings(as.Date(substr(naissance, 1L, 10L)))
  if (!is.na(date_naissance) && date_naissance <= Sys.Date()) {
    aujourd_hui = Sys.Date()
    age = as.integer(format(aujourd_hui, '%Y')) - as.integer(format(date_naissance, '%Y'))
    if (format(aujourd_hui, '%m-%d') < format(date_naissance, '%m-%d')) age = age - 1L
    naissance = paste0(naissance, ' (', age, ' ans)')
  }
  ajouter('Date de naissance', naissance)
  ajouter('Lieu de naissance', champ(ident, 'infoNaissance', 'villeNais'))
  ajouter('D\u00e9partement de naissance', champ(ident, 'infoNaissance', 'depNais'))
  ajouter('Pays de naissance', champ(ident, 'infoNaissance', 'paysNais'))
  profession = valeur(champ(fiche$acteur, 'profession', 'libelleCourant'),
    valeur(champ(fiche$acteur, 'profession', 'libelle')))
  ajouter('Profession', profession)
  # Les liens HATVP sont affiches comme hyperliens, non comme champs bruts.
  ident_table = if (length(lignes)) do.call(rbind, lignes) else info('Identit\u00e9', nom_affiche)
  contenu = section('Identit\u00e9 et parcours', tableau(ident_table, c(.31, .63)), FALSE)
  lien = function(url, etiquette = NULL) {
    url = valeur(url)
    if (!grepl('^https?://', url, ignore.case = TRUE)) return('')
    if (is.null(etiquette)) etiquette = url
    # detokenize evite que les caracteres speciaux des URL cassent LaTeX.
    paste0('\\href{\\detokenize{', url, '}}{', esc(etiquette), '}')
  }
  hatvp = lien(champ(fiche$acteur, 'uri_hatvp'), 'Consulter la declaration HATVP')
  if (nzchar(hatvp)) contenu = paste0(contenu, '\\noindent ', hatvp, '\\par\n')

  # Chaque mandat est une ligne lisible, enrichie du libelle de son organe.
  # On n'affiche plus une seconde fois les objets JSON des organes.
  mandats = fiche$mandats
  if (length(mandats)) {
    libelle_type = c(GP = 'Groupe politique', COMPER = 'Commission permanente',
      BUREAU = 'Bureau de l\'Assembl\u00e9e', CONFPT = 'Conf\u00e9rence des pr\u00e9sidents',
      GE = 'Groupe d\'\u00e9tudes', GA = 'Groupe d\'amiti\u00e9',
      DELEG = 'D\u00e9l\u00e9gation', DELEGBUREAU = 'D\u00e9l\u00e9gation du Bureau',
      CMP = 'Commission mixte paritaire', COMNL = 'Commission non permanente',
      MISINFO = 'Mission d\'information', MISINFOCOM = 'Mission d\'information',
      MISINFOPRE = 'Mission d\'information', ASSEMBLEE = 'Assembl\u00e9e nationale',
      PARPOL = 'Rattachement politique', CIRCONSCRIPTION = 'Mandat de d\u00e9put\u00e9',
      ORGEXTPARL = 'Organisme extraparlementaire')
    items = lapply(mandats, function(m) {
      ref = valeur(champ(m, 'organes', 'organeRef'))
      org = fiche$organes[[ref]]
      type = valeur(m$typeOrgane, 'Autre organisme')
      if (type %in% names(libelle_type)) type = unname(libelle_type[[type]])
      lib = valeur(org$libelle, valeur(org$libelleEdition, type))
      qualite = valeur(champ(m, 'infosQualite', 'codeQualite'),
                      valeur(champ(m, 'infosQualite', 'libQualite')))
      if (!nzchar(qualite)) qualite = 'Membre'
      debut = valeur(m$dateDebut)
      fin = valeur(m$dateFin)
      etat = if (!nzchar(fin)) 'En cours' else 'Termin\u00e9'
      data.frame(Categorie = type, Organisme = lib, Fonction = qualite,
                 Debut = debut, Fin = if (nzchar(fin)) fin else etat,
                 stringsAsFactors = FALSE)
    })
    d = do.call(rbind, items)
    d = unique(d)
    # Les fonctions en cours d'abord, puis l'historique ; aucune reference brute.
    d = d[order(d$Fin != 'En cours', d$Categorie, d$Organisme, -xtfrm(d$Debut)), , drop = FALSE]
    actifs = d[d$Fin == 'En cours', , drop = FALSE]
    anciens = d[d$Fin != 'En cours', , drop = FALSE]
    contenu = paste0(contenu, section('Mandats et responsabilit\u00e9s en cours',
      tableau(actifs, c(.17, .29, .22, .12, .14))))
    if (nrow(anciens)) contenu = paste0(contenu,
      section('Mandats et responsabilit\u00e9s ant\u00e9rieurs',
        tableau(anciens, c(.17, .29, .22, .12, .14))))
  }


  # Collaborateurs : les listes sont portees par les mandats de depute.
  # Plusieurs formats XML->JSON sont possibles (un objet ou une liste).
  collaborateurs = list()
  collecter = function(x) {
    if (is.null(x) || !is.list(x)) return(invisible(NULL))
    if (!is.null(x$collaborateur)) {
      if (is.character(x$collaborateur)) {
        collaborateurs[[length(collaborateurs) + 1L]] <<- x$collaborateur
        return(invisible(NULL))
      }
      return(collecter(x$collaborateur))
    }
    if (!is.null(x$nom) || !is.null(x$prenom) || !is.null(x$nomFamille)) {
      nom = trimws(paste(valeur(x$prenom), valeur(x$nom, valeur(x$nomFamille))))
      if (nzchar(nom)) collaborateurs[[length(collaborateurs) + 1L]] <<- nom
      return(invisible(NULL))
    }
    for (element in x) if (is.list(element)) collecter(element)
    invisible(NULL)
  }
  for (m in fiche$mandats) collecter(m$collaborateurs)
  if (length(collaborateurs)) {
    noms = unique(unlist(collaborateurs, use.names = FALSE))
    contenu = paste0(contenu, section('Collaborateurs parlementaires',
      paste0('\\begin{itemize}\n', paste0('\\item ', esc(noms), '\n', collapse = ''),
             '\\end{itemize}\n')))
  }

  # Coordonnees : lire les champs du referentiel AMO10, sans tableau
  # Information/Valeur. Les adresses peuvent contenir des champs differents.
  adresses = champ(fiche$acteur, 'adresses', 'adresse')
  contacts = character()
  lire_contact = function(a) {
    if (!is.list(a)) return(invisible(NULL))
    if (is.list(a$adresse)) return(lire_contact(a$adresse))
    # Les codes numeriques du referentiel ne sont pas des libelles publics.
    type = valeur(a$type, valeur(a$typeAdresse))
    if (grepl('^[0-9]+$', type)) type = '' 
    if (is.list(a$adressePostale)) {
      postal = a$adressePostale
    } else postal = a
    rue = paste(Filter(nzchar, c(valeur(postal$numeroRue), valeur(postal$typeVoie),
      valeur(postal$nomVoie, valeur(postal$nomRue)), valeur(postal$complementAdresse),
      valeur(postal$adresse), valeur(postal$adresse1), valeur(postal$adresse2))), collapse = ' ')
    ville = paste(Filter(nzchar, c(valeur(postal$codePostal), valeur(postal$ville))), collapse = ' ')
    tel = valeur(a$numeroTelephone, valeur(a$telephone, valeur(a$tel)))
    mail = valeur(a$valElec, valeur(a$email))
    web = valeur(a$siteWeb, valeur(a$url))
    elements = c(if (nzchar(type)) paste0('\\textbf{', esc(type), '}\\par\n'),
      if (nzchar(rue)) paste0(esc(rue), '\\par\n'),
      if (nzchar(ville)) paste0(esc(ville), '\\par\n'),
      if (nzchar(tel)) paste0('Telephone : ', esc(tel), '\\par\n'),
      if (grepl('^[^ @]+@[^ @]+$', mail)) paste0('Courriel : \\href{mailto:', mail, '}{', esc(mail), '}\\par\n'),
      if (nzchar(web) && nzchar(lien(web))) paste0('Site : ', lien(web), '\\par\n'))
    if (length(elements)) contacts <<- c(contacts, paste0(elements, collapse = ''))
    invisible(NULL)
  }
  if (is.list(adresses)) {
    if (!is.null(adresses$uid) || !is.null(adresses$valElec) ||
        !is.null(adresses$adressePostale)) lire_contact(adresses)
    else for (a in adresses) lire_contact(a)
  }
  contenu = paste0(contenu, section('Coordonnees publiques',
    if (length(contacts)) paste(unique(contacts), collapse = '\\medskip\n')
    else '\\emph{Aucune coordonnee publique renseignee.}\\par\n'))

  # Le detail integral des deports reste accessible par fiche_depute(),
  # tandis que le PDF n'en restitue que les elements explicites.
  if (length(fiche$deports)) {
    deports = lapply(fiche$deports, function(x) {
      x = if (!is.null(x$deport)) x$deport else x
      info('D\u00e9port d\u00e9clar\u00e9', paste(c(
        valeur(x$dateDebut), valeur(x$dateFin), valeur(x$motif),
        valeur(x$libelle), valeur(x$objet))[nzchar(c(
        valeur(x$dateDebut), valeur(x$dateFin), valeur(x$motif),
        valeur(x$libelle), valeur(x$objet)))], collapse = ' - '))
    })
    deports = do.call(rbind, deports)
    deports = deports[nzchar(deports$Valeur), , drop = FALSE]
    contenu = paste0(contenu, section('D\u00e9ports d\u00e9clar\u00e9s',
      if (nrow(deports)) tableau(deports, c(.31, .63)) else
        '\\emph{D\u00e9ports pr\u00e9sents dans les donn\u00e9es, sans description synth\u00e9tique disponible.}\\par\n'))
  }
  chemin = .pdf_senateurs(paste0('Fiche : ', nom_affiche),
    paste0('Source : Assembl\u00e9e nationale ; extraction locale du ', format(Sys.Date(), '%d/%m/%Y')),
    contenu, sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir)
  invisible(chemin)
}
