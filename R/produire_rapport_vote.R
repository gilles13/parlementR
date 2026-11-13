# Retrouver un amendement sans se fier au seul numero, reutilise entre textes.
# Une correspondance ambiguë n'est jamais choisie arbitrairement.
.contexte_scrutin = function(db, scrutin, amendement_uid = NULL) {
  titre = scrutin$titre[[1L]]
  motif = "amendement\\s+n[\u00b0o]\\s*([0-9]+)"
  m = regexec(motif, titre, ignore.case = TRUE, perl = TRUE)
  numero = regmatches(titre, m)[[1L]]
  vide = list(statut = "sans_amendement", amendement = NULL)
  if (length(numero) < 2L) return(vide)
  requis = c("an_amendements.amendements", "an_dossiers.documents")
  present = vapply(strsplit(requis, ".", fixed = TRUE), function(x)
    DBI::dbExistsTable(db, DBI::Id(schema = x[1L], table = x[2L])), logical(1))
  if (!all(present)) stop("Tables necessaires au contexte manquantes : ",
                           paste(requis[!present], collapse = ", "))
  candidats = .db_get_tibble(db, "SELECT a.uid,
    json_extract_string(a.contenu, '$.amendement.identification.numeroLong') AS numero,
    json_extract_string(a.contenu, '$.amendement.texteLegislatifRef') AS texte_ref,
    json_extract_string(a.contenu, '$.amendement.signataires.auteur.acteurRef') AS auteur_ref,
    json_extract_string(d.contenu, '$.document.titres.titrePrincipal') AS texte,
    json_extract_string(p.contenu, '$.acteur.etatCivil.ident.prenom') AS prenom,
    json_extract_string(p.contenu, '$.acteur.etatCivil.ident.nom') AS nom,
    json_extract_string(a.contenu, '$.amendement.corps.contenuAuteur.exposeSommaire') AS expose,
    json_extract_string(a.contenu, '$.amendement.corps.contenuAuteur.dispositif') AS dispositif,
    json_extract_string(a.contenu, '$.amendement.cycleDeVie.dateDepot') AS date_depot,
    json_extract_string(a.contenu, '$.amendement.cycleDeVie.etatDesTraitements.etat.libelle') AS etat,
    json_extract_string(a.contenu, '$.amendement.pointeurFragmentTexte.division.articleDesignation') AS article,
    json_extract_string(a.contenu, '$.amendement.seanceDiscussionRef') AS seance_discussion_ref,
    json_extract_string(a.contenu, '$.amendement.examenRef') AS examen_ref,
    json_extract(a.contenu, '$.amendement.signataires.cosignataires')::VARCHAR AS cosignataires_json
    FROM an_amendements.amendements a
    LEFT JOIN an_dossiers.documents d
      ON d.uid = json_extract_string(a.contenu, '$.amendement.texteLegislatifRef')
    LEFT JOIN an_historique.acteurs p
      ON p.uid = json_extract_string(a.contenu, '$.amendement.signataires.auteur.acteurRef')
    WHERE regexp_matches(json_extract_string(a.contenu,
      '$.amendement.identification.numeroLong'), ?)",
    params = list(paste0("(^|-)", numero[[2L]], "$")))
  # Une reference explicite fournie par l'appelant est verifiee contre
  # les candidats de meme numero, sans aucun rapprochement de noms.
  if (!is.null(amendement_uid)) {
    selection = candidats[candidats$uid == amendement_uid, , drop = FALSE]
    if (nrow(selection) != 1L)
      stop("L'UID d'amendement fourni ne correspond pas au numero du scrutin")
    return(list(statut = "resolu", amendement = selection))
  }
  # Ne jamais utiliser un nom de personne pour etablir une jointure.
  # Le numero seul n'est pas une cle : il est reutilise entre textes.
  # On exige une correspondance sur une reference structuree de seance.
  seance = .db_get_tibble(db, "SELECT
    json_extract_string(contenu, '$.scrutin.seanceRef') AS seance_ref
    FROM an_votes.scrutins WHERE uid = ?", params = list(scrutin$uid[[1L]]))$seance_ref[[1L]]
  candidats$seance_ref = candidats$seance_discussion_ref
  concordance = !is.na(seance) & nzchar(seance) &
    !is.na(candidats$seance_ref) & candidats$seance_ref == seance
  selection = candidats[concordance, , drop = FALSE]
  if (nrow(selection) > 1L)
    return(list(statut = "ambigu", amendement = NULL, candidats = selection))
  if (!nrow(selection))
    return(list(statut = "lien_structurel_absent", amendement = NULL,
                candidats = candidats))
  list(statut = "resolu", amendement = selection)

}

# Conserver les paragraphes du HTML officiel, sans tronquer les textes.
# Le LaTeX est echappe apres decodage par xml2.
.texte_amendement_pdf = function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x)))
    return("\\emph{Non renseign\u00e9.}")
  doc = xml2::read_html(paste0("<html><body>", x, "</body></html>"))
  corps = xml2::xml_find_first(doc, "//body")
  blocs = xml2::xml_find_all(corps, ".//p | .//li | .//h1 | .//h2 | .//h3")
  if (!length(blocs)) blocs = corps
  lignes = vapply(blocs, function(noeud) {
    texte = xml2::xml_text(noeud)
    texte = trimws(gsub("[[:space:]\u00a0]+", " ", texte, perl = TRUE))
    if (!nzchar(texte)) return("")
    paste0(if (identical(xml2::xml_name(noeud), "li")) "\\textbullet{} " else "",
           .echapper_latex_senateurs(texte))
  }, character(1))
  lignes = lignes[nzchar(lignes)]
  if (!length(lignes)) return("\\emph{Non renseign\u00e9.}")
  paste(lignes, collapse = "\n\n\\par\n\n")
}

# Le JSON des cosignataires peut contenir des acteurs sous plusieurs niveaux.
.refs_cosignataires = function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(character())
  obj = jsonlite::fromJSON(x, simplifyVector = FALSE)
  collecter = function(z) {
    if (!is.list(z)) return(character())
    direct = if (!is.null(z$acteurRef) && is.character(z$acteurRef)) z$acteurRef else character()
    unique(c(direct, unlist(lapply(z, collecter), use.names = FALSE)))
  }
  unique(collecter(obj))
}

.cosignataires_scrutin = function(db, amendement) {
  refs = .refs_cosignataires(amendement$cosignataires_json[[1L]])
  if (!length(refs)) return(data.frame(uid = character(), nom = character()))
  valeurs = paste(as.character(DBI::dbQuoteString(db, refs)), collapse = ", ")
  .db_get_tibble(db, paste0("SELECT uid, trim(concat_ws(' ',
    json_extract_string(contenu, '$.acteur.etatCivil.ident.prenom'),
    json_extract_string(contenu, '$.acteur.etatCivil.ident.nom'))) AS nom
    FROM an_historique.acteurs WHERE uid IN (", valeurs, ") ORDER BY nom"))
}

#' Produire un rapport PDF sur un scrutin public de l'Assemblee nationale
#'
#' Les effectifs et les positions proviennent exclusivement des donnees locales.
#' Les non-votants nominatifs ne sont pas assimiles aux deputes absents du scrutin.
#' @param scrutin UID officiel du scrutin (ou numero de scrutin).
#' @param db Connexion DuckDB ; NULL utilise connexion().
#' @param contexte Complement facultatif ; le contexte legislatif est recherche automatiquement.
#' @param amendement_uid UID officiel de l'amendement, si aucune liaison structuree unique n'existe.
#' @param sortie Fichier PDF de destination, dans /tmp par defaut.
#' @param ouvrir Ouvrir le PDF apres production.
#' @param pdf Si FALSE, retourner les tableaux sans produire de PDF.
#' @return Chemin du PDF (invisible), ou liste des donnees du rapport.
#' @export
produire_rapport_vote = function(scrutin, db = NULL, contexte = NULL,
                                  sortie = NULL, ouvrir = FALSE, pdf = TRUE,
                                  amendement_uid = NULL) {
  if (!is.character(scrutin) || length(scrutin) != 1L || is.na(scrutin) ||
      !grepl("^(VTANR[[:alnum:]]+|[0-9]+)$", scrutin))
    stop("'scrutin' doit etre un UID officiel ou un numero de scrutin")
  if (!is.null(contexte) && (!is.character(contexte) || length(contexte) != 1L || is.na(contexte)))
    stop("'contexte' doit etre une chaine de caracteres")
  if (!is.null(amendement_uid) &&
      (!is.character(amendement_uid) || length(amendement_uid) != 1L ||
       is.na(amendement_uid) || !grepl("^AMANR", amendement_uid)))
    stop("'amendement_uid' doit etre un UID officiel d'amendement")
  db = .connexion_valide(db)
  requis = c("an_votes.scrutins", "an_historique.acteurs", "an_historique.organes")
  present = vapply(strsplit(requis, ".", fixed = TRUE), function(x)
    DBI::dbExistsTable(db, DBI::Id(schema = x[1], table = x[2])), logical(1))
  if (!all(present)) stop("Tables manquantes : ", paste(requis[!present], collapse = ", "))
  vues = DBI::dbGetQuery(db, "SELECT table_name FROM information_schema.views WHERE table_schema = 'an_votes'")$table_name
  if (!all(c("votes_deputes", "votes_groupes") %in% vues))
    stop("Vues des votes manquantes : reimporter les scrutins")
  s = .db_get_tibble(db, "SELECT uid,
    json_extract_string(contenu, '$.scrutin.numero') AS numero,
    json_extract_string(contenu, '$.scrutin.dateScrutin') AS date,
    json_extract_string(contenu, '$.scrutin.titre') AS titre,
    json_extract_string(contenu, '$.scrutin.sort.code') AS sort_code,
    json_extract_string(contenu, '$.scrutin.sort.libelle') AS sort_libelle
    FROM an_votes.scrutins
    WHERE uid = ? OR json_extract_string(contenu, '$.scrutin.numero') = ?",
    params = list(scrutin, scrutin))
  if (nrow(s) != 1L) stop("Scrutin introuvable ou non unique : ", scrutin)
  uid = s$uid[[1L]]
  votes = .db_get_tibble(db, "SELECT v.categorie, v.depute_uid, v.groupe_uid,
    json_extract_string(a.contenu, '$.acteur.etatCivil.ident.prenom') AS prenom,
    json_extract_string(a.contenu, '$.acteur.etatCivil.ident.nom') AS nom,
    coalesce(json_extract_string(g.contenu, '$.organe.libelleAbrege'),
             json_extract_string(g.contenu, '$.organe.libelleAbrev'),
             json_extract_string(g.contenu, '$.organe.libelle')) AS groupe
    FROM an_votes.votes_deputes v
    LEFT JOIN an_historique.acteurs a ON a.uid = v.depute_uid
    LEFT JOIN an_historique.organes g ON g.uid = v.groupe_uid
    WHERE v.scrutin_uid = ?", params = list(uid))
  if (anyDuplicated(votes$depute_uid)) stop("Votes nominatifs dupliques pour ce scrutin")
  codes = c(pours = "Pour", contres = "Contre", abstentions = "Abstention",
            nonVotants = "Non-votant", nonVotantsVolontaires = "Non-votant volontaire")
  if (any(!votes$categorie %in% names(codes))) stop("Categorie de vote inconnue")
  votes$position = unname(codes[votes$categorie])
  votes$depute = trimws(paste(ifelse(is.na(votes$prenom), "", votes$prenom),
                               ifelse(is.na(votes$nom), "", votes$nom)))
  votes$depute[!nzchar(votes$depute)] = votes$depute_uid[!nzchar(votes$depute)]
  votes$groupe[is.na(votes$groupe) | !nzchar(votes$groupe)] = "Non renseign\u00e9"
  positions = unname(codes)
  groupes = as.data.frame.matrix(table(factor(votes$groupe), factor(votes$position, levels = positions)))
  groupes = data.frame(Groupe = rownames(groupes), groupes, check.names = FALSE, row.names = NULL)
  groupes = groupes[order(groupes$Groupe), , drop = FALSE]
  totaux = data.frame(Position = positions, Nombre = as.integer(table(factor(votes$position, levels = positions))), check.names = FALSE)
  totaux = totaux[totaux$Nombre > 0L, , drop = FALSE]
  votes = votes[order(match(votes$position, positions), votes$groupe, votes$nom, votes$prenom), , drop = FALSE]
  contexte_auto = .contexte_scrutin(db, s, amendement_uid = amendement_uid)
  contexte_auto$cosignataires = if (identical(contexte_auto$statut, "resolu"))
    .cosignataires_scrutin(db, contexte_auto$amendement) else NULL
  resultat = list(scrutin = s, contexte = contexte_auto, totaux = totaux, groupes = groupes, votes = votes)
  if (!isTRUE(pdf)) return(resultat)
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Installer 'knitr' pour produire le PDF")
  if (is.null(sortie)) sortie = file.path("/tmp", paste0("parlementR-scrutin-", uid, ".pdf"))
  if (!is.character(sortie) || length(sortie) != 1L || is.na(sortie) ||
      !nzchar(sortie) || tolower(tools::file_ext(sortie)) != "pdf")
    stop("'sortie' doit designer un fichier PDF")
  esc = .echapper_latex_senateurs
  val = function(x) {
    x = as.character(x)
    x[is.na(x) | !nzchar(trimws(x))] = "Non renseign\u00e9"
    x
  }
  tab = function(x, long = FALSE) {
    if (!nrow(x)) return("Aucune donn\u00e9e nominative.")
    k = knitr::kable(x, format = "latex", booktabs = TRUE, longtable = long,
                      row.names = FALSE, escape = TRUE)
    .zebrer_tableau_pdf(k)
  }
  pour = sum(votes$position == "Pour")
  contre = sum(votes$position == "Contre")
  abst = sum(votes$position == "Abstention")
  exprimes = pour + contre
  intro = c(
    paste0("\\noindent\\textbf{Scrutin n\u00b0 ", esc(val(s$numero[[1L]])), "} \u2014 ", esc(val(s$date[[1L]])), "\\par"),
    paste0("\\noindent\\textbf{Objet officiel :} ", esc(val(s$titre[[1L]])), "\\par"),
    paste0("\\noindent\\textbf{Identifiant :} \\texttt{", esc(uid), "}\\par"),
    if (identical(contexte_auto$statut, "resolu")) {
      a = contexte_auto$amendement
      champ = function(titre, valeur) {
        if (is.na(valeur) || !nzchar(trimws(valeur))) return(NULL)
        paste0("\\noindent\\textbf{", esc(titre), " :} ", esc(valeur), "\\par")
      }
      cosig = contexte_auto$cosignataires
      c("\\section*{La mesure soumise au vote}",
        champ("Amendement", a$numero[[1L]]),
        champ("Auteur", paste(a$prenom[[1L]], a$nom[[1L]])),
        champ("Projet de loi", a$texte[[1L]]),
        champ("Emplacement", a$article[[1L]]),
        champ("Date de d\u00e9p\u00f4t", a$date_depot[[1L]]),
        champ("\u00c9tat de traitement", a$etat[[1L]]),
        if (nrow(cosig)) paste0("\\noindent\\textbf{Cosignataires (", nrow(cosig), ") :} ",
                                 esc(paste(cosig$nom, collapse = ", ")), "\\par") else NULL,
        "\\subsection*{Dispositif propos\u00e9}",
        .texte_amendement_pdf(a$dispositif[[1L]]),
        "\\subsection*{Expos\u00e9 sommaire de l'auteur}",
        .texte_amendement_pdf(a$expose[[1L]]),
        "\\subsection*{R\u00e9f\u00e9rences l\u00e9gislatives}",
        champ("UID amendement", a$uid[[1L]]),
        champ("UID document", a$texte_ref[[1L]]),
        champ("R\u00e9f\u00e9rence de s\u00e9ance de discussion", a$seance_discussion_ref[[1L]]),
        champ("R\u00e9f\u00e9rence d'examen", a$examen_ref[[1L]]),
        "\\clearpage")
    } else paste0("\\noindent\\emph{Rapprochement non etabli (",
                  esc(contexte_auto$statut), "). Fournir amendement_uid si necessaire.}\\par"),
    if (!is.null(contexte) && nzchar(contexte)) paste0("\\medskip\\noindent\\textbf{Compl\u00e9ment :} ", esc(contexte), "\\par") else NULL,
    "\\section*{R\u00e9sultat du scrutin}",
    paste0("\\noindent\\textbf{Suffrages exprim\u00e9s :} ", exprimes,
           " (", pour, " pour, ", contre, " contre). ", abst,
           " abstention(s).\\par"),
    "\\noindent\\footnotesize Les non-votants nominatifs sont distincts des d\u00e9put\u00e9s ne figurant pas dans le d\u00e9compte nominatif. Les r\u00e9sultats d\u00e9crivent ce scrutin, pas l'ensemble des membres de chaque groupe.\\normalsize\\par",
    tab(totaux),
    "\\section*{Positions nominatives par groupe}",
    tab(groupes),
    "\\clearpage\\section*{Participation nominative}"
  )
  for (position in positions) {
    x = votes[votes$position == position, c("groupe", "depute", "depute_uid"), drop = FALSE]
    if (!nrow(x)) next
    names(x) = c("Groupe", "D\u00e9put\u00e9", "Identifiant")
    intro = c(intro, paste0("\\subsection*{", esc(position), " (", nrow(x), ")}"), tab(x, long = TRUE))
  }
  intro = c(intro, "\\medskip\\noindent\\footnotesize Source : Assembl\u00e9e nationale, scrutin public et votes nominatifs import\u00e9s dans la base locale parlementR.\\normalsize")
  chemin = .pdf_senateurs("Assembl\u00e9e nationale \u2014 Rapport de scrutin",
                          paste0("Scrutin n\u00b0 ", val(s$numero[[1L]])),
                          paste(intro, collapse = "\n\n"), sortie)
  .ouvrir_pdf_senateurs(chemin, ouvrir = ouvrir)
}
