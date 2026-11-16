# Fonctions de lecture locale des codes LEGI.
.legi_code = function(db, code) {
  if (length(code) != 1L || is.na(code) || !nzchar(trimws(code)))
    stop("Code invalide")
  textes = DBI::dbGetQuery(db, "SELECT id, titre FROM legi.textes")
  normaliser = function(x) {
    x = iconv(tolower(trimws(x)), from = "UTF-8", to = "ASCII//TRANSLIT")
    if (anyNA(x)) stop("Normalisation des noms de codes impossible")
    gsub("[[:space:]]+", " ", x)
  }
  cible = normaliser(code)
  noms = normaliser(textes$titre)
  candidats = which(noms == cible)
  if (!length(candidats)) {
    variantes = paste(c("code", "code de", "code du", "code de la", "code des", "code d'"), cible)
    candidats = which(noms %in% variantes)
  }
  if (length(candidats) != 1L)
    stop("Code introuvable ou ambigu : ", code)
  textes[candidats, , drop = FALSE]
}
.legi_html = function(x) {
  x = gsub("&", "&amp;", x, fixed = TRUE)
  x = gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
.legi_metadonnees = function(db, id_texte, id_article = NULL) {
  suivi = tryCatch(DBI::dbGetQuery(db, "SELECT date_import, md5 FROM suivi.actualisations WHERE jeu = 'legi'"), error = function(e) NULL)
  extraction = if (!is.null(suivi) && nrow(suivi) && !is.na(suivi$date_import[[1L]])) format(as.Date(suivi$date_import[[1L]]), "%d/%m/%Y") else "non renseignee"
  md5 = if (!is.null(suivi) && nrow(suivi) && !is.na(suivi$md5[[1L]])) suivi$md5[[1L]] else "non renseigne"
  url = if (is.null(id_article)) paste0("https://www.legifrance.gouv.fr/codes/texte_lc/", id_texte) else paste0("https://www.legifrance.gouv.fr/codes/article_lc/", id_article)
  list(extraction = extraction, md5 = md5, url = url)
}
.legi_pdf = function(titre, sous_titre, contenu, sortie, ouvrir, metadata, police = 10) {
  if (length(police) != 1L || !is.numeric(police) || is.na(police) || !is.finite(police) || police <= 0) stop("Police invalide")
  if (!is.data.frame(contenu) || !all(c("texte", "profondeur", "gras") %in% names(contenu)))
    stop("Contenu PDF invalide")
  if (is.null(sortie)) sortie = tempfile("parlementR-legi-", tmpdir = "/tmp", fileext = ".pdf")
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  sortie = normalizePath(sortie, mustWork = FALSE)
  pages = list()
  page = list()
  y = .835
  for (i in seq_len(nrow(contenu))) {
    profondeur = contenu$profondeur[[i]]
    gras = isTRUE(contenu$gras[[i]])
    lignes = strwrap(contenu$texte[[i]], width = max(25L, floor((92L - 5L * profondeur) * 9.5 / police)))
    for (ligne in lignes) {
      if (y < .13) {
        pages[[length(pages) + 1L]] = page
        page = list()
        y = .835
      }
      page[[length(page) + 1L]] = list(texte = ligne, y = y, profondeur = profondeur, gras = gras)
      y = y - .019
    }
    y = y - .009
  }
  pages[[length(pages) + 1L]] = page
  grDevices::cairo_pdf(sortie, width = 8.27, height = 11.69, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (p in seq_along(pages)) {
    grid::grid.newpage()
    .entete(titre, sous_titre)
    for (ligne in pages[[p]]) {
      grid::grid.text(ligne$texte, x = .08 + .025 * ligne$profondeur, y = ligne$y,
        just = "left", gp = grid::gpar(fontsize = police,
          fontface = if (ligne$gras) "bold" else "plain", col = "#252525"))
    }
    grid::grid.lines(x = c(.08, .92), y = c(.105, .105),
      gp = grid::gpar(col = "#AAAAAA", lwd = .6))
    grid::grid.text(paste0("Source : LEGI / L\u00e9gifrance  |  Extraction : ", metadata$extraction,
      "  |  MD5 : ", metadata$md5), x = .08, y = .084, just = "left",
      gp = grid::gpar(fontsize = 7, col = "#666666"))
    grid::grid.text(metadata$url, x = .08, y = .063, just = "left",
      gp = grid::gpar(fontsize = 7, col = "#666666"))
    grid::grid.text(sprintf("%d/%d", p, length(pages)), x = .92, y = .063,
      just = "right", gp = grid::gpar(fontsize = 9))
  }
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)
  if (isTRUE(ouvrir)) {
    lecteur = Sys.which("zathura")
    if (nzchar(lecteur)) system2(lecteur, shQuote(sortie), wait = FALSE)
  }
  invisible(normalizePath(sortie, mustWork = TRUE))
}
.legi_extraire_texte = function(xml) {
  doc = xml2::read_xml(xml, options = "NONET")
  blocs = xml2::xml_find_all(doc, "/ARTICLE/BLOC_TEXTUEL/CONTENU")
  if (!length(blocs)) stop("BLOC_TEXTUEL/CONTENU absent")
  paragraphes = unlist(lapply(blocs, function(bloc) {
    texte = gsub("<br[[:space:]]*/?>", "\n", as.character(bloc), ignore.case = TRUE)
    texte = xml2::xml_text(xml2::read_xml(texte, options = "NONET"))
    lignes = trimws(strsplit(texte, "\n", fixed = TRUE)[[1L]])
    lignes[nzchar(lignes)]
  }), use.names = FALSE)
  if (!length(paragraphes)) stop("Article sans contenu textuel")
  paragraphes
}
#' Produire un article d'un code LEGI en PDF
#'
#' @param code Intitule ou nom abrege du code (par exemple "civil").
#' @param article Numero exact de l'article (par exemple "1240").
#' @param sortie Chemin du PDF ; par defaut un fichier dans /tmp.
#' @param ouvrir Ouvrir le PDF dans zathura.
#' @param police Taille du texte principal en points.
#' @param db Connexion DuckDB facultative.
#' @return Chemin du PDF, invisiblement.
#' @export
produire_code = function(code = "civil", article = "1240", sortie = NULL,
                         ouvrir = TRUE, db = NULL, police = 10) {
  db = if (is.null(db)) connexion() else .connexion_valide(db)
  texte = .legi_code(db, code)
  lignes = DBI::dbGetQuery(db, paste0(
    "SELECT a.id, a.numero, c.contenu FROM legi.articles a ",
    "JOIN legi.articles_contenu c ON a.id = c.id AND a.fichier = c.fichier ",
    "WHERE a.id_texte = ? AND a.numero = ? AND a.etat = 'VIGUEUR'"),
    params = list(texte$id[[1]], as.character(article)))
  if (nrow(lignes) != 1L) stop("Article introuvable ou ambigu : ", article,
                              " (", nrow(lignes), " correspondances)")
  paragraphes = .legi_extraire_texte(lignes$contenu[[1]])
  contenu = data.frame(texte = paragraphes, profondeur = 0L, gras = FALSE)
  .legi_pdf(texte$titre[[1]], paste0("Article ", lignes$numero[[1]]),
            contenu, sortie, ouvrir, .legi_metadonnees(db, texte$id[[1]], lignes$id[[1]]), police = police)
}
#' Produire le sommaire officiel d'un code LEGI en PDF
#'
#' Les subdivisions sont parcourues dans l'ordre des liens LEGI. Le niveau
#' represente la profondeur dans l'arbre officiel : 0 pour les subdivisions
#' racines, 1 pour leurs enfants, etc. Les appellations (livre, titre,
#' chapitre...) sont conservees telles qu'elles figurent dans TITRE_TA.
#'
#' @param code Intitule ou nom abrege du code.
#' @param niveau Profondeur maximale (0, 1, 2, ... ou Inf).
#' @param date Date de validite des liens, par defaut aujourd'hui.
#' @param articles Inclure les numeros d'articles dans le sommaire.
#' @param sortie Chemin du PDF ; par defaut un fichier dans /tmp.
#' @param ouvrir Ouvrir le PDF dans zathura.
#' @param police Taille du texte principal en points.
#' @param db Connexion DuckDB facultative.
#' @return Chemin du PDF, invisiblement.
#' @export
sommaire_code = function(code = "civil", niveau = 3, date = Sys.Date(),
                         articles = FALSE, sortie = NULL, ouvrir = TRUE, db = NULL, police = 10) {
  if (length(niveau) != 1L || is.na(niveau) || niveau < 0 ||
      (!is.infinite(niveau) && niveau != floor(niveau))) stop("Niveau invalide")
  date = as.Date(date)
  if (length(date) != 1L || is.na(date)) stop("Date invalide")
  db = if (is.null(db)) connexion() else .connexion_valide(db)
  texte = .legi_code(db, code)
  id = texte$id[[1]]
  racines = DBI::dbGetQuery(db, paste0(
    "SELECT id_cible, type, ordre FROM legi.liens_racine ",
    "WHERE id_texte = ? AND (debut IS NULL OR debut <= ?) ",
    "AND (fin IS NULL OR fin > ?) AND (etat IS NULL OR etat = 'VIGUEUR') ",
    "ORDER BY ordre"), params = list(id, as.character(date), as.character(date)))
  liens = DBI::dbGetQuery(db, paste0(
    "SELECT id_parent, id_cible, type, ordre FROM legi.liens ",
    "WHERE id_texte = ? AND (debut IS NULL OR debut <= ?) ",
    "AND (fin IS NULL OR fin > ?) AND (etat IS NULL OR etat = 'VIGUEUR') ",
    "ORDER BY id_parent, ordre"), params = list(id, as.character(date), as.character(date)))
  sections = DBI::dbGetQuery(db, "SELECT id, titre FROM legi.sections WHERE id_texte = ?", params = list(id))
  if (!nrow(sections) || anyNA(sections$titre)) stop("Titres LEGI incomplets")
  if (anyDuplicated(sections$id)) stop("Identifiants de subdivisions ambigus")
  titres = stats::setNames(sections$titre, sections$id)
  numeros = NULL
  if (isTRUE(articles)) {
    arts = DBI::dbGetQuery(db, paste0("SELECT id, numero FROM legi.articles ",
      "WHERE id_texte = ? AND (debut IS NULL OR debut <= ?) ",
      "AND (fin IS NULL OR fin > ?) AND etat = 'VIGUEUR'"),
      params = list(id, as.character(date), as.character(date)))
    numeros = stats::setNames(arts$numero, arts$id)
  }
  enfants = split(seq_len(nrow(liens)), liens$id_parent)
  sortie_texte = character()
  sortie_profondeur = integer()
  sortie_gras = logical()
  compteur = 0L
  ajouter = function(cible, type, profondeur, ancetres) {
    if (type == "LIEN_ART") {
      if (isTRUE(articles) && cible %in% names(numeros)) {
        libelle = numeros[[cible]]
        if (!is.na(libelle) && nzchar(libelle)) {
          sortie_texte <<- c(sortie_texte, paste0("Article ", libelle))
          sortie_profondeur <<- c(sortie_profondeur, profondeur)
          sortie_gras <<- c(sortie_gras, FALSE)
          compteur <<- compteur + 1L
        }
      }
      return(invisible(NULL))
    }
    if (type != "LIEN_SECTION_TA" || !cible %in% names(titres) ||
        cible %in% ancetres) return(invisible(NULL))
    libelle = titres[[cible]]
    if (is.na(libelle) || !nzchar(libelle)) stop("Subdivision sans titre : ", cible)
    if (profondeur <= niveau) {
      sortie_texte <<- c(sortie_texte, libelle)
      sortie_profondeur <<- c(sortie_profondeur, profondeur)
      sortie_gras <<- c(sortie_gras, profondeur < 3L)
      compteur <<- compteur + 1L
    }
    if (profondeur < niveau || isTRUE(articles)) {
      positions = enfants[[cible]]
      if (length(positions)) for (j in positions)
        if (liens$type[[j]] == "LIEN_SECTION_TA" || isTRUE(articles))
          ajouter(liens$id_cible[[j]], liens$type[[j]], profondeur + 1L,
                  c(ancetres, cible))
    }
    invisible(NULL)
  }
  for (i in seq_len(nrow(racines)))
    ajouter(racines$id_cible[[i]], racines$type[[i]], 0L, character())
  if (!compteur) stop("Aucune entree de sommaire pour ", texte$titre[[1]])
  contenu = data.frame(texte = sortie_texte, profondeur = sortie_profondeur,
                        gras = sortie_gras, stringsAsFactors = FALSE)
  .legi_pdf(paste0("Sommaire ", texte$titre[[1]]), paste0("Niveau ", niveau),
            contenu, sortie, ouvrir, .legi_metadonnees(db, id), police = police)
}
