#' Produire un code LEGI complet en PDF
#'
#' @param code Nom simplifie du code (voir rech_code()).
#' @param date Date de validite des articles.
#' @param sortie Chemin du PDF ; par defaut /tmp/code_<nom_simplifie>.pdf.
#' @param ouvrir Ouvrir le document dans zathura.
#' @param db Connexion DuckDB facultative.
#' @param police Taille du texte principal en points.
#' @param logo Entier de 1 a 6 (defaut : 1).
#' @return Chemin du PDF, invisiblement.
#' @export
produire_code_complet = function(code = "civil", date = Sys.Date(),
                                  sortie = NULL, ouvrir = TRUE,
                                  db = NULL, police = 9, logo = 1L) {
  image_logo = .logo_code(logo)
  date = as.Date(date)
  if (length(date) != 1L || is.na(date)) stop("Date invalide")
  if (length(police) != 1L || !is.finite(police) || police <= 0) stop("Police invalide")
  db = if (is.null(db)) connexion() else .connexion_valide(db)
  texte = .legi_code(db, code)
  if (is.null(sortie)) sortie = file.path("/tmp", paste0("code_", texte$nom_simplifie[[1L]], ".pdf"))
  id = texte$id[[1L]]
  titre = texte$nom_complet[[1L]]
  sommaire = sommaire_code(code = code, niveau = 1, date = date,
                           articles = FALSE, db = db, format = "donnees")
  # Les sections historiques restent necessaires pour atteindre certains
  # articles en vigueur. Seuls les liens vers les articles sont dates.
  racines = DBI::dbGetQuery(db, paste0(
    "SELECT id_cible, type, ordre FROM legi.liens_racine ",
    "WHERE id_texte = ? AND (debut IS NULL OR debut <= ?) ",
    "AND (fin IS NULL OR fin > ?) AND (etat IS NULL OR etat = 'VIGUEUR') ",
    "ORDER BY ordre, id_cible"),
    params = list(id, as.character(date), as.character(date)))
  liens = DBI::dbGetQuery(db, paste0(
    "SELECT id_parent, id_cible, type, ordre FROM legi.liens ",
    "WHERE id_texte = ? AND (type = 'LIEN_SECTION_TA' OR ",
    "(type = 'LIEN_ART' AND (debut IS NULL OR debut <= ?) ",
    "AND (fin IS NULL OR fin > ?) AND (etat IS NULL OR etat = 'VIGUEUR'))) ",
    "ORDER BY id_parent, ordre, id_cible"),
    params = list(id, as.character(date), as.character(date)))
  articles = DBI::dbGetQuery(db, paste0(
    "SELECT a.id, a.numero, c.contenu FROM legi.articles a ",
    "JOIN legi.articles_contenu c ON c.id = a.id AND c.fichier = a.fichier ",
    "WHERE a.id_texte = ? AND a.etat = 'VIGUEUR' AND a.debut <= ? AND a.fin > ? ",
    "ORDER BY a.id"),
    params = list(id, as.character(date), as.character(date)))
  if (!nrow(articles)) stop("Aucun article en vigueur")
  if (anyDuplicated(articles$id)) stop("Contenu LEGI duplique pour certains articles")
  enfants = split(seq_len(nrow(liens)), liens$id_parent)
  ordre_articles = character()
  vus = new.env(parent = emptyenv(), hash = TRUE)
  parcourir = function(cible, type, ancetres = character()) {
    if (type == "LIEN_ART") {
      if (cible %in% articles$id && !exists(cible, envir = vus, inherits = FALSE)) {
        assign(cible, TRUE, envir = vus)
        ordre_articles <<- c(ordre_articles, cible)
      }
      return(invisible(NULL))
    }
    if (type != "LIEN_SECTION_TA")
      stop("Type de lien LEGI inattendu : ", type)
    if (cible %in% ancetres) stop("Cycle dans la structure LEGI : ", cible)
    positions = enfants[[cible]]
    if (length(positions)) for (j in positions)
      parcourir(liens$id_cible[[j]], liens$type[[j]], c(ancetres, cible))
    invisible(NULL)
  }
  for (i in seq_len(nrow(racines)))
    parcourir(racines$id_cible[[i]], racines$type[[i]])
  if (!length(ordre_articles)) stop("Aucun article dans la structure LEGI du code")
  orphelins = setdiff(articles$id, ordre_articles)
  if (length(orphelins))
    stop("Structure LEGI incomplete : ", length(orphelins),
         " articles en vigueur inaccessibles : ", paste(orphelins, collapse = ", "))
  articles = articles[match(ordre_articles, articles$id), , drop = FALSE]
  pages = list()
  page = list()
  y = 26.9
  nouveau = function() {
    pages[[length(pages) + 1L]] <<- page
    page <<- list()
    y <<- 26.9
  }
  ajouter = function(texte, gras = FALSE, retrait = 0, espace = .13) {
    if (is.na(texte) || !nzchar(trimws(texte))) return(invisible(NULL))
    largeur = max(28L, floor((101 - 5 * retrait) * 9 / police))
    lignes = strwrap(texte, width = largeur)
    if (!length(lignes)) return(invisible(NULL))
    if (gras && y - length(lignes) * .43 < 2.5) nouveau()
    for (ligne in lignes) {
      if (y < 2.25) nouveau()
      page[[length(page) + 1L]] <<- list(texte = ligne, y = y, gras = gras, retrait = retrait)
      y <<- y - .43 * police / 9
    }
    y <<- y - espace
    invisible(NULL)
  }
  ajouter("Sommaire", gras = TRUE, espace = .35)
  for (i in seq_len(nrow(sommaire)))
    ajouter(sommaire$texte[[i]], gras = isTRUE(sommaire$gras[[i]]),
            retrait = sommaire$profondeur[[i]], espace = .18)
  nouveau()
  for (i in seq_len(nrow(articles))) {
    numero = articles$numero[[i]]
    if (is.na(numero) || !nzchar(numero)) numero = "sans numero"
    ajouter(paste("Article", numero), gras = TRUE, espace = .13)
    paragraphes = .legi_extraire_texte(articles$contenu[[i]])
    for (paragraphe in paragraphes) ajouter(paragraphe, espace = .17)
    y = y - .26
  }
  pages[[length(pages) + 1L]] = page
  total = length(pages)
  meta = paste0("Source : LEGI / Legifrance  |  ", titre, "  |  ", .legi_version_installee(db))
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  grDevices::cairo_pdf(sortie, width = 8.27, height = 11.69, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (p in seq_along(pages)) {
    grid::grid.newpage()
    grid::grid.text(titre, x = grid::unit(2, "cm"), y = grid::unit(28.5, "cm"),
                    just = "left", gp = grid::gpar(fontsize = 10, fontface = "bold"))
    grid::grid.raster(image_logo, x = grid::unit(19, "cm"),
                      y = grid::unit(28.5, "cm"),
                      width = grid::unit(.85, "cm"),
                      height = grid::unit(.88, "cm"), just = "right", interpolate = TRUE)
    grid::grid.lines(x = grid::unit(c(2, 19), "cm"), y = grid::unit(c(27.85, 27.85), "cm"),
                     gp = grid::gpar(lwd = .7))
    for (ligne in pages[[p]])
      grid::grid.text(ligne$texte, x = grid::unit(2 + .45 * ligne$retrait, "cm"),
                      y = grid::unit(ligne$y, "cm"), just = "left",
                      gp = grid::gpar(fontsize = police,
                                      fontface = if (ligne$gras) "bold" else "plain"))
    grid::grid.lines(x = grid::unit(c(2, 19), "cm"), y = grid::unit(c(1.8, 1.8), "cm"),
                     gp = grid::gpar(lwd = .7))
    grid::grid.text(meta, x = grid::unit(2, "cm"), y = grid::unit(1.4, "cm"),
                    just = "left", gp = grid::gpar(fontsize = 7, col = "#666666"))
    grid::grid.text(sprintf("%d/%d", p, total), x = grid::unit(19, "cm"),
                    y = grid::unit(1.4, "cm"), just = "right",
                    gp = grid::gpar(fontsize = 8))
  }
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)
  message("Code : ", titre, " | Articles : ", nrow(articles), " | Pages : ", total,
          " | PDF : ", sortie)
  if (isTRUE(ouvrir) && nzchar(Sys.which("zathura")))
    system2("zathura", shQuote(sortie), wait = FALSE)
  invisible(normalizePath(sortie, mustWork = TRUE))
}
