#' Produire la cheatsheet avec Grid et Cairo
#'
#' Genere un guide PDF A4 paysage avec grid et Cairo.
#' Le mode simple tient sur une page ; le mode complet en contient cinq.
#' Les applications Shiny sont identifiees par des blocs jaunes.
#' @param simple Produire une fiche essentielle sur une page (TRUE par defaut).
#' @param sortie Chemin du fichier PDF produit.
#' @param ouvrir Ouvrir le PDF apres generation.
#' @return Chemin du PDF, invisiblement.
#' @export
produire_cheatsheet = function(simple = TRUE, sortie = "/tmp/parlementR-cheatsheet.pdf", ouvrir = TRUE) {
  stopifnot(is.logical(simple), length(simple) == 1L, !is.na(simple))
  if (!capabilities('cairo'))
    stop('Le support Cairo est necessaire pour produire la cheatsheet.', call. = FALSE)
  sortie = normalizePath(sortie, mustWork = FALSE)
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  largeur = 86
  police = 8.8
  police_code = 7.8
  interligne = 4.0
  B = function(titre, texte = '', code = '', classe = '') list(type='block', titre=titre, texte=texte, code=code, classe=classe)
  S = function(titre, classe = 'blue') list(type='step', titre=titre, texte='', code='', classe=classe)
  hauteur_bloc = function(b) {
    if (b$type == 'step') return(13)
    n = length(strwrap(b$texte, width=49))
    nc = if (nzchar(b$code)) length(strsplit(b$code, '\n', fixed=TRUE)[[1]]) else 0
    14 + n * interligne + nc * 4.1 + if (nc) 5 else 0
  }
  colonnes = list(
    list(S('0 \u00B7 Le n\u00E9cessaire','blue'),
         B('R : installation','R 4.1 ou plus. Installer R depuis le CRAN, puis ouvrir une session R. Sous Linux, privil\u00E9gier les d\u00E9p\u00F4ts de la distribution.','https://cran.r-project.org/','system'),
         B('Packages R obligatoires','Install\u00E9s automatiquement avec parlementR : DBI, RPostgres, duckdb, png, grid, tibble, jsonlite, xml2, dplyr et ggplot2. grid est livr\u00E9 avec R.','','rdep'),
         B('Packages R facultatifs','Applications : shiny, DT. Analyse de texte et graphiques : tidytext, stopwords, ggwordcloud, viridis. Documentation : knitr, rmarkdown, pkgdown.','install.packages(c("shiny", "DT",\n  "tidytext", "stopwords",\n  "ggwordcloud", "viridis"))','rdep')),
    list(S('Linux \u00B7 outils syst\u00E8me','blue'),
         B('Installation : principe','Utiliser le gestionnaire de paquets de la distribution. Exemple Arch Linux : pacman ; Debian/Ubuntu : apt. Les noms de paquets varient.','sudo pacman -S curl unzip tar gzip\nsudo apt install curl unzip tar gzip','system'),
         B('Selon les jeux de donn\u00E9es','LEGI : bash, tar, gzip et \u00E9ventuellement pigz. AMELI : PostgreSQL avec initdb, pg_ctl et psql. Ne pas installer PostgreSQL pour un simple import de d\u00E9put\u00E9s.','','system'),
         B('Outils et t\u00E9l\u00E9chargements','PostgreSQL : https://www.postgresql.org/download/ ; DuckDB est install\u00E9 comme package R.','https://www.postgresql.org/download/','system')),
    list(S('Windows \u00B7 outils syst\u00E8me','blue'),
         B('Installation : principe','Installer R depuis le CRAN et les outils de compilation Rtools si n\u00E9cessaires. Installer les logiciels externes requis par les jeux utilis\u00E9s ; rendre leurs ex\u00E9cutables accessibles dans PATH.','https://cran.r-project.org/bin/windows/\nhttps://cran.r-project.org/bin/windows/Rtools/','system'),
         B('Archives et PostgreSQL','Pr\u00E9voir curl, tar et un outil de d\u00E9compression utilisables depuis R. Pour AMELI, installer PostgreSQL avec ses utilitaires de ligne de commande.','https://www.postgresql.org/download/windows/','system'),
         B('V\u00E9rifier les ex\u00E9cutables','Dans R, Sys.which() permet de voir ce qui est accessible. Les cha\u00EEnes vides signalent des ex\u00E9cutables absents du PATH.','Sys.which(c("curl", "tar", "psql",\n  "initdb", "pg_ctl"))','system')),
    list(S('1 \u00B7 D\u00E9marrer','blue'),
         B('Installer et connecter','Installer depuis GitHub avec remotes, puis ouvrir la base DuckDB locale.','install.packages("remotes")\nremotes::install_github("gilles13/parlementR")\nlibrary(parlementR)',''),
         B('Actualiser les donn\u00E9es','T\u00E9l\u00E9charger les jeux n\u00E9cessaires, sans r\u00E9import inutile.','actualiser(jeux = "deputes")',''),
         B('D\u00E9put\u00E9s et portraits','Catalogue, fiche, rapport et photos officielles.','actualiser(jeux = "photos_an")\ncat_deputes()\nfiche_depute("Dupont")\nrapport_depute("Dupont")','')),
    list(S('D\u00E9put\u00E9s et gouvernement','blue'),
         B('Rechercher un parlementaire','Recherche dans les deux chambres.','rechercher_parlementaires("Dupont")',''),
         B('Gouvernements','Catalogues, ministres, missions et rapport.','cat_gvts()\ncat_gvt("Bayrou")\ncat_missions_gvt()\nrapport_gvt(...)',''),
         B('Visualiser un h\u00E9micycle','Repr\u00E9sentation graphique des votes.','graphique_hemicycle(donnees)','')),
    list(S('Explorer les donn\u00E9es','blue'),
         B('Application Shiny : explorer','Exploration interactive des donn\u00E9es import\u00E9es.','explorer()','shiny'),
         B('Conventions de l\'API','actualiser importe ; cat_ liste ; fiche_ consulte ; rech_ recherche ; rapport_ produit.','',''),
         B('Architecture locale','Le r\u00E9seau sert \u00E0 actualiser ; les recherches et rapports utilisent DuckDB en local.','','')),
    list(S('2 \u00B7 Assembl\u00E9e nationale','teal'),
         B('Scrutins','Rapport d\'un scrutin et h\u00E9micycle.','actualiser(jeux = "scrutins")\nrapport_scrutin_an("9336")',''),
         B('Dossiers l\u00E9gislatifs','Catalogue, recherche, rapport de dossier.','actualiser(jeux = "dossiers")\ncat_dosleg_an()\nrech_dosleg_an("logement")\nrapport_legislatif_an(uid)',''),
         B('Rapport l\u00E9gislatif','Parcours du dossier identifie.','rapport_legislatif_an(ref = "UID_DOSSIER")','')),
    list(S('Commissions et d\u00E9bats','teal'),
         B('Trouver des commissions','Rechercher des r\u00E9unions par sujet ou intervenant.','rech_comm_an(...)',''),
         B('Comptes rendus','URL, lecture et rapport PDF ; le rapport re\u00E7oit un objet compte rendu.','url_compte_rendu_an(ref)\ncr = lire_compte_rendu_an(ref)\nrapport_comm_an(...)',''),
         B('Analyse de texte','Extraction de mots et nuage de mots.','m = mots_compte_rendu_an(cr)\nnuage_compte_rendu_an(m)','')),
    list(S('Amendements et questions','teal'),
         B('Amendements AN','Recherche et rapport \u00E0 partir des donn\u00E9es retourn\u00E9es.','actualiser(jeux = "amendements")\na = rech_amdt_an(sujet = "fiscalit\u00E9")\nrapport_amdt_an(a)',''),
         B('Relier aux scrutins','Appariement des amendements et votes.','relier_amdt_scrutins(a)',''),
         B('Questions AN','Importer les questions \u00E9crites, orales et au gvt.','actualiser(jeux = "questions_an")',''),
         B('Shiny \u00B7 Questions','Recherche interactive de questions.','explorer("questions_floues")','shiny')),
    list(S('3 \u00B7 S\u00E9nat','coral'),
         B('S\u00E9nateurs et portraits','Catalogue, fiche, rapport et photos officielles.','actualiser(jeux = "senateurs")\nactualiser(jeux = "photos_senat")\ncat_senateurs()\nfiche_senateur("Dupont")\nrapport_senateur("Dupont")',''),
         B('Questions du S\u00E9nat','Catalogue, recherche, fiche et rapport.','actualiser(jeux = "questions_senat")\ncat_quest_senat()\nrech_quest_senat(sujet = "logement")\nfiche_quest_senat(id)\nrapport_quest_senat(id)','')),
    list(S('S\u00E9nat \u00B7 approfondir','coral'),
         B('Shiny \u00B7 Questions S\u00E9nat','Explorer les questions s\u00E9natoriales.','explorer("questions")','shiny'),
         B('Th\u00E8mes et amendements','Recherche par th\u00E8mes et amendements du S\u00E9nat.','rech_themes_senat()\nrech_amdt_senat("logement")\nrapport_amdt_senat(references)',''),
         B('Dossiers l\u00E9gislatifs S\u00E9nat','Importer les dossiers du S\u00E9nat.','actualiser(jeux = "dosleg")','')),
    list(S('Recherche documentaire','coral'),
         B('Recherche transversale','Recherche et production d\'un rapport documentaire.','docs = rech_docs("logement")\nrapport_docs(references)',''),
         B('DILA \u00B7 DOLE','Dossiers l\u00E9gislatifs diffus\u00E9s par la DILA.','actualiser(jeux = "dole")',''),
         B('Autres sources S\u00E9nat','Amendements et jeux sp\u00E9cialis\u00E9s.','actualiser(jeux = "ameli")','')),
    list(S('4 \u00B7 Sources avanc\u00E9es','gold'),
         B('R\u00E9unions et d\u00E9bats','Actualiser les sources AN compl\u00E9mentaires.','actualiser(jeux = c("reunions", "debats"))',''),
         B('Un seul point d\u2019entr\u00E9e','Utiliser actualiser() pour importer les jeux, sans appeler directement les importeurs sp\u00E9cialis\u00E9s.','actualiser(jeux = c("dole", "dosleg"))','')),
    list(S('LEGI \u00B7 Codes','gold'),
         B('Actualiser LEGI','T\u00E9l\u00E9chargement et import des donn\u00E9es DILA.','actualiser(jeux = "legi")',''),
         B('Parcourir les codes','Sommaire et production d\'un article.','sommaire_code("civil")\nproduire_code("civil", "1240")',''),
         B('Shiny \u00B7 Consulter les codes','Exploration interactive des textes et articles.','explorer("codes")','shiny'),
         B('Code complet en PDF','Exporter un code dans sa version en vigueur.','produire_code_complet("civil")','')),
    list(S('Outils et documentation','gold'),
         B('Produire la cheatsheet','Guide PDF produit directement avec grid et Cairo.','produire_cheatsheet(simple = FALSE)',''),
         B('Connexion locale','Connexion DuckDB partag\u00E9e et fermeture explicite.','connexion(); fermer_base()',''),

         B('Frise historique et aide','Produire une frise et consulter la documentation.','produire_frise(debut = 5, ids = character())\nhelp(package = "parlementR")',''))
  )
  if (!simple) {
    hauteurs = vapply(colonnes, function(col) sum(vapply(col, hauteur_bloc, numeric(1)) + 4), numeric(1))
    if (any(hauteurs > 153)) warning('Colonnes depassant 153 mm : ', paste(which(hauteurs > 153), collapse=', '))
  }
  W = 297; H = 210
  image_logo = .logo_parlementR()
  dessiner_logo = function() {
    grid::grid.raster(image_logo, x = grid::unit(279, "mm"),
      y = grid::unit(H - 15, "mm"), height = grid::unit(18, "mm"),
      width = grid::unit(18 * dim(image_logo)[2L] / dim(image_logo)[1L], "mm"))
  }
  rect = function(x, y, w, h, fill, bord = NA, rayon = 0) grid::grid.roundrect(x = grid::unit(x, 'mm'), y = grid::unit(H-y-h, 'mm'), width = grid::unit(w, 'mm'), height = grid::unit(h, 'mm'), r = grid::unit(rayon, 'mm'), just = c('left','bottom'), gp = grid::gpar(fill = fill, col = bord, lwd = .7))
  trait = function(x1, y1, x2, y2, couleur = '#d8e2e8') grid::grid.lines(x = grid::unit(c(x1,x2), 'mm'), y = grid::unit(H-c(y1,y2), 'mm'), gp = grid::gpar(col = couleur, lwd = .8))
  texte = function(s, x, y, taille = police, couleur = '#1e3444', gras = FALSE, famille = 'sans') grid::grid.text(s, x = grid::unit(x, 'mm'), y = grid::unit(H-y, 'mm'), just = c('left','top'), gp = grid::gpar(fontsize = taille, col = couleur, fontface = if (gras) 'bold' else 'plain', fontfamily = famille))
  wrap = function(s, n = 46) if (!nzchar(s)) character() else strwrap(s, width = n)
  dessiner_bloc = function(b, x, y) {
    h = hauteur_bloc(b)
    if (b$type == 'step') {
      couleur = if (grepl('blue',b$classe)) '#236c9e' else if (grepl('teal',b$classe)) '#218b81' else if (grepl('coral',b$classe)) '#c9614d' else '#ba7a23'
      rect(x,y,largeur,h,couleur,rayon=1.5); texte(b$titre,x+3,y+2.4,11.5,'white',TRUE)
    } else if (b$type == 'note') {
      rect(x,y,largeur,h,'#fff8ec',rayon=1); rect(x,y,1.5,h,'#d38a3a')
      for (k in seq_along(wrap(b$texte,45))) texte(wrap(b$texte,45)[k],x+3,y+3+(k-1)*interligne,police)
    } else {
      fond = if (identical(b$classe, 'shiny')) '#fff0a8' else if (identical(b$classe, 'system')) '#e7e2f5' else if (identical(b$classe, 'rdep')) '#e7f3ed' else '#ffffff'
      rect(x,y,largeur,h,fond,'#d8e2e8',1.5)
      texte(b$titre,x+2.5,y+2.3,10.8,if (identical(b$classe,'shiny')) '#584000' else if (identical(b$classe,'system')) '#503779' else '#1d4865',TRUE)
      yy = y+9
      for (ligne in wrap(b$texte,46)) { texte(ligne,x+2.5,yy); yy = yy+interligne }
      if (nzchar(b$code)) {
        codes = strsplit(b$code,'\n',fixed=TRUE)[[1]]
        rect(x+2,yy+1,largeur-4,length(codes)*4.4+3,if (identical(b$classe,'shiny')) '#f9df76' else '#f1f5f8',rayon=1)
        for (k in seq_along(codes)) texte(codes[k],x+4,yy+2+(k-1)*4.4,police_code,'#143c55',famille='mono')
      }
    }
    y+h+4
  }

  if (simple) {
    colonnes = list(
      list(S("1 - Demarrer"), B("Installer", "Installer le package puis le charger.", "remotes::install_github(\"gilles13/parlementR\")\nlibrary(parlementR)"), B("Base locale", "Connexion DuckDB geree par le package.", "connexion()\nfermer_base()"), B("Actualiser", "Telecharger et importer les sources utiles.", "actualiser(jeux = \"deputes\")"), B("Rechercher", "Trouver un parlementaire des deux chambres.", "rechercher_parlementaires(\"Dupont\")")),
      list(S("2 - Explorer", "teal"), B("Assemblee nationale", "Consulter les deputes et leurs rapports.", "cat_deputes()\nfiche_depute(\"Dupont\")\nrapport_depute(\"Dupont\")"), B("Senat", "Consulter les senateurs et leurs rapports.", "cat_senateurs()\nfiche_senateur(\"Dupont\")"), B("Gouvernements", "Explorer les gouvernements et les ministres.", "cat_gvts()\ncat_missions_gvt()\nrapport_gvt(...)"), B("Scrutins", "Rapport d'un scrutin de l'Assemblee.", "rapport_scrutin_an(\"9336\")")),
      list(S("3 - Analyser", "gold"), B("Dossiers legislatifs", "Rechercher un dossier et produire son rapport.", "cat_dosleg_an()\nrech_dosleg_an(\"logement\")"), B("Questions", "Rechercher les questions senatoriales.", "rech_quest_senat(sujet = \"logement\")"), B("Chronologie", "Frise historique et evenements selectionnes.", "produire_frise(debut = 5, ids = character())"), B("Explorer et documenter", "Interface interactive et aide du package.", "explorer()\nhelp(package = \"parlementR\")"))
    )
    grDevices::cairo_pdf(sortie, width = W / 25.4, height = H / 25.4, onefile = TRUE)
    on.exit(if (grDevices::dev.cur() > 1L) grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    dessiner_logo()
    texte("parlementR - Les commandes essentielles", 9, 8, 19, "#18344e", TRUE)
    texte("Guide rapide - donnees locales, recherches et rapports", 9, 20, 9, "#627789")
    trait(9, 29, 288, 29, "#226b9b")
    for (c in 1:3) {
      y = 35
      for (b in colonnes[[c]]) y = dessiner_bloc(b, 9 + (c - 1) * 93, y)
      if (y > 195) warning("Colonne ", c, " trop longue pour une page.")
    }
    trait(9, 197, 288, 197)
    texte("parlementR - Guide simplifie", 9, 200, 7.5, "#627789")
    texte("1 / 1", 271, 200, 8, "#627789")
    grDevices::dev.off()
    on.exit(NULL, add = FALSE)
    if (isTRUE(ouvrir)) utils::browseURL(sortie)
    return(invisible(sortie))
  }
    grDevices::cairo_pdf(sortie,width=W/25.4,height=H/25.4,onefile=TRUE)
    on.exit(if (grDevices::dev.cur() > 1L) grDevices::dev.off(), add = TRUE)
    for (page in 1:5) {
      grid::grid.newpage()
      dessiner_logo()
      texte('parlementR \u00B7 Guide de d\u00E9marrage',9,8,19,'#18344e',TRUE)
      texte(c('0 \u00B7 Pr\u00E9requis R et syst\u00E8me','D\u00E9couvrir \u00B7 D\u00E9put\u00E9s et premiers r\u00E9sultats','Explorer \u00B7 Sources parlementaires','\u00C9tendre \u00B7 S\u00E9nat et sources compl\u00E9mentaires','Approfondir \u00B7 DILA et LEGI')[page],9,20,9,'#627789')
      trait(9,29,288,29,'#226b9b')
      for (c in 1:3) {
        idx = (page-1)*3+c
        y = 35
        for (b in colonnes[[idx]]) y = dessiner_bloc(b,9+(c-1)*93,y)
      }
      trait(9,197,288,197)
      texte('Donn\u00E9es officielles \u00B7 analyses locales DuckDB',9,200,7.5,'#627789')
      texte(paste0(page,' / 5'),271,200,8,'#627789')
    }
    grDevices::dev.off(); on.exit(NULL, add = FALSE)
    if (isTRUE(ouvrir)) utils::browseURL(sortie)
    invisible(sortie)
}
