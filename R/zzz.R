# Jeux proposes par actualiser() et par le diagnostic au chargement.
.jeux_actualisables = c(
  "deputes", "historique", "scrutins", "dossiers", "amendements",
  "senateurs", "dosleg", "dole", "questions_senat", "questions_an",
  "debats", "reunions", "ameli", "rne", "legi", "photos_an",
  "photos_senat"
)

# Lecture seule : le chargement du package ne cree ni base ni table.
.etat_initial = function() {
  chemin = file.path(tools::R_user_dir("parlementR", "data"),
                     "parlementR.duckdb")
  if (!file.exists(chemin)) return(list(premier = TRUE))

  # Reutiliser la connexion centrale : devtools::load_all() peut etre appele
  # alors qu'une instance DuckDB en lecture-ecriture existe deja.
  db = connexion(chemin = chemin)

  tables = DBI::dbGetQuery(db, paste(
    "SELECT table_schema, table_name FROM information_schema.tables",
    "WHERE table_type = 'BASE TABLE'",
    "AND table_schema NOT IN ('information_schema', 'pg_catalog')"
  ))
  suivi_existe = any(tables$table_schema == "suivi" &
                     tables$table_name == "actualisations")
  autres_tables = any(tables$table_schema != "suivi")
  if (!autres_tables && !suivi_existe)
    return(list(premier = TRUE))
  if (!suivi_existe)
    return(list(premier = FALSE, suivi = NULL))
  colonnes = DBI::dbListFields(db, DBI::Id(schema = "suivi",
                                          table = "actualisations"))
  controle = if ("date_controle" %in% colonnes) {
    "date_controle"
  } else {
    "NULL AS date_controle"
  }
  suivi = DBI::dbGetQuery(db, paste0(
    "SELECT jeu, date_import, ", controle,
    " FROM suivi.actualisations ORDER BY jeu"
  ))
  list(premier = FALSE, suivi = suivi)
}

# Affichage au demarrage, sans modification de la base.
.message_demarrage = function() {
  etat = .etat_initial()
  if (isTRUE(etat$premier)) {
    packageStartupMessage("Bienvenue dans parlementR !")
    packageStartupMessage("Aucune donnee parlementaire locale n'a encore ete importee.")
    packageStartupMessage("Documentation : help(package = \"parlementR\")")
    packageStartupMessage("Cheatsheet : produire_cheatsheet()")
    packageStartupMessage("Premier import : actualiser(jeux = \"deputes\")")
    return(invisible(NULL))
  }

  suivi = etat$suivi
  if (is.null(suivi) || !nrow(suivi)) return(invisible(NULL))

  retard = character()
  for (jeu in .jeux_actualisables) {
    ligne = suivi[!is.na(suivi$jeu) & suivi$jeu == jeu, , drop = FALSE]
    if (!nrow(ligne)) next

    import = as.POSIXct(ligne$date_import, tz = "UTC")
    controle = as.POSIXct(ligne$date_controle, tz = "UTC")
    dernier = ifelse(is.na(controle), as.numeric(import), as.numeric(controle))
    dernier = dernier[!is.na(dernier)]
    if (!length(dernier)) next

    delai = if (jeu %in% c("photos_an", "photos_senat")) 30 else 7
    if (as.numeric(difftime(Sys.time(),
                            as.POSIXct(max(dernier), origin = "1970-01-01", tz = "UTC"),
                            units = "days")) >= delai) {
      retard = c(retard, jeu)
    }
  }

  if (length(retard)) {
    packageStartupMessage("parlementR : ", length(retard),
                          " jeu(x) a actualiser (7 jours ; photos : 30 jours) :")
    largeur = max(nchar(retard))
    for (jeu in retard) {
      packageStartupMessage("  ", format(jeu, width = largeur, justify = "left"),
                            "  actualiser(jeux = \"", jeu, "\")")
    }
  }
  invisible(NULL)
}

.onAttach = function(libname, pkgname) {
  if (!interactive()) return(invisible(NULL))
  tryCatch(.message_demarrage(), error = function(e) {
    packageStartupMessage("parlementR : suivi local indisponible (",
                          conditionMessage(e), ").")
  })
  invisible(NULL)
}
