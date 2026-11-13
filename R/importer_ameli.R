#' Importer le dump officiel AMELI du Senat
#'
#' Restaure le SQL LATIN1 dans PostgreSQL temporaire puis copie les tables
#' par lots vers DuckDB, dans une transaction unique.
#' @param db Connexion DuckDB ; NULL utilise connexion().
#' @param archive Archive ZIP officielle AMELI deja telechargee.
#' @param remplacer Autoriser le remplacement du schema existant.
#' @param taille_lot Nombre de lignes transferees par lot.
#' @param verbose Afficher les etapes.
#' @return Invisiblement, les effectifs importes.
#' @export
importer_ameli = function(db = NULL, archive, remplacer = FALSE,
                           taille_lot = 10000L, verbose = TRUE) {
  if (missing(archive) || !.zip_valide(archive)) stop("Archive AMELI absente ou invalide")
  if (!is.logical(remplacer) || length(remplacer) != 1L || is.na(remplacer))
    stop("'remplacer' doit etre TRUE ou FALSE")
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose))
    stop("'verbose' doit etre TRUE ou FALSE")
  if (length(taille_lot) != 1L || is.na(taille_lot) || !is.finite(taille_lot) ||
      taille_lot < 1 || taille_lot != as.integer(taille_lot))
    stop("'taille_lot' doit etre un entier positif")
  db = if (is.null(db)) connexion(creer = TRUE) else .connexion_valide(db)
  journal = function(...) if (isTRUE(verbose)) message(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [AMELI] ", ...)
  outils = c("initdb", "pg_ctl", "psql", "unzip")
  if (any(!nzchar(Sys.which(outils))))
    stop("Executables requis : ", paste(outils, collapse = ", "))
  dossier = tempfile("parlementR-ameli-")
  dir.create(dossier, recursive = TRUE)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  donnees = file.path(dossier, "data")
  socket = file.path(dossier, "socket")
  dir.create(socket)
  journal("Extraction de l'archive")
  if (system2("unzip", c("-oq", shQuote(archive), "-d", shQuote(dossier))) != 0L)
    stop("Echec de l'extraction AMELI")
  sql = file.path(dossier, "var", "opt", "opendata", "ameli.sql")
  if (!file.exists(sql)) stop("Fichier ameli.sql absent de l'archive")
  init = system2("initdb", c("-D", shQuote(donnees), "-A", "trust", "-E", "UTF8",
                              "--locale=C"), stdout = FALSE, stderr = FALSE)
  if (init != 0L) stop("Echec initdb")
  # Socket Unix privee : aucun port TCP public ni mot de passe necessaire.
  options_pg = paste("-c listen_addresses='' -k", shQuote(socket))
  journal("Demarrage PostgreSQL temporaire")
  if (system2("pg_ctl", c("-D", shQuote(donnees), "-l",
      shQuote(file.path(dossier, "postgres.log")), "-o", shQuote(options_pg),
      "start"), stdout = FALSE, stderr = FALSE) != 0L) stop("Echec pg_ctl start")
  on.exit(system2("pg_ctl", c("-D", shQuote(donnees), "-m", "immediate", "stop"),
                  stdout = FALSE, stderr = FALSE), add = TRUE)
  pg = DBI::dbConnect(RPostgres::Postgres(), host = socket,
                      dbname = "postgres", user = Sys.info()[["user"]])
  DBI::dbExecute(pg, "CREATE ROLE opendata NOLOGIN")
  DBI::dbExecute(pg, "CREATE ROLE postgres NOLOGIN")
  DBI::dbExecute(pg, paste0("CREATE DATABASE ameli WITH ENCODING = 'LATIN1' ",
                           "LC_COLLATE = 'C' LC_CTYPE = 'C' TEMPLATE = template0"))
  DBI::dbDisconnect(pg)
  journal("Restauration du dump PostgreSQL")
  sortie = file.path(dossier, "psql.out")
  erreurs = file.path(dossier, "psql.err")
  statut = system2("psql", c("-X", "-h", shQuote(socket), "-U",
    shQuote(Sys.info()[["user"]]), "-d", "ameli", "-v", "ON_ERROR_STOP=0",
    "-f", shQuote(sql)), stdout = sortie, stderr = erreurs,
    env = "PGCLIENTENCODING=LATIN1")
  if (statut != 0L) stop("Echec psql ; journal : ", erreurs)
  pg = DBI::dbConnect(RPostgres::Postgres(), host = socket,
                      dbname = "ameli", user = Sys.info()[["user"]])
  on.exit(try(DBI::dbDisconnect(pg), silent = TRUE), add = TRUE)
  attendues = c("amd", "amdsen", "avicom", "avigvt", "cab", "com_ameli", "ent",
    "etatxt", "fbu", "grppol_ameli", "gvt", "intora", "irr", "lec_ameli",
    "mot", "nat", "orarol", "sai", "saisen", "sea", "sen_ameli", "ses",
    "sor", "sub", "txt_ameli", "typrect", "typses", "typsub", "w_nivrec")
  presentes_pg = DBI::dbListTables(pg)
  if (!setequal(presentes_pg, attendues)) stop("Restauration AMELI incomplete : tables inattendues ou manquantes")
  # Le dump supprime initialement des contraintes absentes sur une base vierge.
  # Refuser toute erreur PostgreSQL posterieure au debut des CREATE TABLE.
  lecteur = file(sql, open = "r", encoding = "latin1")
  limite = 0L
  repeat {
    ligne = readLines(lecteur, n = 1L, warn = FALSE)
    if (!length(ligne)) stop("CREATE TABLE absent du dump")
    limite = limite + 1L
    if (grepl("^CREATE TABLE ", ligne)) break
  }
  close(lecteur)
  messages = readLines(erreurs, warn = FALSE)
  numeros = suppressWarnings(as.integer(sub("^.*ameli\\.sql:([0-9]+):.*$", "\\1", messages)))
  if (any(!is.na(numeros) & numeros >= limite))
    stop("Erreur SQL apres le debut des creations : ", paste(messages[!is.na(numeros) & numeros >= limite], collapse = " | "))
  effectifs_pg = vapply(attendues, function(nom) {
    n = DBI::dbGetQuery(pg, paste0("SELECT count(*) AS n FROM ",
      as.character(DBI::dbQuoteIdentifier(pg, nom))))$n
    as.numeric(n)
  }, numeric(1))
  if (any(effectifs_pg == 0) || effectifs_pg[["amd"]] < 1000 ||
      effectifs_pg[["amdsen"]] < 1000) stop("Effectifs AMELI incoherents")
  journal("Transfert transactionnel vers DuckDB")
  DBI::dbBegin(db)
  valide = FALSE
  on.exit(if (!valide) DBI::dbRollback(db), add = TRUE)
  existantes = .tables_schema(db, "sen_amendements")
  if (length(existantes) && !remplacer) stop("Schema sen_amendements deja occupe")
  DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS sen_amendements")
  for (nom in existantes) DBI::dbExecute(db, paste0("DROP TABLE sen_amendements.",
    as.character(DBI::dbQuoteIdentifier(db, nom))))
  effectifs = numeric(length(attendues))
  names(effectifs) = attendues
  for (nom in attendues) {
    requete = DBI::dbSendQuery(pg, paste0("SELECT * FROM ",
      as.character(DBI::dbQuoteIdentifier(pg, nom))))
    premier = TRUE
    nombre = 0
    tryCatch({
      repeat {
        lot = DBI::dbFetch(requete, n = as.integer(taille_lot))
        if (!nrow(lot)) break
        DBI::dbWriteTable(db, DBI::Id(schema = "sen_amendements", table = nom),
                          lot, append = !premier)
        premier = FALSE
        nombre = nombre + nrow(lot)
      }
    }, finally = DBI::dbClearResult(requete))
    if (nombre != effectifs_pg[[nom]]) stop("Effectif divergent : ", nom)
    effectifs[[nom]] = nombre
    journal(nom, " : ", nombre, " lignes")
  }
  DBI::dbCommit(db)
  valide = TRUE
  journal("Import termine : ", sum(effectifs), " lignes")
  invisible(data.frame(table = names(effectifs), lignes = unname(effectifs)))
}
