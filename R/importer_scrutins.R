# Import des scrutins publics AN et vues relationnelles sans copie des votes.
.importer_scrutins = function(db, archive, verbose = TRUE) {
  journal = function(...) if (isTRUE(verbose)) message("[SCRUTINS] ", ...)
  if (!file.exists(archive)) stop("Archive des scrutins introuvable : ", archive)
  if (!identical(as.integer(system2("unzip", c("-tq", shQuote(archive)),
                                   stdout = FALSE, stderr = FALSE)), 0L))
    stop("Archive des scrutins invalide")
  fichiers = system2("unzip", c("-Z1", shQuote(archive)), stdout = TRUE)
  if (!is.null(attr(fichiers, "status"))) stop("Inventaire des scrutins impossible")
  json = fichiers[grepl("\\.json$", fichiers, ignore.case = TRUE)]
  if (!length(json) || anyDuplicated(json) ||
      any(grepl("(^/|(^|/)\\.\\.(/|$))", fichiers)) ||
      any(!grepl("^json/[^/]+\\.json$", json)))
    stop("Organisation inattendue du ZIP des scrutins")
  dossier = tempfile("parlementR-scrutins-")
  dir.create(dossier)
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)
  statut = system2("unzip", c("-oq", shQuote(archive), "-d", shQuote(dossier)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(as.integer(statut), 0L) ||
      !all(file.exists(file.path(dossier, json)))) stop("Extraction des scrutins incomplete")
  DBI::dbBegin(db)
  tryCatch({
    DBI::dbExecute(db, "CREATE SCHEMA IF NOT EXISTS an_votes")
    prefixe = as.character(DBI::dbQuoteString(db,
      paste0(normalizePath(dossier, winslash = "/"), "/")))
    motif = as.character(DBI::dbQuoteString(db,
      file.path(dossier, "json", "*.json")))
    DBI::dbExecute(db, "DROP VIEW IF EXISTS an_votes.votes_deputes")
    DBI::dbExecute(db, "DROP VIEW IF EXISTS an_votes.votes_groupes")
    DBI::dbExecute(db, paste0(
      "CREATE OR REPLACE TABLE an_votes.scrutins AS SELECT ",
      "json_extract_string(content, '$.scrutin.uid') AS uid, ",
      "replace(filename, ", prefixe, ", '') AS chemin, ",
      "content::JSON AS contenu FROM read_text(", motif, ")"))
    controle = .db_get_tibble(db, paste0(
      "SELECT count(*) AS n, count(DISTINCT uid) AS distincts, ",
      "count(*) FILTER (WHERE uid IS NULL) AS absents ",
      "FROM an_votes.scrutins"))
    if (controle$n != length(json) || controle$distincts != length(json) ||
        controle$absents != 0) stop("Controle d'integrite des scrutins echoue")
    DBI::dbExecute(db, "
      CREATE OR REPLACE VIEW an_votes.votes_groupes AS
      SELECT s.uid AS scrutin_uid,
        s.contenu ->> '$.scrutin.numero' AS numero,
        s.contenu ->> '$.scrutin.dateScrutin' AS date,
        g.value ->> '$.organeRef' AS groupe_uid,
        TRY_CAST(g.value ->> '$.nombreMembresGroupe' AS INTEGER) AS membres,
        g.value ->> '$.vote.positionMajoritaire' AS position,
        TRY_CAST(g.value ->> '$.vote.decompteVoix.pour' AS INTEGER) AS pour,
        TRY_CAST(g.value ->> '$.vote.decompteVoix.contre' AS INTEGER) AS contre,
        TRY_CAST(g.value ->> '$.vote.decompteVoix.abstentions' AS INTEGER) AS abstentions,
        TRY_CAST(g.value ->> '$.vote.decompteVoix.nonVotants' AS INTEGER) AS non_votants,
        TRY_CAST(g.value ->> '$.vote.decompteVoix.nonVotantsVolontaires' AS INTEGER)
          AS non_votants_volontaires
      FROM an_votes.scrutins AS s,
        LATERAL json_each(s.contenu,
          '$.scrutin.ventilationVotes.organe.groupes.groupe') AS g
    ")
    DBI::dbExecute(db, "
      CREATE OR REPLACE VIEW an_votes.votes_deputes AS
      WITH groupes AS (
        SELECT s.uid AS scrutin_uid,
          s.contenu ->> '$.scrutin.numero' AS numero,
          s.contenu ->> '$.scrutin.dateScrutin' AS date,
          g.value ->> '$.organeRef' AS groupe_uid,
          g.value -> '$.vote.decompteNominatif' AS nominatif
        FROM an_votes.scrutins AS s,
          LATERAL json_each(s.contenu,
            '$.scrutin.ventilationVotes.organe.groupes.groupe') AS g
      ), categories AS (
        SELECT scrutin_uid, numero, date, groupe_uid,
          c.key AS categorie, c.value -> '$.votant' AS votants
        FROM groupes,
          LATERAL json_each(nominatif) AS c
        WHERE c.key IN ('pours', 'contres', 'abstentions', 'nonVotants',
                        'nonVotantsVolontaires')
      )
      SELECT scrutin_uid, numero, date, groupe_uid, categorie,
        v.value ->> '$.acteurRef' AS depute_uid,
        v.value ->> '$.mandatRef' AS mandat_uid,
        v.value ->> '$.parDelegation' AS delegation
      FROM categories,
        LATERAL json_each(CASE
          WHEN json_type(votants) = 'ARRAY' THEN votants
          WHEN json_type(votants) = 'OBJECT' THEN json_array(votants)
          ELSE '[]'::JSON END) AS v
    ")
    DBI::dbCommit(db)
  }, error = function(e) {
    DBI::dbRollback(db)
    stop(e)
  })
  journal("Scrutins importes : ", controle$n)
  invisible(controle$n)
}
