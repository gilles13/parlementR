#' Lister les themes officiels des questions du Senat
#'
#' Les effectifs comptent chaque question dans chacun de ses themes.
#' Une question peut avoir plusieurs themes ; les questions codees `#`
#' ne sont rattachees a aucun theme. Classification non exhaustive.
#' @param db Connexion DuckDB locale.
#' @return Tibble avec identifiant, libelle et nombre de questions par theme.
#' @export
lister_themes = function(db = NULL) {
  db = .connexion_valide(db)
  tibble::as_tibble(DBI::dbGetQuery(db, paste0(
    "SELECT t.thenouidt AS code, t.thelib AS theme, count(q.id) AS questions ",
    "FROM sen_questions.the t LEFT JOIN sen_questions.tam_questions q ",
    "ON contains(q.themes, '#' || t.thenouidt || '#') ",
    "GROUP BY t.thenouidt, t.thelib ORDER BY questions DESC, theme")))
}

# Resolution du code de theme dans la nomenclature officielle.
.questions_code_theme = function(db, theme) {
  if (is.null(theme)) return(NULL)
  if (!is.character(theme) || length(theme) != 1L || is.na(theme) ||
      !nzchar(trimws(theme))) stop("'theme' doit etre un code ou libelle non vide")
  theme = trimws(theme)
  candidats = tibble::as_tibble(DBI::dbGetQuery(db,
    "SELECT thenouidt, thelib FROM sen_questions.the WHERE thenouidt = ? OR lower(thelib) = lower(?)",
    params = list(theme, theme)))
  if (nrow(candidats) != 1L)
    stop("Theme inconnu ou ambigu : ", theme, ". Voir lister_themes().")
  candidats$thenouidt[[1L]]
}

#' Rechercher les questions du Senat dans DuckDB local
#'
#' La recherche porte sur le titre, la rubrique et le texte de la question.
#' Les questions sans reponse sont conservees.
#' @param sujet Expression recherchee ; NULL pour filtrer uniquement par theme.
#' @param db Connexion DuckDB.
#' @param limite Nombre maximal de resultats.
#' @param depuis Date minimale de publication, au format YYYY-MM-DD.
#' @param jusque Date maximale de publication, au format YYYY-MM-DD.
#' @param theme Code `thenouidt` ou libelle officiel de `lister_themes()`.
#'   Plusieurs themes peuvent etre associes a une question.
#' @return Un tibble, une ligne par question.
#' @export
rechercher_questions = function(sujet = NULL, db = NULL, limite = 100L,
                                depuis = NULL, jusque = NULL, theme = NULL) {
  db = .connexion_valide(db)
  if (is.null(sujet) && is.null(theme)) stop("Indiquer un sujet ou un theme")
  if (!is.null(sujet) && (!is.character(sujet) || length(sujet) != 1L ||
      is.na(sujet) || !nzchar(trimws(sujet))))
    stop("'sujet' doit etre une expression non vide")
  code_theme = .questions_code_theme(db, theme)
  if (length(limite) != 1L || is.na(limite) || !is.finite(limite) ||
      limite < 1 || limite != as.integer(limite)) stop("'limite' doit etre positif")
  for (d in list(depuis, jusque))
    if (!is.null(d) && (length(d) != 1L || is.na(d) ||
                        !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", d)))
      stop("Dates attendues au format YYYY-MM-DD")
  motif = if (is.null(sujet)) NULL else paste0("%", trimws(sujet), "%")
  sql = paste0("SELECT q.id, q.reference, q.numero, q.titre, ",
    "substr(q.datejodepot, 1, 10) AS date, q.nom, q.prenom, ",
    "q.rubrique, q.mindepotlib AS ministere, ",
    "e.etaquelib AS etat, n.natquelib AS nature, ",
    "EXISTS (SELECT 1 FROM sen_questions.tam_reponses r ",
    "WHERE r.idque = q.id) AS reponse ",
    "FROM sen_questions.tam_questions q ",
    "LEFT JOIN sen_questions.etatquestion e ON e.etaquecod = q.etaquecod ",
    "LEFT JOIN sen_questions.naturequestion n ON n.natquecod = q.natquecod ",
    "WHERE q.natquecod = 'QE' ")
  params = list()
  if (!is.null(motif)) {
    sql = paste0(sql, " AND (q.titre ILIKE ? OR q.rubrique ILIKE ? OR q.txtque ILIKE ?)")
    params = c(params, list(motif, motif, motif))
  }
  if (!is.null(code_theme)) {
    sql = paste0(sql, " AND contains(q.themes, ?)")
    params = c(params, list(paste0("#", code_theme, "#")))
  }
  if (!is.null(depuis)) {
    sql = paste0(sql, " AND substr(q.datejodepot, 1, 10) >= ?")
    params = c(params, list(depuis))
  }
  if (!is.null(jusque)) {
    sql = paste0(sql, " AND substr(q.datejodepot, 1, 10) <= ?")
    params = c(params, list(jusque))
  }
  sql = paste0(sql, " ORDER BY q.datejodepot DESC NULLS LAST, q.id LIMIT ?")
  tibble::as_tibble(DBI::dbGetQuery(db, sql,
                                   params = c(params, list(as.integer(limite)))))
}

#' Consulter une question et ses reponses
#' @param id Identifiant interne officiel de la question.
#' @param db Connexion DuckDB.
#' @return Liste de deux tibbles : question et reponses.
#' @export
consulter_question = function(id, db = NULL) {
  db = .connexion_valide(db)
  if (length(id) != 1L || is.na(id) || !nzchar(as.character(id)))
    stop("Identifiant invalide")
  question = tibble::as_tibble(DBI::dbGetQuery(db,
    "SELECT * FROM sen_questions.tam_questions WHERE id = ?",
    params = list(as.character(id))))
  if (!nrow(question)) stop("Question introuvable : ", id)
  reponses = tibble::as_tibble(DBI::dbGetQuery(db,
    "SELECT * FROM sen_questions.tam_reponses WHERE idque = ? ORDER BY datejorep",
    params = list(as.character(id))))
  list(question = question, reponses = reponses)
}
