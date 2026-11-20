#' Lancer une application Shiny de parlementR
#'
#' @param application Application a lancer : parlementaires, codes, questions
#'   ou questions_floues.
#' @param db Connexion DuckDB facultative.
#' @param ... Parametres transmis a l'application selectionnee.
#' @return Lance l'application Shiny.
#' @export
explorer = function(application = c("parlementaires", "codes", "questions", "questions_floues"),
                     db = NULL, ...) {
  application = match.arg(application)
  fonction = switch(application,
    parlementaires = explorer_parlementaires,
    codes = explorer_codes,
    questions = explorer_questions,
    questions_floues = explorer_questions_floues
  )
  shiny::runApp(fonction(db = db, ...))
}
