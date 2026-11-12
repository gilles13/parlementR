#' Interface locale des questions du Sénat
#'
#' Recherche par thème officiel et mot-clé, puis affiche la question et toutes
#' ses réponses. Les données sont consultées uniquement dans DuckDB local.
#' Les packages `shiny` et `DT` sont facultatifs et ne sont jamais installés
#' automatiquement.
#'
#' @param db Connexion DuckDB locale ouverte.
#' @return Lance une application Shiny dans la session R.
#' @export
shiny_senat_question = function(db = NULL) {
  requis = c("shiny", "DT")
  manquants = requis[!vapply(requis, requireNamespace, logical(1), quietly = TRUE)]
  if (length(manquants)) stop(
    "L'interface des questions du S\u00e9nat n\u00e9cessite les packages facultatifs : ",
    paste(manquants, collapse = ", "), ".\n",
    "Pour les installer : install.packages(c('shiny', 'DT')).\n",
    "Si vous ne souhaitez pas les installer, vous ne pourrez pas utiliser ",
    "cette interface ; les autres fonctions de parlementR restent disponibles.",
    call. = FALSE)
  db = .connexion_valide(db)
  DBI::dbExecute(db, "SET enable_progress_bar = false")
  themes = lister_themes(db = db)
  choix = c("Choisir un th\u00e8me" = "", stats::setNames(
    themes$theme, paste0(themes$theme, " (", themes$questions, ")")))
  afficher_texte = function(x) {
    if (!length(x) || is.na(x[[1L]]) || !nzchar(trimws(x[[1L]])))
      return(shiny::tags$em("Texte non disponible dans la source officielle"))
    shiny::tags$p(style = "white-space: pre-wrap;", x[[1L]])
  }
  ui = shiny::fluidPage(
    shiny::titlePanel("parlementR \u2014 Questions du S\u00e9nat"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("theme", "Th\u00e8me", choices = choix),
        shiny::textInput("texte", "Recherche", placeholder = "Mot-cl\u00e9 facultatif"),
        shiny::numericInput("limite", "Nombre de r\u00e9sultats", value = 100,
                            min = 5, max = 1000),
        shiny::actionButton("chercher", "Rechercher", class = "btn-primary")
      ),
      shiny::mainPanel(DT::DTOutput("questions"), shiny::hr(), shiny::uiOutput("detail"))
    )
  )
  server = function(input, output, session) {
    resultats = shiny::eventReactive(input$chercher, {
      sujet = trimws(input$texte)
      shiny::req(nzchar(sujet) || nzchar(input$theme))
      rechercher_questions(
        sujet = if (nzchar(sujet)) sujet else NULL,
        theme = if (nzchar(input$theme)) input$theme else NULL,
        limite = input$limite, db = db)
    })
    output$questions = DT::renderDT({
      shiny::validate(shiny::need(input$chercher > 0,
        "Choisissez un th\u00e8me ou saisissez un mot-cl\u00e9, puis cliquez sur Rechercher."))
      x = resultats()
      shiny::validate(shiny::need(nrow(x) > 0, "Aucune question trouv\u00e9e"))
      DT::datatable(x[, c("date", "reference", "titre", "nom", "rubrique", "etat")],
        rownames = FALSE, selection = "single",
        options = list(pageLength = 5, lengthMenu = c(5, 10, 25, 50),
                       scrollX = TRUE))
    })
    question = shiny::reactive({
      i = input$questions_rows_selected
      shiny::req(length(i) == 1L)
      x = resultats()
      shiny::req(i >= 1L, i <= nrow(x))
      consulter_question(x$id[[i]], db = db)
    })
    output$detail = shiny::renderUI({
      q = question()
      fiche = q$question
      reponses = q$reponses
      shiny::req(nrow(fiche) > 0)
      shiny::tagList(
        shiny::h3(fiche$titre[[1L]]),
        shiny::p(shiny::strong("R\u00e9f\u00e9rence : "), fiche$reference[[1L]]),
        shiny::h4("Question"), afficher_texte(fiche$txtque),
        shiny::hr(), shiny::h4("R\u00e9ponses"),
        if (!nrow(reponses)) shiny::tags$em("Aucune r\u00e9ponse enregistr\u00e9e")
        else lapply(seq_len(nrow(reponses)), function(i) shiny::tagList(
          shiny::h5(paste("R\u00e9ponse du", reponses$datejorep[[i]])),
          afficher_texte(reponses$txtrep[i]), shiny::hr()))
      )
    })
  }
  shiny::shinyApp(ui, server)
}
