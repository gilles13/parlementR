#' Explorer les titres des questions au Senat par recherche floue
#'
#' Application autonome, limitee aux titres des questions ecrites (QE).
#' L'index des titres est charge une seule fois depuis DuckDB local ;
#' le texte complet n'est lu que lorsqu'une question est selectionnee.
#' La recherche accepte des caracteres non consecutifs, sans tenir compte
#' des accents ni de la casse. Aucun acces reseau n'est effectue.
#'
#' @param db Connexion DuckDB facultative ; par defaut connexion().
#' @param limite Nombre maximal de lignes affichees dans le tableau.
#' @return Une application Shiny.
#' @export
shiny_fuzzy_questions = function(db = NULL, limite = 30L) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Installer le package facultatif 'shiny' : install.packages('shiny')",
         call. = FALSE)
  db = .connexion_valide(db)
  if (length(limite) != 1L || is.na(limite) || limite < 1L ||
      limite != as.integer(limite)) stop("'limite' doit etre un entier positif")

  # Uniquement des titres et identifiants, jamais les textes complets.
  titres = DBI::dbGetQuery(db, "
    SELECT CAST(id AS VARCHAR) AS id, titre,
           substr(datejodepot, 1, 10) AS date
    FROM sen_questions.tam_questions
    WHERE natquecod = 'QE' AND titre IS NOT NULL AND trim(titre) <> ''")

  normaliser = function(x) {
    x = iconv(tolower(x), from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    gsub("[^a-z0-9]", "", x)
  }
  index = normaliser(titres$titre)

  chercher = function(texte) {
    motif = normaliser(trimws(texte))
    if (!nzchar(motif)) return(integer())
    lettres = strsplit(motif, "", fixed = TRUE)[[1L]]
    # Equivalent au filtrage subsequence de fzf : f...i...n...
    regex = paste(lettres, collapse = ".*")
    candidats = which(grepl(regex, index, perl = TRUE))
    if (!length(candidats)) return(integer())
    valeurs = index[candidats]
    # Favoriser les correspondances exactes, les prefixes et les titres courts.
    score = 100L * (valeurs == motif) +
      30L * startsWith(valeurs, motif) +
      15L * grepl(motif, valeurs, fixed = TRUE) -
      pmin(nchar(valeurs) - nchar(motif), 1000L)
    retenus = candidats[utils::head(order(-score, -xtfrm(titres$date[candidats]),
                                    na.last = TRUE), limite)]
    retenus[order(titres$date[retenus], decreasing = TRUE, na.last = TRUE)]
  }

  if (!requireNamespace("DT", quietly = TRUE))
    stop("Installer le package facultatif 'DT' : install.packages('DT')",
         call. = FALSE)

  ui = shiny::fluidPage(
    shiny::titlePanel("parlementR \u2014 Questions au S\u00e9nat"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("motif", "Rechercher dans les titres",
                         placeholder = "Ex. fin sec soc"),
        shiny::helpText("Recherche floue, sans distinction de casse ni d'accents."),
        shiny::textOutput("compteur"),
        width = 3
      ),
      shiny::mainPanel(
        shiny::h4("Questions correspondantes"),
        DT::DTOutput("resultats"),
        shiny::hr(),
        shiny::h4("Question s\u00e9lectionn\u00e9e"),
        shiny::uiOutput("detail"),
        width = 9
      )
    )
  )
  server = function(input, output, session) {
    saisie = shiny::debounce(shiny::reactive(input$motif), 250)
    choix = shiny::reactive(chercher(saisie()))
    lignes = shiny::reactive({
      ids = choix()
      data.frame(
        Identifiant = titres$id[ids],
        Date = titres$date[ids],
        Titre = titres$titre[ids],
        check.names = FALSE
      )
    })
    output$compteur = shiny::renderText({
      if (!nzchar(trimws(saisie())))
        return(paste(nrow(titres), "titres disponibles \u2014 saisir un terme"))
      paste(nrow(lignes()), "r\u00e9sultat(s) affich\u00e9(s)")
    })
    output$resultats = DT::renderDT({
      DT::datatable(
        lignes(),
        rownames = FALSE,
        selection = "single",
        options = list(
          pageLength = 5,
          autoWidth = FALSE,
          columnDefs = list(
            list(targets = 0, width = "85px", className = "dt-nowrap"),
            list(targets = 1, width = "90px", className = "dt-nowrap"),
            list(targets = 2, width = "auto")
          ),
          lengthChange = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollX = FALSE,
          language = list(
            emptyTable = "Aucun titre correspondant",
            info = "_START_ \u00e0 _END_ sur _TOTAL_",
            infoEmpty = "Aucun r\u00e9sultat",
            paginate = list(previous = "Pr\u00e9c\u00e9dent", `next` = "Suivant")
          )
        )
      )
    })
    output$detail = shiny::renderUI({
      selection = input$resultats_rows_selected
      donnees = lignes()
      if (length(selection) != 1L || selection > nrow(donnees))
        return(shiny::tags$em("S\u00e9lectionner une ligne du tableau pour consulter la question."))
      id = donnees$Identifiant[[selection]]
      q = DBI::dbGetQuery(db,
        "SELECT titre, txtque, datejodepot, reference
         FROM sen_questions.tam_questions WHERE CAST(id AS VARCHAR) = ?",
        params = list(id))
      shiny::req(nrow(q) == 1L)
      shiny::tagList(
        shiny::h3(q$titre[[1L]]),
        shiny::p(shiny::strong("R\u00e9f\u00e9rence : "), q$reference[[1L]],
                 " \u2014 ", q$datejodepot[[1L]]),
        shiny::tags$p(style = "white-space: pre-wrap;", q$txtque[[1L]])
      )
    })
  }
  shiny::shinyApp(ui, server)
}
