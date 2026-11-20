#' Consulter les articles des codes juridiques dans Shiny
#'
#' Les articles sont classes dans l'ordre de l'arborescence LEGI et le niveau
#' affiche correspond a leur subdivision la plus precise.
#' @param db Connexion DuckDB facultative.
#' @return Application Shiny.
#' @export
explorer_codes = function(db = NULL) {
  requis = c("shiny", "DT")
  manquants = requis[!vapply(requis, requireNamespace, logical(1), quietly = TRUE)]
  if (length(manquants)) stop("Packages necessaires : ", paste(manquants, collapse = ", "))
  db = if (is.null(db)) connexion() else .connexion_valide(db)
  codes = .legi_catalogue(db)
  codes = codes[order(codes$nom_complet, codes$id), , drop = FALSE]
  ui = shiny::fluidPage(
    shiny::titlePanel("Consultation des codes juridiques"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput("code", "Code", choices = stats::setNames(codes$id, paste0(codes$nom_simplifie, " \u2014 ", codes$nom_complet)), width = "100%"),
        shiny::textInput("numero", "Numero d'article", placeholder = "Ex. L123-1"),
        shiny::actionButton("chercher", "Rechercher", class = "btn-primary")
      ),
      shiny::mainPanel(shiny::h4("Articles"), DT::DTOutput("articles"), shiny::hr(), shiny::uiOutput("viewer"))
    )
  )
  server = function(input, output, session) {
    resultats = shiny::eventReactive(input$chercher, {
      numero = trimws(input$numero)
      requete = paste0(
        "WITH RECURSIVE arbre AS (",
        "SELECT r.id_cible AS id_section, s.titre AS niveau, LPAD(CAST(r.ordre AS VARCHAR), 8, '0') AS chemin ",
        "FROM legi.liens_racine r JOIN legi.sections s ON s.id = r.id_cible AND s.id_texte = r.id_texte ",
        "WHERE r.id_texte = ? AND r.type = 'LIEN_SECTION_TA' ",
        "UNION ALL ",
        "SELECT l.id_cible, s.titre, a.chemin || '.' || LPAD(CAST(l.ordre AS VARCHAR), 8, '0') ",
        "FROM arbre a JOIN legi.liens l ON l.id_parent = a.id_section AND l.id_texte = ? AND l.type = 'LIEN_SECTION_TA' ",
        "JOIN legi.sections s ON s.id = l.id_cible AND s.id_texte = l.id_texte ",
        "), articles_ordonnes AS (",
        "SELECT a.id, a.numero, ar.niveau, ar.chemin || '.' || LPAD(CAST(l.ordre AS VARCHAR), 8, '0') AS chemin ",
        "FROM legi.articles a JOIN legi.liens l ON l.id_cible = a.id AND l.id_texte = a.id_texte AND l.type = 'LIEN_ART' ",
        "JOIN arbre ar ON ar.id_section = l.id_parent ",
        "WHERE a.id_texte = ? AND a.etat = 'VIGUEUR' AND a.numero IS NOT NULL AND a.numero <> '' ",
        if (nzchar(numero)) "AND a.numero ILIKE ? " else "",
        "), uniques AS (SELECT *, ROW_NUMBER() OVER (PARTITION BY id ORDER BY chemin) AS rang FROM articles_ordonnes) ",
        "SELECT id, niveau, numero FROM uniques WHERE rang = 1 ORDER BY chemin"
      )
      parametres = list(input$code, input$code, input$code)
      if (nzchar(numero)) parametres = c(parametres, list(paste0("%", numero, "%")))
      DBI::dbGetQuery(db, requete, params = parametres)
    }, ignoreNULL = FALSE)
    output$articles = DT::renderDT({
      x = resultats()
      DT::datatable(x[, c("niveau", "numero"), drop = FALSE], rownames = FALSE,
        selection = "single", colnames = c("Niveau", "Article"), filter = "top",
        options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100), searching = TRUE,
          ordering = FALSE, autoWidth = TRUE,
          columnDefs = list(list(width = "75%", targets = 0), list(width = "25%", targets = 1))))
    }, server = TRUE)
    fiche = shiny::reactive({
      i = input$articles_rows_selected
      shiny::req(length(i) == 1L)
      x = resultats()
      shiny::req(i <= nrow(x))
      DBI::dbGetQuery(db, paste0(
        "SELECT a.id, a.numero, c.contenu FROM legi.articles a ",
        "JOIN legi.articles_contenu c ON a.id = c.id AND a.fichier = c.fichier ",
        "WHERE a.id_texte = ? AND a.id = ? AND a.etat = 'VIGUEUR'"),
        params = list(input$code, x$id[[i]]))
    })
    output$viewer = shiny::renderUI({
      article = fiche()
      shiny::validate(shiny::need(nrow(article) == 1L, "Article introuvable ou ambigu"))
      paragraphes = .legi_extraire_texte(article$contenu[[1]])
      shiny::tagList(
        shiny::h3(paste("Article", article$numero[[1]])),
        shiny::div(style = "font-size: 15px; line-height: 1.65; padding: 15px; max-height: 600px; overflow-y: auto;", lapply(paragraphes, shiny::p)),
        shiny::tags$a("Consulter sur Legifrance", href = paste0("https://www.legifrance.gouv.fr/codes/article_lc/", article$id[[1]]), target = "_blank")
      )
    })
  }
  shiny::shinyApp(ui, server)
}
