# Recherche locale commune aux deux chambres, sans dependance Shiny.
#' Rechercher des parlementaires
#'
#' Recherche dans les donnees deja importees, sans acces reseau.
#' @param texte Nom ou prenom, recherche insensible aux accents.
#' @param chambre `toutes`, `an` ou `senat`.
#' @param db Connexion DuckDB facultative.
#' @return Un tibble de parlementaires avec identite et groupe.
#' @export
rechercher_parlementaires = function(texte = "", chambre = c("toutes", "an", "senat"), db = NULL) {
  chambre = match.arg(chambre)
  if (!is.character(texte) || length(texte) != 1L || is.na(texte)) stop("Recherche invalide.")
  db = .connexion_valide(db)
  vide = function() tibble::tibble(chambre = character(), id = character(), nom = character(), prenom = character(), naissance = character(), groupe = character(), territoire = character())
  resultat = list()
  if (chambre %in% c("toutes", "an") && DBI::dbExistsTable(db, DBI::Id(schema = "an_deputes", table = "acteurs"))) {
    sql = "SELECT a.uid AS id, json_extract_string(a.contenu, '$.acteur.etatCivil.ident.nom') AS nom, json_extract_string(a.contenu, '$.acteur.etatCivil.ident.prenom') AS prenom, json_extract_string(a.contenu, '$.acteur.etatCivil.infoNaissance.dateNais') AS naissance, (SELECT json_extract_string(o.contenu, '$.organe.libelle') FROM json_each(a.contenu, '$.acteur.mandats.mandat') m JOIN an_deputes.organes o ON o.uid = json_extract_string(m.value, '$.organes.organeRef') WHERE json_extract_string(m.value, '$.typeOrgane') = 'GP' AND json_extract_string(m.value, '$.dateFin') IS NULL ORDER BY json_extract_string(m.value, '$.dateDebut') DESC LIMIT 1) AS groupe, (SELECT json_extract_string(m.value, '$.election.lieu.departement') FROM json_each(a.contenu, '$.acteur.mandats.mandat') m WHERE json_extract_string(m.value, '$.typeOrgane') = 'ASSEMBLEE' AND json_extract_string(m.value, '$.dateFin') IS NULL LIMIT 1) AS territoire FROM an_deputes.acteurs a WHERE EXISTS (SELECT 1 FROM json_each(a.contenu, '$.acteur.mandats.mandat') m WHERE json_extract_string(m.value, '$.typeOrgane') = 'ASSEMBLEE' AND json_extract_string(m.value, '$.dateFin') IS NULL)"
    x = tibble::as_tibble(DBI::dbGetQuery(db, sql))
    x$chambre = "Assembl\u00e9e nationale"
    resultat[[length(resultat) + 1L]] = x
  }
  if (chambre %in% c("toutes", "senat") && DBI::dbExistsTable(db, DBI::Id(schema = "senateurs", table = "sen"))) {
    x = tibble::as_tibble(DBI::dbGetQuery(db, "SELECT CAST(senmat AS VARCHAR) AS id, sennomuse AS nom, senprenomuse AS prenom, CAST(sendatnai AS VARCHAR) AS naissance, sengrppolliccou AS groupe, sencircou AS territoire FROM senateurs.sen WHERE etasencod = 'ACTIF'"))
    x$chambre = "S\u00e9nat"
    resultat[[length(resultat) + 1L]] = x
  }
  if (!length(resultat)) return(vide())
  x = dplyr::bind_rows(resultat) |>
    dplyr::select(chambre, id, nom, prenom, naissance, groupe, territoire)
  normaliser = function(z) {
    z[is.na(z)] = ""
    y = iconv(z, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
    tolower(ifelse(is.na(y), z, y))
  }
  recherche = gsub("[^a-z0-9]", "", trimws(normaliser(texte)))
  if (nzchar(recherche)) {
    noms = gsub("[^a-z0-9]", "", normaliser(paste(x$prenom, x$nom)))
    direct = grepl(recherche, noms, fixed = TRUE)
    lettres = strsplit(recherche, "", fixed = TRUE)[[1L]]
    motif = paste(lettres, collapse = ".*")
    flou = grepl(motif, noms, perl = TRUE)
    x = x[flou, , drop = FALSE]
    x$score = as.integer(direct[flou])
    return(dplyr::arrange(x, dplyr::desc(score), nom, prenom, chambre) |>
      dplyr::select(-score))
  }
  dplyr::arrange(x, nom, prenom, chambre)
}

#' Explorer les parlementaires dans une application Shiny minimale
#'
#' L'application n'utilise que les tables DuckDB locales. Shiny est facultatif.
#' @param db Connexion DuckDB facultative.
#' @return Une application Shiny (a lancer avec `shiny::runApp()`).
#' @export
explorer_parlementaires = function(db = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) stop("Installer Shiny : install.packages('shiny').", call. = FALSE)
  if (!requireNamespace("DT", quietly = TRUE)) stop("Installer DT : install.packages('DT').", call. = FALSE)
  db = .connexion_valide(db)
  ui = shiny::fluidPage(
    shiny::titlePanel("parlementR \u2014 Explorer la vie parlementaire"),
    shiny::tabsetPanel(id = "onglet",
      shiny::tabPanel("Recherche globale", value = "toutes"),
      shiny::tabPanel("D\u00e9put\u00e9s", value = "an"),
      shiny::tabPanel("S\u00e9nateurs", value = "senat")
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("recherche", "Nom ou pr\u00e9nom", placeholder = "Ex. dup, macron, lehn"),
        shiny::helpText("Recherche floue, sans distinction de casse ni d'accents."),
        shiny::textOutput("compteur"),
        width = 3
      ),
      shiny::mainPanel(
        shiny::h4("R\u00e9sultats"),
        DT::DTOutput("resultats"),
        shiny::hr(),
        shiny::h4("Fiche du parlementaire"),
        shiny::uiOutput("fiche"),
        shiny::hr(),
        shiny::helpText("Sources : Assembl\u00e9e nationale et S\u00e9nat \u2014 donn\u00e9es locales parlementR."),
        width = 9
      )
    )
  )
  server = function(input, output, session) {
    selection = shiny::reactive({
      x = donnees()
      i = input$resultats_rows_selected
      shiny::req(length(i) == 1L, i >= 1L, i <= nrow(x))
      x[i, , drop = FALSE]
    })

    chemin_photo = shiny::reactive({
      y = selection()
      cache = tools::R_user_dir("parlementR", "cache")
      if (y$chambre[[1L]] == "Assembl\u00e9e nationale") {
        fichier = file.path(cache, "assemblee_nationale", "photos", "17",
                            paste0(y$id[[1L]], ".jpg"))
      } else {
        fichier = file.path(cache, "senat", "photos",
                            paste0(y$id[[1L]], ".jpg"))
      }
      if (.photo_jpeg(fichier)) fichier else NULL
    })

    output$portrait = shiny::renderImage({
      fichier = chemin_photo()
      shiny::req(!is.null(fichier))
      list(src = fichier, contentType = "image/jpeg",
           alt = "Photographie officielle", width = 140)
    }, deleteFile = FALSE)

    recherche = shiny::debounce(shiny::reactive(input$recherche), 250)
    donnees = shiny::reactive({
      if (!nzchar(trimws(recherche()))) return(rechercher_parlementaires("", chambre = input$onglet, db = db)[0, ])
      rechercher_parlementaires(recherche(), chambre = input$onglet, db = db)
    })
    output$compteur = shiny::renderText({
      if (!nzchar(trimws(recherche()))) return("Saisir un nom pour commencer.")
      paste(nrow(donnees()), "r\u00e9sultat(s)")
    })
    output$resultats = DT::renderDT({
      x = donnees()
      x = x[, c("chambre", "nom", "prenom", "groupe", "territoire"), drop = FALSE]
      DT::datatable(x, rownames = FALSE, selection = "single",
        colnames = c("Chambre", "Nom", "Pr\u00e9nom", "Groupe politique", "Territoire"),
        options = list(pageLength = 10, lengthChange = FALSE, searching = FALSE,
          ordering = FALSE))
    }, server = FALSE)
    output$fiche = shiny::renderUI({
      y = selection()
      valeur = function(z) if (is.na(z) || !nzchar(as.character(z))) "Non renseign\u00e9" else as.character(z)
      shiny::tagList(
        shiny::h3(paste(y$prenom, y$nom)),
        shiny::fluidRow(
          shiny::column(3,
            if (!is.null(chemin_photo()))
              shiny::imageOutput("portrait", width = "140px", height = "180px")
            else shiny::tags$em("Photographie non disponible dans le cache local")
          ),
          shiny::column(9,
            shiny::tags$dl(
              shiny::tags$dt("Chambre"), shiny::tags$dd(valeur(y$chambre)),
              shiny::tags$dt("Identifiant officiel"), shiny::tags$dd(valeur(y$id)),
              shiny::tags$dt("Date de naissance"), shiny::tags$dd(valeur(y$naissance)),
              shiny::tags$dt("Groupe politique"), shiny::tags$dd(valeur(y$groupe)),
              shiny::tags$dt("D\u00e9partement / circonscription"),
              shiny::tags$dd(valeur(y$territoire))
            )
          )
        )
      )
    })
  }
  shiny::shinyApp(ui, server)
}
