# Rechercher les questions du Senat dans DuckDB local

La recherche porte sur le titre, la rubrique et le texte de la question.
Les questions sans reponse sont conservees.

## Usage

``` r
rechercher_questions(
  sujet = NULL,
  db = NULL,
  limite = 100L,
  depuis = NULL,
  jusque = NULL,
  theme = NULL
)
```

## Arguments

- sujet:

  Expression recherchee ; NULL pour filtrer uniquement par theme.

- db:

  Connexion DuckDB.

- limite:

  Nombre maximal de resultats.

- depuis:

  Date minimale de publication, au format YYYY-MM-DD.

- jusque:

  Date maximale de publication, au format YYYY-MM-DD.

- theme:

  Code \`thenouidt\` ou libelle officiel de \`lister_themes()\`.
  Plusieurs themes peuvent etre associes a une question.

## Value

Un tibble, une ligne par question.
