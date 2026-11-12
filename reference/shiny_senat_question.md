# Interface locale des questions du Sénat

Recherche par thème officiel et mot-clé, puis affiche la question et
toutes ses réponses. Les données sont consultées uniquement dans DuckDB
local. Les packages \`shiny\` et \`DT\` sont facultatifs et ne sont
jamais installés automatiquement.

## Usage

``` r
shiny_senat_question(db = NULL)
```

## Arguments

- db:

  Connexion DuckDB locale ouverte.

## Value

Lance une application Shiny dans la session R.
