# Explorer les parlementaires dans une application Shiny minimale

L'application n'utilise que les tables DuckDB locales. Shiny est
facultatif.

## Usage

``` r
explorer_parlementaires(db = NULL)
```

## Arguments

- db:

  Connexion DuckDB facultative.

## Value

Une application Shiny (a lancer avec \`shiny::runApp()\`).
