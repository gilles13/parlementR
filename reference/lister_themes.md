# Lister les themes officiels des questions du Senat

Les effectifs comptent chaque question dans chacun de ses themes. Une
question peut avoir plusieurs themes ; les questions codees \`#\` ne
sont rattachees a aucun theme. Classification non exhaustive.

## Usage

``` r
lister_themes(db = NULL)
```

## Arguments

- db:

  Connexion DuckDB locale.

## Value

Tibble avec identifiant, libelle et nombre de questions par theme.
