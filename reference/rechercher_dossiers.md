# Rechercher des dossiers parlementaires dans la base locale

Rechercher des dossiers parlementaires dans la base locale

## Usage

``` r
rechercher_dossiers(sujet, db = NULL, limite = 50L)
```

## Arguments

- sujet:

  Expression recherchee dans les titres des dossiers et documents.

- db:

  Connexion DuckDB ; NULL utilise \`connexion()\`.

- limite:

  Nombre maximum de dossiers renvoyes.

## Value

Un tibble de dossiers, avec la date du premier document rattache et
l'origine de la correspondance.
