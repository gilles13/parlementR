# Fiche exhaustive d'un depute dans les donnees locales

Recherche par nom, prenom et nom, ou nom et prenom. Les homonymes
provoquent une erreur explicite et les noms proches sont proposes.

## Usage

``` r
fiche_depute(nom, db = NULL, cache = tools::R_user_dir("parlementR", "cache"))
```

## Arguments

- nom:

  Nom du depute recherche.

- db:

  Connexion DuckDB facultative.

- cache:

  Repertoire du cache local des photographies.

## Value

Liste contenant identite, acteur, mandats, organes, deports et photo.
