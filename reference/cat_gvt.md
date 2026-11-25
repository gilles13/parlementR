# Composition historique ou datee d'un gouvernement

Composition historique ou datee d'un gouvernement

## Usage

``` r
cat_gvt(gvt, date = NULL, db = NULL)
```

## Arguments

- gvt:

  Identifiant PO ou libelle abrege du gouvernement.

- date:

  Date au format AAAA-MM-JJ ; NULL retourne tous les mandats du
  gouvernement.

- db:

  Connexion DuckDB facultative ; NULL utilise connexion().

## Value

Tibble des membres et de leurs fonctions ministerielles. \`preseance\`
est le rang de l'organisme ministeriel (eventuellement NA) ;
\`preseance_qualite\` est la categorie du mandat.
