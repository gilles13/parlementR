# Catalogue historique des gouvernements

Utilise les organismes et mandats AMO30 deja presents dans DuckDB. Les
effectifs comptent les personnes distinctes ayant appartenu au
gouvernement pendant sa duree, et non les seuls membres presents a une
date donnee.

## Usage

``` r
cat_gvts(db = NULL)
```

## Arguments

- db:

  Connexion DuckDB facultative ; NULL utilise connexion().

## Value

Tibble : identifiant, gouvernement, dates, Premier ministre et effectif.
