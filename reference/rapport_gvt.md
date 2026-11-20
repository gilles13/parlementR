# Rapport PDF sur la composition d'un gouvernement

Sans date, restitue les mandats historiques ; avec une date, une
photographie.

## Usage

``` r
rapport_gvt(
  gvt,
  date = NULL,
  sortie = NULL,
  ouvrir = TRUE,
  db = NULL,
  pdf = TRUE
)
```

## Arguments

- gvt:

  Identifiant PO ou libelle abrege du gouvernement.

- date:

  Date AAAA-MM-JJ facultative.

- sortie:

  Chemin PDF, dans /tmp par defaut.

- ouvrir:

  Ouvrir le PDF apres production.

- db:

  Connexion DuckDB facultative.

- pdf:

  Si FALSE, retourner les donnees sans produire le PDF.

## Value

Chemin PDF invisible, ou liste des donnees si pdf = FALSE.
