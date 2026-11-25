# Rapport du parcours legislatif d'un dossier de l'Assemblee nationale

Restitue les etapes et evenements hierarchises, puis les documents lies.
Les relations sont exclusivement celles de la base locale.

## Usage

``` r
rapport_legislatif_an(
  ref,
  ouvrir = TRUE,
  fichier = NULL,
  pdf = TRUE,
  db = NULL
)
```

## Arguments

- ref:

  UID officiel du dossier legislatif.

- ouvrir:

  Ouvrir le PDF apres sa production (TRUE par defaut).

- fichier:

  Chemin du PDF, dans /tmp par defaut.

- pdf:

  Si FALSE, renvoyer les donnees sans produire de PDF.

- db:

  Connexion DuckDB facultative ; NULL utilise connexion().

## Value

Chemin du PDF (invisible), ou liste de tables si pdf = FALSE.
