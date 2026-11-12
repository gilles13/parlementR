# Rapport legislatif thematique, construit exclusivement depuis DuckDB

Rapport legislatif thematique, construit exclusivement depuis DuckDB

## Usage

``` r
rapport_legislatif(
  db = NULL,
  sujet = NULL,
  fichier = NULL,
  limite = 20L,
  pdf = TRUE
)
```

## Arguments

- db:

  Connexion DBI facultative ; NULL utilise connexion().

- sujet:

  Expression recherchee dans les titres des dossiers.

- fichier:

  PDF de sortie, par defaut dans le repertoire temporaire.

- limite:

  Nombre maximum de dossiers dans le rapport.

- pdf:

  Si FALSE, renvoie les donnees sans produire de PDF.

## Value

Chemin du PDF (invisible) ou liste de tables si pdf = FALSE.
