# Produire un catalogue PDF de dossiers correspondant a un sujet

Recherche exclusivement dans la base DuckDB locale. Les titres officiels
sont privilegies ; les references permettent d'appeler
produire_dossier().

## Usage

``` r
produire_sujets(
  sujet,
  db = NULL,
  limite = 100L,
  sortie = NULL,
  ouvrir = FALSE,
  pdf = TRUE
)
```

## Arguments

- sujet:

  Expression recherchee dans les titres officiels.

- db:

  Connexion DuckDB ; NULL utilise connexion().

- limite:

  Nombre maximum de dossiers (100 par defaut).

- sortie:

  Fichier PDF, dans /tmp par defaut.

- ouvrir:

  Ouvrir le PDF apres production.

- pdf:

  Si FALSE, retourner le tibble des resultats.

## Value

Chemin du PDF (invisible), ou tibble si pdf = FALSE.
