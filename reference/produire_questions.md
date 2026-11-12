# Produire un catalogue PDF des questions du Senat

Produire un catalogue PDF des questions du Senat

## Usage

``` r
produire_questions(
  sujet = NULL,
  db = NULL,
  limite = 100L,
  sortie = NULL,
  ouvrir = FALSE,
  pdf = TRUE,
  theme = NULL
)
```

## Arguments

- sujet:

  Expression recherchee ; NULL si filtre par theme seul.

- db:

  Connexion DuckDB.

- limite:

  Nombre de questions.

- sortie:

  Chemin du PDF, dans /tmp par defaut.

- ouvrir:

  Ouvrir le PDF.

- pdf:

  Si FALSE, retourner le tibble.

- theme:

  Code ou libelle d'un theme officiel du Senat.

## Value

Chemin PDF invisible ou tibble.
