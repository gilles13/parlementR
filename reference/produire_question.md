# Produire une fiche PDF de question et reponses

Produire une fiche PDF de question et reponses

## Usage

``` r
produire_question(id, db = NULL, sortie = NULL, ouvrir = FALSE, pdf = TRUE)
```

## Arguments

- id:

  Identifiant officiel interne.

- db:

  Connexion DuckDB.

- sortie:

  Chemin PDF.

- ouvrir:

  Ouvrir le PDF.

- pdf:

  Si FALSE, retourner les tibbles sources.

## Value

Chemin PDF invisible ou liste de tibbles.
