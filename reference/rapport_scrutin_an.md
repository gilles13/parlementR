# Produire un rapport PDF sur un scrutin public de l'Assemblee nationale

Les effectifs et les positions proviennent exclusivement des donnees
locales. Les non-votants nominatifs ne sont pas assimiles aux deputes
absents du scrutin.

## Usage

``` r
rapport_scrutin_an(
  scrutin,
  db = NULL,
  contexte = NULL,
  sortie = NULL,
  ouvrir = TRUE,
  pdf = TRUE,
  amendement_uid = NULL
)
```

## Arguments

- scrutin:

  UID officiel du scrutin (ou numero de scrutin).

- db:

  Connexion DuckDB ; NULL utilise connexion().

- contexte:

  Complement facultatif ; le contexte legislatif est recherche
  automatiquement.

- sortie:

  Fichier PDF de destination, dans /tmp par defaut.

- ouvrir:

  Ouvrir le PDF apres production.

- pdf:

  Si FALSE, retourner les tableaux sans produire de PDF.

- amendement_uid:

  UID officiel de l'amendement, si aucune liaison structuree unique
  n'existe.

## Value

Chemin du PDF (invisible), ou liste des donnees du rapport.
