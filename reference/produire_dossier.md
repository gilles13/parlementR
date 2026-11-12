# Produire la fiche PDF d'un dossier legislatif de l'Assemblee nationale

Utilise exclusivement les references officielles de la base locale. Les
documents sont relies par dossierRef et les amendements par
texteLegislatifRef.

## Usage

``` r
produire_dossier(uid, db = NULL, sortie = NULL, ouvrir = FALSE, pdf = TRUE)
```

## Arguments

- uid:

  Identifiant officiel du dossier.

- db:

  Connexion DuckDB ; NULL utilise connexion().

- sortie:

  Chemin du PDF, dans /tmp par defaut.

- ouvrir:

  Ouvrir le PDF apres production.

- pdf:

  Si FALSE, renvoyer les tables sans produire de PDF.

## Value

Chemin du PDF (invisible), ou liste des tables.
