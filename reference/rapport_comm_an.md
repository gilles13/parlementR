# Produire un compte rendu de commission en PDF (grid et Cairo)

Produire un compte rendu de commission en PDF (grid et Cairo)

## Usage

``` r
rapport_comm_an(
  cr,
  fichier = NULL,
  exclusions = character(),
  n = 100L,
  ouvrir = TRUE
)
```

## Arguments

- cr:

  Reference CRCANR, ligne de rech_comm_an(), ou objet de
  lire_compte_rendu_an().

- fichier:

  Chemin facultatif (par defaut
  /tmp/parlementR-commission-an-REFERENCE.pdf).

- exclusions, n:

  Arguments historiques conserves pour compatibilite.

- ouvrir:

  Ouvrir le PDF apres generation.

## Value

Chemin du PDF (invisible).
