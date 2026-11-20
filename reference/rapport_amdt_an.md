# Produire un suivi PDF des amendements selectionnes

Utilise directement le resultat de rech_amdt_an(), sans requete
supplementaire. Les statistiques portent sur la selection fournie.

## Usage

``` r
rapport_amdt_an(
  donnees,
  sujet = NULL,
  sortie = NULL,
  ouvrir = FALSE,
  pdf = TRUE
)
```

## Arguments

- donnees:

  Tableau retourne par rech_amdt_an().

- sujet:

  Libelle facultatif de la recherche.

- sortie:

  Chemin du PDF ; /tmp par defaut.

- ouvrir:

  Ouvrir le PDF apres production.

- pdf:

  Si FALSE, retourner le tableau sans produire de PDF.

## Value

Chemin du PDF (invisible) ou tableau fourni.
