# Produire une frise chronologique des chefs d'Etat et des evenements

Les donnees peuvent etre fournies explicitement ou lues depuis les CSV
historiques installes avec le package. Le logo hexagonal est place dans
l'en-tete, hors de la zone des evenements.

## Usage

``` r
produire_frise(
  donnees = NULL,
  evenements = NULL,
  debut = 1,
  ids = NULL,
  noms = NULL,
  categories = NULL,
  maximum = 20,
  espacement = 1.5,
  logo = TRUE
)
```

## Arguments

- donnees:

  Table des responsables historiques.

- evenements:

  Table des evenements historiques.

- debut:

  Numero de la Republique de depart, entre 1 et 5.

- ids, noms, categories:

  Filtres facultatifs sur les evenements.

- maximum:

  Nombre maximal d'evenements affiches.

- espacement:

  Espacement minimal entre les etiquettes, en annees.

- logo:

  Afficher le logo hexagonal en haut a droite.

## Value

Un graphique ggplot.
