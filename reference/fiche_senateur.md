# Fiche historique d'un senateur

Recherche par nom ou prenom et nom, sans selection approximative
implicite. Les sections restituent les lignes historiques de la base
locale, y compris les dates manquantes et les codes non encore traduits.

## Usage

``` r
fiche_senateur(nom, db = NULL)
```

## Arguments

- nom:

  Nom du senateur (ou prenom et nom).

- db:

  Connexion DuckDB facultative.

## Value

Liste de tableaux de donnees pour un seul senateur.
