# Produire une fiche biographique lisible pour un senateur

Produire une fiche biographique lisible pour un senateur

## Usage

``` r
produire_fiche_senateur(nom, sortie = NULL, ouvrir = TRUE, db = NULL)
```

## Arguments

- nom:

  Nom ou prenom et nom du senateur.

- sortie:

  Chemin du PDF dans /tmp par defaut.

- ouvrir:

  Ouvrir le PDF produit.

- db:

  Connexion DuckDB facultative.

## Value

Chemin du PDF, invisiblement.
