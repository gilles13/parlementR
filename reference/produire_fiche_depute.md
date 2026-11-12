# Produire la fiche PDF synthetique d'un depute

Produire la fiche PDF synthetique d'un depute

## Usage

``` r
produire_fiche_depute(nom, sortie = NULL, ouvrir = TRUE, db = NULL)
```

## Arguments

- nom:

  Nom ou prenom et nom du depute.

- sortie:

  Chemin du PDF, dans /tmp par defaut.

- ouvrir:

  Ouvrir le PDF apres generation.

- db:

  Connexion DuckDB facultative.

## Value

Chemin du PDF, invisiblement.
