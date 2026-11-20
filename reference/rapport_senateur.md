# Produire une fiche biographique lisible pour un senateur

Produire une fiche biographique lisible pour un senateur

## Usage

``` r
rapport_senateur(
  nom,
  sortie = NULL,
  ouvrir = TRUE,
  db = NULL,
  cache = tools::R_user_dir("parlementR", "cache")
)
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
