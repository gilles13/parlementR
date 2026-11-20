# Rechercher des parlementaires

Recherche dans les donnees deja importees, sans acces reseau.

## Usage

``` r
rechercher_parlementaires(
  texte = "",
  chambre = c("toutes", "an", "senat"),
  db = NULL
)
```

## Arguments

- texte:

  Nom ou prenom, recherche insensible aux accents.

- chambre:

  \`toutes\`, \`an\` ou \`senat\`.

- db:

  Connexion DuckDB facultative.

## Value

Un tibble de parlementaires avec identite et groupe.
