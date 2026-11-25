# Rechercher les amendements de l'Assemblee nationale en local

Les recherches interrogent uniquement DuckDB. Les noms d'auteurs et les
groupes sont des informations de presentation, jamais des cles de
jointure.

## Usage

``` r
rech_amdt_an(
  sujet = NULL,
  db = NULL,
  auteur = NULL,
  groupe = NULL,
  texte = NULL,
  date_debut = NULL,
  date_fin = NULL,
  etat = NULL,
  sort = NULL,
  limite = 100L,
  colonnes = c("complet", "identification")
)
```

## Arguments

- sujet:

  Expression recherchee dans le dispositif ou l'expose.

- db:

  Connexion DuckDB locale.

- auteur:

  Expression recherchee dans le nom de l'auteur.

- groupe:

  Expression recherchee dans le libelle ou l'abreviation du groupe.

- texte:

  Expression recherchee dans le titre du texte legislatif.

- date_debut, date_fin:

  Dates inclusives au format YYYY-MM-DD.

- etat, sort:

  Expressions recherchees dans les etats et sorts officiels.

- limite:

  Nombre maximal de resultats.

- colonnes:

  \`"complet"\` (defaut) ou \`"identification"\`. Le resultat conserve
  le sujet recherche dans l'attribut \`sujet\`.

## Value

Un tibble avec l'UID officiel de chaque amendement.
