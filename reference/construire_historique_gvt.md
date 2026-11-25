# Construire l'historique des actes gouvernementaux

Construire l'historique des actes gouvernementaux

## Usage

``` r
construire_historique_gvt(
  db = NULL,
  catalogue = NULL,
  historique = NULL,
  mandats = NULL
)
```

## Arguments

- db:

  Connexion DuckDB facultative.

- catalogue:

  Catalogue construit par construire_catalogue_gvt().

- historique:

  Historique deja qualifie, facultatif ; prioritaire pour les noms
  officiels.

- mandats:

  Table locale des mandats avec personneLabel, personne_qid, debut et
  fin, ou date_debut et date_fin. Aucun acces reseau n'est effectue.

## Value

Tibble du catalogue enrichi ; les noms non verifies restent NA.
