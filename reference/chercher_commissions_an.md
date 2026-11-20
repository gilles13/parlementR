# Rechercher les reunions de commission et leurs comptes rendus

Rechercher les reunions de commission et leurs comptes rendus

## Usage

``` r
chercher_commissions_an(
  sujet = NULL,
  intervenant = NULL,
  date_debut = NULL,
  date_fin = NULL,
  avec_compte_rendu = TRUE,
  db = NULL
)
```

## Arguments

- sujet, intervenant:

  Texte recherche dans l'ordre du jour.

- date_debut, date_fin:

  Dates limites incluses.

- avec_compte_rendu:

  Exclure les reunions sans reference de compte rendu.

- db:

  Connexion DuckDB facultative.

## Value

Tibble de reunions, avec URL et intervenants mentionnes dans le sujet.
