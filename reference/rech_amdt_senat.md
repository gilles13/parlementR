# Rechercher un sujet dans les amendements du Senat

Recherche litterale dans l'objet et le dispositif AMELI. Les dossiers
DOSLEG sont raccordes par leur signet officiel, jamais par leur titre.

## Usage

``` r
rech_amdt_senat(sujet, db = NULL)
```

## Arguments

- sujet:

  Expression a rechercher (sans jokers SQL).

- db:

  Connexion DuckDB ; NULL utilise connexion(). Le resultat conserve le
  sujet recherche dans l'attribut \`sujet\`.

## Value

Liste de tables : textes, amendements, lectures.
