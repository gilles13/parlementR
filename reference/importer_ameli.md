# Importer le dump officiel AMELI du Senat

Restaure le SQL LATIN1 dans PostgreSQL temporaire puis copie les tables
par lots vers DuckDB, dans une transaction unique.

## Usage

``` r
importer_ameli(
  db = NULL,
  archive,
  remplacer = FALSE,
  taille_lot = 10000L,
  verbose = TRUE
)
```

## Arguments

- db:

  Connexion DuckDB ; NULL utilise connexion().

- archive:

  Archive ZIP officielle AMELI deja telechargee.

- remplacer:

  Autoriser le remplacement du schema existant.

- taille_lot:

  Nombre de lignes transferees par lot.

- verbose:

  Afficher les etapes.

## Value

Invisiblement, les effectifs importes.
