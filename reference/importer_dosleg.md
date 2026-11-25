# Importer DOSLEG depuis une archive officielle locale

Importe les 53 tables du dump PostgreSQL dans \`sen_dosleg\`. Aucune
connexion reseau n'est effectuee par cette fonction.

## Usage

``` r
importer_dosleg(
  db = NULL,
  archive,
  remplacer = FALSE,
  taille_lot = 2000L,
  verbose = TRUE
)
```

## Arguments

- db:

  Connexion DuckDB ; NULL utilise \`connexion()\`.

- archive:

  Chemin de \`dosleg.zip\` deja telecharge.

- remplacer:

  Autoriser le remplacement d'un schema DOSLEG deja complet.

- taille_lot:

  Nombre de lignes inserees par lot.

- verbose:

  Afficher les effectifs par table.

## Value

Invisiblement, les effectifs par table.
