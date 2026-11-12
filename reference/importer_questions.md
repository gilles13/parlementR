# Importer les questions du Senat depuis une archive officielle locale

Lit \`questions.sql\` directement dans \`questions.zip\`, sans
PostgreSQL ni connexion reseau. Les huit tables sont conservees en texte
dans DuckDB.

## Usage

``` r
importer_questions(archive, db = NULL, remplacer = FALSE, verbose = TRUE)
```

## Arguments

- archive:

  Chemin de \`questions.zip\`.

- db:

  Connexion DuckDB ; NULL utilise la connexion geree.

- remplacer:

  Remplacer un schema deja importe.

- verbose:

  Afficher les effectifs et horodatages.

## Value

Invisiblement, un tibble des effectifs par table.
