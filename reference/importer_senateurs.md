# Importer les senateurs depuis une archive officielle locale

Importe les 95 tables du dump PostgreSQL du Senat dans le schema DuckDB
\`senateurs\`, sans serveur PostgreSQL et sans acces reseau. Les tables
sont remplacees dans une transaction ; en cas d'erreur, l'import est
annule. Pour telecharger ou actualiser la source, utiliser
\`actualiser(jeux = "senateurs")\`.

## Usage

``` r
importer_senateurs(db = NULL, archive, verbose = TRUE)
```

## Arguments

- db:

  Connexion DuckDB ; NULL utilise \`connexion()\`.

- archive:

  Chemin du fichier \`export_sens.zip\` deja telecharge.

- verbose:

  Afficher l'extraction et la progression table par table.

## Value

Invisiblement, le nombre de tables et de lignes importees.
