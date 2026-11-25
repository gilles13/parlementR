# Indexer les codes LEGI depuis une archive globale locale

Importe les metadonnees, la hierarchie et les contenus XML des articles.
Les contenus sont conserves dans legi.articles_contenu. Les titres des
subdivisions proviennent directement de SECTION_TA/TITRE_TA. Le niveau
numerique officiel est conserve sur les liens (attribut niv).

## Usage

``` r
importer_legi(archive, db = NULL, verbose = TRUE)
```

## Arguments

- archive:

  Archive Freemium_legi_global\_\*.tar.gz deja telechargee.

- db:

  Connexion DuckDB facultative.

- verbose:

  Afficher les messages de suivi.

## Value

Invisiblement, le nombre de codes indexes.
