# Extraire les articles de textes JORF depuis une archive locale

Extraire les articles de textes JORF depuis une archive locale

## Usage

``` r
extraire_articles_jorf(
  ids,
  archive,
  dossier = "~/.cache/R/parlementR/jorf/gouvernements",
  index = file.path(dossier, "index_archive.txt"),
  limite = 1000L
)
```

## Arguments

- ids:

  Identifiants JORFTEXT demandes.

- archive:

  Chemin vers l archive tar.gz JORF locale.

- dossier:

  Repertoire du cache des XML.

- index:

  Index local des chemins de l archive.

- limite:

  Nombre maximal de textes par appel.

## Value

Data frame des relations texte/article et contenus XML officiels.
