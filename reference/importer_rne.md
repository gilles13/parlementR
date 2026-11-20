# Telecharger et controler les fichiers du Repertoire national des elus

Prepare les CSV et leur manifeste local, sans ouvrir ni modifier DuckDB.
Les fichiers deja valides restent disponibles si une mise a jour echoue.

## Usage

``` r
importer_rne(
  cache = tools::R_user_dir("parlementR", "cache"),
  forcer = FALSE,
  verbose = TRUE
)
```

## Arguments

- cache:

  Repertoire racine du cache parlementR.

- forcer:

  Verifier le serveur meme si le dernier controle date de moins de 24
  heures.

- verbose:

  Afficher les messages horodates et la progression native de curl.

## Value

Un data.frame de metadonnees (invisiblement).
