# Lister tous les dossiers legislatifs de l'Assemblee nationale

Inventaire exhaustif des dossiers presents dans la base DuckDB locale.
Pour rechercher un sujet, utiliser \`rech_dosleg_an()\`.

## Usage

``` r
rech_dosleg(db = NULL)
```

## Arguments

- db:

  Connexion DuckDB facultative ; NULL utilise \`connexion()\`.

## Value

Tibble avec uid, titre, procedure et legislature.
