# Importer le corpus complet DOLE de la DILA

Recupere le stock global officiel et applique toutes les mises a jour
posterieures dans l'ordre chronologique. Les XML originaux sont
conserves dans DuckDB, ainsi que tous leurs liens documentaires. Les
archives sont conservees dans le cache et les mises a jour deja
importees sont ignorees.

## Usage

``` r
importer_dole(
  db = NULL,
  cache = "/tmp/dila_dole",
  archives = NULL,
  reimporter = FALSE,
  verbose = TRUE,
  progression = 500L
)
```

## Arguments

- db:

  Connexion DuckDB ; NULL utilise \`connexion()\`.

- cache:

  Repertoire local des archives DOLE.

- archives:

  Noms d'archives a traiter ; NULL pour le catalogue officiel complet
  (stock global et mises a jour).

- reimporter:

  Reinitialiser les trois tables DOLE depuis le stock global.

- verbose:

  Afficher les etapes et la progression (TRUE par defaut).

- progression:

  Frequence des messages de progression, en fichiers XML.

## Value

Effectifs des archives traitees, invisiblement.
