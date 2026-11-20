# Produire un article d'un code LEGI en PDF

Produire un article d'un code LEGI en PDF

## Usage

``` r
produire_code(
  code = "civil",
  article = "1240",
  sortie = NULL,
  ouvrir = TRUE,
  db = NULL,
  police = 10,
  logo = 1L
)
```

## Arguments

- code:

  Intitule ou nom abrege du code (par exemple "civil").

- article:

  Numero exact de l'article (par exemple "1240").

- sortie:

  Chemin du PDF ; par defaut un fichier dans /tmp.

- ouvrir:

  Ouvrir le PDF dans zathura.

- db:

  Connexion DuckDB facultative.

- police:

  Taille du texte principal en points.

- logo:

  Entier de 1 a 6 (defaut : 1).

## Value

Chemin du PDF, invisiblement.
