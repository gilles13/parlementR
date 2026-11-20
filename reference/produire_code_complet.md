# Produire un code LEGI complet en PDF

Produire un code LEGI complet en PDF

## Usage

``` r
produire_code_complet(
  code = "civil",
  date = Sys.Date(),
  sortie = NULL,
  ouvrir = TRUE,
  db = NULL,
  police = 9,
  logo = 1L
)
```

## Arguments

- code:

  Nom simplifie du code (voir recherche_code()).

- date:

  Date de validite des articles.

- sortie:

  Chemin du PDF ; par defaut /tmp/code\_\<nom_simplifie\>.pdf.

- ouvrir:

  Ouvrir le document dans zathura.

- db:

  Connexion DuckDB facultative.

- police:

  Taille du texte principal en points.

- logo:

  Entier de 1 a 6 (defaut : 1).

## Value

Chemin du PDF, invisiblement.
