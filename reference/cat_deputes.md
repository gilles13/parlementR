# Catalogue PDF des deputes par groupe parlementaire (grid et Cairo)

Exploite les mandats et organes AMO10 deja importes dans DuckDB. Les
responsabilites institutionnelles sont deduites des mandats actifs.

## Usage

``` r
cat_deputes(
  db = NULL,
  groupe = NULL,
  enrichie = FALSE,
  fichier = NULL,
  ouvrir = TRUE
)
```

## Arguments

- db:

  Connexion DBI facultative ; NULL utilise connexion().

- groupe:

  Sigle du groupe (par exemple "LFI-NFP"), ou NULL pour tous.

- enrichie:

  Ajouter les fonctions particulieres (FALSE par defaut).

- fichier:

  Chemin du PDF ; par defaut, /tmp/parlementR-deputes.pdf.

- ouvrir:

  Ouvrir le PDF apres generation.

## Value

Chemin du PDF, invisiblement.
