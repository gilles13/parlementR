# Fiche PDF des deputes par groupe parlementaire

Exploite les mandats et organes AMO10 deja importes dans DuckDB. Les
responsabilites institutionnelles sont deduites des mandats actifs.

## Usage

``` r
fiche_deputes(db = NULL, groupe = NULL, enrichie = FALSE, fichier = NULL)
```

## Arguments

- db:

  Connexion DBI facultative ; NULL utilise connexion().

- groupe:

  Sigle du groupe (par exemple "LFI-NFP"), ou NULL pour tous.

- enrichie:

  Ajouter les fonctions particulieres (FALSE par defaut).

- fichier:

  Chemin du PDF ; par defaut, un fichier dans /tmp.

## Value

Chemin du PDF, invisiblement.
