# Catalogue des missions parlementaires aupres du Gouvernement

Chaque ligne represente une relation entre un mandat de mission et un
organisme de rattachement. Un meme mandat peut donc apparaitre plusieurs
fois. L'objet de la mission provient du champ libelle du mandat AMO30.

## Usage

``` r
cat_missions_gvt(date = NULL, db = NULL)
```

## Arguments

- date:

  Date AAAA-MM-JJ facultative ; NULL conserve tout l'historique.

- db:

  Connexion DuckDB facultative ; NULL utilise connexion().

## Value

Tibble des missions et de leurs organismes de rattachement.
