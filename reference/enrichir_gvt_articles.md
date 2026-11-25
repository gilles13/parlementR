# Rattacher les articles officiels aux actes gouvernementaux

Rattacher les articles officiels aux actes gouvernementaux

## Usage

``` r
enrichir_gvt_articles(historique_gvt, articles, sortie = NULL)
```

## Arguments

- historique_gvt:

  Tableau des actes avec id_jorf et type_acte.

- articles:

  Resultat de extraire_articles_jorf().

- sortie:

  Dossier facultatif pour exporter les controles CSV.

## Value

Liste actes, articles et controles.
