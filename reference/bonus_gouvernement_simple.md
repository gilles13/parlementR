# Frise simplifiee par type de mandat

Frise simplifiee par type de mandat

## Usage

``` r
bonus_gouvernement_simple(
  mandats,
  vue = c("presidences", "gouvernements"),
  depuis = as.Date("1946-01-01"),
  jusqua = Sys.Date()
)
```

## Arguments

- mandats:

  Table des mandats Wikidata deja disponible localement.

- vue:

  Presidences ou gouvernements.

- depuis, jusqua:

  Dates de debut et de fin.

## Value

Graphique ggplot2.
