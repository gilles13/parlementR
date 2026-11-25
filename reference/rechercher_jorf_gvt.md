# Rechercher les decrets JORF relatifs aux gouvernements

Rechercher les decrets JORF relatifs aux gouvernements

## Usage

``` r
rechercher_jorf_gvt(
  db = NULL,
  motif = "composition du gouvernement|relatif a la composition du gouvernement",
  limite = 1000L
)
```

## Arguments

- db:

  Connexion DuckDB ; NULL utilise connexion().

- motif:

  Expression reguliere DuckDB sur le titre.

- limite:

  Nombre maximal de resultats.
