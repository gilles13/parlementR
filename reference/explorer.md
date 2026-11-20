# Lancer une application Shiny de parlementR

Lancer une application Shiny de parlementR

## Usage

``` r
explorer(
  application = c("parlementaires", "codes", "questions", "questions_floues"),
  db = NULL,
  ...
)
```

## Arguments

- application:

  Application a lancer : parlementaires, codes, questions ou
  questions_floues.

- db:

  Connexion DuckDB facultative.

- ...:

  Parametres transmis a l'application selectionnee.

## Value

Lance l'application Shiny.
