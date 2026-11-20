# Explorer les titres des questions au Senat par recherche floue

Application autonome, limitee aux titres des questions ecrites (QE).
L'index des titres est charge une seule fois depuis DuckDB local ; le
texte complet n'est lu que lorsqu'une question est selectionnee. La
recherche accepte des caracteres non consecutifs, sans tenir compte des
accents ni de la casse. Aucun acces reseau n'est effectue.

## Usage

``` r
explorer_questions_floues(db = NULL, limite = 30L)
```

## Arguments

- db:

  Connexion DuckDB facultative ; par defaut connexion().

- limite:

  Nombre maximal de lignes affichees dans le tableau.

## Value

Une application Shiny.
