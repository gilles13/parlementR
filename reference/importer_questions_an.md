# Importer les questions de l'Assemblee nationale depuis trois archives locales

Conserve les JSON complets des questions ecrites (QE), orales sans debat
(QOSD) et au gouvernement (QG) dans \`an_questions.questions\`. Aucune
requete reseau n'est effectuee par cette fonction.

## Usage

``` r
importer_questions_an(archives, db = NULL, verbose = TRUE)
```

## Arguments

- archives:

  Vecteur de trois chemins ZIP nommes \`ecrites\`, \`orales\` et
  \`gouvernement\`.

- db:

  Connexion DuckDB ; \`NULL\` utilise la connexion geree.

- verbose:

  Afficher le bilan d'importation.

## Value

Invisiblement, le nombre de questions importees.
