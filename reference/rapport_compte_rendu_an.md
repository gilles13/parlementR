# Produire un PDF du compte rendu avec nuage de mots

Produire un PDF du compte rendu avec nuage de mots

## Usage

``` r
rapport_compte_rendu_an(
  cr,
  fichier = file.path(tempdir(), paste0(cr$reference, ".pdf")),
  exclusions = character(),
  n = 100L
)
```

## Arguments

- cr:

  Objet issu de lire_compte_rendu_an().

- fichier:

  Chemin du PDF de sortie.

- exclusions:

  Mots supplementaires a exclure.

- n:

  Nombre de mots dans le nuage.

## Value

Chemin du PDF (invisible).
