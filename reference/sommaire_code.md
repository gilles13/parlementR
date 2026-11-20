# Produire le sommaire officiel d'un code LEGI en PDF

Les subdivisions sont parcourues dans l'ordre des liens LEGI. Le niveau
represente la profondeur dans l'arbre officiel : 0 pour les subdivisions
racines, 1 pour leurs enfants, etc. Les appellations (livre, titre,
chapitre...) sont conservees telles qu'elles figurent dans TITRE_TA.

## Usage

``` r
sommaire_code(
  code = "civil",
  niveau = 3,
  date = Sys.Date(),
  articles = FALSE,
  sortie = NULL,
  ouvrir = TRUE,
  db = NULL,
  police = 10,
  format = c("pdf", "donnees"),
  logo = 1L
)
```

## Arguments

- code:

  Intitule ou nom abrege du code.

- niveau:

  Profondeur maximale (0, 1, 2, ... ou Inf).

- date:

  Date de validite des liens, par defaut aujourd'hui.

- articles:

  Inclure les numeros d'articles dans le sommaire.

- sortie:

  Chemin du PDF ; par defaut un fichier dans /tmp.

- ouvrir:

  Ouvrir le PDF dans zathura.

- db:

  Connexion DuckDB facultative.

- police:

  Taille du texte principal en points.

- format:

  Format de sortie : PDF ou donnees structurees.

- logo:

  Entier de 1 a 6 (defaut : 1).

## Value

Chemin du PDF ou tableau si format = 'donnees'.
