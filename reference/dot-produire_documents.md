# Produire un PDF documentaire a partir des references d'un sujet

Aucun resume ni verbatim n'est genere : seuls les champs de la base
locale sont reproduits. Les liens vers des debats ne sont pas des
debats.

## Usage

``` r
.produire_documents(
  references,
  fichier = NULL,
  ouvrir = FALSE,
  surligner = TRUE
)
```

## Arguments

- references:

  Resultat de rech_amdt_senat() ou rech_docs().

- fichier:

  Chemin du PDF, par defaut dans /tmp.

- ouvrir:

  Ouvrir le document apres sa creation.

- surligner:

  Mettre en gras les occurrences du terme recherche dans le contenu.

## Value

Chemin du PDF (invisible).
