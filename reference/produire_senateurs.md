# Produire un annuaire PDF des senateurs

Lit exclusivement la table locale \`senateurs.sen\` issue du dump
officiel du Senat. Le PDF utilise le modele \`senateurs.Rmd\` (sans
fancyhdr).

## Usage

``` r
produire_senateurs(
  type = c("actif", "anciens", "autres", "tous"),
  sortie = file.path("/tmp", paste0("parlementR-senateurs-", match.arg(type), ".pdf")),
  ouvrir = TRUE,
  db = NULL
)
```

## Arguments

- type:

  Statut a retenir : actif, anciens, autres ou tous.

- sortie:

  Chemin du PDF produit, dans /tmp par defaut.

- ouvrir:

  Ouvrir le PDF apres generation.

- db:

  Connexion DuckDB facultative ; NULL utilise connexion().

## Value

Chemin du PDF, invisiblement.
