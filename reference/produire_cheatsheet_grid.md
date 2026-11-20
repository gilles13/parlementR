# Produire la cheatsheet avec Grid et Cairo

Genere un guide PDF de cinq pages A4 paysage avec grid et Cairo. La
premiere page presente les prerequis R et systeme. Les applications
Shiny sont identifiees par des blocs jaunes.

## Usage

``` r
produire_cheatsheet_grid(
  sortie = "/tmp/parlementR-cheatsheet-grid.pdf",
  ouvrir = TRUE
)
```

## Arguments

- sortie:

  Chemin du fichier PDF produit.

- ouvrir:

  Ouvrir le PDF apres generation.

## Value

Chemin du PDF, invisiblement.
