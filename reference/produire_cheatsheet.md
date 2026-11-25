# Produire la cheatsheet avec Grid et Cairo

Genere un guide PDF A4 paysage avec grid et Cairo. Le mode simple tient
sur une page ; le mode complet en contient cinq. Les applications Shiny
sont identifiees par des blocs jaunes.

## Usage

``` r
produire_cheatsheet(
  simple = TRUE,
  sortie = "/tmp/parlementR-cheatsheet.pdf",
  ouvrir = TRUE
)
```

## Arguments

- simple:

  Produire une fiche essentielle sur une page (TRUE par defaut).

- sortie:

  Chemin du fichier PDF produit.

- ouvrir:

  Ouvrir le PDF apres generation.

## Value

Chemin du PDF, invisiblement.
