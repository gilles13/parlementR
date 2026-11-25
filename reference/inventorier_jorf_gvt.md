# Inventorier les actes relatifs aux gouvernements dans le JORF

Lit les TEXTE_VERSION depuis l'archive globale ou des XML locaux. Un
candidat doit contenir une expression documentaire explicite relative a
une nomination, composition ou cessation gouvernementale. Le titre
generique "Decret du ..." ne suffit pas. Les champs de qualification
sont derives.

## Usage

``` r
inventorier_jorf_gvt(
  archive = NULL,
  sortie = "~/.cache/R/parlementR/jorf/gouvernements/inventaire_gvt.csv",
  motif = NULL,
  limite = Inf,
  fichiers = NULL,
  depuis = 1959L
)
```

## Arguments

- archive:

  Archive tar.gz ; facultative si fichiers est renseigne.

- sortie:

  CSV de sortie, jamais ecrase automatiquement.

- motif:

  Expression reguliere supplementaire (facultative) sur les champs
  documentaires. NULL conserve la selection gouvernementale par defaut.

- limite:

  Nombre maximal de TEXTE_VERSION lus.

- fichiers:

  Chemins de XML locaux pour validation, sans ouvrir l'archive.

- depuis:

  Annee minimale pour la selection, NULL pour aucune restriction.

## Value

Tableau des candidats, sans dedoublonnage des fichiers sources.
