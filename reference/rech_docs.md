# Rechercher des documents parlementaires dans les sources locales

La recherche est litterale, insensible a la casse et aux accents. Chaque
resultat conserve sa source, son type et son identifiant officiel. Les
sources absentes sont signalees dans l'attribut \`sources_absentes\`.

## Usage

``` r
rech_docs(terme, sources = NULL, db = NULL, limite = 100L)
```

## Arguments

- terme:

  Expression a rechercher.

- sources:

  Sources parmi \`ameli\`, \`dosleg\`, \`questions_senat\`,
  \`amendements_an\`, \`scrutins_an\`, \`questions_an\`, \`debats_an\`,
  \`reunions_an\`, \`dole\` et \`legi\`. NULL interroge toutes les
  sources disponibles.

- db:

  Connexion DuckDB ; NULL utilise connexion().

- limite:

  Nombre maximum de resultats par source. Le resultat conserve le sujet
  recherche dans l'attribut \`sujet\`.

## Value

Tibble de documents, de classe \`parlementR_documents\`.
