# Importer des reperes historiques structures depuis Wikidata

Les postes et leurs titulaires sont lus dans Wikidata, sans extraction
HTML. La couverture n'est pas garantie exhaustive : les resultats sont
des candidats a rapprocher des actes officiels. Les JSON sont caches
avec leur empreinte MD5.

## Usage

``` r
importer_referentiel_historique(
  db = NULL,
  actualiser = FALSE,
  dossier = file.path(tools::R_user_dir("parlementR", "cache"), "wikidata")
)
```

## Arguments

- db:

  Connexion DuckDB ; NULL utilise connexion().

- actualiser:

  Reinterroger Wikidata ; FALSE relit le cache.

- dossier:

  Dossier de cache des reponses SPARQL.

## Value

Liste des tables importees et des controles.
