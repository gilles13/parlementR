# parlementR

**Comprendre l'activité parlementaire française à partir de données ouvertes, documentées et reproductibles.**

`parlementR` importe les données parlementaires officielles dans DuckDB en conservant les documents sources complets. Le projet vise à faciliter l'analyse des acteurs, mandats, textes et débats et, à terme, la production de documents PDF retraçant le parcours législatif.

## Installation

```r
install.packages("remotes") # si nécessaire
remotes::install_github("gilles13/parlementR")
```

## Première source : AMO10

Les députés en exercice, leurs mandats et organes associés (Assemblée nationale, XVIIe législature) sont importés dans le schéma `an_deputes` de DuckDB.

```r
chemin = file.path(tools::R_user_dir("parlementR", "data"), "parlementR.duckdb")
dir.create(dirname(chemin), recursive = TRUE, showWarnings = FALSE)
db = DBI::dbConnect(duckdb::duckdb(), dbdir = chemin)
parlementR::actualiser(db)
DBI::dbDisconnect(db, shutdown = TRUE)
```

Les archives sont mises en cache dans `tools::R_user_dir("parlementR", "cache")`, indépendamment de la base persistante.

## Documentation

- [À propos](vignettes/apropos.Rmd) : objectifs et sources ouvertes.
- [Documentation des sources](vignettes/documentation.Rmd) : index des fiches techniques.
- [AMO10](inst/documentation/AMO10.md) et [inventaire de ses champs JSON](inst/documentation/AMO10_champs.csv).

Site du package : <https://gilles13.github.io/parlementR/> (après publication via pkgdown).

Deuxième source : **dossiers législatifs** (`actualiser(db, jeux = "dossiers")`), dans le schéma `an_dossiers`. [Documentation](inst/documentation/Dossiers_legislatifs.md) et [inventaire JSON](inst/documentation/Dossiers_legislatifs_champs.csv).

## Amendements et rapports thématiques

```r
actualiser(db, jeux = "amendements")
x = rapport_legislatif(db, "logement", pdf = FALSE)
rapport_legislatif(db, "logement")
```

L'import utilise l'archive JSON officielle de la XVIIe législature, conservée
en cache. Les recherches et la génération PDF ne font aucun appel réseau.
