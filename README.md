# parlementR

**Explorer les travaux parlementaires français à partir de données publiques officielles, conservées et interrogées localement.**

`parlementR` est un package R de collecte, de documentation et d’analyse des données de l’Assemblée nationale et du Sénat. Les archives officielles sont téléchargées pour l’importation et les mises à jour ; les recherches et traitements sont réalisés dans une base **DuckDB locale**, sans requêtes d’analyse à distance.

## Installer

```r
remotes::install_github("gilles13/parlementR")
```

Le package est en développement (`0.0.2`). L’installation depuis GitHub nécessite le package `remotes`.

## Démarrer

```r
library(parlementR)

db = connexion(creer = TRUE)
actualiser(db = db, jeux = "deputes")
fiche_deputes(db = db, fichier = "/tmp/parlementR-deputes.pdf")
fermer_base()
```

`connexion()` utilise par défaut `file.path(tools::R_user_dir("parlementR", "data"), "parlementR.duckdb")`. Il est possible de préciser `chemin = ...` pour réutiliser une base existante, notamment dans le cache. Les archives téléchargées sont conservées dans `tools::R_user_dir("parlementR", "cache")`.

## Données disponibles

| Institution | Jeu | Actualisation | Stockage principal |
|---|---|---|---|
| Assemblée nationale | Députés actifs (AMO10) | `actualiser(db, jeux = "deputes")` | `an_deputes` |
| Assemblée nationale | Historique des acteurs | `actualiser(db, jeux = "historique")` | `an_historique` |
| Assemblée nationale | Réunions | `actualiser(db, jeux = "reunions")` | `an_reunions` |
| Assemblée nationale | Scrutins | `actualiser(db, jeux = "scrutins")` | `an_votes` |
| Assemblée nationale | Dossiers législatifs | `actualiser(db, jeux = "dossiers")` | `an_dossiers` |
| Assemblée nationale | Amendements | `actualiser(db, jeux = "amendements")` | `an_amendements` |
| Assemblée nationale | Débats Syceron | `actualiser(db, jeux = "debats")` | `an_debats` |
| Sénat | Amendements AMELI | `actualiser(db, jeux = "ameli")` | `sen_amendements` |
| Sénat | Sénateurs | `actualiser(db, jeux = "senateurs")` | `senateurs` |
| Sénat | Questions | `actualiser(db, jeux = "questions_senat")` | `sen_questions` |
| Assemblée nationale | Questions QE, QOSD, QG | `actualiser(db, jeux = "questions_an")` | `an_questions.questions` |
| Sénat | Dossiers DOSLEG / DOLE | `actualiser(db, jeux = "dosleg")` / `actualiser(db, jeux = "dole")` | `sen_dosleg` / `dila_dole` |

L’import initial de certains jeux peut prendre plusieurs minutes. Pour les débats Syceron, une archive observée le 12 novembre 2026 contenait 660 fichiers XML (environ 59 Mo). Les effectifs changent au fil des publications.

## Exemples d’analyse

```r
# Après importation des jeux correspondants :
actualiser(db = db, jeux = "amendements")
rapport_legislatif(db, "logement", pdf = FALSE)

# Exploitation des débats déjà importés :
DBI::dbGetQuery(db, "SELECT count(*) AS seances FROM an_debats.debats_seances")
```

Les recherches parlementaires sont exécutées sur la base locale. Les liens entre scrutins et amendements ne sont pas encore reconstruits automatiquement à partir des débats.

## Documentation et reproductibilité

- [Guide : comprendre la vie parlementaire](vignettes/comprendre-vie-parlementaire.Rmd)
- [Documentation des sources et des imports](vignettes/documentation.Rmd)
- [À propos du projet](vignettes/apropos.Rmd)
- [Historique des versions](NEWS.md)
- [Fiche AMO10](inst/documentation/sources/AMO10.md) et [inventaire JSON](inst/documentation/sources/AMO10_champs.csv)
- [Fiche Syceron](inst/documentation/sources/Syceron.md)
- [Fiche Amendements](inst/documentation/sources/Amendements.md)
- [Fiche AMELI — amendements du Sénat](inst/documentation/sources/AMELI.md)
- [Fiche Dossiers législatifs](inst/documentation/Dossiers_legislatifs.md)

La table `suivi.actualisations` conserve les dates de dernière importation et de contrôle, ainsi que les métadonnées HTTP disponibles. Ce suivi n’est pas un archivage exhaustif de toutes les versions des données.

## Extension DuckDB ICU

Certaines dates avec fuseau horaire nécessitent l’extension DuckDB `icu`. Son installation est une opération ponctuelle, à répéter éventuellement après une mise à jour de DuckDB :

```r
db = DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
DBI::dbExecute(db, "INSTALL icu")
DBI::dbDisconnect(db, shutdown = TRUE)
```

`connexion()` charge ensuite l’extension installée avec `LOAD icu`, sans téléchargement.

Site du projet : <https://gilles13.github.io/parlementR/>
