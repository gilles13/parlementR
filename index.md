# parlementR

**Explorer les travaux parlementaires français à partir de données
publiques officielles, conservées et interrogées localement.**

`parlementR` est un package R de collecte, de documentation et d’analyse
des données de l’Assemblée nationale et du Sénat. Les archives
officielles sont téléchargées pour l’importation et les mises à jour ;
les recherches et traitements sont réalisés dans une base **DuckDB
locale**, sans requêtes d’analyse à distance.

## Installer

``` r

remotes::install_github("gilles13/parlementR")
```

Le package est en développement (`0.0.2`). L’installation depuis GitHub
nécessite le package `remotes`.

## Démarrer

``` r

library(parlementR)

db = connexion(creer = TRUE)
actualiser(db = db, jeux = "deputes")
cat_deputes(db = db, fichier = "/tmp/parlementR-deputes.pdf")
fermer_base()
```

[`connexion()`](https://gilles13.github.io/parlementR/reference/connexion.md)
utilise par défaut
`file.path(tools::R_user_dir("parlementR", "data"), "parlementR.duckdb")`.
Il est possible de préciser `chemin = ...` pour réutiliser une base
existante, notamment dans le cache. Les archives téléchargées sont
conservées dans `tools::R_user_dir("parlementR", "cache")`.

## Données disponibles

| Institution | Jeu | Actualisation | Stockage principal |
|----|----|----|----|
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

L’import initial de certains jeux peut prendre plusieurs minutes. Pour
les débats Syceron, une archive observée le 12 novembre 2026 contenait
660 fichiers XML (environ 59 Mo). Les effectifs changent au fil des
publications.

## Exemples d’analyse

``` r

# Après importation des jeux correspondants :
actualiser(db = db, jeux = "amendements")
rapport_legislatif_an(db, "logement", pdf = FALSE)

# Exploitation des débats déjà importés :
DBI::dbGetQuery(db, "SELECT count(*) AS seances FROM an_debats.debats_seances")
```

Les recherches parlementaires sont exécutées sur la base locale. Les
liens entre scrutins et amendements ne sont pas encore reconstruits
automatiquement à partir des débats.

## Documentation et reproductibilité

- [Guide : comprendre la vie
  parlementaire](https://gilles13.github.io/parlementR/vignettes/comprendre-vie-parlementaire.Rmd)
- [Documentation des sources et des
  imports](https://gilles13.github.io/parlementR/vignettes/documentation.Rmd)
- [À propos du
  projet](https://gilles13.github.io/parlementR/vignettes/apropos.Rmd)
- [Historique des
  versions](https://gilles13.github.io/parlementR/NEWS.md)
- [Fiche
  AMO10](https://gilles13.github.io/parlementR/inst/documentation/sources/AMO10.md)
  et [inventaire
  JSON](https://gilles13.github.io/parlementR/inst/documentation/sources/AMO10_champs.csv)
- [Fiche
  Syceron](https://gilles13.github.io/parlementR/inst/documentation/sources/Syceron.md)
- [Fiche
  Amendements](https://gilles13.github.io/parlementR/inst/documentation/sources/Amendements.md)
- [Fiche AMELI — amendements du
  Sénat](https://gilles13.github.io/parlementR/inst/documentation/sources/AMELI.md)
- [Fiche Dossiers
  législatifs](https://gilles13.github.io/parlementR/inst/documentation/Dossiers_legislatifs.md)

La table `suivi.actualisations` conserve les dates de dernière
importation et de contrôle, ainsi que les métadonnées HTTP disponibles.
Ce suivi n’est pas un archivage exhaustif de toutes les versions des
données.

## Extension DuckDB ICU

Certaines dates avec fuseau horaire nécessitent l’extension DuckDB
`icu`. Son installation est une opération ponctuelle, à répéter
éventuellement après une mise à jour de DuckDB :

``` r

db = DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
DBI::dbExecute(db, "INSTALL icu")
DBI::dbDisconnect(db, shutdown = TRUE)
```

[`connexion()`](https://gilles13.github.io/parlementR/reference/connexion.md)
charge ensuite l’extension installée avec `LOAD icu`, sans
téléchargement.

Site du projet : <https://gilles13.github.io/parlementR/>

## API de recherche et de production

``` r

a = rech_amdt_an(sujet = "logement")
attr(a, "sujet")
rapport_amdt_an(a)
rapport_amdt_an(a, pdf = FALSE)

docs = rech_docs("logement")
attr(docs, "sujet")
rapport_docs(docs)
rapport_docs(docs, pdf = FALSE)

s = rech_amdt_senat("logement")
rapport_amdt_senat(s)
rapport_amdt_senat(s, pdf = FALSE)
```

Les fonctions de recherche conservent le sujet dans l’attribut `sujet`.
Les rapports reprennent ce sujet automatiquement lorsque leur titre en
depend.

### Découvrir les parlementaires

``` r

# Après actualiser(jeux = c("deputes", "senateurs"))
rechercher_parlementaires("Dupont")
# Interface facultative : install.packages("shiny")
explorer()
```

L’application utilise uniquement les données déjà présentes dans DuckDB.

## Codes LEGI : articles et sommaires

Les PDF utilisent le logo parlementR et indiquent en pied de page la
source officielle cliquable, la date d’import et le MD5 de l’archive,
lorsque le suivi local les renseigne. La date d’import n’est pas la date
de consolidation juridique.

``` r

produire_code(code = "penal", article = "121-3")
produire_code(code = "civil", article = "1240")
sommaire_code(code = "penal", niveau = 3)
```

Les noms de codes sont normalises sans accents. Les PDF sont produits a
partir de DuckDB, sans requete reseau.
