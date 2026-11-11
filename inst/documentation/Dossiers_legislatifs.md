# Dossiers législatifs — Assemblée nationale

## 1. Source et périmètre

- Producteur : Assemblée nationale, portail [data.assemblee-nationale.fr](https://data.assemblee-nationale.fr/travaux-parlementaires/dossiers-legislatifs).
- Archive JSON : [`Dossiers_Legislatifs.json.zip`](https://data.assemblee-nationale.fr/static/openData/repository/17/loi/dossiers_legislatifs/Dossiers_Legislatifs.json.zip).
- Chemin de publication : `repository/17/loi/dossiers_legislatifs/` ; **ce chemin ne signifie pas que tous les documents ont `legislature = 17`**.
- Archive analysée : fichier transmis le 10 novembre 2026, `Dossiers_Legislatifs.json(1).zip`. Les effectifs ci-dessous décrivent cet instantané, pas une règle de validation permanente.
- Les fichiers contiennent des **notices structurées** de dossiers et documents parlementaires ; ils ne garantissent pas le texte intégral des publications.

## 2. Référentiel documentaire et XSD

La distribution officielle [`Schemas.zip`](https://data.assemblee-nationale.fr/static/openData/repository/SCHEMAS/Schemas.zip) contient une archive imbriquée `Schemas20161125-1554.zip` ; les XSD ne doivent donc pas être recherchés seulement parmi les fichiers HTML externes.

Fichiers XSD pertinents identifiés dans l'archive imbriquée :

- `Schemas/DossierLegislatif/DossierLegislatif.xsd` ;
- `Schemas/DossierLegislatif/ActeLegislatif.xsd`, `ActeLegislatifAbstrait.xsd` et les schémas sous `Actes_Legislatifs/` ;
- `Schemas/DocumentsLegislatifs/Documents.xsd`, `DocumentAbstrait.xsd`, `TexteLegislatif.xsd`, `RapportParlementaire.xsd` ;
- `Schemas/Loi_Interface.xsd` et les schémas de liens et références.

La documentation HTML archivée comporte également `Concepts_legislatifs/Dossier_Legislatif`, `Processus_Legislatif`, `Texte_Legislatif` et `Schemas_Entites/Loi/`.

**Attention aux versions :** les XSD embarqués sont datés de 2016, alors que les JSON analysés sont contemporains. Ils expliquent les concepts et une partie des structures, mais les champs réellement observés sont inventoriés séparément dans [`Dossiers_legislatifs_champs.csv`](Dossiers_legislatifs_champs.csv). Cet inventaire ne prétend pas établir la conformité XSD de chaque JSON.

## 3. Contenu réel de l'archive analysée

| Répertoire ZIP | Racine JSON | Nombre | UID distincts | UID manquants |
|---|---|---:|---:|---:|
| `json/dossierParlementaire/` | `dossierParlementaire` | 3 403 | 3 403 | 0 |
| `json/document/` | `document` | 7 630 | 7 630 | 0 |
| **Total** | | **11 033** | | **0** |

Tous les 11 033 fichiers ont été lus comme JSON lors de l'inventaire. La valeur `uid` est une chaîne dans les deux familles.

### Types de dossiers (`@xsi:type`)

| Type | Nombre |
|---|---:|
| `DossierLegislatif_Type` | 2 628 |
| `DossierResolutionAN` | 472 |
| `DossierMissionControle_Type` | 231 |
| `DossierMissionInformation_Type` | 28 |
| `DossierCommissionEnquete_Type` | 22 |
| `DossierIniativeExecutif_Type` | 20 |
| Attribut absent | 2 |

Deux dossiers sans attribut `@xsi:type` restent des dossiers complets : `DLR5L17N54196` et `DLR5L17N51919`. L'absence de cet attribut n'est pas un critère de rejet.

### Types de documents (`@xsi:type`)

| Type | Nombre |
|---|---:|
| `texteLoi_Type` | 6 400 |
| `rapportParlementaire_Type` | 1 148 |
| `accordInternational_Type` | 43 |
| `documentEtudeImpact_Type` | 21 |
| `avisConseilEtat_Type` | 18 |

**Notices de suppression :** `PRJLANR5L17BTC3188` et `PIONANR5L17BTC2806` possèdent un `uid` et `metadatas.dateSuppression`, mais pas `dossierRef`. Elles doivent être conservées. Dans cet instantané, 7 628 documents sur 7 630 possèdent `dossierRef` ; cette relation n'est pas obligatoire pour toute notice.

## 4. Modèle logique observé

Les dossiers comportent notamment `titreDossier`, `procedureParlementaire`, `initiateur`, `actesLegislatifs` et `fusionDossier`. Les actes législatifs sont **imbriqués** et peuvent décrire plusieurs étapes d'une procédure ; les aplatir prématurément ferait perdre leur structure et leur contexte.

Les documents comportent notamment `cycleDeVie`, `denominationStructurelle`, `provenance`, `titres`, `divisions`, `dossierRef`, `auteurs`, `organesReferents` et des propriétés propres à leur type. Certains champs peuvent être absents, nuls, objets, listes ou variantes de structure selon le document.

Les liens vers les acteurs et organes pourront être exploités avec AMO10, mais **aucune intégrité référentielle inter-archives n'est présumée** : les archives ont des périmètres et des temporalités distincts.

## 5. Import dans parlementR

Appel : `actualiser(db, jeux = "dossiers")` ou `actualiser(db, jeux = "dossiers", archive = "/chemin/vers/archive.zip")`.

1. Réutilisation de l'archive locale en cache, sinon téléchargement par `curl --http1.1` dans `tools::R_user_dir("parlementR", "cache")/assemblee_nationale/dossiers_legislatifs/`.
2. Vérification ZIP (`unzip -tq`), inventaire (`unzip -Z1`), validation des deux répertoires attendus et des chemins d'extraction.
3. Extraction dans un répertoire temporaire, vérification de l'existence de tous les fichiers JSON inventoriés.
4. Transaction DuckDB : `CREATE SCHEMA IF NOT EXISTS an_dossiers`, puis remplacement atomique des deux tables.
5. Lecture via `read_text()` et conversion du contenu en type `JSON` ; extraction du seul UID par `json_extract_string()` ; conservation du chemin relatif d'origine.
6. Vérification par famille : effectif importé égal au nombre de fichiers de l'archive, UID non nuls et distincts, racine JSON de type objet ; `COMMIT` si tout réussit, `ROLLBACK` en cas d'échec.

| Table | Colonnes |
|---|---|
| `an_dossiers.dossiers` | `uid`, `chemin`, `contenu` |
| `an_dossiers.documents` | `uid`, `chemin`, `contenu` |

`contenu` conserve toutes les propriétés JSON dans DuckDB, sans projection destructive. **La conversion au type JSON ne garantit pas une identité octet pour octet** avec le fichier d'origine : le ZIP en cache demeure la référence pour cette exigence. Les mandats, actes, auteurs et autres structures imbriquées ne sont pas décomposés en tables à ce stade.

Le code n'impose **aucun effectif fixe** (11 033 est seulement l'effectif observé). La présence des deux familles, la validité JSON et les UID sont contrôlés à chaque import.

## 6. Actualisation et limites

- Si un ZIP existe déjà dans le cache, il est réutilisé : le code **ne détecte pas automatiquement** les nouvelles versions publiées à la même URL.
- L'import remplace les deux tables de ce schéma lors d'un nouvel appel ; il ne fait pas d'historisation ni de traitement différentiel des notices supprimées.
- Le paramètre `archive` ne s'applique qu'à **un seul jeu** par appel ; `actualiser(db, jeux = c("deputes", "dossiers"))` utilise les caches respectifs.
- Les liens vers les dossiers, les acteurs, les organes et les textes externes ne sont pas encore validés fonctionnellement.
- Les effectifs et les distributions de types sont descriptifs ; ils ne doivent pas être codés comme contraintes immuables.

## 7. Protocole de maintenance lors d'un changement de source

1. Conserver l'ancien ZIP et le nouveau ZIP (date, taille, empreinte SHA-256) avant remplacement du cache.
2. Comparer les noms et nombres de fichiers par famille, les UID, les racines et `@xsi:type`.
3. Régénérer l'inventaire des chemins JSON et comparer fréquences, types, apparitions/disparitions ; vérifier les champs nuls, listes et objets.
4. Rechercher une nouvelle édition de `Schemas.zip`, puis comparer les XSD pertinents et la documentation HTML.
5. Vérifier les notices de suppression et les liens `dossierRef`, sans supposer qu'ils sont toujours présents.
6. Tester l'import dans une base DuckDB temporaire, comparer effectifs et échantillons de JSON ; vérifier que les tables existantes restent intactes en cas d'échec.
7. Adapter **seulement** les transformations devenues nécessaires, mettre à jour cette fiche et son CSV, puis publier le changement avec le code.

## 8. Vérifications reproductibles (REPL R)

```r
actualiser(db, jeux = "dossiers")
DBI::dbGetQuery(db, "SELECT count(*) AS n, count(DISTINCT uid) AS uid_distincts FROM an_dossiers.dossiers")
DBI::dbGetQuery(db, "SELECT count(*) AS n, count(DISTINCT uid) AS uid_distincts FROM an_dossiers.documents")
DBI::dbGetQuery(db, "SELECT json_extract_string(contenu, '$.document.metadatas.dateSuppression') AS date_suppression FROM an_dossiers.documents WHERE json_extract(contenu, '$.document.metadatas.dateSuppression') IS NOT NULL")
```

Les deux dernières requêtes doivent être interprétées relativement à l'archive réellement importée, et non comme des tests d'effectifs figés.
