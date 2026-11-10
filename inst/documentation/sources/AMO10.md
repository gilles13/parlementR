# AMO10 — Députés en exercice, mandats et organes

## 1. Objet et périmètre

Cette fiche décrit **l'import AMO10 tel qu'implémenté dans `parlementR`**, et non l'ensemble des données disponibles à l'Assemblée nationale. Elle sert de référence pour diagnostiquer une modification du fournisseur, du format ou du code. L'archive contient des acteurs, leurs mandats imbriqués, des organes et des déports. L'intitulé « députés en exercice » décrit la sélection des acteurs, **pas** la période de validité de tous les organes ou mandats associés : les 7 122 organes comprennent des enregistrements historiques.

Source officielle : <https://data.assemblee-nationale.fr/acteurs/deputes-en-exercice>.

Archive JSON : <https://data.assemblee-nationale.fr/static/openData/repository/17/amo/deputes_actifs_mandats_actifs_organes/AMO10_deputes_actifs_mandats_actifs_organes.json.zip>.

Documentation officielle : <https://data.assemblee-nationale.fr/static/openData/repository/SCHEMAS/Schemas.zip>.

Les URL sont fixées dans le code ; elles ne prouvent pas que le contenu téléchargé ultérieurement sera identique.

## 2. Référence matérielle examinée

- Archive locale examinée : `AMO10_deputes_actifs_mandats_actifs_organes.json.zip`.
- SHA-256 de cette archive : `0dd99badebe668ae22b7742bd505f45c63250cafde005cdb86740c836b5b5a7f`.
- Inventaire : **574** fichiers `json/acteur/*.json`, **7122** fichiers `json/organe/*.json`, **30** fichiers `json/deport/*.json` ; total **7726**.
- Ces nombres sont un **instantané observé**, et non des constantes de validation dans l'importateur. Les prochains téléchargements peuvent produire d'autres effectifs.
- L'inventaire exhaustif des chemins terminaux observés, avec leur présence, leur nombre d'occurrences et leurs types, figure dans [`AMO10_champs.csv`](AMO10_champs.csv). Ce CSV est un **constat sur cette archive**, non une spécification normative. `[]` signifie un élément de tableau ; `null` signifie une valeur JSON nulle. Une présence inférieure au nombre de fichiers indique un champ absent d'au moins un document ; une occurrence supérieure au nombre de documents traduit notamment les listes imbriquées.

## 3. Documentation XSD : emplacement et limites

Dans `Schemas.zip`, une **archive ZIP imbriquée** `Schemas20161125-1554.zip` contient les XSD, notamment :

- `Schemas/ActeursOrganesMandats/Acteurs.xsd`, `Type_Acteur.xsd`, `Type_Adresse.xsd` ;
- `Schemas/ActeursOrganesMandats/Mandats.xsd`, `Type_MandatAbstrait.xsd`, `Type_MandatParlementaire.xsd`, `Type_MandatSimple.xsd`, `Type_MandatMission.xsd`, `Type_MandatAvecSuppleant.xsd` ;
- `Schemas/ActeursOrganesMandats/Organes.xsd`, `Type_OrganeAbstrait.xsd`, `Type_OrganeBase.xsd`, `Type_OrganeParlementaire.xsd` et autres types d'organes.

Les pages HTML de `Schemas.zip` sont des pages de documentation ; les fichiers XSD exploitables sont dans l'archive imbriquée. **Il s'agit de schémas XML datés de 2016**, pas d'une preuve que tous les documents JSON actuels leur sont strictement conformes. La conversion XML→JSON peut produire des propriétés telles que `@xmlns`, `@xsi:type` et `#text`, et des éléments répétés peuvent devenir des tableaux. Aucun contrôle XSD des JSON n'est exécuté par `parlementR`.

Le répertoire `deport` est observé dans l'archive AMO10. Il n'est pas couvert par les trois fichiers d'entrée `Acteurs.xsd`, `Mandats.xsd` et `Organes.xsd` : **ne pas supposer** qu'ils décrivent les déports.

## 4. Structure observée et identifiants

| Famille | Racine JSON | Chemin de l'UID | Table DuckDB |
|---|---|---|---|
| Acteurs | `acteur` | `$.acteur.uid."#text"` | `an_deputes.acteurs` |
| Organes | `organe` | `$.organe.uid` | `an_deputes.organes` |
| Déports | `deport` | `$.deport.uid` | `an_deputes.deports` |

L'UID des acteurs est représenté par un **objet** JSON avec `#text`, contrairement aux deux autres familles. Cette distinction est codée explicitement dans `R/importer_deputes.R`. Une uniformisation naïve du chemin d'extraction supprimerait les identifiants des acteurs.

Les acteurs contiennent notamment `etatCivil`, `profession`, `adresses`, `mandats`. Les mandats ne sont **pas** extraits dans une table séparée : leur structure complète demeure dans `contenu`. Les organes portent notamment `codeType`, `libelle`, `viMoDe`, `legislature`, des informations de localisation et parfois des attributs spécifiques à leur type. Les déports comportent notamment `refActeur`, `portee`, `lecture`, `instance`, `cible`, `explication`. Pour les chemins exacts et leur variabilité, consulter le CSV associé.

**Attention aux cardinalités** : une propriété JSON peut être `null`, un objet unique ou une liste selon le document. L'import ne force aucun schéma relationnel pour ces sous-structures. Les dates, nombres et booléens représentés sous forme de chaînes restent sous leur forme source dans `contenu`.

## 5. Traitement implémenté

Point d'entrée : `actualiser(db, jeux = "deputes", cache = ..., archive = NULL, verbose = TRUE)` dans `R/actualiser.R`, qui appelle `.importer_deputes()` dans `R/importer_deputes.R`.

1. Crée le cache dans `tools::R_user_dir("parlementR", "cache")/assemblee_nationale/amo10`.
2. Si `archive` est fournie, utilise ce fichier local. Sinon, réutilise le ZIP présent dans le cache ; s'il est absent, télécharge l'URL officielle avec `curl --http1.1 -fL --retry 5 --retry-all-errors`, écrit d'abord un fichier `.part`, puis le renomme.
3. Vérifie le ZIP avec `unzip -tq` ; inventorie les chemins avec `unzip -Z1` ; exige les trois familles et uniquement des JSON sous `json/(acteur|organe|deport)/`. Refuse les chemins traversant `..` et les doublons de noms JSON.
4. Extrait dans un répertoire temporaire et vérifie la présence des JSON annoncés.
5. Démarre une transaction DuckDB, crée si nécessaire le schéma `an_deputes` et remplace successivement les tables `acteurs`, `organes`, `deports` par `CREATE OR REPLACE TABLE ... AS SELECT ... FROM read_text(...)`.
6. Pour chaque fichier, stocke `uid` (extrait par `json_extract_string`), `chemin` (chemin relatif à la racine d'extraction) et `contenu` (`content::JSON`, document entier).
7. Compare le nombre de lignes au nombre de fichiers de la famille, vérifie `count(DISTINCT uid)`, l'absence d'UID nuls et que la racine JSON attendue est un objet. Valide la transaction seulement après les trois familles ; sinon effectue un `ROLLBACK`.
8. Supprime le répertoire temporaire en sortie, y compris en cas d'erreur ; **conserve le ZIP dans le cache**.

Les noms de colonnes et tables ci-dessus constituent le **contrat de stockage actuel**. Aucune colonne métier supplémentaire n'est créée. Les requêtes analytiques pourront utiliser les fonctions JSON de DuckDB ou, si le besoin se confirme, des vues documentées.

## 6. Ce que les contrôles garantissent — et ce qu'ils ne vérifient pas

Contrôles actuels : validité structurelle du ZIP ; trois familles présentes ; nombre de documents importés égal à l'inventaire ; UID non nuls et uniques **au sein de chaque famille** ; racine JSON de type objet ; import transactionnel.

Non contrôlés actuellement : conformité au XSD ; validité métier des dates ; correspondance entre `acteurRef`, `organeRef`, `refActeur` et les tables importées ; unicité globale des UID entre familles ; cardinalités des sous-objets ; conformité des codes de type ; stabilité du schéma entre deux publications ; fraîcheur du cache. L'archive est conservée pour pouvoir réexaminer les octets originaux ; la colonne `contenu` est de type `JSON` et **ne doit pas être présentée comme une copie octet pour octet du fichier original**.

Le cache est prioritaire : **`actualiser(db)` ne télécharge pas une version plus récente si le ZIP existe déjà**. Il s'agit donc aujourd'hui d'un import reproductible depuis le cache, et non d'un mécanisme de détection des mises à jour distantes.

## 7. Vérifications reproductibles dans R

```r
devtools::load_all()

dbdir = file.path(tools::R_user_dir("parlementR", "data"), "parlementR.duckdb")
dir.create(dirname(dbdir), recursive = TRUE, showWarnings = FALSE)
db = DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir)
actualiser(db)

DBI::dbGetQuery(db, "
  SELECT 'acteur' AS famille, count(*) AS n, count(DISTINCT uid) AS uniques
  FROM an_deputes.acteurs
  UNION ALL
  SELECT 'organe', count(*), count(DISTINCT uid)
  FROM an_deputes.organes
  UNION ALL
  SELECT 'deport', count(*), count(DISTINCT uid)
  FROM an_deputes.deports
")

DBI::dbGetQuery(db, "
  SELECT uid,
         json_extract_string(contenu, '$.acteur.etatCivil.ident.prenom') AS prenom,
         json_extract_string(contenu, '$.acteur.etatCivil.ident.nom') AS nom
  FROM an_deputes.acteurs LIMIT 5
")
```

La dernière requête est un exemple d'accès à la donnée imbriquée, **pas** une transformation appliquée pendant l'import. Fermer la connexion avec `DBI::dbDisconnect(db, shutdown = TRUE)` après les vérifications.

## 8. Procédure de maintenance si la source évolue

1. **Conserver les deux archives** (ancienne et nouvelle), calculer leurs SHA-256 et relever la date d'obtention. Ne pas écraser le ZIP de référence avant comparaison.
2. Comparer les inventaires (`unzip -Z1`), les nombres de fichiers par famille, les chemins JSON, les types et les cardinalités ; régénérer l'inventaire de champs avec la même convention que `AMO10_champs.csv`.
3. Vérifier en priorité les **racines** (`acteur`, `organe`, `deport`), la forme de `uid`, les noms de répertoires et les noms de fichiers : ces éléments conditionnent directement l'importateur.
4. Vérifier si les XSD officiels ont changé, **y compris l'archive imbriquée** ; distinguer une modification de la documentation XML d'une modification effectivement observée dans les JSON.
5. Tester l'import avec `actualiser(db, archive = "/chemin/vers/nouveau.zip")` sur **une base de test**, et vérifier les contrôles et les requêtes aval. Ne pas tester en écrasant la base de production.
6. Si une modification casse le contrat de stockage, corriger l'importateur et ses contrôles **dans le même changement**, documenter la migration éventuelle et ajouter un test de non-régression ciblé.
7. Actualiser cette fiche : URL, empreintes, effectifs, nouveaux chemins, changements de types, comportement de l'importateur et résultats de validation.

## 9. Décisions de conception et évolutions non réalisées

- **Conserver le JSON complet**, ne pas décomposer prématurément les mandats, adresses et organes en tables secondaires.
- **Limiter les colonnes matérialisées** à `uid`, `chemin`, `contenu` tant qu'aucun besoin métier récurrent ne justifie autre chose.
- **Une source = un schéma DuckDB** : ici `an_deputes`.
- Ne pas interpréter l'absence de champ ou `null` comme une erreur métier sans règle documentée.
- Évolutions possibles, **non implémentées** : suivi des versions et de la fraîcheur distante, manifeste d'import (URL, SHA-256, date, effectifs), contrôles de références croisées, tests de compatibilité des structures JSON, vues analytiques des mandats.

## 10. Historique

- **2026-11-10 — État initial** : import AMO10 validé sur 574 acteurs, 7 122 organes et 30 déports ; UID uniques et non nuls dans chaque famille ; documentation de la structure et des traitements.
