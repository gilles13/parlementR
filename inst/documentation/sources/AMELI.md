# AMELI — Amendements du Sénat

## Source officielle et périmètre

- Producteur : Sénat, portail [data.senat.fr](https://data.senat.fr/ameli/).
- Archive intégrale : `https://data.senat.fr/data/ameli/ameli.zip` ; fichier interne `var/opt/opendata/ameli.sql`.
- Notice officielle : [notice explicative AMELI](https://data.senat.fr/aide/notice-explicative-ameli/).
- Importeur : `R/importer_ameli.R`, appelé par `actualiser(jeux = "ameli")`.
- Stockage : schéma DuckDB `sen_amendements`, **29 tables** conservées avec leurs noms d'origine.

AMELI est distinct du jeu d'amendements de l'Assemblée nationale (`an_amendements`). Les deux sources ne sont pas fusionnées automatiquement.

## Importation et dépendances

Le ZIP contient un dump **PostgreSQL en LATIN1**, et non un CSV. L'importeur :

1. extrait le dump dans un répertoire temporaire avec `unzip` ;
2. démarre une instance PostgreSQL temporaire (`initdb`, `pg_ctl`, `psql`) avec socket Unix privée, et crée les rôles attendus par le dump (`opendata`, `postgres`) ;
3. restaure le dump dans une base PostgreSQL temporaire LATIN1 ;
4. vérifie les 29 tables et leurs effectifs ;
5. transfère les tables par lots via `RPostgres` et `DBI` vers DuckDB, dans une transaction unique ;
6. vérifie les effectifs transférés avant validation de la transaction.

Les exécutables `initdb`, `pg_ctl`, `psql` et `unzip` doivent être accessibles dans le `PATH`. Le package R `RPostgres` est nécessaire. L'importation ne convertit pas manuellement les instructions SQL du Sénat.

Le dump commence par des suppressions de contraintes absentes d'une base vierge : les erreurs de cette phase initiale sont attendues. L'importeur refuse les erreurs SQL détectées à partir de la création des tables. Les fichiers PostgreSQL temporaires sont supprimés à la fin de l'opération.

## Utilisation depuis R

```r
# Téléchargement / mise à jour depuis le Sénat :
actualiser(jeux = "ameli")

# Réutiliser une archive locale, sans téléchargement :
actualiser(jeux = "ameli", archive = "/chemin/ameli.zip")

# Importer directement un ZIP local :
importer_ameli(archive = "/chemin/ameli.zip", remplacer = TRUE)

# Contrôles locaux dans DuckDB :
db = connexion()
DBI::dbGetQuery(db, "SELECT count(*) AS n FROM sen_amendements.amd")
DBI::dbGetQuery(db, "SELECT * FROM sen_amendements.sor")
```

`actualiser()` applique un délai de contrôle de **sept jours** à AMELI (24 heures pour les autres jeux ordinaires). Il utilise le cache persistant, les métadonnées HTTP disponibles et l'empreinte MD5 pour éviter un import inutile lorsque l'archive est inchangée. `forcer = TRUE` permet de vérifier la source avant l'échéance, sans imposer une réimportation identique. Le suivi est enregistré dans `suivi.actualisations` sous le jeu `ameli`.

## Tables et relations utiles

| Table | Rôle |
|---|---|
| `amd` | Amendements : identifiant `id`, numéro `num`, date de dépôt `datdep`, dispositif `dis`, objet `obj` |
| `txt_ameli` | Textes législatifs, liés par `amd.txtid = txt_ameli.id` |
| `sor` | Sorts des amendements, liés par `amd.sorid = sor.id` |
| `sub` | Subdivisions des textes, liées par `amd.subid = sub.id` |
| `amdsen` | Associations entre amendements et sénateurs |
| `sen_ameli` | Sénateurs présents dans le référentiel AMELI |
| `ent` | Entités institutionnelles utilisées dans plusieurs relations |

Les autres tables (commissions, groupes, séances, avis et nomenclatures) sont également conservées. Les identifiants AMELI ne doivent pas être assimilés sans contrôle à ceux du schéma distinct `senateurs`.

## Recherche locale et format des textes

Les champs `amd.obj` et `amd.dis` contiennent du **HTML**, parfois avec des entités comme `&#233;` ou `&#171;`. Les valeurs originales sont conservées ; les traitements d'affichage et de recherche devront tenir compte de cet encodage. Une recherche SQL `ILIKE` sur le contenu brut est possible, mais ne remplace pas un décodage HTML pour une recherche textuelle complète.

```r
db = connexion()
DBI::dbGetQuery(db, "
  SELECT id, num, datdep, obj
  FROM sen_amendements.amd
  WHERE obj ILIKE '%casseur-payeur%'
     OR dis ILIKE '%casseur-payeur%'
  ORDER BY datdep DESC
  LIMIT 10
")
```

Les recherches s'exécutent **entièrement dans DuckDB**, sans appel réseau à la demande.

## Contrôles observés le 13 novembre 2026

Sur l'archive testée :

| Indicateur | Valeur |
|---|---:|
| Tables restaurées et transférées | 29 |
| Lignes au total | 2 540 242 |
| Amendements (`amd`) | 276 970 |
| Associations (`amdsen`) | 2 051 574 |
| Textes (`txt_ameli`) | 9 066 |
| Première date de dépôt observée | 5 octobre 2001 |
| Dernière date de dépôt observée | 12 novembre 2026 |
| Dispositifs non NULL | 251 086 |
| Objets non NULL | 236 898 |

Les 29 effectifs PostgreSQL et DuckDB étaient identiques. Aucun lien orphelin n'a été relevé sur les références non NULL `amd.txtid` et `amd.sorid`. Une recherche de « casseur-payeur » a retrouvé l'amendement `id = 1331863`, numéro `1`, déposé le 22 octobre 2026.

**Ces effectifs sont des observations datées, pas des constantes à imposer aux mises à jour.** Les contraintes relationnelles du dump PostgreSQL ne sont pas recréées comme contraintes DuckDB : les tables et les valeurs sont transférées, et les contrôles de cohérence doivent rester explicites.
