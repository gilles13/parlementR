# Syceron — comptes rendus des débats de l’Assemblée nationale

## Source et cache

- Fournisseur : Assemblée nationale, données ouvertes, XVIIe législature.
- Archive : `https://data.assemblee-nationale.fr/static/openData/repository/17/vp/syceronbrut/syseron.xml.zip` (**`vp` en minuscules**).
- Cache par défaut : `file.path(tools::R_user_dir("parlementR", "cache"), "syseron.xml.zip")`.
- Entrée du package : `actualiser(jeux = "debats")`, ou `actualiser(jeux = "debats", archive = archive)` pour utiliser un ZIP déjà téléchargé.

Le mécanisme commun d’`actualiser()` conserve le suivi dans `suivi.actualisations`, applique la fenêtre de contrôle de 24 h et utilise les en-têtes HTTP pour éviter les téléchargements inutiles. L'analyse et les recherches s’effectuent **exclusivement en local**.

## Modèle DuckDB

L'importateur `R/importer_debats.R` lit directement chaque XML dans le ZIP via `xml2::read_xml(unz(...))`, sans extraction préalable.

- `an_debats.debats_seances` : fichier source (clé primaire), UID, références de séance/session, métadonnées JSON et XML intégral sans espaces de noms.
- `an_debats.debats_points` : points et sous-points de séance, positions, identifiants Syceron, identifiant parent, attributs JSON et titres.
- `an_debats.debats_paragraphes` : interventions, identifiants Syceron, référence au point, acteur, ordre de séance, code grammatical, attributs JSON, orateurs et textes XML/texte.

Les colonnes `fichier, position` forment les clés primaires des points et paragraphes. L'XML intégral est conservé pour permettre l'exploitation ultérieure de références non encore modélisées, notamment les amendements et les scrutins. **Aucune correspondance amendement–scrutin n'est déduite automatiquement lors de cet import.**

## Import, mise à jour et contrôle

```r
cache = tools::R_user_dir("parlementR", "cache")
archive = file.path(cache, "syseron.xml.zip")
actualiser(db = db, jeux = "debats", archive = archive)

DBI::dbGetQuery(db, "
  SELECT 'seances' AS table_nom, count(*) AS n FROM an_debats.debats_seances
  UNION ALL SELECT 'points', count(*) FROM an_debats.debats_points
  UNION ALL SELECT 'paragraphes', count(*) FROM an_debats.debats_paragraphes
")
```

`actualiser(jeux = "debats")` télécharge l'archive officielle lors de la première mise en place ou lorsqu'une mise à jour est détectée. L'importateur saute les fichiers déjà présents lorsqu'il est appelé avec `remplacer = FALSE`. Via `actualiser()`, les fichiers présents sont remplacés lors d'une réimportation nécessaire. Les opérations sont journalisées avec un timestamp au début, tous les dix XML et à la fin.

**Retour d'expérience du 12 novembre 2026** : 660 XML (58,9 Mo), 660 séances, 34 245 points, 360 952 paragraphes, soit 395 857 lignes. Temps d'import observé : environ 5 à 8 minutes. Ces effectifs décrivent l'archive de cette date, pas une constante.

## Limites

L'importateur conserve les XML complets mais n'en extrait qu'une partie des champs structurés. Les références parlementaires devront être vérifiées sur les XML réels avant toute construction de liens entre amendements et scrutins. Une réimportation ne purge pas automatiquement les fichiers qui auraient disparu de l'archive officielle.
