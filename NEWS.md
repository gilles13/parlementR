# Documentation AMELI (2026-11-13)

- Documentation de la source officielle AMELI du Sénat, de ses 29 tables DuckDB, de la restauration PostgreSQL temporaire et de l'actualisation hebdomadaire.
- Ajout des exemples REPL, des contrôles observés le 13 novembre 2026 et des limites des champs HTML.

# parlementR 0.0.13 (2026-11-12)

- `groupes = TRUE` exclut les amendements sans scrutin lie et retourne un tableau vide type si aucun scrutin n'est lie, plutot que de creer des lignes de votes manquantes.
- Les 660 XML Syceron inspectes ne comportent ni `amendementRef` ni `scrutinRef` : aucune liaison de secours par les debats n'est ajoutee sans cle documentee.

# parlementR 0.0.12 (2026-11-12)

- Liaison amendement-scrutin uniquement par UID officiel reference dans le scrutin.
- Suppression des rapprochements par numero et seance ; absence de reference explicite signalee.
- Une ligne par couple amendement-scrutin par defaut ; groupes en option.

# parlementR 0.0.11 (2026-11-12)

- Liaison locale amendements-scrutins par numero et reference de seance ; ambiguite explicite, positions des groupes.
- Aucune correspondance par nom ou titre de projet de loi.

# parlementR 0.0.10 (2026-11-12)

- Integration de `rechercher_amendements()` avec `colonnes = "identification"` pour retrouver les UID officiels.
- Recherche uniquement locale dans DuckDB.

# parlementR 0.0.9 (2026-11-12)

- Suppression de toute jointure entre scrutin et amendement fondee sur un nom de personne ou le titre d’un texte.
- Rapprochement par references structurees de seance et numero ; ambiguite et absence de liaison explicites.
- Argument `amendement_uid` pour une association officielle explicite lorsque les jeux locaux ne comportent pas de cle commune exploitable.

# parlementR 0.0.8 (2026-11-12)

- Le rapport de scrutin restitue l'intégralité du dispositif et de l'exposé sommaire, en paragraphes et avec décodage HTML, sans troncature.
- Ajout des métadonnées disponibles (article, dépôt, état, références de séance et d'examen) et des cosignataires résolus localement.
- La mesure proposée est séparée des résultats du vote ; aucun contexte thématique n'est codé en dur.

# parlementR 0.0.7 (2026-11-12)

- `produire_rapport_vote()` retrouve localement l’amendement associé au scrutin en croisant numéro, auteur et titre du document législatif ; exposé et dispositif sont décodés depuis le HTML officiel.
- Le résultat `pdf = FALSE` contient désormais `contexte`, avec statut explicite si la correspondance ne peut pas être établie.

# parlementR 0.0.6 (2026-11-12)

- `produire_rapport_vote()` : rapport PDF de scrutin avec résultats, groupes et députés nominatifs, à partir de DuckDB local et du modèle PDF commun.
- `pdf = FALSE` permet de vérifier les tableaux avant génération.

# parlementR 0.0.5 (2026-11-12)
- Décodage des entités HTML dans les extraits d’amendements, correction des accents, de la typographie des dates et UID et des coupures de fiches PDF.

# parlementR 0.0.4 (2026-11-12)
- Nouvelle fonction `produire_suivi_amendement()` : PDF de synthese des resultats de recherche d’amendements, utilisant le moteur et le modele PDF communs.

# parlementR 0.0.3 (2026-11-12)
- Nouvelle fonction exportée `graphique_hemicycle()` : répartition schématique des sièges par groupe, enveloppes convexes transparentes et légende des sigles.
- Intégration du graphique sur la première page du PDF `fiche_deputes()`, sous le tableau récapitulatif, à partir des effectifs AMO10.
- Dépendances `dplyr` et `ggplot2` déclarées dans `Imports`.

# parlementR 0.0.2 (2026-11-12)

## Données et traçabilité

- Import des débats Syceron de l’Assemblée nationale (XVIIe législature) dans `an_debats`, avec conservation des XML sources et suivi des actualisations.
- Documentation des sources officielles, des archives téléchargées, des tables DuckDB et des limites connues.
- Fiche des députés : encadré de provenance (archive AMO10, source, dates de suivi) ; présentation compacte de l’URL et correction des accents dans les correctifs successifs.

## Documents et maintenance

- Génération de la cheatsheet sans dépendance à l’ancien package `assembleer`.
- Corrections des avertissements de portabilité et de qualification des fonctions R.
- Première mise en place du versionnement explicite du package et réorganisation de sa page d’accueil.

## Notes

- Les dates et versions des *données* sont indépendantes de la version du package. `suivi.actualisations` conserve l’état de suivi des jeux importés, mais ne constitue pas encore un historique exhaustif de toutes les versions d’archives.
- Cette entrée récapitule les évolutions réalisées au cours de la phase `0.0.1` ; elle ne garantit pas que tous les correctifs locaux aient été appliqués à une même copie de travail.
