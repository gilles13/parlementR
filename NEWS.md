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
