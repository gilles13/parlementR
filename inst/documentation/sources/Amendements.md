# Amendements — Assemblee nationale

## Source et perimetre

- Producteur : Assemblee nationale, donnees ouvertes.
- Archive actuellement utilisee : `https://data.assemblee-nationale.fr/static/openData/repository/17/loi/amendements_div_legis/Amendements.json.zip`.
- Perimetre du code actuel : XVIIe legislature. Les autres legislatures ne sont pas encore gerees par cet importeur.
- Importeur : `R/importer_amendements.R`, appele par `actualiser(jeux = "amendements")`.

## Stockage local

Table : `an_amendements.amendements`.

| Colonne | Contenu |
|---|---|
| `uid` | Identifiant de l'amendement, extrait de `$.amendement.uid` ou `$.uid` |
| `chemin` | Chemin du fichier JSON dans l'archive |
| `contenu` | Notice JSON integrale, de type DuckDB `JSON` |

La structure imbriquee du JSON est conservee, **sans normalisation relationnelle** des auteurs, cosignataires, textes, articles, statuts ou autres objets. La presence d'un champ dans le JSON ne signifie pas qu'il existe une colonne relationnelle correspondante.

## Controles et actualisation

L'importeur verifie l'integrite du ZIP, l'extraction, le nombre de notices, la presence et l'unicite des UID et la racine JSON `amendement`. L'import est effectue dans une transaction DuckDB.

`actualiser()` gere la frequence de verification (24 h par jeu), les empreintes MD5 et le journal `suivi.actualisations`. Un controle distant peut etre effectue sans nouvel import lorsque l'archive est inchangee.

## Exploitations

- Inventaire et recherche locale des amendements dans DuckDB.
- Analyse des textes, auteurs et sorts : les donnees brutes sont disponibles, mais les tables relationnelles et leurs jointures restent a construire et a valider.
- `rapport_legislatif()` : restitution thematique experimentale ; la couverture des relations entre amendements, documents et dossiers doit etre controlee.

## Travaux restants

Inventorier les chemins JSON observes et leurs frequences, identifier les cles officielles de liaison avec les documents et dossiers, puis construire les tables relationnelles avec des controles de couverture et d'unicite.
