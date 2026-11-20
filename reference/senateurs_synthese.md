# Produire les syntheses statistiques des senateurs en PDF

Une page par theme, a partir de la base DuckDB locale. Les groupes sont
presentes sous leur nom complet suivi du sigle lorsqu'il est connu.

## Usage

``` r
senateurs_synthese(
  type = c("actif", "anciens", "autres", "tous"),
  groupe = NULL,
  syntheses = c("groupes", "sexe", "ages", "professions", "territoires", "commissions"),
  sortie = file.path("/tmp", paste0("parlementR-senateurs-synthese-", match.arg(type),
    ".pdf")),
  ouvrir = TRUE,
  db = NULL
)
```

## Arguments

- type:

  Population : actif, anciens, autres ou tous.

- groupe:

  Code ou libelle de groupe a retenir, ou NULL pour tous.

- syntheses:

  Themes a produire : groupes, sexe, ages, professions, territoires,
  commissions.

- sortie:

  Chemin du PDF, dans /tmp par defaut.

- ouvrir:

  Ouvrir le document apres production.

- db:

  Connexion DuckDB facultative ; NULL utilise connexion().

## Value

Chemin du PDF, invisiblement.
