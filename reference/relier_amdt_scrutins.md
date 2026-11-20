# Relier des amendements aux scrutins publics de l'Assemblee nationale

Liaison locale exclusivement par les UID officiels declares dans
objet.referenceLegislative.amendementsRefs.amendementRef du scrutin. Les
scrutins sans reference directe ne sont pas attribues par conjecture.

## Usage

``` r
relier_amdt_scrutins(amendements, db = NULL, groupes = FALSE)
```

## Arguments

- amendements:

  Tibble retourne par rech_amdt_an(), ou vecteur d'UID officiels
  d'amendements.

- db:

  Connexion DuckDB locale.

- groupes:

  Si TRUE, ne retourner que les scrutins effectivement lies, avec une
  ligne par groupe. Les amendements sans scrutin sont exclus. Par defaut
  FALSE : conserver tous les amendements et leur statut.

## Value

Tibble contenant uid_amendement, uid_scrutin, statut_liaison et, si
demande, les positions des groupes.
