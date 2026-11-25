# Importer seulement les articles de textes JORF selectionnes

Deux lectures de l'archive globale (structures puis articles). Ne touche
pas aux tables JORF historiques et refuse les sections non resolues.

## Usage

``` r
importer_jorf_cible(ids, archive, db = NULL, limite = 100L)
```

## Arguments

- ids:

  Identifiants JORFTEXT des textes cibles.

- archive:

  Archive JORF tar.gz deja presente localement.

- db:

  Connexion DuckDB facultative.

- limite:

  Nombre maximal de textes par appel (securite).
