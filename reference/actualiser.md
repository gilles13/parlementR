# Actualiser les donnees parlementaires

Actualiser les donnees parlementaires

## Usage

``` r
actualiser(
  db = NULL,
  jeux = "deputes",
  cache = tools::R_user_dir("parlementR", "cache"),
  archive = NULL,
  verbose = TRUE,
  forcer = FALSE
)
```

## Arguments

- db:

  Connexion DBI facultative ; NULL initialise la base au besoin.

- jeux:

  Un ou plusieurs jeux : "deputes", "historique", "scrutins",
  "dossiers", "amendements", "senateurs", "dosleg", "dole", "questions",
  "debats".

- cache:

  Repertoire de cache persistant.

- archive:

  Chemin facultatif vers une archive ZIP locale (un seul jeu).

- verbose:

  Afficher les etapes (par defaut TRUE).

- forcer:

  Ignorer le delai de 24 heures et verifier le serveur ; ne force pas
  l'import si l'archive est inchangee.
