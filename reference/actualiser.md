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
  forcer = FALSE,
  autoriser_regression = FALSE
)
```

## Arguments

- db:

  Connexion DBI facultative ; NULL initialise la base au besoin.

- jeux:

  Un ou plusieurs jeux : "deputes", "historique", "scrutins",
  "dossiers", "amendements", "senateurs", "dosleg", "dole",
  "questions_senat", "questions_an", "debats", "reunions", "ameli",
  "rne", "legi", "photos_an", "photos_senat".

- cache:

  Repertoire de cache persistant.

- archive:

  Chemin facultatif vers une archive locale (un seul jeu). Pour LEGI,
  archive globale ou quotidienne tar.gz ; sans archive, telechargement
  DILA.

- verbose:

  Afficher les etapes (par defaut TRUE).

- forcer:

  Ignorer le delai de controle (30 jours pour les photos, 7 jours pour
  AMELI, 24 heures pour les autres jeux soumis a ce delai) et verifier
  le serveur ; ne force pas l'import si l'archive est inchangee.

- autoriser_regression:

  Autoriser explicitement la restauration d'une archive locale
  differente ou d'anciennete inconnue. FALSE par defaut.
