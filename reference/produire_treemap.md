# Treemap des deputes par groupe politique

Les effectifs et les libelles sont extraits des mandats actifs AMO10.
Les couleurs sont celles du catalogue et de l hemicycle.

## Usage

``` r
produire_treemap(db = NULL, titre = "Assemblée nationale", fichier = NULL)
```

## Arguments

- db:

  Connexion DBI facultative ; NULL utilise connexion().

- titre:

  Titre du graphique.

- fichier:

  Chemin facultatif pour exporter le graphique (PDF, PNG, etc.).

## Value

Un objet ggplot, invisiblement si fichier est renseigne.
