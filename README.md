# parlementR

Explorer les donnees parlementaires francaises avec R, a partir de sources officielles et d'une base DuckDB locale.

## Installation

```r
install.packages("remotes")
remotes::install_github("gilles13/parlementR")
```

## Actualiser les donnees des deputes

```r
library(parlementR)
actualiser(jeux = "deputes")
```

Les donnees sont telechargees puis conservees localement. Les recherches et analyses utilisent la base DuckDB locale.

Pour decouvrir les autres fonctions, consulter les [vignettes du package](https://gilles13.github.io/parlementR/articles/) et la cheatsheet generee avec `produire_cheatsheet()`.
