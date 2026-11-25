# parlementR

Explorer les données parlementaires françaises avec R, à partir de sources officielles et d'une base DuckDB locale.

## Installation

```r
install.packages("remotes")
remotes::install_github("gilles13/parlementR")
```

## Actualiser les données des députes

```r
library(parlementR)
actualiser(jeux = "deputes")
```

Les données sont téléchargées puis conservées localement. Les recherches et analyses utilisent la base DuckDB locale.

Pour découvrir les autres fonctions, consulter les [vignettes du package](https://gilles13.github.io/parlementR/articles/) et la cheatsheet génerée avec `produire_cheatsheet()`.
