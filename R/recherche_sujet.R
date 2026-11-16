# Metadonnees de la recherche : conservees hors des colonnes des tableaux.
.sujet_recherche = function(x, sujet = NULL, filtres = NULL) {
  attr(x, "sujet") = sujet
  if (!is.null(filtres)) attr(x, "filtres") = filtres
  x
}

# Le titre peut etre explicitement remplace par le producteur.
.titre_recherche = function(x, sujet = NULL) {
  if (!is.null(sujet)) return(sujet)
  attr(x, "sujet")
}
