# Convertit les valeurs JSON scalaires et les objets XML serialises en texte.
# Ne transforme jamais un objet complexe en chaine JSON affichee dans les PDF.
.valeur_parlementaire = function(x, champ = NULL) {
  extraire = function(v, champ = NULL) {
    if (is.null(v) || length(v) == 0L) return(NA_character_)
    if (is.atomic(v)) {
      if (length(v) != 1L || is.na(v)) return(NA_character_)
      z = trimws(as.character(v))
      if (!nzchar(z) || tolower(z) %in% c("null", "na")) return(NA_character_)
      return(z)
    }
    if (!is.list(v) || is.null(names(v))) return(NA_character_)
    if (identical(tolower(as.character(v[["@xsi:nil"]])), "true"))
      return(NA_character_)
    cles = unique(c(champ, "#text", "text", "valeur", "libelle", "libelleCourt",
                    "nomCanonique", "sort", "statut", "code", "titre"))
    for (cle in cles) {
      if (!is.null(v[[cle]])) {
        z = extraire(v[[cle]])
        if (!is.na(z)) return(z)
      }
    }
    # Un seul champ metier simple : il peut representer le texte XML.
    noms = names(v)
    metier = which(!startsWith(noms, "@") & !startsWith(noms, "#"))
    if (length(metier) == 1L) return(extraire(v[[metier]]))
    NA_character_
  }
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(trimws(s))) return(NA_character_)
    s = trimws(s)
    if (!startsWith(s, "{") && !startsWith(s, "[")) return(extraire(s))
    objet = tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (is.null(objet)) return(NA_character_)
    extraire(objet, champ)
  }, character(1), USE.NAMES = FALSE)
}
