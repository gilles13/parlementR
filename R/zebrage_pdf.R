# Coloration explicite des lignes de donnees, y compris dans longtable.
.zebrer_tableau_pdf = function(latex) {
  lignes = strsplit(latex, "\n", fixed = TRUE)[[1]]
  donnees = FALSE
  n = 0L
  for (i in seq_along(lignes)) {
    x = trimws(lignes[i])
    if (grepl("\\midrule", x, fixed = TRUE) ||
        grepl("\\endhead", x, fixed = TRUE)) {
      donnees = TRUE
      next
    }
    if (grepl("\\bottomrule", x, fixed = TRUE) ||
        grepl("\\end{", x, fixed = TRUE)) donnees = FALSE
    if (donnees && endsWith(x, "\\\\")) {
      n = n + 1L
      if (n %% 2L == 0L) lignes[i] = paste0("\\rowcolor{gray!12} ", lignes[i])
    }
  }
  paste(lignes, collapse = "\n")
}
