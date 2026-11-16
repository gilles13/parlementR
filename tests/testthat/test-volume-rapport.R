test_that("le garde-fou arrete les rapports trop volumineux", {
  expect_silent(parlementR:::.verifier_volume_rapport(100L))
  expect_warning(parlementR:::.verifier_volume_rapport(101L), "101 amendements")
  expect_warning(parlementR:::.verifier_volume_rapport(500L), "500 amendements")
  expect_error(parlementR:::.verifier_volume_rapport(501L), "Rapport non produit")
})

test_that("rapport_amdt_senat controle le volume avant le rendu PDF", {
  references = structure(list(amendements = data.frame(id = seq_len(501L))),
    class = "parlementR_sujet")
  expect_identical(rapport_amdt_senat(references, pdf = FALSE), references)
  expect_error(rapport_amdt_senat(references, ouvrir = FALSE),
    "501 amendements")
})


test_that("les seuils et unites sont adaptables", {
  expect_silent(parlementR:::.verifier_volume_rapport(700L, 700L, 1500L, "senateurs"))
  expect_warning(parlementR:::.verifier_volume_rapport(701L, 700L, 1500L, "senateurs"), "701 senateurs")
  expect_error(parlementR:::.verifier_volume_rapport(1501L, 700L, 1500L, "senateurs"), "1501 senateurs")
})

test_that("rapport_docs bloque avant la preparation du PDF", {
  x = structure(data.frame(id = seq_len(501L)), class = c("parlementR_documents", "data.frame"))
  expect_identical(rapport_docs(x, pdf = FALSE), x)
  expect_error(rapport_docs(x, ouvrir = FALSE), "501 documents")
})

test_that("rapport_amdt_an compte les amendements distincts", {
  colonnes = c("uid", "numero", "auteur", "groupe", "date_depot", "etat",
    "sort", "texte", "article", "dispositif", "expose")
  x = as.data.frame(setNames(rep(list(rep("x", 501L)), length(colonnes)), colonnes))
  x$uid = as.character(seq_len(501L))
  expect_identical(rapport_amdt_an(x, pdf = FALSE), x)
  expect_error(rapport_amdt_an(x, ouvrir = FALSE), "501 amendements")
})
