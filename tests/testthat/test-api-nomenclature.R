test_that("le sujet est conserve dans les resultats structures", {
  d = tibble::tibble(uid = "A", titre = "Logement")
  x = parlementR:::.sujet_recherche(d, "logement", list(groupe = "G"))
  expect_s3_class(x, "tbl_df")
  expect_identical(attr(x, "sujet"), "logement")
  expect_identical(attr(x, "filtres")$groupe, "G")
  expect_identical(parlementR:::.titre_recherche(x), "logement")
  expect_identical(parlementR:::.titre_recherche(x, "Titre personnalise"), "Titre personnalise")
})

test_that("les deux producteurs retournent les donnees sans PDF", {
  a = structure(list(sujet = "logement"), class = "parlementR_sujet")
  a = parlementR:::.sujet_recherche(a, "logement")
  expect_identical(rapport_amdt_senat(a, pdf = FALSE), a)
  d = tibble::tibble(source = "ameli")
  class(d) = c("parlementR_documents", class(d))
  d = parlementR:::.sujet_recherche(d, "logement")
  expect_identical(rapport_docs(d, pdf = FALSE), d)
})
