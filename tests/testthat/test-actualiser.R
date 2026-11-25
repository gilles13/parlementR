test_that("Les fonctions publiques sont presentes", {
  expect_true(is.function(parlementR::actualiser))
  expect_true(is.function(parlementR::importer_questions_an))
  expect_true(is.function(parlementR::importer_questions_senat))
})

test_that("Les archives locales ne degradent pas une base existante", {
  con = DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE SCHEMA suivi")
  DBI::dbExecute(con, "CREATE TABLE suivi.actualisations (jeu VARCHAR, md5 VARCHAR)")
  DBI::dbExecute(con, "CREATE SCHEMA an_votes")
  DBI::dbExecute(con, "CREATE TABLE an_votes.scrutins (uid VARCHAR)")
  expect_true(parlementR:::.jeu_present(con, "scrutins", "an_votes.scrutins"))
  zip = tempfile(fileext = ".zip")
  d = tempfile(); dir.create(d)
  writeLines("x", file.path(d, "x.txt"))
  old = setwd(d); on.exit(setwd(old), add = TRUE)
  utils::zip(zip, "x.txt", flags = "-q")
  expect_error(parlementR:::.controle_archive_locale(con, "scrutins", zip, TRUE), "remplacement refuse")
  DBI::dbExecute(con, "INSERT INTO suivi.actualisations VALUES (?, ?)",
    params = list("scrutins", parlementR:::.empreinte_archive(zip)))
  expect_identical(parlementR:::.controle_archive_locale(con, "scrutins", zip, TRUE), "identique")
  expect_identical(parlementR:::.controle_archive_locale(con, "scrutins", zip, TRUE, TRUE), "identique")
  expect_false(parlementR:::.jeu_present(con, "questions_senat", "sen_questions"))
})

test_that("Les dates HTTP sont independantes de LC_TIME", {
  ancienne = Sys.getlocale("LC_TIME")
  x = sprintf("Tue, 01 %s 2026 12:00:00 GMT", month.abb)
  attendu = as.POSIXct(sprintf("2026-%02d-01 12:00:00", 1:12), tz = "UTC")
  expect_equal(parlementR:::.date_http(x), attendu)
  expect_true(all(is.na(parlementR:::.date_http(c(NA_character_, "", "invalide")))))
  expect_true(parlementR:::.date_http("Tue, 24 Nov 2026 16:00:00 GMT") >
    parlementR:::.date_http("Fri, 13 Nov 2026 12:00:00 GMT"))
  expect_identical(Sys.getlocale("LC_TIME"), ancienne)
})
