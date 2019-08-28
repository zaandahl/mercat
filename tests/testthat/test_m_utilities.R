library(testthat)
data_path <- "./data/"

context("Mercat Utility Functions")

testthat::test_that("m_oct2bin works", {
  expect_that(m_oct2bin(""), throws_error())
  expect_that(m_oct2bin("012345678912345"), throws_error())
  expect_that(m_oct2bin("777743677760771"), 
    testthat::equals("1111111111111000111101111111111100001111111"))
  expect_that(m_oct2bin("77774367776077111"), gives_warning())
})

testthat::test_that("m_hex2bin works", {
  expect_that(m_hex2bin(""), throws_error())
  expect_that(m_hex2bin("7F-7E-1E-FF-F0"), throws_error())
  expect_that(m_hex2bin("7F-7E-1E-FF-F0-FF"), 
    testthat::equals("1111111111111000111101111111111100001111111"))
  expect_that(m_hex2bin("7F-7E-1E-FF-F0-FF-00"), gives_warning())
})

testthat::test_that("m_antibiotic_full_names works", {
  expect_that(m_antibiotic_full_names(c("RMP", "CFR", "SM")),
    testthat::equals(c("rifampicin", "cefadroxil", "streptomycin")))
  expect_that(m_antibiotic_full_names("TRB"),
    testthat::equals("terbinafine"))
  expect_that(m_antibiotic_full_names(2),
    testthat::equals("2"))
  expect_that(m_antibiotic_full_names(c("JUNK", "INH")),
    testthat::equals(c("JUNK", "isoniazid")))
})

testthat::test_that("m_mlva_names works", {
  mont <- m_read(paste0(data_path, "Monteserin.smef"), validate = FALSE)
  expect_that(m_mlva_names(mont)[1],
    testthat::equals("Mtub04"))
})

testthat::test_that("m_export_forest works", {
  testthat::skip_on_cran()
  mont <- m_read(paste0(data_path, "Monteserin.smef"), validate = FALSE)
  expect_that(m_export_forest(mont, "links")[9, 2],
    testthat::equals(62))
  expect_that(m_export_forest(mont, "nodes")[34, 3],
    testthat::equals("RSSSS"))
})

testthat::test_that("m_export_mst works", {
  testthat::skip_on_cran()
  mont <- m_read(paste0(data_path, "Monteserin.smef"), validate = FALSE)
  expect_that(m_export_mst(mont, "links")[27, 3],
    testthat::equals(13))
  expect_that(m_export_mst(mont, "nodes")[22, 3],
    testthat::equals("1111111111111111111100001111011100001111111"))
})