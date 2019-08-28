library(testthat)
data_path <- "./data/"

context("Mercat Core Functions")
# http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/
# This function returns TRUE wherever elements are the same, including NA's,
# and FALSE everywhere else.
compare_na <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}

# Tests for exported functions

# test m_aggregate
test_that("m_aggregate works", {
  infile <- paste0(data_path, "Monteserin.smef")
  mont <- m_read(infile, validate = FALSE)
  mont_mlva <- m_aggregate(mont, "MLVA")
  mont_spol <- m_aggregate(mont, "SPOL")
  mont_res_note <- m_aggregate(mont, c("RES", "NOTE"))
  expect_equal(summary(mont)$FULL$genotypes, 87)
  expect_equal(summary(mont_mlva)$FULL$genotypes, 66)
  expect_equal(summary(mont_spol)$FULL$genotypes, 43)
  expect_equal(summary(mont_res_note)$FULL$genotypes, 44)
})

# test S3 summary (not bothering with print or m_summary because of side effects)
test_that("S3 summary works", {
  infile <- paste0(data_path, "Monteserin.smef")
  mont <- m_read(infile, validate = FALSE)
  mont_sum <- summary(mont)
  expect_equal(mont_sum$FULL$virtual_heterozygosity, 0.9852)
  expect_equal(mont_sum$SPOL$rti_n_1, 0.57)
  expect_equal(length(mont_sum$MLVA15$loci_freq), 15)
  expect_equal(mont_sum$RES$genotypes, 17)
  expect_equal(mont_sum$NOTE$singletons, 3)
})

# Tests for non-exported functions

# test m_calc_theta (for now just testing that the output is the same)
test_that("m_calc_theta works", {
  expect_equal(m_calc_theta(4, 2), 0.8793802, tolerance = 1e-4)
  expect_equal(m_calc_theta(12, 4), 1.672153, tolerance = 1e-4)
})

# thest m_virtual_heterozygosity works
test_that("m_virtual_heterozygosity works", {
  cvec1 <- c(1, 4, 5, 2, 3, 10)
  test_vh <- 1 - sum((cvec1 / sum(cvec1))^2)
  expect_equal(m_virtual_heterozygosity(cvec1), test_vh)
})

test_that("m_hunter_gaston_index works", {
	expect_error(m_hunter_gaston_index(c("A")))
	expect_error(m_hunter_gaston_index(c(-1, 1)))
	expect_equal(m_hunter_gaston_index(c(c(40, 30, 7, 7), rep(1, 16))),
               0.7460606, tolerance = 1e-4)
	expect_equal(m_hunter_gaston_index(rep(5, 20)),
               0.959596, tolerance = 1e-4)
})

test_that("Hunter Gaston Index for mercat data", {
	infile <- paste0(data_path, "Taype.smef")
	merdata <- m_read(infile, validate = FALSE)
	# expect_equal(m_general_hunter_gaston(merdata, marker = "SPOL"),
 #               0.997, tolerance = 1e-4)
  expect_equal(summary(merdata)$FULL$hunter_gaston,
               0.997, tolerance = 1e-3)
})

test_that("m_get_res_mat works", {
  res_data <- gtools::permutations(2, 4, c("R", "S"), repeats.allowed = T)
  res_data <- res_data %>%
    apply(1, function(x) paste(x, sep = "", collapse = ""))
  out_mat <- gtools::permutations(2, 4, c(0, 1), repeats.allowed = T)
  expect_equal(m_get_res_mat(rev(res_data)), out_mat)
})


test_that("m_make_matrix works", {
	teststring <- c("1100", "abcd", "zxcv")
	correctmat <- matrix(c(1, 1, 0, 0,
                        10, 11, 12, 13,
                        NA, NA, 12, 31),
                      byrow = TRUE, ncol = 4)      	
	testmat <- m_make_matrix(teststring)
	expect_true(apply(compare_na(correctmat, testmat), 1, all) %>% all)
})

test_that("m_hunter_gaston_index works", {
   testtype <- c("00", "01", "10", "11")
   testcount <- c(1, 2, 1, 1)
   testhg <- c(m_hunter_gaston_index(c(3, 2)),
                  m_hunter_gaston_index(c(2, 3)))
   expect_equal(as.vector(m_calc_specific_hg(testtype, testcount)), testhg)
	
})

test_that("m_summary_res works", {
	details <- data.frame(COUNTRY = "Neverland", AUTHOR = "Peter Pan")
	coredata <- data.frame(COUNT = rep(10, 8), 
	                      RES = c("SSS", "SSR", "SRS", "SRR",
                                "RSS", "RSR", "RRS", "RRR"), 
	                      ID=c("a", "b", "c", "d", "e", "f", "g", "h"))
	d <- m_create(details = details, data = coredata)
	correctfreq <- list(res_frequency = rep(0.5, 3))
	testfreq <- m_summary_res(d, "RES")
	expect_equal(testfreq$res_frequency, correctfreq$res_frequency)
})

# test_that("summary by type works correctly", {
# 	# test data
# 	# m_summary_res, summaryNOTE, summaryMLVA, summarySPOL, summaryCommon
# })

