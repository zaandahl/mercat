library(testthat) 
data_path <- "./data/"

context("Mercat Analysis functions")

# Tests for exported functions

# test m_spol
test_that("m_spol works", {
  infile <- paste0(data_path, "Neverland.smef")
  neverland <- m_read(infile, validate = FALSE)
  dim_n <- dim(m_spol(neverland))
  expected_dim <- c(6, 44)
  expect_true(all.equal(dim_n, expected_dim))
  expect_true(max(m_spol(neverland)[,1]) == 6)
  expect_true(colnames(m_spol(neverland))[1] == "COUNT")
})

# test m_mlva
test_that("m_mlva works", {
  infile <- paste0(data_path, "Monteserin.smef")
  mont <- m_read(infile, validate = FALSE)
  dim_m <- dim(m_mlva(mont))
  expected_dim <- c(10, 15)
  expect_true(all.equal(dim_m, expected_dim))
  expect_equal(colnames(m_mlva(mont))[9], "802")
  expect_equal(max(m_mlva(mont), na.rm = TRUE), 93)
  expect_equal(min(m_mlva(mont), na.rm = TRUE), 1)
  expect_true(is.na(m_mlva(mont)[10, "3192"]))
})

# test m_res 
test_that("m_res works", {
  infile <- paste0(data_path, "Neverland.smef")
  neverland <- m_read(infile, validate = FALSE)
  dim_r <- dim(m_res(neverland))
  expected_dim <- c(6, 3)
  nl1 <- as.integer(m_res(neverland)[5, 1]) # remove names for comparison
  nl2 <- as.integer(m_res(neverland)[4, 2])
  expect_true(all.equal(dim_r, expected_dim))
  expect_equal(colnames(m_res(neverland))[3], "A2")
  expect_equal(nl1, 2)
  expect_equal(nl2, 1)
  expect_true(is.na(m_res(neverland)[1, "A1"]))
})

# test m_res_correlation
test_that("m_res_correlation works", {
  infile <- paste0(data_path, "Neverland.smef")
  neverland <- m_read(infile, validate = FALSE)
  test_res <- m_res_correlation(neverland)
  pa1 <- 0.7
  pa2 <- 0.6
  pa1a2 <- 0.4
  r2 <- (pa1a2 - (pa1 * pa2))^2 / (pa1 * (1 - pa1) * pa2 * (1 - pa2)) 
  expect_true(all.equal(r2, test_res[2, 1]))

  perfect_r2 = 1
  infile <- paste0(data_path, "perfect-correlation.smef")
  merdata <- m_read(infile, validate = FALSE)
  test_res <- m_res_correlation(merdata)
  expect_true(all.equal(perfect_r2, test_res[2, 1]))

  infile <- paste0(data_path, "independent.smef")
  merdata <- m_read(infile, validate = FALSE)
  test_res <- m_res_correlation(merdata)
  expect_true(all.equal(perfect_r2, test_res[2, 1]))
})

# Tests for non-exported functions

# test m_get_r_squared
test_that("m_get_r_squared works", {
  expect_equal(m_get_r_squared(p_a = 0.2, p_b = 0.2, p_ab = 0.2), 1)
  expect_equal(m_get_r_squared(p_a = 0, p_b = 0, p_ab = 0), 1)
  expect_equal(m_get_r_squared(p_a = 1, p_b = 1, p_ab = 1), 1)
  expect_equal(m_get_r_squared(p_a = 1, p_b = 0, p_ab = 0), 0)
  expect_equal(m_get_r_squared(p_a = 0, p_b = 1, p_ab = 0), 0)
  expect_equal(m_get_r_squared(p_a = 0.2, p_b = 0.5, p_ab = 0.15), 0.0625,
                               tolerance = 1e-4)
})


# test m_get_res_freq 
test_that("m_get_res_freq works", {
  res1 <- c(1, 0, 1, 0, 1, 1, 1, 1)
  res2 <- c(1, 1, 0, 1, 0, 1, 1, 1)
  res3 <- c(1, 0, 0, 1, NA, 1, 0, 0)
  resmat <- data.frame(res1, res2, res3)
  res_freq1 <- m_get_res_freq(resmat, 1, 2)
  res_freq2 <- m_get_res_freq(resmat, 1, 3)
  res_freq3 <- m_get_res_freq(resmat, 2, 3)
  expect_equal(res_freq1$a_freq, 0.75)
  expect_equal(res_freq1$b_freq, 0.75)
  expect_equal(res_freq1$ab_freq, 0.5)
  expect_equal(res_freq2$a_freq, 0.75)
  expect_equal(res_freq2$b_freq, 0.375)
  expect_equal(res_freq2$ab_freq, 0.25)
  expect_equal(res_freq3$a_freq, 0.75)
  expect_equal(res_freq3$b_freq, 0.375)
  expect_equal(res_freq3$ab_freq, 0.375)
  expect_error(m_get_res_freq(resmat, 1, 8))
  expect_error(m_get_res_freq(resmat, -1, 1))
})


