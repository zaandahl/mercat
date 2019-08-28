library(testthat) 
data_path <- "./data/"

context("Mercat Graph functions")

# Tests for exported functions





test_that("spol distance is correct",{
  # test data
  ancestor <- c(1, 0, 0, 1)
  descendant <- c(0, 0, 0, 0)
  
  # expectation for m_get_spol_dist_from_ancestor
  expect_equal(m_get_spol_dist_from_ancestor(ancestor, descendant), 1)
  expect_that(m_get_spol_dist_from_ancestor(descendant, ancestor), 
              gives_warning())
  
  # test cases from archived test files
  ancestor <-   "1111111"
  descendant <- "1101001"
  expect_equal(m_spol_dist(ancestor, descendant), 2)

  ancestor <-   "1100101"
  descendant <- "1100011"
  expect_equal(m_spol_dist(ancestor, descendant), 2)

  ancestor <-   "1100111"
  descendant <- "1000011"
  expect_equal(m_spol_dist(ancestor, descendant), 1)


  ancestor <-   "1100111000"
  descendant <- "1000011111"
  expect_equal(m_spol_dist(ancestor, descendant), 2)

    
  # expectations for all distance types
  a <- "253533233433427"
  b <- "243413342212437"
  expect_equal(m_get_dist(a, b, "L1"), 13)
  expect_equal(m_get_dist(a, b, "hamming"), 10)
  expect_equal(m_get_dist(a, b, "note"), 1)
  expect_that(m_get_dist(a, b, "random-string"), gives_warning())

  # L1 test cases from archived tests
  a <- "11111"
  b <- "10001"
  expect_equal(m_get_dist(a, b, "L1"), 3)

  a <- "123456789"
  b <- "112345678"
  expect_equal(m_get_dist(a, b, "L1"), 8)

  a <- "A123456789"
  b <- "112345678F"
  expect_equal(m_get_dist(a, b, "L1"), 15)

  a <- "fff"
  b <- "111"
  expect_equal(m_get_dist(a, b, "L1"), 42)
  expect_equal(m_get_dist(b, a, "L1"), 42)

  # Hamming test cases from archived tests
  a <- "11111"
  b <- "10001"
  expect_equal(m_get_dist(a, b, "hamming"), 3)

  a <- "RRRRRSSSSS"
  b <- "RRSRRSSRSS"
  expect_equal(m_get_dist(a, b, "hamming"), 2)

  a <- "A123456789"
  b <- "112345678X"
  expect_equal(m_get_dist(a, b, "hamming"), 2)

  a <- "R"
  b <- "S"
  expect_equal(m_get_dist(a, b, "hamming"), 1)
})

# test_that("minimum distance is correct", {
#   # getMinimumDistance for all distance types

# })

test_that("m_get_dist_matrix works", {
  # test data from archived tests
  result <- matrix(1, ncol = 8, nrow = 8)
  result[2, 1] <- result[7, 6] <- 0
  result <- as.dist(result, diag = FALSE, upper = FALSE)  
  type_a <- c("1", "1", "2", "3", "4", "5", "5", "6")
  test.mat <- m_get_dist_matrix(type_a, "hamming")
  expect_true(all(test.mat == result))

  result <- matrix(2, ncol = 8, nrow = 8)
  result[2, 1] <- result[3, 1] <- result[4, 2] <- result[6, 3] <- 1
  result[7, 2] <- result[7, 4] <- result[8, 6] <- result[8, 7] <- 1
  result <- as.dist(result, diag = FALSE, upper = FALSE)
  type_b <- c("16", "12", "66", "42", "50", "63", "72", "73")
  test.mat <- m_get_dist_matrix(type_b, "hamming")  
  expect_true(all(test.mat == result))

  result <- matrix(2, ncol = 8, nrow = 8)
  result[3, 1] <- result[4, 2] <- result[6, 2] <- result[6, 4] <- 0
  result[5, 1] <- result[7, 1] <- result[8, 2] <- result[5, 3] <- 1
  result[8, 4] <- result[8, 5] <- result[8, 7] <- result[7, 3] <- result[8, 6] <- 1
  result[2, 1] <- result[3, 2] <- result[6, 1] <- result[4, 1] <- 3
  result[4, 3] <- result[6, 3] <- 3
  result <- as.dist(result, diag = FALSE, upper = FALSE)
  type_c <- c("SRR", "RSS", "SRR", "RSS", "RRR", "RSS", "SRS", "RRS")
  test.mat <- m_get_dist_matrix(type_c, "hamming")  
  expect_true(all(test.mat == result))
})

# test_that("m_expand_by_res", {
#   # expectations for concentricRingsLayoutSizes, sortByRes, getRoots
# })

# test_that("m_res_spol_forest", {
#   # expectations for getCandidateParents, getUniqueRandomParent, 
#   # getReducedSet
# })

# test_that("m_mst_from_data", {
#   # expectations for processWeights, weightedDistFromData, 
#   # defaultDistance, graphFromDist
# })
