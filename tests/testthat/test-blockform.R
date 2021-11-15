test_that("blockform works", {
  m1 <- matrix(c(1,0,0,0,2,0,0,0,3), ncol = 3)
  m2 <- matrix(c(4,0,0,0,4,0,0,0,5), ncol = 3)
  m3 <- matrix(c(6,7,0,7,8,0,0,0,9), ncol = 3)
  m4 <- matrix(c(9,9,0,0,9,9,0,0,0,0,9,9,0,0,9,9), ncol = 4)
  m5 <- matrix(8)
  m6 <- matrix(c(1,0,0,0,0,0,0,0,0,0,
                 0,2,0,0,0,0,0,0,0,0,
                 0,0,3,2,0,0,0,0,0,0,
                 0,0,2,4,0,0,0,0,0,0,
                 0,0,0,0,11,0,0,0,0,0,
                 0,0,0,0,0,111,0,0,0,0,
                 0,0,0,0,0,0,5,2,3,0,
                 0,0,0,0,0,0,2,6,2,0,
                 0,0,0,0,0,0,3,2,7,0,
                 0,0,0,0,0,0,0,0,0,99), ncol = 10
  )
  m7 <- matrix(c(1,0.8,0.7,0.8,2,0,0.7,0,3), ncol = 3)
  m8 <- matrix(c(11,0,0,14,0,22,0,0,0,0,33,0,14,0,0,44), ncol = 4)
  m9 <- matrix(c(11,0,0,0,0,22,0,24,0,0,33,0,0,24,0,44), ncol = 4)
  m10 <- matrix(c(1,0,0,0,0,0,0,0,2,0,24,0,0,0,0,0,3,0,0,0,0,0,24,0,4,0,0,0,0,0,0,0,5,0,57,0,0,0,0,0,6,0,0,0,0,0,57,0,7), ncol = 7)
  #  m11 <- bmat(1, 0.1, 2, 0, 0, 1, 0, 0, 0.1, 2)
  #  m12 <- mrgsolve:::SUPERMATRIX(list(bmat(1, 0, 2, 3, 0, 4), bmat(1, 0, 2, 3, 0, 4)))

  testthat::expect_equal(blockform(m1), c(1,2,3))
  testthat::expect_equal(blockform(m2), c(1,1,2))
  testthat::expect_equal(blockform(m3), c(1,1,2))
  testthat::expect_equal(blockform(m4), c(1,1,1,1))
  testthat::expect_equal(blockform(m5), c(1))
  testthat::expect_equal(blockform(m6), c(1,2,3,3,4,5,6,6,6,7))
  testthat::expect_equal(blockform(m7), c(1,1,1))
  testthat::expect_equal(blockform(m8), c(1,1,1,1))
  testthat::expect_equal(blockform(m9), c(1,2,2,2))
  testthat::expect_equal(blockform(m10), c(1,2,2,2,3,3,3))
  #  testthat::expect_equal(blockform(m11), c(1,1,1,1))
  #  testthat::expect_equal(blockform(m12), c(1,1,1,1,1,1))
})
