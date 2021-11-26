test_that("infer_blockform works", {
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

  testthat::expect_equal(infer_blockform(m1), c(1,2,3))
  testthat::expect_equal(infer_blockform(m2), c(1,1,2))
  testthat::expect_equal(infer_blockform(m3), c(1,1,2))
  testthat::expect_equal(infer_blockform(m4), c(1,1,1,1))
  testthat::expect_equal(infer_blockform(m5), c(1))
  testthat::expect_equal(infer_blockform(m6), c(1,2,3,3,4,5,6,6,6,7))
  testthat::expect_equal(infer_blockform(m7), c(1,1,1))
  testthat::expect_equal(infer_blockform(m8), c(1,1,1,1))
  testthat::expect_equal(infer_blockform(m9), c(1,2,2,2))
  testthat::expect_equal(infer_blockform(m10), c(1,2,2,2,3,3,3))
  #  testthat::expect_equal(infer_blockform(m11), c(1,1,1,1))
  #  testthat::expect_equal(infer_blockform(m12), c(1,1,1,1,1,1))
})

test_that("blockform are parsed from lst", {

  lst003a <- readLines(system.file("nm", "run003a.lst", package = "uncrtnty"))
  lst003b <- readLines(system.file("nm", "run003b.lst", package = "uncrtnty"))
  lst003c <- readLines(system.file("nm", "run003c.lst", package = "uncrtnty"))
  lst003d <- readLines(system.file("nm", "run003d.lst", package = "uncrtnty"))
  lst003e <- readLines(system.file("nm", "run003e.lst", package = "uncrtnty"))
  lst003f <- readLines(system.file("nm", "run003f.lst", package = "uncrtnty"))
  lst003g <- readLines(system.file("nm", "run003g.lst", package = "uncrtnty"))
  lst003h <- readLines(system.file("nm", "run003h.lst", package = "uncrtnty"))
  lst003i <- readLines(system.file("nm", "run003i.lst", package = "uncrtnty"))

  expect_equal(parse_lst(lst003a), list(nid = 1, nobs = 0, om_blockform = c(1, 1, 2, 3, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 9, 9), si_blockform = c(1, 1, 2, 3, 3, 4, 5)))
  expect_equal(parse_lst(lst003b), list(nid = 1, nobs = 0, om_blockform = c(1, 1, 2, 3, 3, 3, 4, 5, 6, 6, 7, 8, 9, 9, 9, 9), si_blockform = c(1, 2, 3)))
  expect_error(parse_lst(lst003c), "Cannot found both \\$OMEGA and \\$SIGMA in the model")
  expect_equal(parse_lst(lst003d), list(nid = 1, nobs = 0, om_blockform = c(1, 2, 3, 4), si_blockform = c(1, 1, 2, 3, 3, 4, 5)))
  expect_equal(parse_lst(lst003e), list(nid = 1, nobs = 0, om_blockform = c(1, 2, 3, 4), si_blockform = c(1, 2, 3)))
  expect_error(parse_lst(lst003f), "Cannot found both \\$OMEGA and \\$SIGMA in the model")
  expect_error(parse_lst(lst003g), "Cannot found both \\$OMEGA and \\$SIGMA in the model")
  expect_error(parse_lst(lst003h), "Cannot found both \\$OMEGA and \\$SIGMA in the model")
  expect_error(parse_lst(lst003i), "Cannot found both \\$OMEGA and \\$SIGMA in the model")
})

test_that("matrix_to_list works", {
  m <- matrix(c(1,0.1,0,0,0.1,2,0,0,0,0,.3,0,0,0,0,.4), ncol = 4)
  expect_equal(matrix_to_list(m), list(matrix(c(1,0.1,0.1,2), ncol = 2), matrix(0.3), matrix(0.4)))
  expect_equal(matrix_to_list(m, blockform = c(1,1,1,1)), list(m))
  expect_equal(matrix_to_list(m, blockform = c(1,2,2,3)), list(matrix(1), diag(c(2, .3)), matrix(.4)))
  expect_error(matrix_to_list(m, blockform = c(1,1, 2)), "Matrix length \\(4\\) is different from blockform length \\(3\\)\\.")
})
