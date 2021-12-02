test_that("symmetric matrix check works", {

  expect_message(x <- is.symmetric(1:9), "Not a matrix")
  expect_false(x)

  expect_message(x <- is.symmetric(matrix(1:8, nrow = 2)), "Not a square matrix")
  expect_false(x)

  M <- low_to_matrix(c(1,12,2,13,23,3))

  expect_message(x <- is.symmetric(M), NA)
  expect_true(x)

  M[1,3] <- 99

  expect_message(x <- is.symmetric(M), "Not symmetric matrix \\(between positions \\[1,3\\] and \\[3,1\\])\\.")
  expect_false(x)
})
