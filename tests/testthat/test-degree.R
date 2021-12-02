test_that("degree works", {
  expect_equal(compute_df(est = 0.2, se = 0.05), 32)
  expect_equal(compute_df(est = matrix(0.2), se = matrix(0.05)), 32)
  expect_error(compute_df(est = matrix(0.2), se = 0.05))
  expect_error(compute_df(est = matrix(c(0.2, 0, 0, 0.5), ncol = 2), se = matrix(0.05)))
  expect_equal(compute_df(est = matrix(c(0.2, 0, 0, 0.5), ncol = 2), se =matrix(c(0.05, 0, 0, 0.1), ncol = 2)), 32)
  expect_equal(compute_df(est = 0.5, se = 0.1), 50)
  expect_equal(compute_df(est = 0.5, se = 0.1, maxdf = 49), 49)
})
