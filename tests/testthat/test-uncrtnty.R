test_that("uncrtnty object integrity is ok", {
  x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
  u <- u_from_xpdb(x)

  expect_s3_class(u, "uncrtnty")
  expect_named(u, c("model", "nid", "nobs", "th_est", "th_unc", "om_est", "om_unc", "si_est", "si_unc"))

  expect_type(u$model, "character")
  expect_length(u$model, 1)

  expect_type(u$nid, "integer")
  expect_length(u$nid, 1)

  expect_type(u$nobs, "integer")
  expect_length(u$nobs, 1)

  expect_type(u$th_est, "double")

  expect_true(is.matrix(u$th_unc))

  expect_true(all(sapply(u$om_est, is.matrix)))
  expect_type(u$om_unc, "integer")
  expect_equal(length(u$om_est), length(u$om_unc))

  expect_true(all(sapply(u$si_est, is.matrix)))
  expect_type(u$si_unc, "integer")
  expect_equal(length(u$si_est), length(u$si_unc))
})

testthat::test_that("uncrnrty validator errors well", {

  expect_error(uncrtnty(th_est = 1, th_unc = low_to_matrix(c(1, 12, 2))), "`th_est` is of length 1 but `th_unc` is a 2x2 matrix\\.")

  expect_error(uncrtnty(om_est = matrix(1), om_unc = 1), "is.list\\(x\\$om_est\\)")
  expect_error(uncrtnty(om_est = list(matrix(1), matrix(2)), om_unc = 10), "`om_est` is made of 2 matrix/ces, but `om_unc` is a length 1 vector\\.")
  expect_error(uncrtnty(nid = 9, om_est = list(matrix(1)), om_unc = 10), "Omega degrees of freedom cannot exceed the number of subjects \\(9\\)\\.")
  expect_error(uncrtnty(om_est = list(matrix(c(1,0.1,0.1,2), nrow = 2)), om_unc = 1), "Omega degrees of freedom cannot be lower than the dimension of the matrix \\(`om_est`\\)\\.")

  expect_error(uncrtnty(si_est = list(matrix(1), matrix(2)), si_unc = 10), "`si_est` is made of 2 matrix/ces, but `si_unc` is a length 1 vector\\.")
  expect_error(uncrtnty(nobs = 99, si_est = list(matrix(1)), si_unc = 100), "Sigma degrees of freedom cannot exceed the number of observations \\(99\\)\\.")
  expect_error(uncrtnty(si_est = list(matrix(c(1,0.1,0.1,2), nrow = 2)), si_unc = 1), "Sigma degrees of freedom cannot be lower than the dimension of the matrix \\(`si_est`\\)\\.")

})
