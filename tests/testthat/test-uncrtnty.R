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
  expect_type(u$om_unc, "double")
  expect_equal(length(u$om_est), length(u$om_unc))

  expect_true(all(sapply(u$si_est, is.matrix)))
  expect_type(u$si_unc, "double")
  expect_equal(length(u$si_est), length(u$si_unc))
})
