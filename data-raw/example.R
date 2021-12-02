u_example <-  uncrtnty(
  model  = "example001",
  nid = 100,
  nobs = 1000,
  th_est = c(111, 22, 333, 4.44),
  th_unc = matrix(c(1.11, 0.12, 0.13, 0.14, 0.12, 2, 0.23, 0.24, 0.13, 0.23, 33, 0.34, 0.14, 0.24, 0.34, 4.4), nrow = 4, ncol = 4),
  om_est = list(matrix(c(1, 0.12, 0.12, 2), nrow = 2), matrix(3)),
  om_unc = c(45, 67),
  si_est = list(matrix(c(0.05, 1, 1, 10), nrow = 2)),
  si_unc = 789
)
usethis::use_data(u_example, overwrite = TRUE)
