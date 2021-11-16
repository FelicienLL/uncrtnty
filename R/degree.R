#' Compute the degree of freedom for Inverse Wishart distribution
#'
#' @param est a variance-covariance matrix of estimation (i.e. OMEGA² or SIGMA²)
#' @param se a matrix of the standard errors of each element of `est`
#' @param add_1 add +1 to the computation of degree of freedom. See details.
#'
#' @details The function uses a formula "courtesy of Mats Karlsson", found in Robert J. Bauer's "NONMEM Tutorial Part II" (CPT:Pharmacometrics Syst Pharmacol (2019), 8, 538–556 ; Supplementary Materials S1 - Part C). If the dimension of the matrix is > 1, the lowest degree of freedom is selected. The degree of freedom value cannot be lower than the dimension of the matrix. The degree of freedom value should not be higher than the number of individual in the original data for an "OMEGA matrix", or the number of observation for a "SIGMA matrix".
#'
#' @return a single integer value
#' @export
#'
#' @examples
#' est_om <- matrix(c(0.2, 0.01, 0.01, 0.1), ncol = 2)
#' se_om <- matrix(c(0.02, 0.005, 0.005, 0.03), ncol = 2)
#' compute_df(est = est_om, se = se_om)
compute_df <- function(est, se, add_1 = FALSE){
  stopifnot(length(est) == length(se), is.logical(add_1), all(class(est)==class(se)))
  if(is.matrix(est) & is.matrix(se)){
    est <- diag(est)
    se <- diag(se)
  }
  df <- 2 * (est / se)^2 + as.integer(add_1)
  df <- max(round(min(df), 0), length(df))
  df
}
