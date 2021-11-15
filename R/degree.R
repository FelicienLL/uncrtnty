compute_df <- function(est, se, add_1 = FALSE){
  stopifnot(length(est) == length(se), is.logical(add_1), all(class(est)==class(se)))
  if(is.matrix(est) & is.matrix(se)){
    est <- diag(est)
    se <- diag(se)
  }
  df <- 2 * (sqrt(est) / se)^2 + as.integer(add_1)
  round(max(df), 0)
}
