#' Build uncrtnty-list from xpdb object
#'
#' @param xpdb an object of "xpose_data" class
#'
#' @return an uncrtnty-list object
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' u <- u_from_xpdb(x)
#' class(u)
u_from_xpdb <- function(xpdb){
  stopifnot(inherits(xpdb, "xpose_data"))
  blocks <- parse_blockform_from_lst(get_lst(xpdb))
  p_ext <- parse_ext(get_ext(xpdb))
  p_cov <- parse_cov(get_cov(xpdb))

  omega <- matrix_to_list(p_ext$omega, blockform = blocks$omega)
  sigma <- matrix_to_list(p_ext$sigma, blockform = blocks$sigma)

  ans <- list(
    model  = xpdb$summary$value[xpdb$summary$label=="run"],
    th_est = p_ext$theta,
    th_unc = p_cov$theta,
    om_est = omega,
    om_unc = mapply(compute_df, omega, matrix_to_list(p_cov$omega, blockform = blocks$omega)),
    si_est = sigma,
    si_unc = mapply(compute_df, sigma, matrix_to_list(p_cov$sigma, blockform = blocks$sigma))
  )
  class(ans) <- "uncrtnty"
  return(ans)
}
