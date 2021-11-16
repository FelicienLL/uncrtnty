#' Get uncertainty data from xpose object
#'
#' @param xpdb an object of "xpose_data" class
#'
#' @return a list of estimations and their uncertainties, suited to be passed to `simpar::simpar()`
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' xpdb_to_simpar(x)
xpdb_to_simpar <- function(xpdb){
  stopifnot(inherits(xpdb, "xpose_data"))
  blocks <- parse_blockform_from_lst(get_lst(xpdb))
  p_ext <- parse_ext(get_ext(xpdb))
  p_cov <- parse_cov(get_cov(xpdb))

  omega <- matrix_to_list(p_ext$omega, blockform = blocks$omega)
  sigma <- matrix_to_list(p_ext$sigma, blockform = blocks$sigma)

  list(
    theta = p_ext$theta,
    covar = p_cov$theta,
    omega = omega,
    sigma = sigma,
    odf = mapply(compute_df, omega, matrix_to_list(p_cov$omega, blockform = blocks$omega)),
    sdf = mapply(compute_df, sigma, matrix_to_list(p_cov$sigma, blockform = blocks$sigma))
  )
}


