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
  #parse elements
  p_lst <- parse_lst(xpdb$code$code[xpdb$code$subroutine=="lst"])
  p_ext <- parse_ext(xpdb$files$data[xpdb$files$extension=="ext"][[1]])
  p_cov <- parse_cov(xpdb$files$data[xpdb$files$extension=="cov"][[1]])

  omega <- matrix_to_list(p_ext$omega, blockform = p_lst$om_blockform)
  sigma <- matrix_to_list(p_ext$sigma, blockform = p_lst$si_blockform)

  #make uncrtnty object
  uncrtnty(
    model  = xpdb$summary$value[xpdb$summary$label=="run"],
    nid    = p_lst$nid,
    nobs   = p_lst$nobs,
    th_est = p_ext$theta,
    th_unc = p_cov$theta,
    om_est = omega,
    om_unc = mapply(compute_df, omega, matrix_to_list(p_cov$omega, blockform = p_lst$om_blockform), MoreArgs = list(maxdf = p_lst$nid)),
    si_est = sigma,
    si_unc = mapply(compute_df, sigma, matrix_to_list(p_cov$sigma, blockform = p_lst$si_blockform), MoreArgs = list(maxdf = p_lst$nobs))
    )
}

# =============  UTILS to work on XPOSE objects =============


#' Extract .phi file from xpose object
#'
#' @param xpdb an object of "xpose_data" class
#'
#' @return phi file in the form of a tibble dataset.
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' uncrtnty:::get_phi(x)
get_phi <- function(xpdb){
  stopifnot(inherits(xpdb, "xpose_data"))
  xpdb$files$data[xpdb$files$extension=="phi"][[1]]
}
