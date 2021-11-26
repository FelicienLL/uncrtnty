#' Turn uncrtnty-list into list of arguments for simpar
#'
#' @param u an uncrtnty-list object
#' @param nsim scalar numeric specifying the number of sets to attempt
#' @param ... additional named arguments to be passed to `simpar::simpar()` (i.e. "digits", "min", "max").
#' @details The arguments "theta", "covar", "omega", "sigma", "odf" and "sdf" are filled from the information contained in the uncrtnty-list object, unless they are explicitly specified in `...` .
#' @return a list of arguments to be called in `simpar::simpar()`.
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' u <- u_from_xpdb(x)
#' args <- u_to_simpar(u, n = 10, sdf = 1000, digits = 5, max = Inf)
#' #remotes::install_github("metrumresearchgroup/simpar")
#' #do.call(simpar::simpar, args)
#'
u_to_simpar <- function(u, nsim, ...){
  stopifnot(inherits(u, "uncrtnty"))
  L <- list(
    nsim = nsim,
    theta = u$th_est,
    covar = u$th_unc,
    omega = u$om_est,
    sigma = u$si_est,
    odf = u$om_unc,
    sdf = u$si_unc
  )
  dots <- list(...)
  L[names(dots)] <- dots
  L
}

