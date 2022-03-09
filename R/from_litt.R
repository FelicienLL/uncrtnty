# reporttype: an attribute to know how the information is passed by the user
# - model, nid, nobs: optional...
# - th_est:
#   + expected: estimates
#   + could pass: estimates... no ambiguity...
# - th_unc:
#   + expected: full variance-covariance matrix
#   + could pass: list of RSE, list of SE, list of 95CI, list of 90CI #ci(width = ?)
# - om_est:
#   + expected: list of omega variance-covariance matrices
#   + could pass: list of variances, list of CV%, list of CVe1%
# - om_unc:
#   + expected: degrees of freedom
#   + could pass: list of RSE%, list of SE, list of 95CI, list of 90CI
# - si_est:
#   + expected: list of omega variance-covariance matrices
#   + could pass: list of variances, list of CV%, list of CVe1%
# - si_unc:
#   + expected: degrees of freedom
#   + could pass: list of RSE%, list of SE, list of 95CI, list of 90CI
#
# reporttype values:
#  - RSE, CV, SE, 95CI, 90CI



###







#' Build uncrtnty-list from literature
#'
#' @param model a character, model name
#' @param nid,nobs number of individuals (observations) used to build the model. Will be used as a maximum for `OMEGA` (`SIGMA`) degrees of freedom.
#' @param th_est a vector of numeric, point estimates of `THETA`
#' @param th_unc a full variance-covariance matrix of estimation of `THETA`, alternatively a list wrapped inside `as_rse()`, `as_se()`, `as_ci95()` or `as_ci90()` (see details)
#' @param om_est a list of variances or variance-covariance matrices, alternatively a list wrapped inside `as_cv()` or `as_cve1()`
#' @param om_unc a list wrapped inside `as_rse()`, `as_ci95()` or `as_ci90()`
#' @param si_est a list of variances or variance-covariance matrices, alternatively a list wrapped inside `as_cv()` or `as_cve1()`
#' @param si_unc a list wrapped inside `as_rse()`, `as_ci95()` or `as_ci90()`
#'
#' @return an uncrtnty-list object
#' @export
#'
#' @examples
#' u_from_liter(
#' model = "Test", nid = 250, nobs = 12345, #initial model
#' th_est = c(1,22,333,0.444),
#' th_unc = as_rse(list(0.2, 0.25, 0.3, .80)), #RSE on THETA are 20%, 25%, 30% and 80%
#' om_est = list(0.25, 0.33, 0.88), #omega reported as variances, i.e. OMEGA2
#' om_unc = as_ci95(list(c(0.2, 0.3), c(0.16, 0.50), c(0.8, 0.96))), #CI from bootstrap for instance
#' si_est = as_cv(list(0.2)), #sigma reported as a CV%, for a proportional error for instance
#' si_unc = as_rse(list(0.05)) #5% RSE
#' )
#'
u_from_liter <- function(model = "Prior model", nid = Inf, nobs = Inf, th_est, th_unc, om_est, om_unc, si_est, si_unc){
  #parse elements
  ## THETA
  .th_unc <- liter_th_unc(th_unc, th_est)

  ## OMEGA
  .om_est <- liter_re_est(om_est)
  .om_unc <- liter_re_unc(om_unc, re_est = .om_est, attr(om_est, "reporttype"), re = "OMEGA")

  ## SIGMA
  .si_est <- liter_re_est(si_est)
  .si_unc <- liter_re_unc(si_unc, re_est = .si_est, attr(si_est, "reporttype"), re = "SIGMA")

  #make uncrtnty object
  uncrtnty(
    model  = model,
    nid    = nid,
    nobs   = nobs,
    th_est = th_est,
    th_unc = .th_unc,
    om_est = .om_est,
    om_unc = mapply(compute_df, est = .om_est, se = .om_unc, MoreArgs = list(maxdf = nid)),
    si_est = .si_est,
    si_unc = mapply(compute_df, est = .si_est, se = .si_unc, MoreArgs = list(maxdf = nobs))
  )
}


#' Attribute a "report type"
#'
#' @param x list of values
#' @name reporttype
NULL

#' @rdname reporttype
#' @export
as_cv   <- function(x) {attr(x, "reporttype") <- "cv"   ; x}

#' @rdname reporttype
#' @export
as_cve1 <- function(x) {attr(x, "reporttype") <- "cve1" ; x}

#' @rdname reporttype
#' @export
as_rse  <- function(x) {attr(x, "reporttype") <- "rse"  ; x}

#' @rdname reporttype
#' @export
as_se   <- function(x) {attr(x, "reporttype") <- "se"   ; x}

#' @rdname reporttype
#' @export
as_ci95 <- function(x) {attr(x, "reporttype") <- "ci95" ; x}

#' @rdname reporttype
#' @export
as_ci90 <- function(x) {attr(x, "reporttype") <- "ci90" ; x}


is_cv   <- function(x) isTRUE(attr(x, "reporttype") == "cv")
is_cve1 <- function(x) isTRUE(attr(x, "reporttype") == "cve1")

is_rse  <- function(x) isTRUE(attr(x, "reporttype") == "rse")
is_se   <- function(x) isTRUE(attr(x, "reporttype") == "se")
is_ci95 <- function(x) isTRUE(attr(x, "reporttype") == "ci95")
is_ci90 <- function(x) isTRUE(attr(x, "reporttype") == "ci90")




liter_th_unc <- function(th_unc, th_est){
  if(is_rse(th_unc)){
    stopifnot(length(th_unc) == length(th_est))
    rse <- unlist(th_unc)
    moy <- unlist(th_est)
    se <- rse * moy
    var <- se * se
    return(diag(var))
  }

  if(is_se(th_unc)){
    se <- unlist(th_unc)
    var <- se * se
    return(diag(var))
  }

  if(is_ci95(th_unc)){
    se <- sapply(th_unc, ci2se, 0.95)
    var <- se * se
    return(diag(var))
  }

  if(is_ci90(th_unc)){
    se <- sapply(th_unc, ci2se, 0.90)
    var <- se * se
    return(diag(var))
  }

  return(th_unc)
}


liter_re_est <- function(re_est){
  if(is_cv(re_est)){
    cv <- unlist(re_est)
    var <- cv * cv
    return(lapply(var, as.matrix))
  }

  if(is_cve1(re_est)){
    cve1 <- unlist(re_est)
    var <- log(1 + cve1 * cve1)
    return(lapply(var, as.matrix))
  }

  return(lapply(re_est, as.matrix))
}

liter_re_unc <- function(re_unc, re_est, re_est_reporttype, re = "OMEGA/SIGMA"){
  #re_est = random effect estimate = always as a variance (already processed before)
  #retrieve uncertainty on random effect stored as list of SE matrices since SE will be used to compute degree freedom

  if(is_rse(re_unc)){
    stopifnot(length(re_est) == length(re_unc))
    if(is.null(re_est_reporttype)){ #if random effect value was initially given as a variance
      SE <- mapply(FUN = function(x, y) x * y, x = re_est, y = re_unc, SIMPLIFY = FALSE)
      return(lapply(SE, as.matrix))
    }

    if(re_est_reporttype %in% c("cv", "cve1")){ #if random effect value was initially given as a CV%
      SE <- mapply(FUN = function(x, y) x * y * 2, x = re_est, y = re_unc, SIMPLIFY = FALSE)
      return(lapply(SE, as.matrix))
    }
  }

  if(is_se(re_unc)){
    if(is.null(re_est_reporttype)){ #if random effect value was initially given as a variance
      SE <- re_unc
      return(lapply(SE, as.matrix))
    }
    if(re_est_reporttype %in% c("cv", "cve1")){ #if random effect value was initially given as a CV%
      stop(glue::glue("Cannot report uncertainty on {re} as SE if estimation of {re} was reported as CV%"))
    }
  }

  if(is_ci95(re_unc)){
    if(is.null(re_est_reporttype)){ #if random effect value was initially given as a variance
      SE <- lapply(re_unc, ci2se, ciwidth = 0.95)
      return(lapply(SE, as.matrix))
    }

    if(re_est_reporttype %in% c("cv", "cve1")){ #if random effect value was initially given as a CV%
      stop(glue::glue("Cannot report uncertainty on {re} as CI95 if estimation of {re} was reported as CV%"))
    }
  }

  if(is_ci90(re_unc)){
    if(is.null(re_est_reporttype)){ #if random effect value was initially given as a variance
      SE <- lapply(re_unc, ci2se, ciwidth = 0.90)
      return(lapply(SE, as.matrix))
    }

    if(re_est_reporttype %in% c("cv", "cve1")){ #if random effect value was initially given as a CV%
      stop(glue::glue("Cannot report uncertainty on {re} as CI90 if estimation of {re} was reported as CV%"))
    }
  }

  stop(glue::glue("Could not determine how uncertainty on {re} was reported (as SE, RSE, 95CI or 90CI ?)"))
}

ci2se <- function(x, ciwidth = 0.95){
  stopifnot(length(x)==2)
  down <- x[[1]]
  up <- x[[2]]
  width <- up - down
  q <- 1-((1-ciwidth)/2) # 95CI% interval = 0.975 quantile. 90CI% interval = 0.95 quantile
  width /(2 * stats::qnorm(q))
}
