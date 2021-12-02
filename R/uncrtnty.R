#Constructor
new_uncrtnty <- function(model  = character(),
                         nid    = integer(),
                         nobs   = integer(),
                         th_est = double(),
                         th_unc = matrix(),
                         om_est = list(),
                         om_unc = integer(),
                         si_est = list(),
                         si_unc = integer()){

  x <- list(
    model  = model,
    nid    = as.integer(nid),
    nobs   = as.integer(nobs),
    th_est = th_est,
    th_unc = th_unc,
    om_est = om_est,
    om_unc = as.integer(om_unc),
    si_est = si_est,
    si_unc = as.integer(si_unc)
  )

  structure(x, class = "uncrtnty")

}

#Validator
validate_uncrtnty <- function(x){
  expected_names <- c("model", "nid", "nobs", "th_est", "th_unc", "om_est", "om_unc", "si_est", "si_unc")

  if(any(names(x) != expected_names)){
    stop(glue::glue("Expected names are: {glue::glue_collapse(expected_names, sep = ', ', last = ' and ')}."), call. = FALSE)
  }

  #THETA

  stopifnot(is.double(x$th_est))

  if(length(x$th_est > 0)){

    stopifnot(is.double(x$th_unc), is.matrix(x$th_unc), is.symmetric(x$th_unc))

    if(length(x$th_est) != dim(x$th_unc)[1]){
      stop(glue::glue("`th_est` is of length {length(x$th_est)} but `th_unc` is a {dim(x$th_unc)[1]}x{dim(x$th_unc)[1]} matrix."), call. = FALSE)
    }
  }

  #OMEGA

  stopifnot(is.list(x$om_est))

  if(length(x$om_est) > 0){

    stopifnot(sapply(x$om_est, is.matrix), sapply(x$om_est, is.double), sapply(x$om_est, is.symmetric), is.integer(x$om_unc))

    if(length(x$om_est) != length(x$om_unc)){
      stop(glue::glue("`om_est` is made of {length(x$om_est)} matrix/ces, but `om_unc` is a length {length(x$om_unc)} vector."), call. = FALSE)
    }

    if((length(x$nid) > 0 & any(x$om_unc > x$nid))){
      stop(glue::glue("Omega degrees of freedom cannot exceed the number of subjects ({x$nid})."), call. = FALSE)
    }
    if(any(x$om_unc < sapply(x$om_est, function(x)dim(x)[1]))){
      stop(glue::glue("Omega degrees of freedom cannot be lower than the dimension of the matrix (`om_est`)."), call. = FALSE)
    }
  }

  #SIGMA

  stopifnot(is.list(x$si_est))

  if(length(x$si_est) > 0){

    stopifnot(sapply(x$si_est, is.matrix), sapply(x$si_est, is.double), sapply(x$si_est, is.symmetric), is.integer(x$si_unc))

    if(length(x$si_est) != length(x$si_unc)){
      stop(glue::glue("`si_est` is made of {length(x$si_est)} matrix/ces, but `si_unc` is a length {length(x$si_unc)} vector."), call. = FALSE)
    }

    if(length(x$nobs) > 0 & any(x$si_unc > x$nobs)){
      stop(glue::glue("Sigma degrees of freedom cannot exceed the number of observations ({x$nobs})."), call. = FALSE)
    }

    if(any(x$si_unc < sapply(x$si_est, function(x)dim(x)[1]))){
      stop(glue::glue("Sigma degrees of freedom cannot be lower than the dimension of the matrix (`si_est`)."), call. = FALSE)
    }
  }
  x
}

#Helper
uncrtnty <- function(model  = character(),
                     nid    = integer(),
                     nobs   = integer(),
                     th_est = double(),
                     th_unc = matrix(),
                     om_est = list(),
                     om_unc = integer(),
                     si_est = list(),
                     si_unc = integer()){

  validate_uncrtnty(new_uncrtnty(model, nid, nobs, th_est, th_unc, om_est, om_unc, si_est, si_unc))

}

#' Symmetric matrix
#'
#' @param x object to be tested
#'
#' @return returns TRUE if the argument is a symmetric square matrix, or FALSE with a message otherwise
#' @export
#'
#' @examples
#' M <- matrix(c(11, 12, 13, 12, 22, 23, 13, 23, 33), nrow = 3)
#' is.symmetric(M) #is TRUE
#' M[1,2] <- 99
#' is.symmetric(M) #is FALSE with a message
is.symmetric <- function(x){

  if(!is.matrix(x)){
    message("Not a matrix")
    return(FALSE)
  }

  dimx <- dim(x)

  if(dimx[1] != dimx[2]){
    message("Not a square matrix")
    return(FALSE)
  }

  seqdimx <- seq_len(dimx[1])

  for(i in seqdimx){
    for(j in seqdimx){
      if(i < j){
        if(x[i,j] != x[j,i]){
          message(glue::glue("Not symmetric matrix (between positions [{i},{j}] and [{j},{i}])."))
          return(FALSE)
        }
      }
    }
  }

  return(TRUE)
}
