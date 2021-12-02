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
    nid    = nid,
    nobs   = nobs,
    th_est = th_est,
    th_unc = th_unc,
    om_est = om_est,
    om_unc = om_unc,
    si_est = si_est,
    si_unc = si_unc
  )

  structure(x, class = "uncrtnty")

}

#Validator
validate_uncrtnty <- function(x){

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
