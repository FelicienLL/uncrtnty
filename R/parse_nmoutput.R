#' Parse a ext table to a list with final estimates
#'
#' @param tab a ext table from NONMEM
#'
#' @return a list, with a vector of `THETA` and `OMEGA`/`SIGMA` matrices
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' ext <- get_ext(x)
#' parse_ext(ext)
parse_ext <- function(tab){
  stopifnot(inherits(tab, "data.frame"))
  lastiter <- tab[tab$ITERATION==-1E9, ]
  list(
    theta = as.double(lastiter[,stringr::str_detect(names(lastiter), "THETA")]),
    omega = low_to_matrix(as.double(lastiter[,stringr::str_detect(names(lastiter), "OMEGA")])),
    sigma = low_to_matrix(as.double(lastiter[,stringr::str_detect(names(lastiter), "SIGMA")]))
  )
}

#' Parse a correlation table to useful matrices
#'
#' @param tab a correlation table from NONMEM
#' @param matrix whether the `THETA`, `OMEGA` or `SIGMA` matrix should be parsed ?
#'
#' @return a matrix of numeric. If matrix is `THETA`, the variance-covariance matrix of estimation of `THETA`. If matrix is `OMEGA`/`SIGMA`, a matrix of the standard errors of every elements of the `OMEGA`/`SIGMA` matrices.
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' cor <- get_cor(x)
#' parse_cor(cor, matrix = "THETA")
#' parse_cor(cor, matrix = "OMEGA")
parse_cor <- function(tab, matrix = c("THETA", "OMEGA", "SIGMA")){
  stopifnot(inherits(tab, "data.frame"))
  m <- matrix[1]
  stopifnot(matrix %in% c("THETA", "OMEGA", "SIGMA"))
  ans <- as.matrix(tab[stringr::str_detect(tab$NAME, m), stringr::str_detect(names(tab), m)])
  if(matrix != "THETA") {
    ans <- low_to_matrix(sqrt(diag(ans))) # /!\ Diagonal of the covariance matrix, so sqrt to obtain SE !
  }
  return(ans)
}


low_to_matrix <- function(x){
  lenx <- length(x)
  dimx <- sqrt(2*length(x) + 0.25) - 0.5 #Reverse of the triangular low
  stopifnot(as.integer(dimx)==dimx, length(dimx)==1L, dimx > 0)
  v <- seq_len(dimx)
  index_x <- v*(v+1)/2 #triangular low ; where are on-diagonal elements ?
  is.diag <- seq_len(lenx) %in% index_x #logical. is an on-diag element ?

  M <- matrix(nrow = dimx, ncol = dimx) #empty matrix

  diag(M) <- x[is.diag]
  M[lower.tri(M, diag = FALSE)] <- x[!is.diag]
  M[upper.tri(M, diag = FALSE)] <- x[!is.diag]
  return(M)
}
