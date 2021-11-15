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
#'
#' @return a list with the variance-covariance matrix of estimation of `THETA`, and the matrices of the standard errors of every elements of the `OMEGA`/`SIGMA` matrices.
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' cor <- get_cor(x)
#' parse_cor(cor)
parse_cor <- function(tab){
  stopifnot(inherits(tab, "data.frame"))
  TH <- as.matrix(tab[stringr::str_detect(tab$NAME, "THETA"), stringr::str_detect(names(tab), "THETA")])
  OM <- as.matrix(tab[stringr::str_detect(tab$NAME, "OMEGA"), stringr::str_detect(names(tab), "OMEGA")])
  OM <- low_to_matrix(sqrt(diag(OM)))
  SI <- as.matrix(tab[stringr::str_detect(tab$NAME, "SIGMA"), stringr::str_detect(names(tab), "SIGMA")])
  SI <- low_to_matrix(sqrt(diag(SI)))
  list(theta = TH, omega = OM, sigma = SI)
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
