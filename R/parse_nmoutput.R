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

#' Parse a covariance table to useful matrices
#'
#' @param tab a covariance table from NONMEM
#'
#' @return a list with the variance-covariance matrix of estimation of `THETA`, and the matrices of the standard errors of every elements of the `OMEGA`/`SIGMA` matrices.
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' cov <- get_cov(x)
#' parse_cov(cov)
parse_cov <- function(tab){
  stopifnot(inherits(tab, "data.frame"))
  TH <- as.matrix(tab[stringr::str_detect(tab$NAME, "THETA"), stringr::str_detect(names(tab), "THETA")])
  OM <- as.matrix(tab[stringr::str_detect(tab$NAME, "OMEGA"), stringr::str_detect(names(tab), "OMEGA")])
  OM <- low_to_matrix(sqrt(diag(OM)))
  SI <- as.matrix(tab[stringr::str_detect(tab$NAME, "SIGMA"), stringr::str_detect(names(tab), "SIGMA")])
  SI <- low_to_matrix(sqrt(diag(SI)))
  list(theta = TH, omega = OM, sigma = SI)
}

#' Parse a phi table to useful individual etas and covariance matrix
#'
#' @param tab a phi table from NONMEM
#'
#' @return a list (length = number of individual), with the vector of estimated etas and the associated variance-covariance matrix of estimation
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' phi <- get_phi(x)
#' parse_phi(head(phi, 3)) #first 3 individuals for the example
parse_phi <- function(tab){
  IDs <- tab$ID
  stopifnot(IDs == unique(IDs))
  ans <- lapply(IDs, function(x, phi){
    eta <- phi[phi[["ID"]] == x, stringr::str_detect(names(phi), "ETA")]
    etc <- phi[phi[["ID"]] == x, stringr::str_detect(names(phi), "ETC")]
    return(list(
      ID = x,
      ETA = unname(as.double(eta)),
      ETC = low_to_matrix(unname(as.double(etc)))
    ))
  }, phi = tab)
  return(ans)
}


low_to_matrix <- function(x){
  lenx <- length(x)
  dimx <- sqrt(2*length(x) + 0.25) - 0.5 #Reverse of the triangular law
  stopifnot(as.integer(dimx)==dimx, length(dimx)==1L, dimx > 0)
  v <- seq_len(dimx)
  index_x <- v*(v+1)/2 #triangular law ; where are on-diagonal elements ?
  is.diag <- seq_len(lenx) %in% index_x #logical. is an on-diag element ?

  M <- matrix(nrow = dimx, ncol = dimx) #empty matrix

  diag(M) <- x[is.diag]  #fill diagonal
  M[upper.tri(M, diag = FALSE)] <- x[!is.diag] #fill upper matrix
  for(i in v){ #fill lower matrix
    for(j in v){
      if(is.na(M[i,j])){
        M[i,j] <- M[j,i]
      }
    }
  }
  return(M)
}
