#' Parse a lst file to a list of relevant information
#'
#' @param lst lst file in the form of a character vector.
#'
#' @return a list with the number of individuals, number of observations, and the "blockforms" of omega and sigma matrices
#' @export
#'
#' @examples
#' lst3a <- readLines(system.file("nm", "run003a.lst", package = "uncrtnty"))
#' parse_lst(lst = lst3a)
#' lst001 <- readLines(system.file("xposerun", "run001.lst", package = "uncrtnty"))
#' parse_lst(lst = lst001)
parse_lst <- function(lst){

  # ======  N OBS/ID  =======
  nobs_line <- stringr::str_which(lst, " TOT. NO. OF OBS RECS:")
  nobs <- unique(as.integer(stringr::str_extract(lst[nobs_line], "\\d+$")))
  if(length(nobs) > 1){
    message("Not a unique number of observations in lst file. Set to the maximal value.")
    nobs <- max(nobs, na.rm = TRUE)
  }

  nid_line <- stringr::str_which(lst, " TOT. NO. OF INDIVIDUALS:")
  nid <- unique(as.integer(stringr::str_extract(lst[nid_line], "\\d+$")))
  if(length(nid) > 1){
    message("Not a unique number of individuals in lst file. Set to the maximal value.")
    nid <- max(nobs, na.rm = TRUE)
  }

  # ====== BLOCK FORM =======

  #Test that both $OMEGA and $SIGMA are defined in .lst

  omT <- any(stringr::str_detect(lst, "0OMEGA"))
  siT <- any(stringr::str_detect(lst, "0SIGMA"))

  if(!all(omT, siT)) stop("Cannot found both $OMEGA and $SIGMA in the model", call. = FALSE)

  #$OMEGA

  om_line_diagonly <- stringr::str_which(lst, "0OMEGA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:")

  if(length(om_line_diagonly) != 0){
    om_dimmat <- as.integer(stringr::str_extract(lst[om_line_diagonly], "\\d+$"))
    om_bf <- seq_len(om_dimmat)
  } else {
    om_line_start <- stringr::str_which(lst, "0OMEGA HAS BLOCK FORM:") + 1L
    om_line_end <- stringr::str_which(lst, "0DEFAULT OMEGA BOUNDARY TEST OMITTED:") -1L
    om_lines_block <- lst[seq.int(om_line_start, om_line_end)]
    om_bf <- as.integer(stringr::str_extract(om_lines_block, "\\d+$"))
  }

  #$SIGMA
  si_line_diagonly <- stringr::str_which(lst, "0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:")

  if(length(si_line_diagonly) != 0){
    si_dimmat <- as.integer(stringr::str_extract(lst[si_line_diagonly], "\\d+$"))
    si_bf <- seq_len(si_dimmat)
  } else {
    si_line_start <- stringr::str_which(lst, "0SIGMA HAS BLOCK FORM:") + 1L
    si_line_end <- stringr::str_which(lst, "0DEFAULT SIGMA BOUNDARY TEST OMITTED:") -1L
    si_lines_block <- lst[seq.int(si_line_start, si_line_end)]
    si_bf <- as.integer(stringr::str_extract(si_lines_block, "\\d+$"))
  }

  list(
    nid = nid,
    nobs = nobs,
    om_blockform = om_bf,
    si_blockform = si_bf
    )

}

#' Parse a ext table to a list with final estimates
#'
#' @param tab a ext table from NONMEM
#'
#' @return a list, with a vector of `THETA` and `OMEGA`/`SIGMA` matrices
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' ext <- x$files$data[x$files$extension=="ext"][[1]]
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
#' cov <- x$files$data[x$files$extension=="cov"][[1]]
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
#' phi <- uncrtnty:::get_phi(x)
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
