#' Infer Block Form from a Matrix
#'
#' @param x a symmetric matrix of numeric
#'
#' @return a vector of integer describing the blocks in the matrix.
#' @export
#'
#' @examples
#' m <- matrix(c(1, 2, 0, 2, 3, 0, 0, 0, 4), nrow = 3)
#' infer_blockform(m)
infer_blockform <- function(x){
  dimx <- dim(x)[1]
  stopifnot(dimx > 0) #to do: test if is numeric, is square, is symmetric, is dim > 1 etc...
  past_km <- 0
  for(i in seq_len(dimx)){
    if(i < dimx){
      kmi <- max(past_km, i + which(x[i, seq.int(i+1, dimx)] != 0), na.rm = TRUE) #in upper triangle, position of non-zero at same line ?
    }
    if(i == 1L){
      k <- 1L # initial block number
    } else {
      if(i <= past_km){ #still in a block whatever values in lower triangle
        k <- c(k, max(k)) #so keep the same block number
      } else {
        if(any(x[i, seq.int(1, i-1)] != 0)){ #if non-zero value in the lower triangle
          k <- c(k, max(k)) #keep the same block number
        } else {
          if(x[i,i] == x[i-1,i-1]){ #if the previous in diag value is the SAME (like in $OMEGA BLOCK(1) SAME)
            k <- c(k, max(k)) #keep the same block number
          } else {
            k <- c(k, max(k) + 1L) #new block number
          }
        }
      }
    }
    if(exists("kmi")) past_km <- kmi
  }
  k
}

#' Parse Block Form from a .lst NONMEM output
#'
#' @param lst lst file in the form of a character vector.
#'
#' @return a vector of integer describing the blocks in the matrix.
#' @export
#'
#' @examples
#' lst3a <- readLines(system.file("nm", "run003a.lst", package = "uncrtnty"))
#' parse_blockform_from_lst(lst = lst3a)
parse_blockform_from_lst <- function(lst){
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

  list(OMEGA = om_bf, SIGMA = si_bf)

}

#' Transpose a full matrix into a list of matrices
#'
#' @param x a symmetric matrix of numeric
#' @param blockform integer vector, explicitly defined by the user or by `parse_blockform_from_lst()`. If empty (the default), will be infered with `infer_blockform()`.
#'
#' @return a list of matrices
#' @export
#'
#' @examples
#' m <- matrix(c(1,0.1,0,0,0.1,2,0,0,0,0,.3,0,0,0,0,.4), ncol = 4)
#' matrix_to_list(m)
matrix_to_list <- function(x, blockform = integer(0)){
  stopifnot(is.matrix(x)) #test symmetric, numeric etc ?
  x <- unname(x)
  dimmat <- dim(x)[1]
  if(length(blockform) == 0) blockform <- infer_blockform(x)
  if(length(blockform) != dimmat) stop(glue::glue("Matrix length ({dimmat}) is different from blockform length ({length(blockform)})."))
  lapply(seq_len(max(blockform)), function(i){
    k <- which(blockform == i)
    as.matrix(x[k,k])
  })
}
