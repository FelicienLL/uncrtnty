#' Infer Block Form from a Matrix
#'
#' @param x a symmetric matrix of numeric
#'
#' @return a vector of integer describing the block in the matrix.
#' @export
#'
#' @examples
#' m <- matrix(c(1, 2, 0, 2, 3, 0, 0, 0, 4), nrow = 3)
#' blockform(m)
blockform <- function(x){
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
