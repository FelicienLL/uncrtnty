#' Turn uncrtnty-list into code for NWPRI in NONMEM
#'
#' @param u an uncrtnty-list object
#' @param cat if TRUE (the default), the character vector is passed to `cat` and printed to the console.
#' @param priors_only if TRUE, only the blocks with priors values and weight are returned.
#'
#' @return a character vector to be pasted into a NONMEM code
#' @export
#'
#' @examples
#' xpdb <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' u <- u_from_xpdb(xpdb)
#' a <- u_to_nwpri(u)
#'
u_to_nwpri <- function(u, cat = TRUE, priors_only = FALSE){
  stopifnot(inherits(u, "uncrtnty"))
  ans <- paste(c(
    nwpri_value(u$th_est, blocktype = "THETAP"),
    nwpri_matrix(u$th_unc, blocktype = "THETAPV"),
    paste(sapply(u$om_est, nwpri_matrix, blocktype = "OMEGAP"), collapse = "\n"),
    nwpri_value(u$om_unc, blocktype = "OMEGAPD"),
    paste(sapply(u$si_est, nwpri_matrix, blocktype = "SIGMAP"), collapse = "\n"),
    nwpri_value(u$si_unc, blocktype = "SIGMAPD")
  ), collapse = "\n\n")

  if(!priors_only){
    prior <- "$PRIOR NWPRI"
    intro <- ";======== PRIOR BLOCKS ======="
    end <- ";===== End of PRIOR BLOCKS ====="
    cov <- "$COVARIANCE MATRIX = R"
    ans <- paste("\n", prior, intro, ans, end, cov, sep = "\n\n")
  }

  if(cat){
    cat(ans)
  }

  invisible(ans)

}

#' Set a block of matrix for the NWPRI routine in NONMEM
#'
#' @param x a symmetric matrix of numeric, typically the covariance matrix of theta or the estimates of omega/sigma.
#' @param blocktype a character, either "THETAP", "OMEGAPD" or "SIGMAPD".
#'
#' @return a single character value to be pasted into the NONMEM code
#' @export
#'
#' @examples
#' x <- matrix(c(1, 0.12, 0.13, 0.12, 22, 0.23, 0.13, 0.23, 333), nrow = 3, ncol = 3)
#' nwpri_matrix(x = x, blocktype = "THETAPV")
#' nwpri_matrix(x = x, blocktype = "OMEGAP")
nwpri_matrix <- function(x, blocktype){
  stopifnot(blocktype %in% c("THETAPV", "OMEGAP", "SIGMAP"))
  xsize <- nrow(x)
  xdescr <- nwpri_blockdescr(blocktype)
  xhead <- glue::glue("${blocktype} BLOCK({xsize}) FIXED ; {xdescr}")

  xval <- pad_matrix(x)
  xname <- colnames(x)
  if(is.null(xname)) xname <- ""
  xblock <- paste(glue::glue("{xval} ; {xname}"), collapse = "\n")

  glue::glue('{xhead}\n{xblock}')
}


#' Set a block of values for the NWPRI routine in NONMEM
#'
#' @param x a (named) vector of numeric, typically the estimates of theta or the degree of freedom associated to omega/sigma.
#' @param blocktype a character, either "THETAP", "OMEGAPD" or "SIGMAPD".
#'
#' @return a single character value to be pasted into the NONMEM code
#' @export
#'
#' @examples
#' nwpri_value(x = c(CL = .123, 456, V = 789), blocktype = "THETAP")
#' nwpri_value(x = c(123, 45, 67), blocktype = "OMEGAPD")
nwpri_value <- function(x, blocktype){
  stopifnot(blocktype %in% c("THETAP", "OMEGAPD", "SIGMAPD"))
  xdescr <- nwpri_blockdescr(blocktype)
  xhead <- glue::glue("${blocktype} ; {xdescr}")

  xval <- stringr::str_pad(x, width = max(stringr::str_length(x)), side = "right")
  xindex <- seq_len(length(x))
  xname <- names(x)
  if(is.null(xname)) xname <- ""
  xblock <- paste(glue::glue("{xval} FIXED ; {xindex} {xname}"), collapse = "\n")

  glue::glue('{xhead}\n{xblock}')
}

# pad_matrix(matrix(c(1, 0.12, 0.13, 0.12, 22, 0.23, 0.13, 0.23, 333), nrow = 3))
pad_matrix <- function(x){  #x = a symmetric matrix
  y <- character()
  pad <- max(stringr::str_length(x))
  for(i in seq_len(nrow(x))){
    y[i] <- paste(stringr::str_pad(x[i, seq_len(i)], width = pad, side = "right"), collapse = " ")
  }
  y #y = a vector of characters = rows of the lower matrix (as padded characters)
}

# nwpri_blockdescr(x = "SIGMAP")
nwpri_blockdescr <- function(x){
  blockname <- c("THETAP",
                 "THETAPV",
                 "OMEGAP",
                 "OMEGAPD",
                 "SIGMAP",
                 "SIGMAPD")

  blockdescr <- c("Prior values of THETA ",
                  "Prior weight on THETA (variance-covariance matrix)",
                  "Prior values of OMEGA",
                  "Prior weight on OMEGA (degrees of freedom)",
                  "Prior values of SIGMA",
                  "Prior weight on SIGMA (degrees of freedom)")

  blockdescr[blockname==x]
}
