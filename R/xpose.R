#' Extract .lst file from xpose object
#'
#' @param xpdb an object of "xpose_data" class
#'
#' @return lst file in the form of a character vector.
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' get_lst(x)
get_lst <- function(xpdb){
  stopifnot(inherits(xpdb, "xpose_data"))
  xpdb$code$code[xpdb$code$subroutine=="lst"]
}

#' Extract .cor file from xpose object
#'
#' @param xpdb an object of "xpose_data" class
#'
#' @return cor file in the form of a tibble dataset.
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' get_cor(x)
get_cor <- function(xpdb){
  stopifnot(inherits(xpdb, "xpose_data"))
  xpdb$files$data[xpdb$files$extension=="cor"][[1]]
}

#' Extract .ext file from xpose object
#'
#' @param xpdb an object of "xpose_data" class
#'
#' @return ext file in the form of a tibble dataset.
#' @export
#'
#' @examples
#' x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#' get_ext(x)
get_ext <- function(xpdb){
  stopifnot(inherits(xpdb, "xpose_data"))
  xpdb$files$data[xpdb$files$extension=="ext"][[1]]
}

