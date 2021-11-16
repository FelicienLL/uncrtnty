
<!-- README.md is generated from README.Rmd. Please edit that file -->

# uncrtnty

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of {uncrtnty} is to provide functions to deal with uncertainty
in NONMEM analyses.

-   Parse information about the “Block Form” of an OMEGA/SIGMA matrix
    from:
    -   a *.lst* file with `parse_blockform_from_lst()`.
    -   the matrix itself with `infer_blockform()`.
-   Turn a full OMEGA/SIGMA matrix into a list of OMEGA/SIGMA matrix
    blocks with `matrix_to_list()`.
-   Parse NONMEM estimate file (*.ext*) and covariance matrix file
    (*.cov*) into convenient output-list with `parse_ext()` and
    `parse_cov()`.
-   Compute degrees of freedom of the Inverse-Wishart distribution for
    OMEGA/SIGMA matrices with `compute_df()`.

These functions are executed under the hood in the following functions,
which turns an [xpose](https://github.com/UUPharmacometrics/xpose)
database object into:

-   a list of arguments to simulate with uncertainty, for instance with
    [simpar](https://github.com/metrumresearchgroup/simpar) with
    `xpose_to_simpar()`.
-   the code needed for the “$PRIOR NWPRI” routine in NONMEM (*yet to
    come… soon*).

## Installation

You can install the development version of uncrtnty from
[github](https://github.com/FelicienLL/uncrtnty) with:

``` undefined
install.packages("remotes")
remotes::install_github("FelicienLL/uncrtnty")
```

## Example

``` r
library(uncrtnty)
x <- readRDS(system.file("xposerun", "xpdb_ex_pk.rds", package = "uncrtnty"))
#Example NONMEM run from the xpose package
xpdb_to_simpar(x)
#> $theta
#> [1] 26.29090000  1.34809000  4.20364000  0.20795800  0.20461000  0.01055270
#> [7]  0.00717161
#> 
#> $covar
#>            THETA1       THETA2       THETA3       THETA4       THETA5
#> [1,]  0.794733000  2.05165e-02  0.072218300 -3.45023e-03  8.70613e-04
#> [2,]  0.020516500  1.91942e-03 -0.008296640 -6.41703e-05  2.53143e-04
#> [3,]  0.072218300 -8.29664e-03  0.654666000  3.21678e-03 -4.70861e-03
#> [4,] -0.003450230 -6.41703e-05  0.003216780  2.46749e-04 -5.78968e-05
#> [5,]  0.000870613  2.53143e-04 -0.004708610 -5.78968e-05  5.03582e-04
#> [6,]  0.000630374 -3.17438e-06 -0.000652174 -1.52831e-05 -3.14227e-05
#> [7,] -0.000330216  5.45514e-06 -0.000314608  2.46140e-06  3.14954e-06
#>            THETA6       THETA7
#> [1,]  6.30374e-04 -3.30216e-04
#> [2,] -3.17438e-06  5.45514e-06
#> [3,] -6.52174e-04 -3.14608e-04
#> [4,] -1.52831e-05  2.46140e-06
#> [5,] -3.14227e-05  3.14954e-06
#> [6,]  1.33803e-05 -1.58021e-06
#> [7,] -1.58021e-06  2.87818e-06
#> 
#> $omega
#> $omega[[1]]
#>           [,1]
#> [1,] 0.0729525
#> 
#> $omega[[2]]
#>           [,1]
#> [1,] 0.0380192
#> 
#> $omega[[3]]
#>         [,1]
#> [1,] 1.90699
#> 
#> 
#> $sigma
#> $sigma[[1]]
#>      [,1]
#> [1,]    1
#> 
#> 
#> $odf
#> [1] 67 19 23
#> 
#> $sdf
#> [1] Inf
```
