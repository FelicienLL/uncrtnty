
<!-- README.md is generated from README.Rmd. Please edit that file -->

# uncrtnty

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

{uncrtnty} provides functions and an “uncrtnty”-list class in order to
work with uncertainty in NONMEM analyses.

## Installation

You can install the development version of uncrtnty from
[github](https://github.com/FelicienLL/uncrtnty) with:

``` r
install.packages("remotes")
remotes::install_github("FelicienLL/uncrtnty")
```

## The big idea

In NONMEM analyses, the uncertainty about parameter estimation can be
described as followed. The estimation of THETA follows a multivariate
normal distribution, with a mean vector (`th_est`) and a covariance
matrix (`th_unc`) that carries the information about uncertainty. The
estimates of OMEGA/SIGMA are matrices (`om_est/si_est`), and the
uncertainty is informed by degrees of freedom (`om_unc/si_unc`). In
{uncrtnty}, these information are storred into a single “uncrtnty”-list
object.

``` r
library(uncrtnty)
u_example
#> $model
#> [1] "example001"
#> 
#> $nid
#> [1] 100
#> 
#> $nobs
#> [1] 1000
#> 
#> $th_est
#> [1] 111.00  22.00 333.00   4.44
#> 
#> $th_unc
#>      [,1] [,2]  [,3] [,4]
#> [1,] 1.11 0.12  0.13 0.14
#> [2,] 0.12 2.00  0.23 0.24
#> [3,] 0.13 0.23 33.00 0.34
#> [4,] 0.14 0.24  0.34 4.40
#> 
#> $om_est
#> $om_est[[1]]
#>      [,1] [,2]
#> [1,] 1.00 0.12
#> [2,] 0.12 2.00
#> 
#> $om_est[[2]]
#>      [,1]
#> [1,]    3
#> 
#> 
#> $om_unc
#> [1] 45 67
#> 
#> $si_est
#> $si_est[[1]]
#>      [,1] [,2]
#> [1,] 0.05    1
#> [2,] 1.00   10
#> 
#> 
#> $si_unc
#> [1] 789
#> 
#> attr(,"class")
#> [1] "uncrtnty"
```

This “uncrtnty”-list object can be created:

-   from an [xpose](https://github.com/UUPharmacometrics/xpose) database
    object: `u_from_xpdb()`.
-   from NONMEM output files (*.lst*, *.ext*, *.cov*): *available soon*.
-   from a NONMEM *.xml* output file: *available soon*.
-   from a model reported in the literature: *available soon*

This “uncrtnty”-list object can be used to generate:

-   the code implemented within the NONMEM $PRIOR NWPRI routine:
    `u_to_nwpri()`
-   the arguments to simulate with uncertainty thanks to the
    [simpar](https://github.com/metrumresearchgroup/simpar) package:
    `u_to_simpar()`

## Applications

#### Create the code for the $PRIOR routine in NONMEM

``` r
u_to_nwpri(u_example)
#> 
#> 
#> 
#> $PRIOR NWPRI
#> 
#> ;======== PRIOR BLOCKS =======
#> 
#> $THETAP ; Prior values of THETA 
#> 111  FIXED ; 1 
#> 22   FIXED ; 2 
#> 333  FIXED ; 3 
#> 4.44 FIXED ; 4 
#> 
#> $THETAPV BLOCK(4) FIXED ; Prior weight on THETA (variance-covariance matrix)
#> 1.11 ; 
#> 0.12 2    ; 
#> 0.13 0.23 33   ; 
#> 0.14 0.24 0.34 4.4  ; 
#> 
#> $OMEGAP BLOCK(2) FIXED ; Prior values of OMEGA
#> 1    ; 
#> 0.12 2    ; 
#> $OMEGAP BLOCK(1) FIXED ; Prior values of OMEGA
#> 3 ; 
#> 
#> $OMEGAPD ; Prior weight on OMEGA (degrees of freedom)
#> 45 FIXED ; 1 
#> 67 FIXED ; 2 
#> 
#> $SIGMAP BLOCK(2) FIXED ; Prior values of SIGMA
#> 0.05 ; 
#> 1    10   ; 
#> 
#> $SIGMAPD ; Prior weight on SIGMA (degrees of freedom)
#> 789 FIXED ; 1 
#> 
#> ;===== End of PRIOR BLOCKS =====
#> 
#> $COVARIANCE MATRIX = R
```

#### Create the arguments to simulate with uncrtnty in simpar

``` r
u_to_simpar(u_example, nsim = 100)
#> $nsim
#> [1] 100
#> 
#> $theta
#> [1] 111.00  22.00 333.00   4.44
#> 
#> $covar
#>      [,1] [,2]  [,3] [,4]
#> [1,] 1.11 0.12  0.13 0.14
#> [2,] 0.12 2.00  0.23 0.24
#> [3,] 0.13 0.23 33.00 0.34
#> [4,] 0.14 0.24  0.34 4.40
#> 
#> $omega
#> $omega[[1]]
#>      [,1] [,2]
#> [1,] 1.00 0.12
#> [2,] 0.12 2.00
#> 
#> $omega[[2]]
#>      [,1]
#> [1,]    3
#> 
#> 
#> $sigma
#> $sigma[[1]]
#>      [,1] [,2]
#> [1,] 0.05    1
#> [2,] 1.00   10
#> 
#> 
#> $odf
#> [1] 45 67
#> 
#> $sdf
#> [1] 789
```

## Other useful features

-   Parse information about the “Block Form” of an OMEGA/SIGMA matrix
    from:
    -   a *.lst* file with `parse_blockform_from_lst()`.

``` r
cat(lstfile[84:110], sep = "\n") # zoom inside a NONMEM .lst file
#> 0OMEGA HAS BLOCK FORM:
#>   1
#>   1  1
#>   0  0  2
#>   0  0  0  3
#>   0  0  0  3  3
#>   0  0  0  3  3  3
#>   0  0  0  0  0  0  4
#>   0  0  0  0  0  0  0  5
#>   0  0  0  0  0  0  0  0  6
#>   0  0  0  0  0  0  0  0  0  6
#>   0  0  0  0  0  0  0  0  0  0  7
#>   0  0  0  0  0  0  0  0  0  0  0  8
#>   0  0  0  0  0  0  0  0  0  0  0  0  9
#>   0  0  0  0  0  0  0  0  0  0  0  0  9  9
#>   0  0  0  0  0  0  0  0  0  0  0  0  0  0  9
#>   0  0  0  0  0  0  0  0  0  0  0  0  0  0  9  9
#> 0DEFAULT OMEGA BOUNDARY TEST OMITTED:    NO
#> 0SIGMA HAS BLOCK FORM:
#>   1
#>   1  1
#>   0  0  2
#>   0  0  0  3
#>   0  0  0  0  3
#>   0  0  0  0  0  4
#>   0  0  0  0  0  0  5
#> 0DEFAULT SIGMA BOUNDARY TEST OMITTED:    NO
parse_lst(lst = lstfile)$om_blockform
#>  [1] 1 1 2 3 3 3 4 5 6 6 7 8 9 9 9 9
parse_lst(lst = lstfile)$si_blockform
#> [1] 1 1 2 3 3 4 5
```

-   + the matrix itself with `infer_blockform()`.

``` r
m
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] 1.00 0.12 0.13    0    0
#> [2,] 0.12 2.00 0.23    0    0
#> [3,] 0.13 0.23 3.00    0    0
#> [4,] 0.00 0.00 0.00    4    0
#> [5,] 0.00 0.00 0.00    0    5
infer_blockform(m)
#> [1] 1 1 1 2 3
```

-   Turn a full OMEGA/SIGMA matrix into a list of OMEGA/SIGMA matrix
    blocks with `matrix_to_list()`.

``` r
matrix_to_list(m)
#> [[1]]
#>      [,1] [,2] [,3]
#> [1,] 1.00 0.12 0.13
#> [2,] 0.12 2.00 0.23
#> [3,] 0.13 0.23 3.00
#> 
#> [[2]]
#>      [,1]
#> [1,]    4
#> 
#> [[3]]
#>      [,1]
#> [1,]    5
```

-   Parse NONMEM estimate file (*.ext*) and covariance matrix file
    (*.cov*) into convenient output-list with `parse_ext()` and
    `parse_cov()`.

-   Compute degrees of freedom of the Inverse-Wishart distribution for
    OMEGA/SIGMA matrices with `compute_df()`.

``` r
est_om <- matrix(c(0.2, 0.01, 0.01, 0.1), ncol = 2)
se_om <- matrix(c(0.02, 0.005, 0.005, 0.03), ncol = 2)
compute_df(est = est_om, se = se_om)
#> [1] 22
```

-   Parse the covariance matrix of estimations of ETA in the context on
    individual parameter estimation `parse_phi()`.
