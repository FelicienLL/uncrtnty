#Constructor
new_uncrtnty <- function(model  = character(),
                         nid    = integer(),
                         nobs   = integer(),
                         th_est = double(),
                         th_unc = matrix(),
                         om_est = list(),
                         om_unc = integer(),
                         si_est = list(),
                         si_unc = integer()){

  x <- list(
    model  = model,
    nid    = nid,
    nobs   = nobs,
    th_est = th_est,
    th_unc = th_unc,
    om_est = om_est,
    om_unc = om_unc,
    si_est = si_est,
    si_unc = si_unc
  )

  structure(x, class = "uncrtnty")

}

#Validator
validate_uncrtnty <- function(x){

}

#Helper
uncrtnty <- function(model  = character(),
                     nid    = integer(),
                     nobs   = integer(),
                     th_est = double(),
                     th_unc = matrix(),
                     om_est = list(),
                     om_unc = integer(),
                     si_est = list(),
                     si_unc = integer()){

}
