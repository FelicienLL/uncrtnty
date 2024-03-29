% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_nwpri.R
\name{u_to_nwpri}
\alias{u_to_nwpri}
\title{Turn uncrtnty-list into code for NWPRI in NONMEM}
\usage{
u_to_nwpri(u, cat = TRUE, priors_only = FALSE)
}
\arguments{
\item{u}{an uncrtnty-list object}

\item{cat}{if TRUE (the default), the character vector is passed to `cat` and printed to the console.}

\item{priors_only}{if TRUE, only the blocks with priors values and weight are returned.}
}
\value{
a character vector to be pasted into a NONMEM code
}
\description{
Turn uncrtnty-list into code for NWPRI in NONMEM
}
\examples{
code <- u_to_nwpri(u_example)

}
