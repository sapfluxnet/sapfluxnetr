% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{.flag}
\alias{.flag}
\title{helper function to flag the mutated data in sfn_mutate and sfn_mutate_at}
\usage{
.flag(x)
}
\arguments{
\item{x}{variable to flag}
}
\value{
A vector of the same length than x with the variable flags modified
}
\description{
This function will add the "USER_MODF" flag to the data point flags
}
\examples{
sapfluxnetr:::.flag('')
sapfluxnetr:::.flag('OUT_WARNING')

}
\keyword{internal}
