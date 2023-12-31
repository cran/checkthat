% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-typechecks.R
\name{is_integerlike}
\alias{is_integerlike}
\title{Check if a Numeric Value is Integer-like}
\usage{
is_integerlike(x)
}
\arguments{
\item{x}{Numeric value to check.}
}
\value{
\code{TRUE} if \code{x} is integer-like, otherwise \code{FALSE}.
}
\description{
This function checks if a numeric value is and integer-like scalar, meaning
it is numeric and its length is 1.
}
\examples{
is_integerlike(3) # TRUE
is_integerlike(3.5) # FALSE
is_integerlike("3") # FALSE
is_integerlike(c(1, 2)) # FALSE
}
\seealso{
\code{\link{is_proportion}}, \code{\link{is_count}},
\code{\link{validate_proportion}}, \code{\link{validate_count}}
}
