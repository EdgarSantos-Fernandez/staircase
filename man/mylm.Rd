% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{mylm}
\alias{mylm}
\title{Rearranges a data frame into a formula style arrangement}
\usage{
mylm(formula, data)
}
\arguments{
\item{formula}{A formula as in lm() e.g. Y ~ X1 + X2}

\item{data}{A data.frame containing the elements specified in the formula}
}
\value{
A list of 2 matrices, the first is the response variable defined in
the formula parameter. The second is a list containing all the other
variables defined in the formula parameter.
}
\description{
Rearranges a data frame into a formula style arrangement
}
\examples{
#options(na.action='na.pass')
#out_list = mylm(formula = y ~ X1 + X2 + X3, data = data)
}
\author{
Jay ver Hoef
}
