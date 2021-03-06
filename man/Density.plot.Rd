% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Density.plot.R
\name{Density.plot}
\alias{Density.plot}
\title{Density estimation plot}
\usage{
Density.plot(x,n = 500,method,from,to)
}
\arguments{
\item{x}{numeric variable}

\item{n}{point you are evaluating to obtain its density}

\item{method}{method for estimating the density. Is either "naive" or "kernel". Set default as "naive".}

\item{from}{r}

\item{to}{r}
}
\value{
Density plot
}
\description{
This function estimates the density of the given data.
}
\examples{
Density.plot(cars$speed,n = 500,method = "kernel")
}
\author{
Nina Louise Pedersen \cr
Institute of Mathematics and Computer Science (IMADA) \cr
University of Sourthern Denmark, (SDU) \cr
\email{npede14@student.sdu.dk} \cr
}

