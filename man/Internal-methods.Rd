% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{Internal-methods}
\alias{Internal-methods}
\alias{plot.SPE}
\alias{plot.MAC}
\title{Internal methods for base S3 generics}
\usage{
\method{plot}{SPE}(x, y, type = c("bar", "line", "point"),
  xval = c("energy", "channel"), yval = c("rate", "count"),
  info = TRUE, fill = FALSE, ...)

\method{plot}{MAC}(x, y, ...)
}
\arguments{
\item{x}{object}

\item{y}{currently not used}

\item{type}{\code{\link{character}} (default: \code{"bar"}): 
Options: \code{"bar", "line", "point"}}

\item{xval}{\code{\link{character}} (default: '\code{"energy"}): 
Options: \code{"energy", "channel"}}

\item{yval}{\code{\link{character}} (default: \code{"rate"}): 
Options: \code{"rate", "count"}}

\item{info}{\code{\link{logical}} (default: \code{TRUE}): 
Show or hide info of the SPE file}

\item{fill}{\code{\link{logical}} (default: \code{FALSE}): 
Fill the area under the curve}

\item{...}{Arguments to be passed to \code{\link{plot}}.}
}
\description{
Methods for S3-generics implemented for the package 'gammaSpec'.
This document summarises all implemented S3-generics.
}
