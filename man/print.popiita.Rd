\name{print.popiita}
\alias{print.popiita}
\title{Print Method for Objects of class popiita}
\description{
S3 method to print objects of class \code{popiita}.
}
\usage{
\method{print}{popiita}(x, \dots)
}
\arguments{
  \item{x}{a required object of class \code{popiita}, obtained from a call to the function pop.iita.}
  \item{...}{further arguments to be passed to or from other methods.}
}
\details{
Prints the main results from inductive item tree analysis algorithms in population values.    
}
\value{
   If the argument \code{x} is of required type, \code{print.popiita} prints a vector of population \emph{diff} values, a vector of population error rates, and the quasi order with minimum population \emph{diff values}.
}
\references{
  Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons.  Manuscript under
  revision.  \url{http://www.math.uni-augsburg.de/~uenlueal/}

  Sargin, A. and Uenlue, A. (2009) \pkg{DAKS}: An \R package for data
  analysis in knowledge space theory. Manuscript submitted for
  publication.  \url{http://www.math.uni-augsburg.de/~uenlueal/}
}
\author{
  Anatol Sargin \email{anatol.sargin@math.uni-augsburg.de},
  Ali Uenlue \email{ali.uenlue@math.uni-augsburg.de}
}
\seealso{
  \code{\link{pop_iita}}. See also \code{\link{DAKS-package}} for general
  information about this package.
}
\keyword{htest}
\keyword{manip}
\keyword{math}
\keyword{models}
\keyword{multivariate}
\keyword{univar}
\keyword{utilities}
