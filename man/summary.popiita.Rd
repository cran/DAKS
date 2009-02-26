\name{summary.popiita}
\alias{summary.popiita}
\title{Summary Method for Objects of class popiita}
\description{
S3 method to summarize objects of class \code{popiita}.
}
\usage{
\method{summary}{popiita}(object, \dots)
}
\arguments{
  \item{object}{a required object of class \code{popiita}, obtained from a call to the function popiita.}
  \item{...}{further arguments to be passed to or from other methods.}
}
\details{
Summarizes the main results from inductive item tree analysis algorithms in population values.    
}
\value{
   If the argument \code{x} is of required type, \code{summary.popiita} returns the results from \code{\link{pop_iita}} and returns an object of class \code{summ.popiita}.
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
