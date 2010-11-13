\name{summary.iita}
\alias{summary.iita}
\title{Summary Method for Objects of Class iita}
\description{
S3 method to summarize objects of class \code{iita}.
}
\usage{
\method{summary}{iita}(object, \dots)
}
\arguments{
  \item{object}{a required object of class \code{iita}, obtained from a call to the function \code{\link{iita}}.}
  \item{...}{further arguments to be passed to or from other methods.}
}
\details{
Summarizes the main results from inductive item tree analysis algorithms.    
}
\value{
  If the argument \code{object} is of required type, \code{summary.iita}
  returns the vector of \emph{diff} values, the quasi order, the error
  rate, and the index in the selection set of the quasi order with minimum
  \emph{diff} value.
}
\references{
Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons. \emph{Mathematical Social Sciences}, \bold{58}, 376--392. 

  Uenlue, A. and Sargin, A. (2010) \pkg{DAKS}: An \R package for data
  analysis methods in knowledge space theory. \emph{Journal of Statistical
  Software}, \bold{37}(2), 1--31.
  URL \url{http://www.jstatsoft.org/v37/i02/}.
}
\author{
  Anatol Sargin \email{sargin@statistik.tu-dortmund.de},

  Ali Uenlue \email{uenlue@statistik.tu-dortmund.de}
}
\seealso{
  \code{\link{iita}}, the interface that provides the three (sample) inductive item tree analysis methods
  under one umbrella. See also \code{\link{DAKS-package}} for general
  information about this package.
}
\keyword{htest}
\keyword{manip}
\keyword{math}
\keyword{models}
\keyword{multivariate}
\keyword{univar}
\keyword{utilities}
