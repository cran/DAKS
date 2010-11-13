\name{print.summpopiita}
\alias{print.summpopiita}
\title{Print Method for Objects of Class summpopiita}
\description{
S3 method to print objects of class \code{summpopiita}.
}
\usage{
\method{print}{summpopiita}(x, \dots)
}
\arguments{
  \item{x}{a required object of class \code{summpopiita}, obtained from a call to the function \code{\link{summary.popiita}}.}
  \item{...}{further arguments to be passed to or from other methods.}
}
\details{
Prints the main results from inductive item tree analysis algorithms in population values.    
}
\value{
   If the argument \code{x} is of required type, \code{print.summpopiita} prints a vector of population \emph{diff} values, a vector of population error rates, the population matrix, and the obtained selection set.
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
