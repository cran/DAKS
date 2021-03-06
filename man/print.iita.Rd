\name{print.iita}
\alias{print.iita}
\title{Print Method for Objects of Class iita}
\description{
S3 method to print objects of class \code{iita}.
}
\usage{
\method{print}{iita}(x, \dots)
}
\arguments{
  \item{x}{a required object of class \code{iita}, obtained from a call to the function iita.}
  \item{...}{further arguments to be passed to or from other methods.}
}
\details{
Prints the main results from inductive item tree analysis algorithms.    
}
\value{
   If the argument \code{x} is of required type, \code{print.iita} prints the set of implications.}

\references{
Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons. \emph{Mathematical Social Sciences}, \bold{58}, 376--392. 

  Uenlue, A. and Sargin, A. (2010) \pkg{DAKS}: An \R package for data
  analysis methods in knowledge space theory. \emph{Journal of Statistical
  Software}, \bold{37}(2), 1--31.
  URL \url{http://www.jstatsoft.org/v37/i02/}.
}  
\author{
  Anatol Sargin, Ali Uenlue
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
