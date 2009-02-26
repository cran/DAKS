\name{print.iita}
\alias{print.iita}
\title{Print Method for Objects of class iita}
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
   If the argument \code{x} is of required type, \code{print.iita} prints a vector of \emph{diff} values, and the set of implications, the error rate, and the index in the selection set of the quasi order corresponding to the minimum \emph{diff} value.
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
