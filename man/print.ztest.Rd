\name{print.ztest}
\alias{print.ztest}
\title{Print Method for Objects of class ztest}
\description{
S3 method to print objects of class \code{ztest}.
}
\usage{
\method{print}{ztest}(x, \dots)
}
\arguments{
  \item{x}{a required object of class \code{ztest}, obtained from a call to the function z_test.}
  \item{...}{further arguments to be passed to or from other methods.}
}
\details{
Prints the main results from inductive item tree analysis algorithms.    
}
\value{
   If the argument \code{x} is of required type, \code{print.ztest} prints the Z- and p-value. The alternative hpypothesis, the confidence interval, and the sample estimates. 
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
  \code{\link{z_test}}, the function for performing a Z-test. See also \code{\link{DAKS-package}} for general
  information about this package.
}
\keyword{htest}
\keyword{manip}
\keyword{math}
\keyword{models}
\keyword{multivariate}
\keyword{univar}
\keyword{utilities}
