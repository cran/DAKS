\name{print.pat}
\alias{print.pat}
\title{Print Method for Objects of class pat}
\description{
S3 method to print objects of class \code{pat}.
}
\usage{
\method{print}{pat}(x, \dots)
}
\arguments{
  \item{x}{a required object of class \code{pat}, obtained from a call to the function pat.}
  \item{...}{further arguments to be passed to or from other methods.}
}
\details{
Prints the main results from inductive item tree analysis algorithms.    
}
\value{
   If the argument \code{x} is of required type, \code{print.pat} prints the n largest states
   in the dataset, and optional returns the number of times prespecified states occur in the data. 
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
  See also \code{\link{DAKS-package}} for general information about this package.
}
\keyword{htest}
\keyword{manip}
\keyword{math}
\keyword{models}
\keyword{multivariate}
\keyword{univar}
\keyword{utilities}
