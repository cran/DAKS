\name{print.ztest}
\alias{print.ztest}
\title{Print Method for Objects of Class ztest}
\description{
S3 method to print objects of class \code{ztest}.
}
\usage{
\method{print}{ztest}(x, \dots)
}
\arguments{
  \item{x}{a required object of class \code{ztest}, obtained from a call to
    the function \code{\link{z_test}}.}
  \item{...}{further arguments to be passed to or from other methods.}
}
\details{
  Prints the main results from inductive item tree analysis algorithms.    
}
\value{
  If the argument \code{x} is of required type, \code{print.ztest} prints
  the Z- and p-value, the alternative hypothesis, the confidence interval,
  and the sample estimates. 
}
\references{
  Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons. \emph{Mathematical Social Sciences}, \bold{58}, 376-- 392. 

Uenlue, A. and Sargin, A. (2010) \pkg{DAKS}: An \R package for data
  analysis methods in knowledge space theory. Manuscript under revision.   
}
\author{
  Anatol Sargin \email{sargin@statistik.tu-dortmund.de},

  Ali Uenlue \email{uenlue@statistik.tu-dortmund.de}
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
