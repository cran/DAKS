\name{summary.popiita}
\alias{summary.popiita}
\title{Summary Method for Objects of Class popiita}
\description{
S3 method to summarize objects of class \code{popiita}.
}
\usage{
\method{summary}{popiita}(object, \dots)
}
\arguments{
  \item{object}{a required object of class \code{popiita}, obtained from a call to the function \code{\link{pop_iita}}.}
  \item{...}{further arguments to be passed to or from other methods.}
}
\details{
Summarizes the main results from inductive item tree analysis algorithms in population values.    
}
\value{
  If the argument \code{object} is of required type, \code{summary.popiita}
  returns the results obtained from \code{\link{pop_iita}}.}
\references{
Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons. \emph{Mathematical Social Sciences}, \bold{58}, 376--392. 

Uenlue, A. and Sargin, A. (2010) \pkg{DAKS}: An \R package for data
  analysis methods in knowledge space theory. Manuscript under revision.

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
