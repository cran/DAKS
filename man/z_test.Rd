\name{z_test}
\alias{z_test}
\concept{performs a Z-test}
\title{Z-Test for diff values}
\description{
  Performs one- and two-sample Z-Tests for \emph{diff} values}
\usage{
z_test(dataset, imp, imp_alt = NULL, alternative = c("two.sided", "less","greater"), mu = 0, conf.level = 0.95, v)
}
\arguments{
  \item{dataset}{a required data frame or matrix consisting of binary,
    \eqn{1} or \eqn{0}, numeric data.}
  \item{imp}{a required object of class \code{\link[sets]{set}}
    representing the set of implications (ought to be a quasi order).}
  \item{imp_alt}{an optional set of implications, representing the alternative quasi order.}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".}
  \item{mu}{a number indicating the true value of the mean (or difference in means if you are performing a two sample test).}
  \item{conf.level}{confidence level of the interval.}
  \item{v}{a required numeric giving the inductive item tree analysis
    algorithm to be performed; \code{v = 1} (minimized corrected) and
    \code{v = 2} (corrected).}
}
\details{
  This function performs a Z-test for comparing the \emph{diff} values of one or two quasi orders.    
}
\value{
   If the arguments are of required types, \code{z_test} returns a named list consisting of the following four
   components:
   \item{Z.value}{the value of the z-statistic.}
   \item{p.value}{the p-value for the test.}
   \item{conf}{a confidence interval for the mean appropriate to the specified alternative hypothesis.}
   \item{diff.value}{The corresponding \emph{diff} values for the used quasi orders according to the specified method.}
   \item{alternative}{a character string specifying the alternative hypothesis.}
   \item{mu}{a number indicating the true value of the mean (or difference in means if you are performing a two sample test).}
   \item{conf.level}{specifying what kind of confidence interval is specified.}
}
\references{
  Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons.  Manuscript under
  revision.  \url{http://www.math.uni-augsburg.de/~uenlueal/}

  Sargin, A. and Uenlue, A. (2009) \pkg{DAKS}: An \R package for data
  analysis in knowledge space theory. Manuscript submitted for
  publication.  \url{http://www.math.uni-augsburg.de/~uenlueal/}

  Uenlue, A. and Sargin, A. (2008) Maximum likelihood methodology for
  \emph{diff} fit measures for quasi orders.  Manuscript submitted for
  publication.  \url{http://www.math.uni-augsburg.de/~uenlueal/}
}
\author{
  Anatol Sargin \email{anatol.sargin@math.uni-augsburg.de},
  Ali Uenlue \email{ali.uenlue@math.uni-augsburg.de}
}
\note{
  The current version of the package \pkg{DAKS} does not support
  peforming a Z-test for the original inductive
  item tree analysis algorithm.
}
\seealso{
  \code{\link{iita}}, the interface that provides the three (sample) inductive item tree analysis methods
  under one umbrella. See also \code{\link{DAKS-package}} for general
  information about this package.
}
\examples{
sel_set<-ind_gen(ob_counter(pisa))
z_test(pisa, sel_set[[9]], sel_set[[10]], v = 1)
}
\keyword{htest}
\keyword{manip}
\keyword{math}
\keyword{models}
\keyword{multivariate}
\keyword{univar}
\keyword{utilities}
