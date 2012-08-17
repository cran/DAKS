\name{variance}
\alias{variance}
\concept{estimated asymptotic variance}
\title{Estimated Asymptotic Variance}
\description{
  \code{variance} computes estimated asymptotic variances of the
  maximum likelihood estimators \emph{diff} from data, assuming a
  multinomial probability distribution on the set of all response
  patterns.
}
\usage{
variance(dataset, imp, v)
}
\arguments{
  \item{dataset}{a required data frame or matrix consisting of binary,
    \eqn{1} or \eqn{0}, numeric data.}
  \item{imp}{a required object of class \code{\link[sets]{set}}
    representing the set of implications (ought to be a quasi order)
    for which \emph{diff} is computed, for instance obtained from a
    call to \code{\link{simu}}.}
  \item{v}{a required numeric giving the inductive item tree analysis
    algorithm to be performed; \code{v = 1} (minimized corrected) and
    \code{v = 2} (corrected).}
}
\details{
  Subject to the selected version to be performed, \code{variance}
  computes a consistent estimator for the population asymptotic
  variance of the maximum likelihood estimator \emph{diff}, which here
  is formulated for the relation specified in \code{imp} and for the
  data in \code{dataset}.  This estimated asymptotic variance is
  obtained using the delta method, which requires calculating the
  Jacobian matrix of the \emph{diff} coefficient and the inverse of
  the expected Fisher information matrix for the multinomial
  distribution on the set of all response patterns.  In the expression
  for the exact asymptotic variance, the true parameter vector of
  multinomial probabilities is estimated by its corresponding maximum
  likelihood estimate (vector of the relative frequencies of the
  response patterns).

  A set of implications, an object of the class
  \code{\link[sets]{set}}, consists of \eqn{2}-tuples \eqn{(i, j)} of
  the class \code{\link[sets]{tuple}}, where a \eqn{2}-tuple
  \eqn{(i, j)} is interpreted as `mastering item \eqn{j} implies
  mastering item \eqn{i}.'

  The data must contain only ones and zeros, which encode solving or
  failing to solve an item, respectively.
}
\value{
  If the arguments \code{dataset}, \code{imp}, and \code{v} are of
  required types, \code{variance} returns a numeric giving the
  estimated asymptotic variance of the maximum likelihood estimator
  \emph{diff} (formulated for the relation in \code{imp} and the data
  in \code{dataset}).
}
\references{
Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons. \emph{Mathematical Social Sciences}, \bold{58}, 376--392.

  Uenlue, A. and Sargin, A. (2010) \pkg{DAKS}: An \R package for data
  analysis methods in knowledge space theory. \emph{Journal of Statistical
  Software}, \bold{37}(2), 1--31.
  URL \url{http://www.jstatsoft.org/v37/i02/}.

 Uenlue, A. and Sargin, A. (2010) Maximum likelihood methodology for
  \emph{diff} fit measures for quasi orders.  Manuscript submitted for
  publication. 
}
\author{
  Anatol Sargin, Ali Uenlue
}
\note{
  The current version of the package \pkg{DAKS} does not support
  computing estimated asymptotic variances for the original inductive
  item tree analysis algorithm; population asymptotic variances can be
  estimated only for the corrected and minimized corrected algorithms.

  The two types of estimators for the population asymptotic variances
  of the \emph{diff} coefficients obtained using the expected Fisher
  information matrix on the one hand, and the observed Fisher
  information matrix on the other, yield the same result, in the case
  of the multinomial distribution.  Since computation based on
  expected Fisher information is faster, this is implemented in
  \code{variance}.

  The sample \emph{diff} coefficients of the three inductive item tree
  analysis algorithms can be transformed into maximum likelihood
  estimators, by division through the square of sample size.  These
  transformed \emph{diff} coefficients are considered in sample and
  population quantities.

  Population (exact) asymptotic variances of the maximum likelihood
  estimators \emph{diff} are implemented in the function
  \code{\link{pop_variance}}.
}
\seealso{
  \code{\link{pop_variance}} for population asymptotic variances of
  \emph{diff} coefficients; \code{\link{pop_iita}} for population
  inductive item tree analysis; \code{\link{iita}}, the interface that
  provides the three (sample) inductive item tree analysis methods
  under one umbrella; \code{\link{z_test}} for one- and two-sample Z-tests.  See also \code{\link{DAKS-package}} for general
  information about this package.
}
\examples{
x <- simu(5, 100, 0.05, 0.05, delta = 0.15)
variance(x$dataset, x$implications, v = 2)
}
\keyword{htest}
\keyword{manip}
\keyword{math}
\keyword{models}
\keyword{multivariate}
\keyword{univar}
\keyword{utilities}
