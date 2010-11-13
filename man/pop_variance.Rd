\name{pop_variance}
\alias{pop_variance}
\concept{population asymptotic variance}
\title{Population Asymptotic Variance}
\description{
  \code{pop_variance} computes the population (exact) asymptotic
  variances of the maximum likelihood estimators \emph{diff}, assuming
  a multinomial probability distribution on the set of all response
  patterns.
}
\usage{
pop_variance(pop_matrix, imp, error_pop, v)
}
\arguments{
  \item{pop_matrix}{a required matrix of all possible response
    patterns and their corresponding population occurrence
    probabilities, for instance obtained from a call to
    \code{\link{pop_iita}}.}
  \item{imp}{a required object of class \code{\link[sets]{set}}
    representing the set of implications (ought to be a quasi order)
    for which \emph{diff} is computed, for instance obtained from a
    call to \code{\link{pop_iita}}.}
  \item{error_pop}{a required numeric giving the \eqn{\gamma}{gamma}
    rate to be used for computing \emph{diff}, for instance obtained
    from a call to \code{\link{pop_iita}}.}
  \item{v}{a required numeric giving the inductive item tree analysis
    algorithm to be performed, in population quantities; \code{v = 1}
    (minimized corrected) and \code{v = 2} (corrected).}
}
\details{
  Subject to the selected version to be performed, \code{pop_variance}
  computes the population asymptotic variance of the maximum
  likelihood estimator \emph{diff}, which here is formulated for the
  relation specified in \code{imp} and for the \eqn{\gamma}{gamma}
  rate in \code{error_pop}.  This population variance is obtained
  using the delta method, which requires calculating the Jacobian
  matrix of the \emph{diff} coefficient and the inverse of the
  expected Fisher information matrix for the multinomial distribution
  with cell probabilities as specified in \code{pop_matrix}.

  A set of implications, an object of the class
  \code{\link[sets]{set}}, consists of \eqn{2}-tuples \eqn{(i, j)} of
  the class \code{\link[sets]{tuple}}, where a \eqn{2}-tuple
  \eqn{(i, j)} is interpreted as `mastering item \eqn{j} implies
  mastering item \eqn{i}.'
}
\value{
  If the arguments \code{pop_matrix}, \code{imp}, \code{error_pop},
  and \code{v} are of required types, \code{pop_variance} returns a
  numeric giving the population asymptotic variance of the maximum
  likelihood estimator \emph{diff} (formulated for the relation in
  \code{imp} and the \eqn{\gamma}{gamma} rate in \code{error_pop}).
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
  Anatol Sargin \email{sargin@statistik.tu-dortmund.de},

  Ali Uenlue \email{uenlue@statistik.tu-dortmund.de}
}
\note{
  The current version of the package \pkg{DAKS} does not support
  computing population asymptotic variances for the original inductive
  item tree analysis algorithm; population asymptotic variances can be
  calculated only for the corrected and minimized corrected
  algorithms.

  The sample \emph{diff} coefficients of the three inductive item tree
  analysis algorithms can be transformed into maximum likelihood
  estimators, by division through the square of sample size.  These
  transformed \emph{diff} coefficients are considered in population
  quantities.  The \eqn{\gamma}{gamma} rates are the algorithms'
  specific estimates of the postulated response error probability.

  Estimated asymptotic variances of the maximum likelihood estimators
  \emph{diff} are implemented in the function \code{\link{variance}}.
}
\seealso{
  \code{\link{variance}} for estimated asymptotic variances of
  \emph{diff} coefficients; \code{\link{pop_iita}} for population
  inductive item tree analysis; \code{\link{ind_gen}} for (sample)
  inductive generation procedure; \code{\link{iita}}, the interface
  that provides the three (sample) inductive item tree analysis
  methods under one umbrella.  See also \code{\link{DAKS-package}} for
  general information about this package.
}
\examples{
x <- simu(5, 100, 0.05, 0.05, delta = 0.15)
y <- pop_iita(x$implications, 0.05, 0.05, 5, x$dataset, v = 2)
pop_variance(y$pop.matrix, 
             y$selection.set[[which(y$pop.diff == min(y$pop.diff))]],
             y$error.pop[which(y$pop.diff == min(y$pop.diff))], v = 2)
}
\keyword{htest}
\keyword{math}
\keyword{models}
\keyword{multivariate}
\keyword{univar}
\keyword{utilities}
