\name{ob_counter}
\alias{ob_counter}
\concept{counterexample}
\title{Computation of Numbers of Counterexamples}
\description{
  \code{ob_counter} computes from a dataset for all item pairs the
  corresponding numbers of counterexamples.
}
\usage{
ob_counter(dataset)
}
\arguments{
  \item{dataset}{a required data frame or matrix consisting of binary,
    \eqn{1} or \eqn{0}, numeric data.}
}
\details{
  For an item pair \eqn{(i, j)}, the number of subjects solving item
  \eqn{j} but failing to solve item \eqn{i}, is the corresponding
  number of counterexamples.  Their response patterns contradict the
  interpretation of \eqn{(i, j)} as `mastering item \eqn{j} implies
  mastering item \eqn{i}.'

  The data must contain only ones and zeros, which encode solving or
  failing to solve an item, respectively.
}
\value{
  If the argument \code{dataset} is of required type,
  \code{ob_counter} returns a matrix of the numbers of counterexamples 
  for all pairs of items.
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
  Anatol Sargin, Ali Uenlue
}
\note{
  The function \code{ind_gen} can be used to inductively generate from
  the returned matrix of the numbers of counterexamples a set of quasi
  orders.

  The function \code{\link{iita}} calls \code{\link{ob_counter}}.
}
\seealso{
  \code{\link{ind_gen}} for inductive generation procedure using
  numbers of counterexamples; \code{\link{iita}}, the interface that
  provides the three inductive item tree analysis methods under one
  umbrella. See also \code{\link{DAKS-package}} for general
  information about this package.
}
\examples{
ob_counter(pisa)
}
\keyword{manip}
\keyword{univar}
\keyword{utilities}
