\name{corr_iita}
\alias{corr_iita}
\concept{corrected inductive item tree analysis}
\concept{IITA}
\title{Corrected Inductive Item Tree Analysis}
\description{
  \code{corr_iita} performs the corrected inductive item tree analysis 
  procedure and returns the corresponding \emph{diff} values.
}
\usage{
corr_iita(dataset, A)
}
\arguments{
  \item{dataset}{a required data frame or matrix consisting of binary,
    \eqn{1} or \eqn{0}, numeric data.}
  \item{A}{a required list of competing quasi orders (surmise
    relations), for instance obtained from a call to
    \code{\link{ind_gen}}.}
}
\details{
  Corrected inductive item tree analysis is a data analysis method for
  deriving knowledge structures (more precisely, surmise relations)
  from binary data.  Details on this procedure can be found in
  \code{\link{iita}}.  The set of competing quasi orders is passed via
  the argument \code{A}, so any selection set of quasi orders can be
  used.

  The set of competing quasi orders must be a list of objects of the
  class \code{\link[sets]{set}}.  These objects (quasi orders) consist
  of \eqn{2}-tuples \eqn{(i, j)} of the class
  \code{\link[sets]{tuple}},  where a \eqn{2}-tuple \eqn{(i, j)} is
  interpreted as `mastering item \eqn{j} implies mastering item
  \eqn{i}.'

  The data must contain only ones and zeros, which encode solving or
  failing to solve an item, respectively.
}
\value{
  If the arguments \code{dataset} and \code{A} are of required types,
  \code{corr_iita} returns a named list of the following components:

\item{diff.value}{a vector of the \emph{diff} values
  corresponding to the competing quasi orders in \code{A}.}
\item{error.rate}{a vector of the error rates corresponding to the competing quasi orders in \code{A}.}
}
\references{
  Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons. \emph{Mathematical Social
  Sciences}, \bold{58}, 376--392.

  Uenlue, A. and Sargin, A. (2010) \pkg{DAKS}: An \R package for data
  analysis methods in knowledge space theory. \emph{Journal of Statistical
  Software}, \bold{37}(2), 1--31.
  URL \url{http://www.jstatsoft.org/v37/i02/}.
}
\author{
  Anatol Sargin \email{sargin@statistik.tu-dortmund.de},

  Ali Uenlue \email{uenlue@statistik.tu-dortmund.de}
}
\note{
  The function \code{\link{iita}} can be used to perform one of the
  three inductive item tree analysis procedures (including the
  corrected inductive item tree analysis method) selectively.  Whereas
  for the function \code{corr_iita} a selection set of competing quasi
  orders has to be passed via the argument \code{A} manually,
  \code{iita} automatically generates a selection set from the data
  using the inductive generation procedure implemented in
  \code{\link{ind_gen}}.

  The latter approach using \code{\link{iita}} is common so far, in
  knowledge space theory, where the inductive data analysis methods
  have been utilized for exploratory derivations of surmise relations
  from data.  The function \code{corr_iita}, on the other hand, can be
  used to select among surmise relations for instance obtained from
  querying experts or from competing psychological theories.
}
\seealso{
  \code{\link{orig_iita}} for original inductive item tree analysis;
  \code{\link{mini_iita}} for minimized corrected inductive item tree
  analysis; \code{\link{iita}}, the interface that provides the three
  inductive item tree analysis methods under one umbrella;
  \code{\link{pop_variance}} for population asymptotic variances of
  \emph{diff} coefficients; \code{\link{variance}} for estimated
  asymptotic variances of \emph{diff} coefficients;
  \code{\link{pop_iita}} for population inductive item tree analysis.
  See also \code{\link{DAKS-package}} for general information about
  this package.
}
\examples{
ind <- ind_gen(ob_counter(pisa))
corr_iita(pisa, ind)
}
\keyword{manip}
\keyword{models}
\keyword{univar}
