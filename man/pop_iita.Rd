\name{pop_iita}
\alias{pop_iita}
\concept{population inductive item tree analysis}
\title{
  Population Inductive Item Tree Analysis
}
\description{
  \code{pop_iita} can be used to perform one of the three inductive
  item tree analysis algorithms (original, corrected, and minimized
  corrected) in population quantities (in a known population)
  selectively.
}
\usage{
pop_iita(imp, ce, lg, items, dataset = NULL, A = NULL, v)
}
\arguments{
  \item{imp}{a required object of class \code{\link[sets]{set}}
    representing the underlying set of implications (assumed to be a
    quasi order), for instance obtained from a call to
    \code{\link{iita}}.}
  \item{ce}{a required numeric giving the probability for a careless
    error.}
  \item{lg}{a required numeric giving the probability for a lucky
    guess.}
  \item{items}{a required numeric giving the number of items of the
    domain taken as basis for \code{imp}.}
  \item{dataset}{an optional data frame or matrix consisting of
    binary, \eqn{1} or \eqn{0}, numeric data.  The default
    \code{dataset = NULL} corresponds to no dataset being used.}
  \item{A}{an optional list of competing quasi orders (surmise
    relations).}
  \item{v}{a required numeric giving the inductive item tree analysis
    algorithm to be performed, in population quantities; \code{v = 1}
    (minimized corrected), \code{v = 2} (corrected), and \code{v = 3}
    (original).}
}
\details{
  The three inductive item tree analysis algorithms are exploratory
  methods for extracting quasi orders (surmise relations) from data.
  In each algorithm, competing binary relations are generated (in the
  same way for all three versions), and a fit measure (differing from
  version to version) is computed for every relation of the selection
  set in order to find the quasi order that fits the data best.  In
  all three algorithms, the idea is to estimate the numbers of
  counterexamples for each quasi order, and to find, over all
  competing quasi orders, the minimum value for the discrepancy
  between the observed and expected numbers of counterexamples.  The
  three data analysis methods differ in their choices of estimates for
  the expected numbers of counterexamples.  For details see
  \code{\link{iita}}.  The algorithms are described in the paper about
  the \pkg{DAKS} package by \cite{Uenlue and Sargin (2010)}, and in
  the papers by \cite{Sargin and Uenlue (2009)} and
  \cite{Uenlue and Sargin (2010)}.

  Compared to \code{\link{iita}}, the function \code{pop_iita}
  implements the three inductive item tree analysis algorithms in
  population, \bold{not} sample, quantities.  The argument \code{imp}
  must give a quasi order, and equipped with the error probabilities
  \code{ce} and \code{lg}, it is considered a special case of the
  basic local independence model (\cite{Doignon and Falmagne, 1999}).
  The latter then is considered as the underlying population model.
  If \code{dataset = NULL} a set of competing quasi orders is
  constructed based on a population analog of the inductive generation
  procedure implemented in sample quantities in \code{\link{ind_gen}}.
  If a dataset is specified explicitly, that data are used to generate
  the set of competing quasi orders based on the sample version of the
  inductive generation procedure.

  A set of implications, an object of the class
  \code{\link[sets]{set}}, consists of \eqn{2}-tuples \eqn{(i, j)} of
  the class \code{\link[sets]{tuple}}, where a \eqn{2}-tuple
  \eqn{(i, j)} is interpreted as `mastering item \eqn{j} implies
  mastering item \eqn{i}.'

  The data (in \code{dataset}) must contain only ones and zeros, which
  encode solving or failing to solve an item, respectively.
}
\value{
  If the arguments \code{imp}, \code{ce}, \code{lg}, \code{items},
  \code{dataset}, \code{A}, and \code{v} are of required types,
  \code{pop_iita} returns a named list consisting of the following five
  components:

  \item{pop.diff}{a vector giving the population \emph{diff} values
    corresponding to the (inductively generated) competing quasi
    orders (subject to selected version that was performed).}
  \item{pop.matrix}{a matrix of all possible response patterns and
    their corresponding population occurrence probabilities.}
  \item{error.pop}{a vector of the population \eqn{\gamma}{gamma}
    rates corresponding to the (inductively generated) competing quasi
    orders (subject to selected version that was performed).}
  \item{selection.set}{a list of the (inductively generated) competing
    quasi orders.}
  \item{v}{the version used; \code{v = 1} (minimized corrected),
    \code{v = 2} (corrected), and \code{v = 3} (original).} 
}
\references{
  Doignon, J.-P. and Falmagne, J.-C. (1999) \emph{Knowledge Spaces}.
  Berlin, Heidelberg, and New York: Springer-Verlag.

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
  The single careless error \code{ce} and lucky guess \code{lg}
  probabilities are assumed to be constant over all items.  The most
  general case that can be specified thus includes two error
  probabilities, which are the same for all items.

  The sample \emph{diff} coefficients of the three inductive item tree
  analysis algorithms can be transformed into maximum likelihood
  estimators, by division through the square of sample size.  These
  transformed \emph{diff} coefficients are considered in population
  quantities.  The \eqn{\gamma}{gamma} rates are the algorithms'
  specific estimates of the postulated response error probability.

  Population and estimated asymptotic variances of the maximum
  likelihood estimators \emph{diff} are implemented in the functions
  \code{\link{pop_variance}} and \code{\link{variance}}, respectively.
}
\seealso{
  \code{\link{pop_variance}} for population asymptotic variances of
  \emph{diff} coefficients; \code{\link{variance}} for estimated
  asymptotic variances of \emph{diff} coefficients; \code{\link{simu}}
  for data simulation tool; \code{\link{ind_gen}} for (sample)
  inductive generation procedure; \code{\link{iita}}, the interface
  that provides the three (sample) inductive item tree analysis
  methods under one umbrella.  See also \code{\link{DAKS-package}} for
  general information about this package.
}
\examples{
x <- simu(3, 10000, ce = 0.05, lg = 0.05, delta = 0.12)
y <- iita(x$dataset, v = 2)
z <- pop_iita(x$implications, 0.05, 0.05, 3, x$dataset, v = 2)

## similar sample and population diff values are obtained
(y$diff) / (10000^2)
z
}
\keyword{htest}
\keyword{manip}
\keyword{models}
\keyword{univar}
