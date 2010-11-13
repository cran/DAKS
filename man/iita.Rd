\name{iita}
\alias{iita}
\concept{inductive item tree analysis}
\concept{IITA}
\title{
  Inductive Item Tree Analysis
}
\description{
  \code{iita} can be used to perform one of the three inductive item
  tree analysis algorithms (original, corrected, and minimized
  corrected) selectively.
}
\usage{
iita(dataset, v)
}
\arguments{
  \item{dataset}{a required data frame or matrix consisting of binary,
    \eqn{1} or \eqn{0}, numeric data.}
  \item{v}{a required numeric giving the inductive item tree analysis
    algorithm to be performed; \code{v = 1} (minimized corrected),
    \code{v = 2} (corrected), and \code{v = 3} (original).}
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
  the expected numbers of counterexamples.  (For an item pair
  \eqn{(i, j)}, the number of subjects solving item \eqn{j} but
  failing to solve item \eqn{i}, is the corresponding number of
  counterexamples.  Their response patterns contradict the
  interpretation of \eqn{(i, j)} as `mastering item \eqn{j} implies
  mastering item \eqn{i}.') The algorithms are described in the paper
  about the \pkg{DAKS} package by \cite{Uenlue and Sargin (2010)}, and
  in the papers by \cite{Sargin and Uenlue (2009)} and
  \cite{Uenlue and Sargin (2010)}.

  \code{iita} calls \code{\link{ind_gen}} for constructing the set of
  competing quasi orders according to the inductive generation
  procedure.  Subject to the selected version to be performed,
  \code{iita} computes the discrepancies between observed and expected
  numbers of counterexamples under each relation, and finds a quasi
  order with the minimum discrepancy (\emph{diff}) value.

  A set of implications, an object of the class
  \code{\link[sets]{set}}, consists of \eqn{2}-tuples \eqn{(i, j)} of
  the class \code{\link[sets]{tuple}}, where a \eqn{2}-tuple
  \eqn{(i, j)} is interpreted as `mastering item \eqn{j} implies
  mastering item \eqn{i}.'

  The data must contain only ones and zeros, which encode solving or
  failing to solve an item, respectively.
}
\value{
  If the arguments \code{dataset} and \code{v} are of required types,
  \code{iita} returns a named list consisting of the following five
  components:

  \item{diff}{a vector giving the \emph{diff} values corresponding to
    the (inductively generated) competing quasi orders.}
  \item{implications}{an object of class \code{\link[sets]{set}}
    representing the solution quasi order (with smallest \emph{diff}
    value) under the selected algorithm.}
  \item{error.rate}{a value giving the estimated error rate corresponding to the best fitting quasi order.}
  \item{selection.set.index}{a numeric giving the index of the
    solution quasi order in the selection set.}
  \item{v}{the version used; \code{v = 1} (minimized corrected),
    \code{v = 2} (corrected), and \code{v = 3} (original).} 
}
\references{
 Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons. \emph{Mathematical Social Sciences}, \bold{58}, 376--392.

  Schrepp, M. (1999) On the empirical construction of implications
  between bi-valued test items. \emph{Mathematical Social Sciences},
  \bold{38}, 361--375.

  Schrepp, M. (2003) A method for the analysis of hierarchical
  dependencies between items of a questionnaire. \emph{Methods of
  Psychological Research}, \bold{19}, 43--79.

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
  The function \code{\link{iita}} can be used to perform one of the
  three inductive item tree analysis procedures selectively.  Whereas
  for the functions \code{\link{orig_iita}}, \code{\link{corr_iita}},
  \code{\link{mini_iita}} selection sets of competing quasi orders
  have to be passed via an argument manually, \code{iita}
  automatically generates a selection set from the data using the
  inductive generation procedure implemented in \code{\link{ind_gen}}.

  The latter approach using \code{iita} is common so far, in knowledge
  space theory, where the inductive data analysis methods have been
  utilized for exploratory derivations of surmise relations from data. 
  The functions \code{\link{orig_iita}}, \code{\link{corr_iita}},
  \code{\link{mini_iita}}, on the other hand, can be used to select
  among surmise relations for instance obtained from querying experts
  or from competing psychological theories.
}
\seealso{
  \code{\link{orig_iita}} for original inductive item tree analysis;
  \code{\link{corr_iita}} for corrected inductive item tree analysis;
  \code{\link{mini_iita}} for minimized corrected inductive item tree
  analysis; \code{\link{ind_gen}} for inductive generation procedure;
  \code{\link{pop_variance}} for population asymptotic variances of
  \emph{diff} coefficients; \code{\link{variance}} for estimated
  asymptotic variances of \emph{diff} coefficients; \code{\link{z_test}} for one- and two-sample Z-tests; 
  \code{\link{pop_iita}} for population inductive item tree analysis.
  See also \code{\link{DAKS-package}} for general information about
  this package.
}
\examples{
iita(pisa, v = 1)
iita(pisa, v = 3)
}
\keyword{manip}
\keyword{models}
\keyword{univar}
