\name{ind_gen}
\alias{ind_gen}
\concept{inductive generation}
\concept{counterexample}
\title{Inductive Generation Procedure}
\description{
  \code{ind_gen} generates inductively a set of competing quasi
  orders.
}
\usage{
ind_gen(b)
}
\arguments{
  \item{b}{a required matrix of the numbers of counterexamples for all
    pairs of items, for instance obtained from a call to
    \code{\link{ob_counter}}.}
}
\value{
  If the argument \code{b} is of required type, \code{ind_gen} returns
  a list of the inductively generated quasi orders.
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
  analysis methods in knowledge space theory. Manuscript under revision.
  
}
\author{
  Anatol Sargin \email{sargin@statistik.tu-dortmund.de},

  Ali Uenlue \email{uenlue@statistik.tu-dortmund.de}
}
\note{
  The function \code{\link{iita}} calls \code{\link{ind_gen}} for
  constructing the set of competing quasi orders according to the
  inductive generation procedure.

  The set of competing quasi orders is a list of objects of the class
  \code{\link[sets]{set}}.  These objects (quasi orders) consist of
  \eqn{2}-tuples \eqn{(i, j)} of the class \code{\link[sets]{tuple}},
  where a \eqn{2}-tuple \eqn{(i, j)} is interpreted as `mastering item
  \eqn{j} implies mastering item \eqn{i}.'
}
\seealso{
  \code{\link{ob_counter}} for computation of numbers of
  counterexamples; \code{\link{iita}}, the interface that provides the
  three inductive item tree analysis methods under one umbrella\code{\link{z_test}} for one- and two-sample Z-tests.  See
  also \code{\link{DAKS-package}} for general information about this
  package.
}
\examples{
ob <- ob_counter(pisa)
ind_gen(ob)
}
\keyword{manip}
\keyword{models}
