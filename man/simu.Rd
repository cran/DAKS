\name{simu}
\alias{simu}
\concept{basic local independence model}
\concept{simulation}
\concept{simulated data}
\concept{simulated quasi order}
\title{
  Data and Quasi Order Simulation Tool
}
\description{
  \code{simu} can be used to simulate binary, of type \eqn{1}/\eqn{0},
  data using a basic local independence model. The number of
  items, the sample size, and two parameters for the careless error
  and lucky guess probabilities can be set explicitly. The underlying
  combinatorial structure used for simulating the data can either be
  specified manually or is generated randomly.
}
\usage{
simu(items, size, ce, lg, imp = NULL, delta)
}
\arguments{
  \item{items}{a required numeric giving the number of items of the
    domain taken as basis for the simulation.}
  \item{size}{a required numeric giving the number of response
    patterns to be simulated (the sample size).}
  \item{ce}{a required numeric giving the probability for a careless
    error.}
  \item{lg}{a required numeric giving the probability for a lucky
    guess.}
  \item{imp}{an optional object of class \code{\link[sets]{set}}
    representing the underlying set of implications (assumed to be a
    quasi order) used for simulating the data, for instance obtained
    from a call to \code{\link{iita}}.  The default \code{imp = NULL}
    corresponds to generating the quasi order used for simulating the
    data randomly.}
  \item{delta}{a required (if \code{imp = NULL}) numeric giving the
    probability for adding an item pair to the randomly generated
    quasi order (reflexive pairs are always included a priori).}
}
\details{
  The function \code{simu} simulates data using a special case of the
  basic local independence model, which is a fundamental restricted
  latent class model in knowledge space theory
  (\cite{Doignon and Falmagne, 1999}).  The single careless error
  \code{ce} and lucky guess \code{lg} probabilities are assumed to be
  constant over all items.  The most general case that can be
  specified thus includes two error probabilities at each item, the
  same two rates for all items.  The general form of the basic local
  independence model allows for varying careless error and lucky guess
  rates from item to item (not identifiable in general, however).

  If a quasi order is specified in \code{imp} explicitly, Birkhoff's
  theorem is used to derive its corresponding quasi ordinal knowledge
  space, which is equipped with the error probabilities \code{ce} and
  \code{lg} to give the basic local independence model used for
  simulating the data.  If \code{imp = NULL}, the underlying quasi
  order is generated randomly as follows.  All reflexive pairs are
  added to the relation.  The constant specified in \code{delta} is
  utilized as the probability for adding each of the remaining
  non-reflexive item pairs to the relation.  The transitive closure of
  this relation is computed, and the resulting quasi order is then the
  relation underlying the simulation.

  A set of implications, an object of the class
  \code{\link[sets]{set}}, consists of \eqn{2}-tuples \eqn{(i, j)} of
  the class \code{\link[sets]{tuple}}, where a \eqn{2}-tuple
  \eqn{(i, j)} is interpreted as `mastering item \eqn{j} implies
  mastering item \eqn{i}.'

  The simulated dataset contains only ones and zeros, which encode
  solving or failing to solve an item, respectively.
}
\value{
  If the arguments \code{items}, \code{size}, \code{ce}, \code{lg},
  \code{imp}, and \code{delta} are of required types, \code{simu}
  returns a named list consisting of the following three components:

  \item{dataset}{a matrix of binary, \eqn{1} or \eqn{0}, numeric
    data.}
  \item{implications}{an object of class \code{\link[sets]{set}}
    representing the underlying set of implications (assumed to be a
    quasi order) used for simulating the data.  If \code{imp = NULL},
    this is the quasi order that was randomly generated; otherwise
    identical to the set of implications specified in the argument
    \code{imp}.}
  \item{states}{a matrix consisting of ones or zeros (the quasi
    ordinal knowledge space), in which each row represents the
    \eqn{1}/\eqn{0}-pattern of a knowledge state.  This is the
    knowledge structure corresponding to the set of implications
    specified in \code{implications}.}
}
\references{
  Doignon, J.-P. and Falmagne, J.-C. (1999) \emph{Knowledge Spaces}.
  Berlin, Heidelberg, and New York: Springer-Verlag.

Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons. \emph{Mathematical Social Sciences}, \bold{58}, 376--392. 

Uenlue, A. and Sargin, A. (2010) \pkg{DAKS}: An \R package for data
  analysis methods in knowledge space theory. Manuscript under revision. 

 Uenlue, A. and Sargin, A. (2010) Maximum likelihood methodology for
  \emph{diff} fit measures for quasi orders.  Manuscript submitted for
  publication. 
}
\author{
  Anatol Sargin \email{sargin@statistik.tu-dortmund.de},

  Ali Uenlue \email{uenlue@statistik.tu-dortmund.de}
}
\note{
  To pass a quasi order as the argument \code{imp} to \code{simu} it
  may be more convenient to transform from knowledge states to
  implications using the function \code{\link{state2imp}}.

  The probability specified in \code{delta} does not necessarily
  correspond to the ratio of implications in the randomly generated
  quasi order, because the transitive closure is formed after having
  added item pairs.  In \cite{Sargin and Uenlue (2009)} a normal
  sampling scheme for drawing \code{delta} values using
  \eqn{\mu = 0.16}{mean = 0.16} and \eqn{\sigma = 0.06}{sd = 0.06} for
  nine items has been proposed.  This sampling scheme provides far
  better representative samples of quasi orders than sampling
  \code{delta} values uniformly from the unit interval.
}
\seealso{
  \code{\link{state2imp}} for transformation from knowledge states to
  implications; \code{\link{imp2state}} for transformation from
  implications to knowledge states; \code{\link{pop_iita}} for
  population inductive item tree analysis; \code{\link{iita}}, the
  interface that provides the three (sample) inductive item tree
  analysis methods under one umbrella.  See also
  \code{\link{DAKS-package}} for general information about this
  package.
}
\examples{
simu(7, 20, 0.1, 0.1, delta = 0.15)
}
\keyword{datagen}
\keyword{distribution}
\keyword{math}
\keyword{models}
\keyword{multivariate}
\keyword{utilities}
