\name{DAKS-package}
\alias{DAKS-package}
\concept{knowledge space theory}
\concept{inductive item tree analysis}
\docType{package}
\title{
  Data Analysis and Knowledge Spaces: The R Package DAKS
}
\description{
  The package \pkg{DAKS} implements three inductive item tree analysis
  algorithms for deriving quasi orders from binary data, the original,
  corrected, and minimized corrected algorithms.  It provides
  functions for computing population and estimated asymptotic
  variances of the \emph{diff} fit measures, and for switching between
  test item and knowledge state representations.  Other features are a
  Hasse diagram drawing device, a data and quasi order simulation tool
  based on a finite mixture latent variable model, and a function for
  computing response pattern and knowledge state frequencies.
}
\details{
  \tabular{ll}{
    Package: \tab \pkg{DAKS}\cr
    Type: \tab Package\cr
    Version: \tab 2.1-2\cr
    Date: \tab 2012-08-17\cr
    License: \tab \acronym{GPL} (>= 2)
  }
  Knowledge space theory is a recent psychometric test theory based on
  combinatorial mathematical structures (order and lattice theory);
  see \cite{Doignon and Falmagne (1999)}.  Solvability dependencies
  between dichotomous test items play an important role in knowledge
  space theory.  Utilizing hypothesized dependencies between items,
  knowledge space theory has been successfully applied for the
  computerized, adaptive assessment and training of knowledge.  For
  instance, see the \acronym{ALEKS} system, a fully automated math
  tutor on the Internet (\url{http://www.aleks.com/}).

  The package \pkg{DAKS} is implemented based on the S3 system.  It
  comes with a namespace and consists of the following functions (all
  functions are external, there are no internal functions):
  \code{\link{corr_iita}}, \code{\link{hasse}}, \code{\link{iita}},
  \code{\link{imp2state}}, \code{\link{ind_gen}},
  \code{\link{mini_iita}}, \code{\link{ob_counter}},
  \code{\link{orig_iita}}, \code{\link{pattern}},
  \code{\link{pop_iita}}, \code{\link{pop_variance}},
  \code{\link{print.iita}}, \code{\link{print.pat}},
  \code{\link{print.popiita}}, \code{\link{print.summpopiita}},
  \code{\link{print.ztest}}, \code{\link{simu}}, \code{\link{state2imp}},
  \code{\link{summary.iita}}, \code{\link{summary.popiita}},
  \code{\link{variance}}, and \code{\link{z_test}}.  There is an empirical
  dataset, \code{\link{pisa}}, accompanying the package \pkg{DAKS}.  This
  dataset is part of the 2003 Programme for International Student
  Assessment (PISA; \url{http://www.pisa.oecd.org/}).
}
\author{
  Anatol Sargin, Ali Uenlue

  Maintainer: Ali Uenlue \email{ali.uenlue@tum.de}
}
\references{
  Doignon, J.-P. and Falmagne, J.-C. (1999) \emph{Knowledge Spaces}.
  Berlin, Heidelberg, and New York: Springer-Verlag.

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
\keyword{package}
