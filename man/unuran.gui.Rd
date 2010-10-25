\name{unuran.gui}
\alias{unuran.gui}

\title{
  A GUI for the UNU.RAN random variate generators.
}

\description{
  Starts a GUI that allows to build generators for non-uniform random
  variates interactively for quite arbitrary distributions.
}

\usage{
unuran.gui(envir=parent.frame())
}

\arguments{
  \item{envir}{the \code{\link{environment}} in which generator object
    and other requested data is stored.}
}

\details{
  The GUI allows to build and (to some extend) analyse generators or
  quite arbitrary distributions. This is done in three stages:
  \describe{
    \item{Stage 1:}{Select type of distribution (continuous or discrete)
      and genertion method (automatic, inversion, rejection, or select a
      particular algorithm). In addition one can pick a distribution
      from a list of built-in ones or decide to define a distribution from
      scratch.}
    
    \item{Stage 2:}{Provide parameters for built-in distributions or
      required information (like the density) for user-defined
      distributions. In addition the domain of the distribution can be
      truncated. Furthermore, default parameters for the selected
      methods may be modified.}

    \item{Stage 3 (optional):}{Show R code that creates generator
      object and properties of this \sQuote{Runuran} object.
      In addition it is possible to perform some tests (like
      goodness-of-fit tests) on the generator.
      Notice, however, that these tests ar not available for all types
      of generators. E.g., approximation errors are only shown for
      methods that are based on numerical inversion.}
  }

}

%% \value{}

\author{
  Josef Leydold \email{unuran@statmath.wu.ac.at}.
}

%% \seealso{}

\examples{
## start GUI to Runuran random number generator
unuran.gui()
}

\keyword{distribution}
\keyword{datagen}
