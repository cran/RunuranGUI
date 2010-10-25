\name{stage1}
\alias{stage1}

\title{
  Stage 1 of the GUI for the UNU.RAN random variate generators.
}

\description{
  Stage 1 allows to select the type of distribution and the generation
  method.
}

\section{Distribution}{
  \enumerate{
    \item Select type of distribution:\cr
    - \sQuote{continuous univariate}, or \cr
    - \sQuote{discrete univariate}.
    %%
    \item Choose one of the alternatives:\cr
    - \pkg{Runuran} \sQuote{built-in} distribution, or \cr
    - create your own \sQuote{user-defined} distribution.
    %%
    \item Select one of the built-in distributions (if requested).
  }
  The parameters of \sQuote{built-in} distributions and required input for the
  \sQuote{user-defined} distributions are inserted on Stage 2. 
}

\section{Generation Method}{
  \enumerate{
    \item Select one of the alternatives:
    \describe{
      \item{Automatic -}{UNU.RAN tries to find an appropriate method for
	the given distribution.}
      \item{Inversion -}{An appropriate inversion method is used.}
      \item{Rejection -}{An appropriate rejection method is used.}
      \item{Select method -}{Choose a method by your own.}
    }
    \item In case of \sQuote{Select method} choose one of the generation
    metods from the list.
  }
}

\section{Proceed to Stage 2}{
  Click on button \sQuote{Ok} to proceed to Stage 2.
}
