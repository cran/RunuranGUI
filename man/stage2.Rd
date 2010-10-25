\name{stage2}
\alias{stage2}

\title{
  Stage 2 of the GUI for the UNU.RAN random variate generators.
}

\description{
  Stage 2 allows to set all required parameters for the selected
  distribution and generation method.
}

\section{Distribution}{
  The input data for the distribution heavily depends on the chosen
  distribution, see the second page of this notebook for details.

  \itemize{
    \item \pkg{Runuran} \sQuote{built-in} distributions require the 
    parameters of the distributions. Default values are preset and can
    be changed.
    \item \sQuote{user-defined} distributions usually require data like
    the density function and/or the cumulative distribution function
    (CDF) as well as the center (mode) of the distribution.
  }
  In addition the domain of the distribution has to be set / may be
  changed. 
  For \sQuote{built-in} distributions one can easily draw samples from
  truncated distributions.
}

\section{Generation Method}{
  Set parameters for the generation method.
  These depend on the particular choice of the method, 
  see the third page of this notebook for details.
}

\section{Perform}{
  The following UNU.RAN object are created and can be assigned to \R
  objects of the given names.
  \itemize{
    \item distribution object
    \item generator object
    \item random sample of given size
  }
  The checked objects are stored in the calling environment (or the
  environment passed as argument when calling \code{unuran.gui}).

  In addition one can display the \R code that generates these UNU.RAN
  objects when checking the item 
  \itemize{
    \item show \R code and validate generator
  }
  Then a notebook is created that shows \R code as well as the
  properties of the generator object. Moreover, other pages may be shown
  that allow to investigate the quality of the generator object.
}

\section{Create Object}{
  Click on button \sQuote{Ok} to create the UNU.RAN objects
  (and show \R code if requested).
}
