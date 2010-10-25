\name{RunuranGUI-package}
\alias{RunuranGUI-package}
\alias{RunuranGUI}
\docType{package}

\title{
  A GUI for the UNU.RAN random variate generators.
}

\description{
  This package provides a GUI (Graphical User Interface) for the UNU.RAN
  random variate generators.
}

\details{
  \tabular{ll}{
    Package: \tab RunuranGUI     \cr
    Type:    \tab Package        \cr
    Version: \tab 0.1           \cr
    Date:    \tab 2010-10-25    \cr
    License: \tab GPL 2 or later \cr
  }

  This package allows to build generators for non-uniform random
  variates interactively for quite arbitrary distributions.
  Thus one can either select one of the predefined distributions,
  or one can build a distribution object from scratch by providing data
  like the density function.
  
  \R code for the required calls from package \pkg{Runuran} can be
  displayed and stored for later use. Some basic analysis like
  goodness-of-fit tests can be performed.
}

\note{
  Package \pkg{RunuranGUI} is based on package \pkg{gWidgets}.
  It has been developed and tested using GUI toolkit \code{"RGtk2"}.
  It also works with other toolkits of the \pkg{gWidgets} family albeit
  the layout may look a bit sloppy and the images may not displayed
  properly. These can be used by installing the corresponding package
  and setting option \code{"guiToolkit"}, e.g.,

  \samp{options("guiToolkit"="tcltk")}
  
  The following toolkits should work:
  \tabular{llll}{
    \emph{toolkit} \tab \emph{package name} \tab \emph{source} \tab \cr
    \code{"RGtk2"} \tab \pkg{gWidgetsRGtk2} \tab CRAN    \tab (default)  \cr
    \code{"tcltk"} \tab \pkg{gWidgetstcltk} \tab CRAN    \tab \cr
    \code{"rJava"} \tab \pkg{gWidgetsrJava} \tab CRAN    \tab \cr
    \code{"Qt"}    \tab \pkg{gWidgetsrQt}   \tab R-forge \tab \cr
  }
}

\author{
  Josef Leydold \email{unuran@statmath.wu.ac.at}.
}

\section{Acknowledgements}{%
  The author thanks John Verzani for many helpful hints.}

\references{
  W. H\"ormann, J. Leydold, and G. Derflinger (2004):
  Automatic Nonuniform Random Variate Generation.
  Springer-Verlag, Berlin Heidelberg

  J. Leydold and W. H\"ormann (2000-2010):
  UNU.RAN User Manual, see
  \url{http://statmath.wu.ac.at/unuran/}.
}

\seealso{
  Start GUI by means of function \code{\link{unuran.gui}}.
  See package \code{\link[Runuran:Runuran-package]{Runuran}}
  for help on the UNU.RAN library of universal non-uniform random
  variate generators.
  Tests are implemented in package
  \code{\link[rvgtest:rvgtest-package]{rvgtest}}.
}

\keyword{distribution}
\keyword{datagen}
