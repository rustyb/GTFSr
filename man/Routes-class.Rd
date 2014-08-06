\name{Routes-class}
\Rdversion{1.1}
\docType{class}
\alias{Routes-class}

\title{Class \code{"Routes"}}
\description{An S4 class to hold routes.txt data that is checked for consistency }
\author{Colin Broderick}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Routes", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
    \item{\code{names}:}{Object of class \code{"character"} ~~ }
    \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{data.frame}"}, directly.
Class \code{"\linkS4class{list}"}, by class "data.frame", distance 2.
Class \code{"\linkS4class{oldClass}"}, by class "data.frame", distance 2.
Class \code{"\linkS4class{vector}"}, by class "data.frame", distance 3.
}
\section{Methods}{
No methods defined with class "Routes" in the signature.
}

\examples{
showClass("Routes")
}
\keyword{classes}
