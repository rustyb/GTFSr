\name{GTFS-class}
\Rdversion{1.1}
\docType{class}
\alias{GTFS-class}

\title{Class \code{"GTFS"}}
\description{Super class of GTFS containing all other classes}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("GTFS", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{agency_id}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{agency_name}:}{Object of class \code{"factor"} ~~ }
    \item{\code{agencies}:}{Object of class \code{"Agency"} ~~ }
    \item{\code{stops}:}{Object of class \code{"Stops"} ~~ }
    \item{\code{stop_times}:}{Object of class \code{"StopTimes"} ~~ }
    \item{\code{routes}:}{Object of class \code{"Routes"} ~~ }
    \item{\code{trips}:}{Object of class \code{"Trips"} ~~ }
    \item{\code{calendars}:}{Object of class \code{"Calendar"} ~~ }
    \item{\code{shapes}:}{Object of class \code{"Shapes"} ~~ }
    \item{\code{calendar_dates}:}{Object of class \code{"CalendarDates"} ~~ }
  }
}
%\section{Methods}{No methods defined with class "GTFS" in the signature.}
\references{GTFS Reference Guide: \url{https://developers.google.com/transit/gtfs/reference}}
\author{Colin Broderick}


\examples{
showClass("GTFS")
}
\keyword{classes}
