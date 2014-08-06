\name{gtfs.make}
\alias{gtfs.make}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to import a GTFS feed and create an instance of GTFS class}
\description{
Given a directory which contains the main files of a GTFS feed, import the files into 
an instance of GTFS class. Required files: agency.txt, stops.txt, routes.txt, trips.txt, 
stop_times.txt, calendar.txt, shapes.txt 
}
\usage{gtfs.make(path)}

\arguments{
  \item{path}{Provide the path to the directory containing the files.}
}
%\value{}
\references{
GTFS Reference Guide: \url{https://developers.google.com/transit/gtfs/reference}
}
\author{Colin Broderick}
\examples{
download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
## load in your gtfs dataset
air <- gtfs.make("data/aircoach")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ gtfs }
