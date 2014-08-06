\name{read.gtfs.file}
\alias{read.gtfs.file}
\title{function to read the csv file of a given name and path}
\description{this function is used by gtfs.make to load each csv file into it's class.}
\usage{
read.gtfs.file(name, path)
}

\arguments{
  \item{name}{name of .txt file to load e.g. "agency.txt" }
  \item{path}{directory path to file to load.}
}

\references{ondrejivanic's repository 131500: \url{https://github.com/ondrejivanic/131500}}
\author{Colin Broderick with inspiration from ondrejivanic}
\examples{
download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
## load the routes csv file
read.gtfs.file("routes.txt", "data/aircoach")
}

\keyword{ csv }
\keyword{ gtfs }
