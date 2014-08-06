\name{gtfs.trips}
\alias{gtfs.trips}
\title{function to create data.frame of trips for a given day.}
\description{creates subset of GTFS object and returns all trips for a given date.}
\usage{
gtfs.trips(gtfs, date = Sys.time())
}

\arguments{
  \item{gtfs}{object of class GTFS}
  \item{date}{date to be queried in form of as.POSIXct("2013-08-09") }
}
\references{ondrejivanic's repository 131500: \url{https://github.com/ondrejivanic/131500}}
\author{Colin Broderick with inspiration from ondrejivanic}

\examples{

download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
air <- gtfs.make("data/aircoach")

## get all trips for a given day
air.20140808 <- gtfs.trips(air, as.POSIXct("2014-08-08"))

}

\keyword{ gtfs }
\keyword{ gtfs.trips }
