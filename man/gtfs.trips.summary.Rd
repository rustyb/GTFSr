\name{gtfs.trips.summary}
\alias{gtfs.trips.summary}
\title{Given the trips on a given day from gtfs.trips, give summary in data.frame }
\description{Given the trips on a given day from gtfs.trips, give summary in data.frame}
\usage{
gtfs.trips.summary(trips)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trips}{object returned from gtfs.trips function}
}
\references{ondrejivanic's repository 131500: \url{https://github.com/ondrejivanic/131500}}
\author{Colin Broderick with inspiration from ondrejivanic}

\examples{
download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
air <- gtfs.make("data/aircoach")

## get all trips for a given day
air.20140808 <- gtfs.trips(air, as.POSIXct("2014-08-08"))

## get a summary of thes trips on this day. includes distance, duration, number of stops etc. ...
air.sum <- gtfs.trips.summary(air.20140808)

}
\keyword{ gtfs }
