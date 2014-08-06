\name{timetable.stats}
\alias{timetable.stats}

\title{Function to return basic statistics of a given day in GTFS feed}
\description{function to compute some basic timetable stats including number of trips per mode and longest trip by mode.}
\usage{
timetable.stats(day, gtfs)
}
\arguments{
  \item{day}{takes an object outputted from gtfs.trips.summary}
  \item{gtfs}{object of class GTFS}
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

# get some basic stats for this day, includes number of trips and longest route.
air.stats <- timetable.stats(air.sum, air)
air.stats
}
\keyword{gtfs}