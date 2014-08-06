\name{truncDiffTime}
\alias{truncDiffTime}
\title{function to truncate the time given a start and end time}
\description{function to truncate the time given a start and end time}
\usage{
truncDiffTime(s, e, i = 900)
}

\arguments{
  \item{s}{Start time as POSIXct datetime format: 2014-08-09 01:00:00}
  \item{e}{End time as POSIXct datetime format: 2014-08-09 02:00:00}
  \item{i}{seconds value}
}
\references{ondrejivanic's repository 131500: \url{https://github.com/ondrejivanic/131500}}
\author{Colin Broderick with inspiration from ondrejivanic}
\examples{
download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
air <- gtfs.make("data/aircoach")

## get all trips for a given day
air.20140808 <- gtfs.trips(air, as.POSIXct("2014-08-08"))

# compute the difference between each arrival for a given day
with(air.20140808,
truncDiffTime(min(arrival_time, na.rm  = TRUE), arrival_time) %% 86400
)
}
\keyword{ gtfs }

