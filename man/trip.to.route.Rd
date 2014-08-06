\name{trip.to.route}
\alias{trip.to.route}
\title{function given a trip_id will return route long name}
\description{function that returns the route name given a trip_id.}
\usage{
trip.to.route(trip_id, gtfs)
}

\arguments{
  \item{trip_id}{trip_id as a string}
  \item{gtfs}{object of class GTFS}
}

\references{ondrejivanic's repository 131500: \url{https://github.com/ondrejivanic/131500}}
\author{Colin Broderick with inspiration from ondrejivanic}

\examples{
download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
# load a gtfs dataset
air <- gtfs.make("data/aircoach")

trip.to.route("70.daily.1-702-y11-3.2.O", air)
}

\keyword{ gtfs }
