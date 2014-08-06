\name{time_to.seconds}
\alias{time_to.seconds}
\title{helper function to return seconds given time HH:MM:SS}
\description{returns the seconds given a time. this is used to help calculations of route duration in gtfs.trips.summary}
\usage{
time_to.seconds(t)
}
\arguments{
  \item{t}{time value}
}
\references{ondrejivanic's repository 131500: \url{https://github.com/ondrejivanic/131500}}
\author{Colin Broderick with inspiration from ondrejivanic}
\examples{
#return time in seconds
time_to.seconds("09:00:23")

}



\keyword{ gtfs }
