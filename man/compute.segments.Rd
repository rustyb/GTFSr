\name{compute.segments}
\alias{compute.segments}

\title{functoin to aggregate the shapes for each route}
\description{returns aggregated shape data. Can then be used to display network on a map.}
\usage{
compute.segments(shapes)
}
\arguments{
  \item{shapes}{Object of class GTFS@shapes}
}
\references{ondrejivanic's repository 131500: \url{https://github.com/ondrejivanic/131500}}
\author{Colin Broderick with inspiration from ondrejivanic}
\examples{
download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
air <- gtfs.make("data/aircoach")

new.segments <- compute.segments(air@shapes)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ gtfs }
