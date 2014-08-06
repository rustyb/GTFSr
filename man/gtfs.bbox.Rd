\name{gtfs.bbox}
\alias{gtfs.bbox}
\title{Function to find the bounding box for an object of class GTFS}
\description{Can compute the bounding box using either bbox or convex}
\usage{
gtfs.bbox(gtfs, type = c("bbox", "convex"))
}

\arguments{
  \item{gtfs}{object of class GTFS}
  \item{type}{type of bounding box to use: bbox or convex}
}


\references{
ondrejivanic's repository 131500: \url{https://github.com/ondrejivanic/131500}
}
\author{
Colin Broderick with inspiration from ondrejivanic
}

\seealso{
\code{\link{gtfs.bbox_plot}}
}
\examples{
download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
air <- gtfs.make("data/aircoach")

### show graphic of bounding box of services
gtfs.bbox(air, "bbox")
}

\keyword{ bbox }
\keyword{ gtfs }
