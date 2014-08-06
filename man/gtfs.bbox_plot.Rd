\name{gtfs.bbox_plot}
\alias{gtfs.bbox_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a ggplot showing a map, google maps basemap, with the bounding box or convex hull.}
\description{Output a ggplot graphic given a GTFS instance and type of bounding box and zoom level of map. }
\usage{
gtfs.bbox_plot(gtfs_file, type = c("bbox", "convex"), zoom_level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gtfs_file}{input instance of class GTFS.}
  \item{type}{The type of bounding box to display. convex or bbox.}
  \item{zoom_level}{set the zoom level of the resulting google map.}
}

\references{
ondrejivanic's repository 131500: \url{https://github.com/ondrejivanic/131500}
}
\author{
Colin Broderick with inspiration from ondrejivanic
}


\examples{

download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
## load in your gtfs dataset
air <- gtfs.make("data/aircoach")

### show graphic of bounding box of services
### this will open your plot window

gtfs.bbox_plot(air, "bbox", 8)
}

\keyword{ gtfs }
\keyword{ plot }
