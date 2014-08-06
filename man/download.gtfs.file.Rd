\name{download.gtfs.file}
\alias{download.gtfs.file}
\title{download and unzip gtfs file}
\description{Function to download and unzip the }
\usage{
download.gtfs.file(url, path)
}

\arguments{
  \item{url}{url of .zip file}
  \item{path}{Path to directory you wish to download the file to.}
}
\author{Colin Broderick}

\examples{
#use this function to download a file and store it in a directory.
download.gtfs.file("http://www.transportforireland.ie/transitData/google_transit_aircoach.zip", "data/aircoach")
}
\keyword{ gtfs}