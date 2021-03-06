% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bag_geo_download.R
\name{get_woonkernen}
\alias{get_woonkernen}
\title{Download de woonkernen (bebouwde kommen) voor een gemeente}
\usage{
get_woonkernen(grens, ...)
}
\arguments{
\item{grens}{Een polygoon (projectie 4326 of 28992, wordt automatisch geconverteerd),
waarbinnen de woonkernen gezocht worden.}
}
\description{
Download woonkernen uit de Top10nl database.
}
\examples{
grens <- get_geo("Nederweert", "grens")
}
