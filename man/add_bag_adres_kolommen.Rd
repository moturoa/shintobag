% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bag_geo_download.R
\name{add_bag_adres_kolommen}
\alias{add_bag_adres_kolommen}
\title{Maak extra adres velden aan in het BAG}
\usage{
add_bag_adres_kolommen(data)
}
\arguments{
\item{data}{De BAG dataset (spatial (rds) of niet (feather)).}
}
\description{
Combineert openbareruimtenaam, huisnummer, huisletter, huisnummertoevoeging
in twee nieuwe kolommen: huisnummerhuisletter (12, 13B), en bag_adres (bv. Huisstraat 1A 12, Schoolplein 1).
Vervangt ook NA met "" in huisnummer, huisletter, huisnummertoevoeging kolommen.
}
\examples{
\dontrun{

bag <- add_bag_adres_kolommen(bag)

}
}
