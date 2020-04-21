


bag <- feather::read_feather("cache/bag_Nederweert.feather")

adres <- read.csv2("test/adressen_nederweert.csv",
                   stringsAsFactors = FALSE,
                   colClasses = "character")


dat <- adres[1:10,]

#
# source("R/utils.R")
# source("R/functions.R")
library(shintobag)

adres$adresveld1 <- with(adres, paste(straat, huisnummer, huisletter))


make_address_field(data = dat,
  columns = list(
    straat = "straat",
    huisnummer = "huisnummer"
))



make_address_field(data = adres$adresveld1[1:10],
                   template = "{straat} {huisnummer}"
                   ) %>%
  match_bag_address(., bag = bag, bag_columns = c("woonplaatsnaam","adresseerbaarobject"))



search_bag_address(adres$adresveld1[11], bag, template = "{straat}{huisnummer}")



options(shintobag_conf = "c:/repos/wbm3.0/conf/config.yml")

download_gemeente_opendata("Rozendaal", out_path = "cache")










