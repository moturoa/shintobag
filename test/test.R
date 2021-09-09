

options(shintobag_conf = "c:/repos/conf/config.yml")

k <- get_kws("Eersel", "buurten", 2015)

g1 <- get_geo("Eersel", "buurten", jaar = "2021") %>%
  add_kws(peiljaar = 2021)
dim(g1)

g2 <- get_geo("Eersel", "buurten", jaar = "2021") %>%
  add_kws(peiljaar = 2018:2021)
dim(g2)


g3 <- get_geo("Eersel", "wijken", jaar = "2021") %>%
  add_kws(peiljaar = 2014)
dim(g3)

g3 <- get_geo("Eersel", "wijken", jaar = "2021") %>%
  add_kws(peiljaar = 2014)
dim(g3)

g4 <- get_geo("Eersel", "wijken", jaar = "2021", kws = TRUE, kws_jaar = 2018)
dim(g4)

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










