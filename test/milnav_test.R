

if(FALSE){

  mm <- read.csv("test/bag_mismatches_example.csv")


  data <- select(mm,
                 straat = mil_businessnumber,
                 huisnummer = mil_street,
                 huisletter = mil_housenumber,
                 huisnummertoevoeging = mil_housecharacter) %>%
    filter(!is.na(straat), !is.na(huisnummer))

  write.csv2(data, "test/milnav_bag_mismatch.csv", row.names=FALSE)

}



library(shintobag)
library(sf)

data <- read.csv2("test/milnav_bag_mismatch.csv")
data$rowid <- 1:nrow(data)

bag <- readRDS("c:/repos/kamerverhuur_data/data_public/bag_Eindhoven.rds") %>%
  st_drop_geometry()

# Straten komen niet voor in het BAG
data[which(!data$straat %in% bag$openbareruimtenaam),]

# match alles op straat + huisnummer (--> duplicates)
data2 <- left_join(data, select(bag,
                                straat = openbareruimtenaam,
                                huisnummer,
                                huisletter, huisnummertoevoeging),
                   by = c("straat","huisnummer")) %>%
  distinct

# Complete match
data3 <- left_join(data, select(bag,
                                straat = openbareruimtenaam,
                                huisnummer,
                                huisletter, huisnummertoevoeging,
                                adresseerbaarobject),
                   by = c("straat","huisnummer","huisletter","huisnummertoevoeging"))
dim(data3)

# full match: op simpele join
data_fm <- filter(data3, !is.na(adresseerbaarobject)) %>% distinct

m1 <- make_address_field(data = data_fm,
                   columns = list(straat = "straat",
                                  huisnummer = "huisnummer",
                                  huisletter = "huisletter",
                                  huisnummertoevoeging = "huisnummertoevoeging")) %>%
  as.character()

data_fm_bag <- filter(bag, adresseerbaarobject %in% data_fm$adresseerbaarobject)


m2 <- make_address_field(data = data_fm_bag,
                   columns = list(straat = "openbareruimtenaam",
                                  huisnummer = "huisnummer",
                                  huisletter = "huisletter",
                                  huisnummertoevoeging = "huisnummertoevoeging")) %>%
  as.character()

all.equal(m1,m2)



# zoals in milnav
af <- make_address_field(data = data,
                          columns = list(straat = "straat",
                                         huisnummer = "huisnummer",
                                         huisletter = "huisletter",
                                         huisnummertoevoeging = "huisnummertoevoeging"))

ad1 <- match_bag_address(af, bag = bag, bag_columns = c("adresseerbaarobject",
                                                        "openbareruimtenaam",
                                                        "huisnummer",
                                                        "huisletter"))

View(cbind(data, ad1))

attr(af, "adres_fields") %>%  convert_adres_fields(.)


