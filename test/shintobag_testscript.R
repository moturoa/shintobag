


adres <- read.table(text = "            straat huisnummer postcode      woonplaats
1  Veldweverstraat          3             6031LM      NEDERWEERT
2        Banendijk          5             6034SV NEDERWEERT-EIND
3         Ommelpad          8             6035PC           OSPEL
4  Roeventerschans          1             6031RR      NEDERWEERT
5      Kolenhofweg          3             6091NB         LEVEROY
6     Booldersdijk         36             6031PK      NEDERWEERT
7     Schepengraaf        123             6035PT           OSPEL
8             Eind         85             6034SM NEDERWEERT-EIND
9       Swelstraat          6             6091NW         LEVEROY
10            Vlut          3             6035PJ           OSPEL",
                    header = TRUE,
                    stringsAsFactors = FALSE)


library(shintobag)

# !!
options(shintobag_conf = "c:/repos/conf/config.yml")

# Eenmalig
download_gemeente_opendata("Nederweert", out_path = ".")

# BAG
bag_nederweert <- read_feather("bag_Nederweert.feather")

# Vind adressen:
make_address_field(data = adres,
                   columns = list(
                     straat = "straat",
                     huisnummer = "huisnummer"
                   )) %>%
  match_bag_address(bag = bag_nederweert,
                    bag_columns = c("woonplaatsnaam",
                                    "openbareruimtenaam",
                                    "huisnummer"))


#
adres$testfield1 <- paste(adres$straat, adres$huisnummer, adres$woonplaats)

make_address_field(data = adres$testfield1,
                   template = "{straat} {huisnummer} {woonplaatsnaam}") %>%
  match_bag_address(bag = bag_nederweert,
                    bag_columns = c("woonplaatsnaam",
                                    "openbareruimtenaam",
                                    "huisnummer"))



