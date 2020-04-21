


adres <- read.table(text = "            straat huisnummer huisletter postcode      woonplaats
1  Veldweverstraat          3             6031 LM      NEDERWEERT
2        Banendijk          5             6034 SV NEDERWEERT-EIND
3         Ommelpad          8             6035 PC           OSPEL
4  Roeventerschans          1             6031 RR      NEDERWEERT
5      Kolenhofweg          3             6091 NB         LEVEROY
6     Booldersdijk         36             6031 PK      NEDERWEERT
7     Schepengraaf        123             6035 PT           OSPEL
8             Eind         85             6034 SM NEDERWEERT-EIND
9       Swelstraat          6             6091 NW         LEVEROY
10            Vlut          3             6035 PJ           OSPEL",
                    header = TRUE,
                    stringsAsFactors = FALSE)


library(shintobag)

# !!
options(shintobag_conf = "/path/to/config.yml")

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
  match_bag_address(.,
                    bag = bag_nederweert,
                    bag_columns = c("woonplaatsnaam",
                                    "openbareruimtenaam",
                                    "huisnummer"))






