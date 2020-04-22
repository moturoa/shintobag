



silv_1 <- readRDS("c:/repos/kamerverhuur_data/silv_1.rds")

library(shintobag)

bag_eindhoven <- readRDS("c:/repos/kamerverhuur_data/output/bag/bag_eindhoven.rds")



af <- make_address_field(data = silv_1[1:200,],
                         columns = list(
                           straat = "straat",
                           huisnummer = "huisnummer"
                         ))

out <- match_bag_address(af,
                  bag = bag_eindhoven,
                  bag_columns = c("adresseerbaarobject",
                                  "openbareruimtenaam",
                                  "huisnummer",
                                  "huisletter",
                                  "huisnummertoevoeging"))

out2 <- cbind(af, out)

View(filter(out2, !is.na(huisletter)))





  af2 <- make_address_field(data = silv_1[3000:3100,],
                            columns = list(
                              straat = "straat",
                              huisnummer = "huisnummer"
                            ))

  system.time(
    out <- match_bag_address(af2,
                             bag = bag_eindhoven,
                      bag_columns = c("adresseerbaarobject",
                                      "openbareruimtenaam",
                                      "huisnummer",
                                      "huisletter",
                                      "huisnummertoevoeging"))
)






