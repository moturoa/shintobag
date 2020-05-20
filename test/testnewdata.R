


con_old <- shinto_db_connection("BAGdata", "c:/repos/wbm3.0/conf/config.yml")

out <- st_read(con_old, query = "select * from bagactueel.adres_full where gemeentenaam = 'Rozendaal' limit 10")


con_new <- shinto_db_connection("BAGdata_new", "testconfig.yml")

out <- st_read(con_new, query = "select * from bagactueel.adres_full where gemeentenaam = 'Nederweert'")


con_ds <- shinto_db_connection("datastraat", "testconfig.yml")

out <- st_read(con_ds, query = "select * from bagactueel.adres_full where gemeentenaam = 'Rozendaal' limit 10")


con_tt <- shinto_db_connection("top10nl", "testconfig.yml")

out <- st_read(con_tt, query = "select * from latest.gebouw limit 10")

