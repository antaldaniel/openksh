
megye_map <- sf::st_read('C:/Users/Daniel Antal/Documents/Shapefiles/kozighatarok/admin6.shp')
megye_map$mid <- sf::st_centroid(megye_map$geometry)
megye_map$name = c("Heves", "Csongrád", "Jász-Nagykun-Szolnok", "Borsod-Abaúj-Zemplén",
                   "Békés", "Hajdú-Bihar", "Szabolcs-Szatmár-Bereg","Vas", "Zala",
                   "Győr-Moson-Sopron", "Somogy", "Veszprém", "Baranya",
                   "Komárom-Esztergom", "Tolna", "Fejér", "Pest", "Bács-Kiskun", "Budapest",
                   "Nógrád")

megyenev <- readxl::read_excel("data-raw/megyek.xlsx") %>%
  dplyr::select ( name2, isocode ) %>%
  dplyr::rename ( name = name2)  %>%
  dplyr::filter ( isocode != "HU-VM")

na_to_0 <- function(x) ifelse ( is.na(x), 0, x)
megye_map <- dplyr::left_join ( megye_map, megyenev, by = "name" )

megye_map$isocode <- ifelse ( is.na(megye_map$isocode ), "HU-GS", megye_map$isocode)

devtools::use_data(megye_map, internal = TRUE, overwrite = TRUE)
