library(sf)
library(dplyr)
library(purrr)

files <- list.files("./data/pre_ecosub_fix")

fix_ecosub <- function(file){
  dat <- read.csv(paste0("./data/pre_ecosub_fix/", file))
  cols <- names(dat)

  dat1 <- dat |>
    rename(ecosubcd_key = ecosubcd,
           ecodivision_key = ecodivision)

  ecoprov <- read_sf("./data/S_USA.EcoMapProvinces.shp") |> st_transform(4326) |> as.data.frame() |>
    select(ecodivision = MAP_UNIT_S, prov_key = PROVINCE_)

  ecosub <- read_sf("./data/S_USA.EcomapSubsections.shp") |> st_transform(4326) |> as.data.frame() |>
    select(ecosubcd = MAP_UNIT_S, ecosub_key = SUBSECTION)

  dat2 <- left_join(dat1, ecoprov, by = c("ecodivision_key" = "prov_key"))
  dat3 <- left_join(dat2, ecosub, by = c("ecosubcd_key" = "ecosub_key"))

  dat4 <- dat3[,cols]

  print(table(dat4$ecodivision, dat4$park))
  print(table(dat4$ecosubcd, dat4$park))

  write.csv(dat4, paste0("./data/final_datasets/", file), row.names = F)

}

map(files, ~fix_ecosub(.x))
