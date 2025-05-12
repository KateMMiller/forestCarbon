library(tidyverse)
library(sf)
#install.packages('hoardr') #dependency for rgeoboundaries
#devtools::install_github("wmgeolab/rgeoboundaries") # package no longer on CRAN
library(rgeoboundaries) # for state/county boundaries
library(tidycensus) # for FIPS state/county codes
library(tmap) # to check plots vs ecoregion layers

grsm <- read.csv("./data/GRSM_Carbon_Project_20250512.csv")
netn <- read.csv("./data/NETN_tree_sapling_data.csv")
setdiff(names(grsm), names(netn))
setdiff(names(netn), names(grsm))

#---- Spatial join GRSM plots to State and County for FIPS codes ----
plots <- unique(data.frame(park = grsm$park, plt_cn = grsm$plt_cn, lat = grsm$lat, long = grsm$long,
                           y = grsm$lat, x = grsm$long))

plots_sf <- st_as_sf(plots, coords = c("x", "y"), crs = 4326)
sf_use_s2(FALSE) # troubleshooting st_join for next line

#---- State, county and FIPS Codes ----
us_states <- geoboundaries("USA", "adm1")
us_county <- geoboundaries("USA", "adm2")
plots_state <- st_join(plots_sf, us_states, left = T) |> select(plt_cn, state_name = shapeName) |>
  st_drop_geometry()

plots_county <- st_join(plots_sf, us_county, left = T) |>
  mutate(county = paste0(shapeName, " County")) |>
  select(plt_cn, park, county) |>
  st_drop_geometry()

# join state and county data to plot locations
plots_comb <- left_join(plots_state, plots_county, by = c("plt_cn"))
data("fips_codes")
plots_comb2 <- left_join(plots_comb, fips_codes, by = c("state_name", "county")) |>
  select(plt_cn, statecd = state_code, countycd = county_code)

grsm2 <- left_join(plots_comb2,
                   grsm |> select(-statecd, -countycd),
                   by = c("plt_cn"))
grsm_final <- grsm2[,names(grsm)]
write.csv(grsm_final, "./data/GRSM_tree_sapling_data.csv", row.names = F)

