#-----------------------------------------------------------------------------
# Checking and adding location information for SECN
#-----------------------------------------------------------------------------

library(tidyverse)
library(sf)
#install.packages('hoardr') #dependency for rgeoboundaries
#devtools::install_github("wmgeolab/rgeoboundaries") # package no longer on CRAN
library(rgeoboundaries) # for state/county boundaries
library(tidycensus) # for FIPS state/county codes
library(tmap) # to check plots vs ecoregion layers

# import latest NETN data.
trees <- read.csv('./data/SECN/bmoore_secn_carseq_tree_mfboyle_edits.csv')
saps <- read.csv('./data/SECN/bmoore_secn_carseq_sapling_mfboyle_edits.csv')
events <- read.csv('./data/SECN/SECN_VegExport_Events.csv')

trees2 <- left_join(trees,
                    events |> select(event_name_date_calc, X_Coord, Y_Coord, UTM_Zone) |>
                      filter(!is.na(X_Coord)) |> unique(),
                    by = c("concat_event" = "event_name_date_calc")) |>
  filter(statuscd < 3) |>
  mutate(statuscd = ifelse(statuscd == 1, "live", "dead"),
         cull = case_when(vigor == 1 ~ 0,
                          vigor == 2 ~ 0.1,
                          vigor == 3 ~ 0.2,
                          vigor == 4 ~ 0.3,
                          vigor == 5 ~ 0.5,
                          TRUE ~ 0),
         htcd = 4,
         statusclassifier = NA_character_,
         htcd = stand_height,
         tpa_unadj = (400/4046.86)^-1) |>  # average of 3 codoms, but individuals not tracked
  select(concat_event, plt_cn, tre_cn, year, cycle, spcd, jenkins_spgrcpd, scientific_name, usda_symbol,
         statuscd, statusclassifier, treeclcd, dbhcm, ht, htcd, cull, habit,
         decaycd, tpa_unadj)

#  trees2 ready to join to saplings for spatial joins
#----- Reshape/format sapling data
# DBH Midpoints are 1.5, 3.5, 7.5

saps_long <- saps |> pivot_longer(cols = c(Sapling_DBH_1_2half, Sapling_DBH_2half_5, Sapling_DBH_5_10),
                                  names_to = "size_class",
                                  values_to = "count",
                                  values_drop_na = FALSE) |> data.frame() |>
  mutate(dbhcm = case_when(grepl("1_2half", size_class) ~ 1.5,
                         grepl("2half_5", size_class) ~ 3.5,
                         grepl("5_10", size_class) ~ 7.5),
         statuscd = "live",
         statusclassifier = NA_character_,
         cull = 0,
         ht = NA_real_,
         htcd = NA_real_,
         treeclcd = 5,
         habit = "Sapling",
         decaycd = NA_real_,
         tpa_unadj = (80/4046.86)^-1)

saps_long$count[is.na(saps_long$count)] <- 0
saps_expand <- saps_long |> uncount(count) |>
  select(concat_event, plt_cn, year, cycle, spcd, jenkins_spgrcpd, scientific_name, usda_symbol,
         Species_Original, statuscd, statusclassifier, treeclcd, dbhcm, ht, htcd, cull, habit,
         decaycd, tpa_unadj)

saps2 <- saps_expand |>
         arrange(concat_event) |>
         group_by(plt_cn) |>
         mutate(sap_order = row_number(),
                tre_cn = paste0(plt_cn, "-sap", sprintf("%03d", sap_order))) |>
         data.frame()
saps3 <- saps2[,names(trees2)]

tree_sap <- rbind(trees2, saps3)

tree_sap2 <- left_join(tree_sap,
                       events |> select(event_name_date_calc, X_Coord, Y_Coord, UTM_Zone) |>
                         filter(!is.na(X_Coord)) |> unique(),
                       by = c("concat_event" = "event_name_date_calc"))

# evs <- events |> select(event_name_date_calc, X_Coord, Y_Coord, UTM_Zone) |> filter(is.na(X_Coord))
# write.csv(evs, "./data/SECN/SECN_missing_coords.csv", row.names = F) # fixed

tree_sap16 <- tree_sap2 |> filter(UTM_Zone == 16) |> st_as_sf(coords = c("X_Coord", "Y_Coord"), crs = 32616) |>
  st_transform(crs = 4326)
tree_sap17 <- tree_sap2 |> filter(UTM_Zone == 17) |> st_as_sf(coords = c("X_Coord", "Y_Coord"), crs = 32617) |>
  st_transform(crs = 4326)
tree_sap18 <- tree_sap2 |> filter(UTM_Zone == 18) |> st_as_sf(coords = c("X_Coord", "Y_Coord"), crs = 32618) |>
  st_transform(crs = 4326)

tree_sap16b <- data.frame(tree_sap16, lat = st_coordinates(tree_sap16)[,2], long = st_coordinates(tree_sap16)[,1]) |>
  st_drop_geometry()

tree_sap17b <- data.frame(tree_sap17, lat = st_coordinates(tree_sap17)[,2], long = st_coordinates(tree_sap17)[,1]) |>
  st_drop_geometry()

tree_sap18b <- data.frame(tree_sap18, lat = st_coordinates(tree_sap18)[,2], long = st_coordinates(tree_sap18)[,1]) |>
  st_drop_geometry()

tree_sap_comb <- rbind(tree_sap16b, tree_sap17b, tree_sap18b)

#---- Compile external data -----
# FIA REF_SPECIES.csv to get SPCD
refspp <- read.csv(unzip("./data/FIADB_REFERENCE.zip", "REF_SPECIES.csv")) |>
  select(SPCD, SPECIES_SYMBOL, SCIENTIFIC_NAME, GENUS, SPECIES, JENKINS_SPGRPCD)
# Ecological Province shapefile
ecoprov <- read_sf("./data/S_USA.EcoMapProvinces.shp") |> st_transform(4326)
#st_crs(ecoprov) #EPSG 4326 WGS84 latlong

# Download Ecological Subsection shapefile
ecosub <- read_sf("./data/S_USA.EcomapSubsections.shp") |> st_transform(4326)
#st_crs(ecosub) #EPSG 4326 WGS84 latlong

#---- Spatial join NETN plots to Ecological Province and Subsection ----
sf_use_s2(FALSE) # troubleshooting st_join for next line

plots <- tree_sap_comb |> select(concat_event, plt_cn, year, lat, long) |>
  mutate(lat2 = lat, long2 = long) |>
  unique() |>
  st_as_sf(coords = c("long2", "lat2"), crs = 4326)

# Province
plots_ecoprov <- st_join(plots, ecoprov, left = TRUE) |>
  select(concat_event, plt_cn, year, lat, long, PROVINCE = PROVINCE_, MAP_UNIT_S, MAP_UNIT_N) |>
  st_drop_geometry() |>
  as.data.frame() |>
  arrange(concat_event) |>
  mutate(park = substr(concat_event, 1, 4))

table(plots_ecoprov$PROVINCE, plots_ecoprov$park)

# Some plots are classified as water instead of the province
# Manually fixing below:
plots_ecoprov$MAP_UNIT_S[plots_ecoprov$park %in%
                           c("APOL", "BOIS", "CALO", "CEDA", "CUIS", "FOMA", "FOPU", "NCOR")] <- "232"
plots_ecoprov$MAP_UNIT_N[plots_ecoprov$park %in%
                           c("APOL", "BOIS", "CALO", "CEDA", "CUIS", "FOMA", "FOPU", "NCOR")] <-
  "Outer Coastal Plain Mixed Forest Province"
plots_ecoprov$PROVINCE[plots_ecoprov$park %in%
                         c("APOL", "BOIS", "CALO", "CEDA", "CUIS", "FOMA", "FOPU", "NCOR")] <- 35

# Subsection
plots_ecosub <- st_join(plots, ecosub, left = TRUE) |>
  select(concat_event, plt_cn, lat, long, MAP_UNIT_S, MAP_UNIT_N, SUBSECTION) |>
  st_drop_geometry() |>
  as.data.frame() |>
  arrange(plt_cn) |>
  mutate(park = substr(plt_cn, 1, 4))

table(plots_ecosub$SUBSECTION, plots_ecosub$park, useNA = 'always')

# Some plots are classified as water. Manually fixing below:
plots_ecosub$MAP_UNIT_S[plots_ecosub$park %in% c("APOL", "FOMA")] <- "232Gb"
plots_ecosub$MAP_UNIT_N[plots_ecosub$park %in% c("APOL", "FOMA")] <- "Eastern Beach and Dunes"
plots_ecosub$SUBSECTION[plots_ecosub$park %in% c("APOL", "FOMA")] <- 1219

plots_ecosub$MAP_UNIT_S[plots_ecosub$park %in% c("BOIS", "CALO", "NCOR")] <- "232Ib"
plots_ecosub$MAP_UNIT_N[plots_ecosub$park %in% c("BOIS", "CALO", "NCOR")] <- "Tidal Area"
plots_ecosub$SUBSECTION[plots_ecosub$park %in% c("BOIS", "CALO", "NCOR")] <- 1099

plots_ecosub$MAP_UNIT_S[plots_ecosub$park %in% c("CEDA", "CUIS", "FOPU")] <- "232Ce"
plots_ecosub$MAP_UNIT_N[plots_ecosub$park %in% c("CEDA", "CUIS", "FOPU")] <- "Coastal Marsh and Island"
plots_ecosub$SUBSECTION[plots_ecosub$park %in% c("CEDA", "CUIS", "FOPU")] <- 1143

table(plots_ecosub$MAP_UNIT_N, plots_ecosub$park) # No more Water
table(plots_ecosub$MAP_UNIT_S, plots_ecosub$park) # No more Water
table(plots_ecosub$SUBSECTION, plots_ecosub$park) # No more 816 (water)

# Join with eco prov/sub codes
plots_eco <- left_join(plots_ecoprov |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       plots_ecosub |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       by = c("concat_event", "plt_cn", "park", "lat", "long")) |>
  select(concat_event, plt_cn, ecodivision = PROVINCE, ecosubcd = SUBSECTION)

#---- State, county and FIPS Codes ----
us_states <- geoboundaries("USA", "adm1")
us_county <- geoboundaries("USA", "adm2")
plots_state <- st_join(plots, us_states, left = T) |>
  select(concat_event, plt_cn, state_name = shapeName) |>
  st_drop_geometry()

plots_county <- st_join(plots, us_county, left = T) |>
  mutate(county = paste0(shapeName, " County")) |>
  select(concat_event, plt_cn, county) |>
  st_drop_geometry()

# Check that plots are showing up in right place
tm_shape(ecoprov, bbox = plots) + tm_fill("MAP_UNIT_S") +
 tm_shape(us_county) + tm_borders() +
 tm_shape(us_states) + tm_borders(lwd = 1.5) +
 tm_shape(plots) + tm_dots(fill = "orange")

# join state and county data to plot locations
plots_comb <- left_join(plots_state, plots_county, by = c("concat_event", "plt_cn"))
data("fips_codes")
plots_comb2 <- left_join(plots_comb, fips_codes, by = c("state_name", "county"))
head(plots_comb2)

plots2 <- left_join(
  plots |> select(concat_event, plt_cn, lat, long),
  plots_eco, by = c("concat_event", "plt_cn"))

head(plots2)
head(plots_comb2)
plots_final <- left_join(plots2, plots_comb2, by = c("concat_event", "plt_cn")) |>
  mutate(park = substr(concat_event, 1, 4),
        parksubunit = NA_character_,
        network = "SECN") |>
  select(concat_event, plt_cn, park, parksubunit, network,
         state_name, ecosubcd, lat, long, ecodivision,
         statecd = state_code,
         countycd = county_code) |>
  data.frame() |>
  select(-geometry)

head(plots_final)
length(unique(plots_final$plt_cn)) #335
# This is the left df to join tree and sapling data to.

head(tree_sap_comb)
tree_sap2 <- tree_sap_comb |> select(-geometry)

tree_sap_loc <- left_join(tree_sap2, plots_final, by = c("concat_event", "plt_cn", "long", "lat"))
names(tree_sap_loc)[names(tree_sap_loc) == "jenkins_spgrcpd"] <- "jenkins_spgrpcd"

head(tree_sap_loc)

keep_cols <- c("plt_cn", "tre_cn", "ecosubcd", "ecodivision", "year", "cycle",
               "tpa_unadj",
               "lat", "long", "network", "park", "parksubunit",
               "state_name", "statecd", "countycd", "spcd", "jenkins_spgrpcd", "scientific_name",
               "usda_symbol", "statuscd", "statusclassifier", "treeclcd", "dbhcm", "ht",
               "htcd", "cull", "habit", "decaycd")

tree_sap_final <- tree_sap_loc[order(tree_sap_loc$tre_cn), keep_cols]
write.csv(tree_sap_final, "./data/SECN/SECN_tree_sapling_data.csv", row.names = F)
