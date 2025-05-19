#-----------------------------------------------------------------------------
# Compiling Mid-Atlantic Network forest data for forest carbon project
#   Parks included: ASIS, APCO, BOWA, COLO, FRSP, GETT, GEWA, HOFU, PETE, RICH, SAHI, THST, VAFO
#-----------------------------------------------------------------------------

library(forestMIDN) # can be installed via: devtools::install_github("katemmiller/forestMIDN")
library(tidyverse)
library(sf)
library(rgeoboundaries) # for state/county boundaries
library(tidycensus) # for FIPS state/county codes
library(tmap) # to check plots vs ecoregion layers
# dir.create("./data")
#remotes::install_github('r-tmap/tmap')

# import latest MIDN data.
importCSV(path = "./data/", zip_name = "records-2306436.zip")

#---- Download and compile external data -----
# Download FIA REF_SPECIES.csv to get SPCD
#ref_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/FIADB_REFERENCE.zip"
#download.file(ref_url, "./data/FIADB_REFERENCE.zip")
refspp <- read.csv(unzip("./data/FIADB_REFERENCE.zip", "REF_SPECIES.csv")) |>
  select(SPCD, SPECIES_SYMBOL, SCIENTIFIC_NAME, GENUS, SPECIES, JENKINS_SPGRPCD)
#file.remove("REF_SPECIES.csv")
#write.csv(refspp, "./data/REF_SPECIES.csv", row.names = F)

# Download Ecological Province shapefile
#ecoprov_url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.EcoMapProvinces.zip"
#download.file(ecoprov_url, "./data/S_USA.EcoMapProvinces.zip")
unzip("./data/S_USA.EcoMapProvinces.zip", exdir = "./data")
ecoprov <- read_sf("./data/S_USA.EcoMapProvinces.shp") |> st_transform(4326)
st_crs(ecoprov) #EPSG 4326 WGS84 latlong

# Download Ecological Subsection shapefile
#ecosub_url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.EcomapSubsections.zip"
#download.file(ecosub_url, "./data/S_USA.EcomapSubsections.zip")
unzip("./data/S_USA.EcomapSubsections.zip", exdir = "./data")
ecosub <- read_sf("./data/S_USA.EcomapSubsections.shp") |> st_transform(4326)
st_crs(ecosub) #EPSG 4326 WGS84 latlong

#---- Spatial join MIDN plots to Ecological Province and Subsection ----
plots <- joinLocEvent(output = 'verbose')
plots1 <- plots |> select(Plot_Name, ParkUnit, Lat, Long) |>
  mutate(lat = Lat, long = Long) |> unique()
plots_sf <- st_as_sf(plots1, coords = c("Long", "Lat"), crs = 4326)
sf_use_s2(FALSE) # troubleshooting st_join for next line

# Province
plots_ecoprov <- st_join(plots_sf, ecoprov, left = TRUE) |>
  select(Plot_Name, ParkUnit, lat, long, PROVINCE = PROVINCE_, MAP_UNIT_S, MAP_UNIT_N) |>
  st_drop_geometry() |>
  as.data.frame() |>
  arrange(Plot_Name)

table(plots_ecoprov$MAP_UNIT_N)

# Some COLO and GEWA plots are classified as water instead of 232, Outer Coastal Plain Mixed Forest Province
# Manually fixing below:
plots_ecoprov$MAP_UNIT_S[plots_ecoprov$ParkUnit %in% c("GEWA", "COLO")] <- "232"
plots_ecoprov$MAP_UNIT_N[plots_ecoprov$ParkUnit %in% c("GEWA", "COLO")] <- "Outer Coastal Plain Mixed Forest Province"
plots_ecoprov$PROVINCE[plots_ecoprov$ParkUnit %in% c("GEWA", "COLO")] <- 35

table(plots_ecoprov$MAP_UNIT_N, plots_ecoprov$ParkUnit) # No more Water
table(plots_ecoprov$MAP_UNIT_S, plots_ecoprov$ParkUnit) # No more Water
table(plots_ecoprov$PROVINCE, plots_ecoprov$ParkUnit) # No more 816 (water)

# Subsection
plots_ecosub <- st_join(plots_sf, ecosub, left = TRUE) |>
  select(Plot_Name, ParkUnit, lat, long, MAP_UNIT_S, MAP_UNIT_N, SUBSECTION) |>
  st_drop_geometry() |>
  as.data.frame() |>
  arrange(Plot_Name)

# Some COLO/GEWA plots are classified as water or tidal
# Manually fixing below:
plots_ecosub$MAP_UNIT_S[plots_ecosub$ParkUnit %in% c("COLO", "GEWA")] <- "232Ha"
plots_ecosub$MAP_UNIT_N[plots_ecosub$ParkUnit %in% c("COLO", "GEWA")] <- "Atlantic Southern Loam Hills"
plots_ecosub$SUBSECTION[plots_ecosub$ParkUnit %in% c("COLO", "GEWA")] <- 882

table(plots_ecosub$MAP_UNIT_N, plots_ecosub$ParkUnit) # No more Water
table(plots_ecosub$MAP_UNIT_S, plots_ecosub$ParkUnit) # No more Water
table(plots_ecosub$SUBSECTION, plots_ecosub$ParkUnit) # No more 816 (water)

# Join with eco prov/sub codes
plots_eco <- left_join(plots_ecoprov |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       plots_ecosub |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       by = c("Plot_Name", "ParkUnit", "lat", "long")) |>
  select(Plot_Name, ecodivision = PROVINCE, ecosubcd = SUBSECTION)

#---- State, county and FIPS Codes ----
us_states <- geoboundaries("USA", "adm1")
us_county <- geoboundaries("USA", "adm2")
plots_state <- st_join(plots_sf, us_states, left = T) |> select(Plot_Name, state_name = shapeName) |>
  st_drop_geometry()

plots_county <- st_join(plots_sf, us_county, left = T) |>
  mutate(county = paste0(shapeName, " County")) |>
  select(Plot_Name, ParkUnit, county) |>
  st_drop_geometry()

# Check that plots are showing up in right place
tm_shape(ecoprov, bbox = plots_sf) + tm_fill("MAP_UNIT_S") +
  tm_shape(us_county) + tm_borders() +
  tm_shape(us_states) + tm_borders(lwd = 1.5) +
  tm_shape(plots_sf) + tm_dots()

# join state and county data to plot locations
plots_comb <- left_join(plots_state, plots_county, by = c("Plot_Name"))
data("fips_codes")
plots_comb2 <- left_join(plots_comb, fips_codes, by = c("state_name", "county"))
head(plots_comb2)

plots2 <- left_join(
  plots |> select(Plot_Name, Network, ParkUnit, ParkSubUnit, Lat, Long) |> unique(),
  plots_eco, by = "Plot_Name"
)

plots_final <- left_join(plots2, plots_comb2, by = c("Plot_Name", "ParkUnit")) |>
  select(plt_cn = Plot_Name, park = ParkUnit, parksubunit = ParkSubUnit,  network = Network,
         state_name, ecosubcd, lat = Lat, long = Long, ecodivision, statecd = state_code,
         countycd = county_code)

head(plots_final)
# This is the left df to join tree and sapling data to.

#---- Compile MIDN tree data ----
trees <- joinTreeData(park = "all", from = 2007, to = 2024, status = 'active') |>
  mutate(tree_id = paste0(Plot_Name, "-", sprintf("%06d", TagCode))) |>
  select(tree_id, Plot_Name, Network, ParkUnit, ParkSubUnit, SampleYear, cycle,
         TSN, ScientificName, Fork, DBHcm, TreeStatusCode, CrownClassCode, DecayClassCode, num_stems)

# Fix unk tree where hardwood or conifer is known (legacy of old database)
trees$ScientificName[trees$TSN == -9999999939] <- "Unknown Hardwood"
trees$TSN[trees$TSN == -9999999939] <- -9999999944

trees$ScientificName[trees$TSN == -9999999938] <- "Unknown Conifer"
trees$TSN[trees$TSN == -9999999938] <- -9999999943

trees$ScientificName[trees$TSN == -9999999937] <- "Unknown species"
trees$TSN[trees$TSN == -9999999937] <- -9999999950

# Join USDA Plants Symbol and drop stunted woodland plots
taxa <- VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN[,c("TSN", "TaxonCode")]

trees2 <- left_join(trees, taxa, by = "TSN")
# Species names missing SPCD b/c of synonyms
# Acer floridanum; replace with Acer barbatum; ACBA3
# Carya tomentosa: replace with Carya alba; CAAL27
# Quercus (Red group): replace with Quercus; QUERC
# Amelanchier canadensis: replace with Amelanchier spp. AMELA
trees2$TaxonCode[trees2$ScientificName == "Acer floridanum"] <- "ACBA3"
trees2$TaxonCode[trees2$ScientificName == "Amelanchier canadensis"] <- "AMELA"
trees2$TaxonCode[trees2$ScientificName == "Carya tomentosa"] <- "CAAL27"
trees2$TaxonCode[trees2$ScientificName == "Quercus (Red group)"] <- "QUERC"

# Join REF_SPECIES SPCD and JENKINS_SPGRPCD to tree data
trees3 <- left_join(trees2, refspp, by = c("TaxonCode" = "SPECIES_SYMBOL"))
trspp_na <- trees3 |> filter(is.na(SPCD)) |> select(ScientificName) |> unique()

#----- Fix missing tree status and crown class codes -----
source("compile_data_MIDN_fix_missing_crownclasses.R")

#----- Status -----
# Drop Dead Fallen and Dead Cut trees and set up status codes
live <- c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
dead <- c("2", "DB", "DL", "DM", "DS")

trees4 <- trees3 |> filter(TreeStatusCode %in% c(live, dead)) |>
  mutate(STATUSCD = ifelse(TreeStatusCode %in% live, "live", "dead"),
         STATUS2 = substr(TreeStatusCode, 2, 2),
         STATUSclassifier = case_when(STATUS2 == "B" ~ "broken",
                                      STATUS2 == "F" ~ "fallen",
                                      STATUS2 == "L" ~ "leaning",
                                      STATUS2 == "S" ~ "standing",
                                      STATUS2 == "M" ~ "missed",
                                      TRUE ~ "unknown"))

#---- Compile tree height ----
# Joining individual tree heights first for heights with tags.
# Then adding average codom and interm heights for trees not measured.
# For visits before intermediate heights were measured, will use the first visit with an intermediate height.

# Individual tree heights
trht_ind <- VIEWS_MIDN_NCBN$StandTreeHeights_MIDN_NCBN |>
  filter(!IsQAQC) |> filter(PlotTypeCode == "VS") |> filter(!IsAbandoned) |>
  mutate(tree_id = ifelse(!is.na(TagCode),
                          paste0(Plot_Name, "-", sprintf("%06d", TagCode)),
                          NA_character_)) |>
  select(tree_id, SampleYear, HEIGHT_IND = Height) |> filter(!is.na(tree_id)) |>
  mutate(htcd = 1) #1 = measured in the field

trees5a <- left_join(trees4, trht_ind, by = c("tree_id", "SampleYear"))

# Tree height wasn't sampled in 2007. Assigning the individual 2011 height if measured.
trees07 <- trees5a |> filter(SampleYear == 2007)
trees11 <- trees5a |> filter(SampleYear == 2011)

trees07b <- left_join(trees07 |> select(-HEIGHT_IND),
                      trees11 |> select(tree_id, HEIGHT_IND), by = "tree_id") |>
  mutate(htcd = 4)

trees5 <- rbind(trees5a |> filter(SampleYear > 2007), trees07b)
table(trees5$htcd)

# Stand level tree heights
trht_avga <- joinStandData() |> select(Plot_Name, SampleYear, Avg_Height_Codom, Avg_Height_Inter)

# Tree height wasn't sampled in 2007. Assigning the average 2011 heights.
trht_avg07 <- trht_avga |> filter(SampleYear == 2007)
trht_avg11 <- trht_avga |> filter(SampleYear == 2011)

trht_avg07b <- left_join(trht_avg07 |> select(Plot_Name, SampleYear),
                         trht_avg11 |> select(Plot_Name, Avg_Height_Codom, Avg_Height_Inter),
                         by = "Plot_Name")

trht_avg <- rbind(trht_avga |> filter(SampleYear > 2007), trht_avg07b)

# Estimating tree heights from MIDN Ecological Integrity SOP, page 158
treehts <- left_join(trees5, trht_avg, by = c("Plot_Name", "SampleYear")) |>
  mutate(ht =
           case_when(!is.na(HEIGHT_IND) ~ HEIGHT_IND,
                     CrownClassCode == 1 ~ Avg_Height_Codom, # could be taller, but typically codom
                     CrownClassCode == 2 ~ Avg_Height_Codom * 1.1,
                     CrownClassCode == 3 & is.na(HEIGHT_IND) ~ Avg_Height_Codom,
                     CrownClassCode == 4 & is.na(HEIGHT_IND) & !is.na(Avg_Height_Inter) ~ Avg_Height_Inter,
                     CrownClassCode == 4 & is.na(HEIGHT_IND) & is.na(Avg_Height_Inter) ~ Avg_Height_Codom * 0.8,
                     CrownClassCode %in% c(5, 6) ~ Avg_Height_Codom * 0.5 # subcanopy and gap exploiters
           )) |>
  select(tree_id, Plot_Name, Network, ParkUnit, ParkSubUnit, SampleYear, cycle, ScientificName, usda_symbol = TaxonCode,
         SPCD, JENKINS_SPGRPCD, STATUSCD, STATUSclassifier, CrownClassCode, DBHcm, ht, htcd, decaycd = DecayClassCode)

#----- Tricky heights to fix manually -----
source("compile_data_MIDN_fix_tree_heights.R")

# checking for missing live tree heights
trhts_na <- treehts |> filter(is.na(ht)) |> filter(STATUSCD == "live") |>
  select(tree_id, Plot_Name, SampleYear, cycle, ScientificName, DBHcm, STATUSCD, STATUSclassifier,
         CrownClassCode, ht)

table(trhts_na$Plot_Name)

trees_check <- trees3 |>
  select(tree_id, Plot_Name, SampleYear, cycle, TreeStatusCode, CrownClassCode, DBHcm) |>
  filter(tree_id %in% trhts_na$tree_id)

tr_miss_dbh <- treehts |> filter(is.na(DBHcm)) |> filter(SampleYear == "live")
tr_miss_dbh #none

#----- CULL -----
treecond1 <- joinTreeConditions() |> mutate(tree_id = paste0(Plot_Name, "-", sprintf("%03d", TagCode))) |>
  select(tree_id, SampleYear, AD, CAVL, CAVS, DBT)

treecond1$AD[is.na(treecond1$AD)] <- 0
treecond1$CAVL[is.na(treecond1$CAVL)] <- 0
treecond1$CAVS[is.na(treecond1$CAVS)] <- 0
treecond1$DBT[is.na(treecond1$DBT)] <- 0

treecond <- left_join(treehts, treecond1, by = c("tree_id", "SampleYear")) |>
  mutate(CULL_br = ifelse(STATUSclassifier == "broken", 0.5, 0),
         CULL_AD = ifelse(AD == 1, 0.25, 0),
         CULL_CAVL = ifelse(AD == 0 & CAVL == 1, 0.1, 0),
         CULL_CAVS = ifelse(AD == 0 & CAVS == 1, 0.05, 0),
         CULL_DBT = ifelse(AD == 0 & DBT == 1, 0.1, 0),
         cull = CULL_br + CULL_AD + CULL_CAVL + CULL_CAVS + CULL_DBT,
         habit = "Tree",
         tpa_unadj = ifelse(ParkUnit == "ACAD", (225/4046.86)^-1, (400/4046.86)^-1))

treecond$htcd[is.na(treecond$htcd) & !is.na(treecond$ht)] <- 4

tree6 <- treecond |> select(plt_cn = Plot_Name, tre_cn = tree_id, year = SampleYear, cycle,
                            tpa_unadj, scientific_name = ScientificName, usda_symbol,
                            spcd = SPCD, jenkins_spgrpcd = JENKINS_SPGRPCD, statuscd = STATUSCD,
                            statusclassifier = STATUSclassifier,
                            treeclcd = CrownClassCode, dbhcm = DBHcm, ht, htcd, cull, habit, decaycd)


tree_plots <- left_join(plots_final, tree6, by = c("plt_cn")) |>
  select(plt_cn,  tre_cn, ecosubcd, ecodivision, year, cycle, tpa_unadj,
         lat, long, network, park, parksubunit, state_name, statecd, countycd,
         spcd, jenkins_spgrpcd, scientific_name, usda_symbol,
         statuscd, treeclcd,
         dbhcm, ht, htcd, cull, habit, decaycd)

#---- Compile MIDN sapling data ----
saps <- joinMicroSaplings(park = "all", from = 2007, to = 2024) |>
  arrange(Plot_Name, SampleYear, MicroplotCode, TagCode, SaplingStatusCode, DBHcm) |>
  mutate(tree_id = paste0(Plot_Name, sprintf("%06d", TagCode)),
         CrownClassCode = 5,
         DecayClassCode = NA_real_) |>
  select(tree_id, Plot_Name, Network, ParkUnit, ParkSubUnit, SampleYear, cycle,
         TSN, ScientificName, Fork, DBHcm, SaplingStatusCode, CrownClassCode, DecayClassCode,
         num_stems = Count) |>
  filter(!ScientificName %in% c("Not Sampled", "None present"))

sap_micros <- joinMicroSaplings(park = "all", from = 2007, to = 2024) |>
  filter(!ScientificName %in% c("Not Sampled")) |>
  select(Plot_Name, SampleYear, MicroplotCode) |> unique() |>
  group_by(Plot_Name, SampleYear) |>
  summarize(num_micros = sum(!is.na(MicroplotCode)), .groups = "drop")

table(sap_micros$num_micros, useNA = "always")

saps2 <- left_join(saps, sap_micros, by = c("Plot_Name", "SampleYear"))

# Join USDA Plants Symbol and drop stunted woodland plots
taxa <- VIEWS_MIDN_NCBN$Taxa_MIDN_NCBN[,c("TSN", "TaxonCode")]

saps3 <- left_join(saps2, taxa, by = "TSN")

# Species names missing SPCD b/c of synonyms
# Acer floridanum; replace with Acer barbatum; ACBA3
# Carya tomentosa: replace with Carya alba; CAAL27
# Quercus (Red group): replace with Quercus; QUERC
# Amelanchier canadensis: replace with Amelanchier spp. AMELA
# Quercus montana: replace with Quercus prinus; QUPR2
saps3$TaxonCode[saps3$ScientificName == "Acer floridanum"] <- "ACBA3"
saps3$TaxonCode[saps3$ScientificName == "Amelanchier canadensis"] <- "AMELA"
saps3$TaxonCode[saps3$ScientificName == "Carya tomentosa"] <- "CAAL27"
saps3$TaxonCode[saps3$ScientificName == "Quercus (Red group)"] <- "QUERC"
saps3$TaxonCode[saps3$ScientificName == "Quercus (White group)"] <- "QUERC"
saps3$TaxonCode[saps3$ScientificName == "Quercus montana"] <- "QUPR2"

table(saps3$TaxonCode, useNA = 'always') # 1 NA that's an unknown species

# Join REF_SPECIES SPCD and JENKINS_SPGRPCD to tree data
saps4 <- left_join(saps3, refspp, by = c("TaxonCode" = "SPECIES_SYMBOL"))
sapspp_na <- saps4 |> filter(is.na(SPCD)) |> select(ScientificName)

micro_size_m2 = 3*3*pi

saps5 <- saps4 |> mutate(tpa_unadj = ((micro_size_m2 * num_micros)/4046.86)^-1,
                         ht = NA_real_,
                         htcd = NA_real_,
                         cull = 0,
                         habit = "Sapling",
                         statusclassifier = "unknown") |>
  select(plt_cn = Plot_Name, tre_cn = tree_id, network = Network, park = ParkUnit, parksubunit = ParkSubUnit,
         year = SampleYear, cycle, tpa_unadj,
         spcd = SPCD, jenkins_spgrpcd = JENKINS_SPGRPCD, scientific_name = ScientificName, usda_symbol = TaxonCode,
         statuscd = SaplingStatusCode, treeclcd = CrownClassCode,
         dbhcm = DBHcm, ht, htcd, cull, habit, decaycd = DecayClassCode)

saps_plots <- right_join(plots_final, saps5, by = c("plt_cn", "park", "parksubunit", "network"))
head(data.frame(saps_plots))

setdiff(names(saps_plots), names(tree_plots))
setdiff(names(tree_plots), names(saps_plots))

#---- Bind tree and sapling data ----
tree_sap <- rbind(tree_plots, saps_plots)
# Update SPCD to use use 999 for unknown tree, 998 for unknown hardwood/broadleaf, and 299 for unknown conifer
# based on REF_SPECIES.csv
tree_sap$spcd[tree_sap$scientific_name == "Unknown species"] <- 999 #8
tree_sap$spcd[tree_sap$scientific_name == "Unknown Hardwood"] <- 998 #8
tree_sap$spcd[tree_sap$scientific_name == "Unknown Conifer"] <- 299 #4
# Update Jenkins as 8 for unknown tree, 8 for unknown hardwood/broadleaf, and 4 for unknown conifer
# based on REF_SPECIES.csv
tree_sap$jenkins_spgrpcd[tree_sap$scientific_name == "Unknown species"] <- 8
tree_sap$jenkins_spgrpcd[tree_sap$scientific_name == "Unknown Hardwood"] <- 8
tree_sap$jenkins_spgrpcd[tree_sap$scientific_name == "Unknown Conifer"] <- 4

# Add 0 to decay class for live trees, so NAs mean it wasn't recorded.
tree_sap$decaycd[tree_sap$statuscd == "live"] <- 0

length(unique(tree_sap$plt_cn)) #393

write.csv(tree_sap, "./data/MIDN_tree_sapling_data.csv", row.names = F)

#---- Metadata for tree and sapling data ----
meta <- read.csv("./data/metadata_NETN.csv")
meta$MIDN <- NA_character_

meta$MIDN[meta$column_name == "plt_cn"] <- "Plot numbers are unique to MIDN"
meta$MIDN[meta$column_name == "tre_cn"] <- "Tree tag numbers are unique to MIDN; Saplings are not tagged and can be linked across time, including when saplings grow into tree size (they have the same tag number)."
meta$MIDN[meta$column_name == "cycle"] <- "Plots are typically sampled on a 4-year rotation, with cycle 1 being 2007-2010 for MIDN parks, 2008-2011 for GEWA & THST, 2011-2014 for COLO, and 2019-2024 for ASIS. The sampling schedule was disrupted by COVID in 2020 and 2021, so that some parks/plots have 5 or 6 year intervals between visits."
meta$MIDN[meta$column_name == "treecld"] <- "Gap exploiter is either an intermediate or subcanopy tree expected to grow faster than normal because it is within a canopy gap."
meta$MIDN[meta$column_name == "habit"] <- "Individual saplings are tracked over time the same as trees, although crown class, heights and decay class are not recorded for saplings."
write.csv(meta, "./data/metadata_NETN_MIDN.csv", row.names = F)




