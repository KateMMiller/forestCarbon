#-----------------------------------------------------------------------------
# Compiling Northeast Temperate Network forest data for forest carbon project
#   Parks included: ACAD, MABI, MIMA, MORR, ROVA, SAGA, SARA, WEFA
#-----------------------------------------------------------------------------

library(forestNETN) # can be installed via: devtools::install_github("katemmiller/forestNETN")
library(tidyverse)
library(sf)
library(rgeoboundaries) # for state/county boundaries
library(tidycensus) # for FIPS state/county codes
# dir.create("./data")

# import latest NETN data
importCSV(path = "./data", zip_name = "NETN_forest_data_package_20240926.zip")

#---- Download and compile external data -----
# Download FIA REF_SPECIES.csv to get SPCD
ref_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/FIADB_REFERENCE.zip"
download.file(re_furl, "./data/FIADB_REFERENCE.zip")
refspp <- read.csv(unzip("./data/FIADB_REFERENCE.zip", "REF_SPECIES.csv")) |>
  select(SPCD, SPECIES_SYMBOL, SCIENTIFIC_NAME, GENUS, SPECIES, JENKINS_SPGRPCD)
file.remove("REF_SPECIES.csv")
write.csv(refspp, "./data/REF_SPECIES.csv", row.names = F)

# Download Ecological Province shapefile
ecoprov_url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.EcoMapProvinces.zip"
download.file(ecoprov_url, "./data/S_USA.EcoMapProvinces.zip")
unzip("./data/S_USA.EcoMapProvinces.zip", exdir = "./data")
ecoprov <- read_sf("./data/S_USA.EcoMapProvinces.shp")
# st_crs(ecoprov) #EPSG 4269 NAD83 latlong

# Download Ecological Subsection shapefile
ecosub_url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.EcomapSubsections.zip"
download.file(ecosub_url, "./data/S_USA.EcomapSubsections.zip")
unzip("./data/S_USA.EcomapSubsections.zip", exdir = "./data")
ecosub <- read_sf("./data/S_USA.EcomapSubsections.shp")
# st_crs(ecosub) #EPSG 4269 NAD83 latlong

#---- Spatial join NETN plots to Ecological Province and Subsection ----
plots <- joinLocEvent(output = 'verbose')
plots1 <- plots |> select(Plot_Name, ParkUnit, Lat, Long) |>
  mutate(lat = Lat, long = Long) |> unique()
plots_sf <- st_as_sf(plots1, coords = c("Long", "Lat"), crs = 4269)
sf_use_s2(FALSE) # troubleshooting st_join

# Province
plots_ecoprov <- st_join(plots_sf, ecoprov, left = TRUE) |>
  select(Plot_Name, ParkUnit, lat, long, PROVINCE = PROVINCE_, MAP_UNIT_S, MAP_UNIT_N) |>
  st_drop_geometry() |>
  as.data.frame() |>
  arrange(Plot_Name)

# Some ACAD plots are classified as water instead of Northeastern Mixed Forest Province/211
# Manually fixing below:
plots_ecoprov$MAP_UNIT_S[plots_ecoprov$ParkUnit == "ACAD"] <- "211"
plots_ecoprov$MAP_UNIT_N[plots_ecoprov$ParkUnit == "ACAD"] <- "Northeastern Mixed Forest Province"
plots_ecoprov$PROVINCE[plots_ecoprov$ParkUnit == "ACAD"] <- 8

table(plots_ecoprov$MAP_UNIT_N, plots_ecoprov$ParkUnit) # No more Water
table(plots_ecoprov$MAP_UNIT_S, plots_ecoprov$ParkUnit) # No more Water
table(plots_ecoprov$PROVINCE, plots_ecoprov$ParkUnit) # No more 816 (water)

# Subsection
plots_ecosub <- st_join(plots_sf, ecosub, left = TRUE) |>
  select(Plot_Name, ParkUnit, lat, long, MAP_UNIT_S, MAP_UNIT_N, SUBSECTION) |>
  st_drop_geometry() |>
  as.data.frame() |>
  arrange(Plot_Name)

# Some ACAD plots are classified as water instead of Maine Eastern Coastal.
# Manually fixing below:
plots_ecosub$MAP_UNIT_S[plots_ecosub$ParkUnit == "ACAD"] <- "211Cb"
plots_ecosub$MAP_UNIT_N[plots_ecosub$ParkUnit == "ACAD"] <- "Maine Eastern Coastal"
plots_ecosub$SUBSECTION[plots_ecosub$ParkUnit == "ACAD"] <- 322

table(plots_ecosub$MAP_UNIT_N, plots_ecosub$ParkUnit) # No more Water
table(plots_ecosub$MAP_UNIT_S, plots_ecosub$ParkUnit) # No more Water
table(plots_ecosub$SUBSECTION, plots_ecosub$ParkUnit) # No more 816 (water)

# Join with eco prov/sub codes
plots_eco <- left_join(plots_ecoprov |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       plots_ecosub |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       by = c("Plot_Name", "ParkUnit", "lat", "long")) |>
  select(Plot_Name, ecodivision = PROVINCE, ecosubcd = SUBSECTION)

#---- State, county and FIPS Codes ----
us_states <- st_transform(geoboundaries("USA", "adm1"), 4269)
us_county <- st_transform(geoboundaries("USA", "adm2"), 4269)
plots_state <- st_join(plots_sf, us_states, left = T) |> select(Plot_Name, state_name = shapeName) |>
  st_drop_geometry()
plots_county <- st_join(plots_sf, us_county, left = T) |>
  mutate(county = paste0(shapeName, " County")) |>
  select(Plot_Name, ParkUnit, county) |>
  st_drop_geometry()

plots_comb <- left_join(plots_state, plots_county, by = c("Plot_Name"))
data("fips_codes")
plots_comb2 <- left_join(plots_comb, fips_codes, by = c("state_name", "county"))
head(plots_comb2)

plots2 <- left_join(
  plots |> select(Plot_Name, Network, ParkUnit, ParkSubUnit, Lat, Long, IsStuntedWoodland) |> unique(),
  plots_eco, by = "Plot_Name"
)

plots_final <- left_join(plots2, plots_comb2, by = c("Plot_Name", "ParkUnit")) |>
  select(plt_cn = Plot_Name, park = ParkUnit, parksubunit = ParkSubUnit,  network = Network,
         state_name, ecosubcd, lat = Lat, long = Long, ecodivision, statecd = state_code,
         countycd = county_code)

head(plots_final)
# This is the left df to join tree and sapling data to.

#---- Compile NETN tree data ----
trees <- joinTreeData(park = "all", from = 2006, to = 2024, status = 'active') |>
  mutate(tree_id = paste0(Plot_Name, "-", sprintf("%03d", TagCode))) |>
  select(tree_id, Plot_Name, Network, ParkUnit, ParkSubUnit, SampleYear, cycle,
         TSN, ScientificName, Fork, DBHcm, TreeStatusCode, CrownClassCode, DecayClassCode, num_stems)

# Drop 4 stunted woodlands, which are sampled at DRC and typically <5m tall.
stunted <- joinLocEvent() |> filter(IsStuntedWoodland == TRUE) |>
  select(Plot_Name) |> unique()

# Fix unk tree where hardwood or conifer is known (legacy of old database)
trees$ScientificName[trees$TSN == -9999999939] <- "Unknown Hardwood"
trees$TSN[trees$TSN == -9999999939] <- -9999999944

trees$ScientificName[trees$TSN == -9999999938] <- "Unknown Conifer"
trees$TSN[trees$TSN == -9999999938] <- -9999999943

trees$ScientificName[trees$TSN == -9999999937] <- "Unknown species"
trees$TSN[trees$TSN == -9999999937] <- -9999999950

# Join USDA Plants Symbol and drop stunted woodland plots
taxa <- VIEWS_NETN$Taxa_NETN[,c("TSN", "TaxonCode")]

trees2 <- left_join(trees, taxa, by = "TSN") |>
  filter(!Plot_Name %in% stunted$Plot_Name)

# Species names missing SPCD b/c of synonyms
  # Betula cordifolia; replace with Betula papyrifera; BEPA
  # Betula X cearulea; replace with Betula papyrifera; BEPA
  # Carya tomentosa: replace with Carya alba; CAAL27
  # Quercus montana: replace with Quercus prinus; QUPR2
trees2$TaxonCode[trees2$ScientificName == "Betula cordifolia"] <- "BEPA"
trees2$TaxonCode[trees2$ScientificName == "Betula X cearulea"] <- "BEPA"
trees2$TaxonCode[trees2$ScientificName == "Carya tomentosa"] <- "CAAL27"
trees2$TaxonCode[trees2$ScientificName == "Quercus montana"] <- "QUPR2"

# Join REF_SPECIES SPCD and JENKINS_SPGRPCD to tree data
trees3 <- left_join(trees2, refspp, by = c("TaxonCode" = "SPECIES_SYMBOL"))
trspp_na <- trees3 |> filter(is.na(SPCD)) |> select(ScientificName) |> unique()

#----- Fix missing tree status and crown class codes -----
source("compile_data_NETN_fix_missing_crownclasses.R")
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
trht_ind <- VIEWS_NETN$StandTreeHeights_NETN |>
  filter(!IsQAQC) |> filter(PlotTypeCode == "VS") |> filter(!IsAbandoned) |>
  mutate(tree_id = ifelse(!is.na(TagCode),
                          paste0(Plot_Name, "-", sprintf("%03d", TagCode)),
                          NA_character_)) |>
  select(tree_id, SampleYear, HEIGHT_IND = Height) |> filter(!is.na(tree_id)) |>
  mutate(htcd = 1) #1 = measured in the field

trees5 <- left_join(trees4, trht_ind, by = c("tree_id", "SampleYear")) |>
  filter(!Plot_Name %in% stunted$Plot_Name)

# Stand level tree heights
trht_avg <- joinStandData() |> select(Plot_Name, SampleYear, Avg_Height_Codom, Avg_Height_Inter)

# Estimating tree heights from NETN Ecological Integrity SOP, page 158
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
source("compile_data_NETN_fix_tree_heights.R")

# checking for missing live tree heights
trhts_na <- treehts |> filter(is.na(ht)) |> filter(STATUSCD == "live") |>
  select(tree_id, Plot_Name, SampleYear, cycle, ScientificName, DBHcm, STATUSCD, STATUSclassifier,
         CrownClassCode, ht)

table(trhts_na$Plot_Name)

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
         statuscd, statusclassifier, treeclcd,
         dbhcm, ht, htcd, cull, habit, decaycd)
head(tree_plots)
names(tree_plots)

#write.csv(tree_plots, "./data/NETN_tree_data_WIP.csv", row.names = F)

#---- Compile NETN sapling data ----
saps <- joinMicroSaplings(park = "all", from = 2006, to = 2024) |>
  arrange(Plot_Name, SampleYear, MicroplotCode, DBHcm) |>
  group_by(Plot_Name) |>
  mutate(sap_order = row_number(),
         tree_id = paste0(Plot_Name, "-sap", sprintf("%03d", sap_order)),
         TreeStatusCode = "live",
         CrownClassCode = 5,
         DecayClassCode = NA_real_,
         Fork = NA_character_) |>
  ungroup() |>
  select(tree_id, Plot_Name, Network, ParkUnit, ParkSubUnit, SampleYear, cycle,
         TSN, ScientificName, Fork, DBHcm, TreeStatusCode, CrownClassCode, DecayClassCode,
         num_stems = Count) |>
  filter(!ScientificName %in% c("Not Sampled", "None present"))

sap_micros <- joinMicroSaplings(park = "all", from = 2006, to = 2024) |>
  filter(!ScientificName %in% c("Not Sampled")) |>
  select(Plot_Name, SampleYear, MicroplotCode) |> unique() |>
  group_by(Plot_Name, SampleYear) |>
  summarize(num_micros = sum(!is.na(MicroplotCode)), .groups = "drop")

table(sap_micros$num_micros, useNA = "always")

saps2 <- left_join(saps, sap_micros, by = c("Plot_Name", "SampleYear"))

# Fixing an unknown spp that is likely ACESAC based on other visits
saps2$TSN[saps2$tree_id == "WEFA-001-sap009"] <- 28731
saps2$ScientificName[saps2$tree_id == "WEFA-001-sap009"] <- "Acer saccharum"

# Join USDA Plants Symbol and drop stunted woodland plots
taxa <- VIEWS_NETN$Taxa_NETN[,c("TSN", "TaxonCode")]

saps3 <- left_join(saps2, taxa, by = "TSN") |>
  filter(!Plot_Name %in% stunted$Plot_Name)

table(saps3$TaxonCode, useNA = 'always') # 2 NAs
# Species names missing SPCD b/c of synonyms
# Betula cordifolia; replace with Betula papyrifera; BEPA
# Betula X cearulea; replace with Betula papyrifera; BEPA
# Carya tomentosa: replace with Carya alba; CAAL27
# Photinia villosa doesn't have a SPCD. The closest match is Photinia x fraseri or Photinia davidiana
saps3$TaxonCode[saps3$ScientificName == "Betula cordifolia"] <- "BEPA"
saps3$TaxonCode[saps3$ScientificName == "Betula X cearulea"] <- "BEPA"
saps3$TaxonCode[saps3$ScientificName == "Carya tomentosa"] <- "CAAL27"

saps3$ScientificName[saps3$TSN == -9999999937] <- "Unknown species"
saps3$TSN[saps3$TSN == -9999999937] <- -9999999950

# Join REF_SPECIES SPCD and JENKINS_SPGRPCD to tree data
saps4 <- left_join(saps3, refspp, by = c("TaxonCode" = "SPECIES_SYMBOL"))
sapspp_na <- saps4 |> filter(is.na(SPCD)) |> select(ScientificName)

micro_size_m2 = 2*2*pi

saps5 <- saps4 |> mutate(tpa_unadj = ((micro_size_m2 * num_micros)/4046.86)^-1,
                         ht = NA_real_,
                         htcd = NA_real_,
                         cull = 0,
                         habit = "Sapling",
                         statusclassifier = "unknown") |>
  select(plt_cn = Plot_Name, tre_cn = tree_id, network = Network, park = ParkUnit, parksubunit = ParkSubUnit,
         year = SampleYear, cycle, tpa_unadj,
         spcd = SPCD, jenkins_spgrpcd = JENKINS_SPGRPCD, scientific_name = ScientificName, usda_symbol = TaxonCode,
         statuscd = TreeStatusCode, statusclassifier, treeclcd = CrownClassCode,
         dbhcm = DBHcm, ht, htcd, cull, habit, decaycd = DecayClassCode)

saps_plots <- right_join(plots_final, saps5, by = c("plt_cn", "park", "parksubunit", "network"))
head(data.frame(saps_plots))

setdiff(names(saps_plots), names(tree_plots))
setdiff(names(tree_plots), names(saps_plots))

tree_sap <- rbind(tree_plots, saps_plots) |> filter(!plt_cn %in% stunted$Plot_Name)
# Update SPCD to use use 999 for unknown tree, 998 for unknown hardwood/broadleaf, and 299 for unknown conifer
# based on REF_SPECIES.csv
tree_sap$spcd[tree_sap$scientific_name == "Unknown species"] <- 999
tree_sap$spcd[tree_sap$scientific_name == "Unknown Hardwood"] <- 998
tree_sap$spcd[tree_sap$scientific_name == "Unknown Conifer"] <- 299

write.csv(tree_sap, "./data/NETN_tree_sapling_data.csv", row.names = F)

#---- Metadata for tree and sapling data ----
meta <- data.frame(column_name = names(tree_sap),
                   description = NA_character_,
                   units = NA_character_,
                   NETN = NA_character_)

meta$description[meta$column_name == "plt_cn"] <- "Unique plot identifier using 4-letter park code and 3-digit plot number"
meta$units[meta$column_name == "plt_cn"] <- NA_character_
meta$NETN[meta$column_name == "plt_cn"] <- "Plot numbers are unique to parks, but not the network"

meta$description[meta$column_name == "tre_cn"] <- "Unique tree identifier using plot_cn and tree tag number"
meta$units[meta$column_name == "tre_cn"] <- NA_character_
meta$NETN[meta$column_name == "tre_cn"] <- "Tree tag numbers are unique to plots, but not the park or network. Saplings are not tagged, and are given a unique number based on when they were sampled. Sapling ids are therefore not linkable across time."

meta$description[meta$column_name == "ecosubcd"] <- "Ecological subsection code"
meta$description[meta$column_name == "ecodivision"] <- "Ecological subsection code"

meta$description[meta$column_name == "year"] <- "Year plot was sampled, ranging from 2006-2024"

meta$description[meta$column_name == "cycle"] <- "Number of times a plot has been visited"
meta$units[meta$column_name == "cycle"] <- NA_character_
meta$NETN[meta$column_name == "cycle"] <- "Plots are typically sampled on a 4-year rotation, with cycle 1 being 2006-2009. The sampling schedule was disrupted by COVID in 2020 and 2021, so that some parks/plots have 5 or 6 year intervals between visits."

meta$description[meta$column_name == "tpa_unadj"] <- "Tree and sapling expansion factor for sampling area. Equates to 1/acres of sampling area."
meta$units[meta$column_name == "tpa_unadj"] <- "1/acre"
meta$NETN[meta$column_name == "tpa_unadj"] <- "In 2006, only 1 microplot was sampled for saplings. In 2007 and on, 3 microplots were sampled. The tpa_unadj reflects that change."

meta$description[meta$column_name == "lat"] <- "Latitude of plot center in NAD83"
meta$units[meta$column_name == "lat"] <- "decimal degrees"

meta$description[meta$column_name == "long"] <- "Longitude of plot center in NAD83"
meta$units[meta$column_name == "long"] <- "decimal degrees"

meta$description[meta$column_name == "network"] <- "4-letter network code"
meta$description[meta$column_name == "park"] <- "4-letter park code"
meta$description[meta$column_name == "parksubunit"] <- "Subunit within a park as potential grouping variable."
meta$description[meta$column_name == "state_name"] <- "State park occurs in."
meta$description[meta$column_name == "statecd"] <- "State FIPS code"
meta$description[meta$column_name == "countycd"] <- "County FIPS code"
meta$description[meta$column_name == "spcd"] <- "Species code used by FIA taken from REF_SPECIES. When a species code doesn't exist, code is NA."
meta$description[meta$column_name == "jenkins_spgrpcd"] <- "Jenkins species group used by FIA and taken from REF_SPECIES"
meta$description[meta$column_name == "scientific_name"] <- "Latin name"
meta$description[meta$column_name == "usda_symbol"] <- "USDA Plants Symbol"
meta$description[meta$column_name == "statuscd"] <- "Tree status: dead or live"

meta$description[meta$column_name == "statusclassifier"] <-
  "Indicates whether stem is standing, leaning (30-45 degrees) or fallen (>45degrees)"
meta$NETN[meta$column_name == "statusclassifier"] <-
  "Only recorded for trees, not saplings. Classifier wasn't recorded until 2010 and later."

meta$description[meta$column_name == "treeclcd"] <- "Crown class of tree. 1 = open grown; 2 = dominant; 3 = codominant; 4 = intermediate; 5 = subcanopy; 6 = gap exploiter"
meta$NETN[meta$column_name == "treecld"] <- "Gap exploiter is either an intermediate or subcanopy tree expected to grow faster than normal because it is within a canopy gap."

meta$description[meta$column_name == "dbhcm"] <- "DBH of tree or sapling."
meta$units[meta$column_name == "dbhcm"] <- "cm"

meta$description[meta$column_name == "ht"] <- "Height of tree"
meta$units[meta$column_name == "ht"] <- "meters"

meta$description[meta$column_name == "htcd"] <- "Indicates how tree height was assigned. 1 = the individual tree was measured for tree height. 4 = the height was derived."

meta$description[meta$column_name == "cull"] <- "Percent of wood considered cull from 0 to 1 based on recorded tree conditions."

meta$description[meta$column_name == "habit"] <- "Denotes if stem is a tree or sapling. Trees are defined as 1) any live stem >=10cm DBH regardless of whether it is standing, leaning or fallen, or 2) any dead stem >= 10cm DBH that is standing or leaning. Saplings are stems >= 1cm DBH and <10cm DBH and are only measured if live."
meta$NETN[meta$column_name == "habit"] <- "Individual saplings are not tracked over time, and sometimes are missed across years because crews because QA/QC isn't as strict as with trees."

meta$description[meta$column_name == "decaycd"] <- "FIA decay class for snags ranging from 1-5."


#++++ Questions for group ++++
# 1.  Photinia villosa doesn't have a SPCD. The closest match is Photinia x fraseri or Photinia davidiana. Do I use
#     one of those codes, or leave it blank? This is mainly for saplings.
# 2. I set sapling crown class code as subcanopy. Is that okay? I did not add a height. Should?




