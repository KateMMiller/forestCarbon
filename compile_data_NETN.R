library(forestNETN)
library(tidyverse)
library(sf)
library(rgeoboundaries) # for state/county boundaries
library(tidycensus) # for FIPS state/county codes

# import latest NETN data
importData()

# dir.create("./data")

#---- Download and compile external data -----
# Download FIA REF_SPECIES.csv to get SPCD
ref_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/FIADB_REFERENCE.zip"
download.file(refurl, "./data/FIADB_REFERENCE.zip")
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
plots <- joinLocEvent(output = 'verbose') |> select(Plot_Name, ParkUnit, Lat, Long) |>
  mutate(lat = Lat, long = Long) |> unique()
plots_sf <- st_as_sf(plots, coords = c("Long", "Lat"), crs = 4269)
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

table(plots_eco$MAP_UNIT_N, plots_eco$ParkUnit) # No more Water
table(plots_eco$MAP_UNIT_S, plots_eco$ParkUnit) # No more Water
table(plots_eco$SUBSECTION, plots_eco$ParkUnit) # No more 816 (water)

# Join with eco prov/sub codes
plots_eco <- left_join(plots_ecoprov |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       plots_ecosub |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       by = c("Plot_Name", "ParkUnit", "lat", "long")) |>
  select(Plot_Name, ParkUnit, PROV = PROVINCE, SUBS = SUBSECTION, lat, long)

head(plots_eco)
length(unique(plots_eco$Plot_Name)) #351
table(complete.cases(plots_eco)) # all TRUE

#---- State, county and FIPS Codes ----
head(plot_eco)
us_states <- geoboundaries("USA", "adm1")
us_county <- geoboundaries("USA", "adm2")

data("fips_codes")

#---- Compile NETN tree data ----
trees <- joinTreeData(park = "all", from = 2006, to = 2024, status = 'active') |>
  mutate(tree_id = paste0(Plot_Name, "-", sprintf("%03d", TagCode))) |>
  select(tree_id, Plot_Name, Network, ParkUnit, ParkSubUnit, SampleYear, cycle,
         TSN, ScientificName, Fork, DBHcm, TreeStatusCode, CrownClassCode, DecayClassCode, num_stems)

# Drop 4 stunted woodlands, which are sampled at DRC and typically <5m tall.
stunted <- joinLocEvent() |> filter(IsStuntedWoodland == TRUE) |>
  select(Plot_Name) |> unique()

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

#----- Fix missing tree status and crown class codes -----
# For live trees missing a crown class or status qualifier, using the class from the next consecutive measurement
trees3$CrownClassCode[trees3$tree_id == "ACAD-002-010" & trees3$SampleYear == 2006] <- 4
trees3$CrownClassCode[trees3$tree_id == "ACAD-008-014" & trees3$SampleYear == 2010] <- 5
trees3$CrownClassCode[trees3$tree_id == "ACAD-022-007" & trees3$SampleYear == 2006] <- 5
trees3$TreeStatusCode[trees3$tree_id == "ACAD-031-029" & trees3$SampleYear == 2010] <- "DM"
trees3$CrownClassCode[trees3$tree_id == "ACAD-046-010" & trees3$SampleYear == 2007] <- 4
trees3$TreeStatusCode[trees3$tree_id == "ACAD-046-010" & trees3$SampleYear == 2007] <- "AS"
trees3$CrownClassCode[trees3$tree_id == "ACAD-046-010" & trees3$SampleYear == 2011] <- 4
trees3$TreeStatusCode[trees3$tree_id == "ACAD-046-010" & trees3$SampleYear == 2011] <- "AS"
trees3$CrownClassCode[trees3$tree_id == "ACAD-046-016" & trees3$SampleYear == 2011] <- 5
trees3$TreeStatusCode[trees3$tree_id == "ACAD-046-016" & trees3$SampleYear == 2011] <- "AS"
trees3$CrownClassCode[trees3$tree_id == "ACAD-064-002" & trees3$SampleYear == 2011] <- 1
trees3$TreeStatusCode[trees3$tree_id == "ACAD-064-002" & trees3$SampleYear == 2011] <- "AS"
trees3$CrownClassCode[trees3$tree_id == "ACAD-066-010" & trees3$SampleYear == 2011] <- 1
trees3$TreeStatusCode[trees3$tree_id == "ACAD-066-010" & trees3$SampleYear == 2011] <- "AS"
trees3$CrownClassCode[trees3$tree_id == "ACAD-075-017" & trees3$SampleYear == 2007] <- 5
trees3$CrownClassCode[trees3$tree_id == "ACAD-079-018" & trees3$SampleYear == 2011] <- 5
trees3$CrownClassCode[trees3$tree_id == "ACAD-086-002" & trees3$SampleYear == 2008] <- 3
trees3$CrownClassCode[trees3$tree_id == "ACAD-102-026" & trees3$SampleYear == 2008] <- 3
trees3$CrownClassCode[trees3$tree_id == "ACAD-109-001" & trees3$SampleYear == 2008] <- 5
trees3$CrownClassCode[trees3$tree_id == "ACAD-161-026" & trees3$SampleYear == 2009] <- 5
trees3$CrownClassCode[trees3$tree_id == "ACAD-176-033" & trees3$SampleYear == 2011] <- 5

trees3$CrownClassCode[trees3$tree_id == "MABI-011-007" & trees3$SampleYear == 2010] <- 3
trees3$CrownClassCode[trees3$tree_id == "MABI-018-001" & trees3$SampleYear == 2008] <- 5

trees3$CrownClassCode[trees3$tree_id == "MIMA-001-006" & trees3$SampleYear == 2006] <- 1
trees3$CrownClassCode[trees3$tree_id == "MIMA-001-006" & trees3$SampleYear == 2010] <- 1

trees3$CrownClassCode[trees3$tree_id == "MORR-010-013" & trees3$SampleYear == 2007] <- 5

trees3$CrownClassCode[trees3$tree_id == "ROVA-029-011" & trees3$SampleYear == 2013] <- 5

trees3$CrownClassCode[trees3$tree_id == "SAGA-011-021" & trees3$SampleYear == 2012] <- 5
trees3$CrownClassCode[trees3$tree_id == "SAGA-020-013" & trees3$SampleYear == 2012] <- 5

trees3$CrownClassCode[trees3$tree_id == "SARA-002-009" & trees3$SampleYear == 2010] <- 5
trees3$CrownClassCode[trees3$tree_id == "SARA-005-013" & trees3$SampleYear == 2010] <- 5
trees3$CrownClassCode[trees3$tree_id == "SARA-014-015" & trees3$SampleYear == 2010] <- 5

trees3$CrownClassCode[trees3$tree_id == "WEFA-001-014" & trees3$SampleYear == 2011] <- 5

# Tricky Zombie trees (live > dead > live)
trees3$CrownClassCode[trees3$tree_id == "ACAD-088-015" & trees3$SampleYear == 2012] <- 2
trees3$TreeStatusCode[trees3$tree_id == "ACAD-088-015" & trees3$SampleYear == 2012] <- "AS"
trees3$CrownClassCode[trees3$tree_id == "ACAD-088-015" & trees3$SampleYear == 2016] <- 4
trees3$TreeStatusCode[trees3$tree_id == "ACAD-088-015" & trees3$SampleYear == 2016] <- "AB"
trees3$CrownClassCode[trees3$tree_id == "ACAD-088-015" & trees3$SampleYear == 2021] <- 4
trees3$TreeStatusCode[trees3$tree_id == "ACAD-088-015" & trees3$SampleYear == 2021] <- "AB"

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

table(trees3$STATUSclassifier, useNA = 'always')

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
  select(tree_id, SampleYear, HEIGHT_IND = Height) |> filter(!is.na(tree_id))

trees5 <- left_join(trees4, trht_ind, by = c("tree_id", "SampleYear")) |>
  filter(!Plot_Name %in% stunted$Plot_Name)

# Stand level tree heights
trht_avg <- joinStandData() |> select(Plot_Name, SampleYear, Avg_Height_Codom, Avg_Height_Inter)

# Estimating tree heights from NETN Ecological Integrity SOP, page 158
treehts <- left_join(trees5, trht_avg, by = c("Plot_Name", "SampleYear")) |>
  mutate(HEIGHT_CALC =
           case_when(!is.na(HEIGHT_IND) ~ HEIGHT_IND,
                     CrownClassCode == 1 ~ Avg_Height_Codom, # could be taller, but typically codom
                     CrownClassCode == 2 ~ Avg_Height_Codom * 1.1,
                     CrownClassCode == 3 & is.na(HEIGHT_IND) ~ Avg_Height_Codom,
                     CrownClassCode == 4 & is.na(HEIGHT_IND) & !is.na(Avg_Height_Inter) ~ Avg_Height_Inter,
                     CrownClassCode == 4 & is.na(HEIGHT_IND) & is.na(Avg_Height_Inter) ~ Avg_Height_Codom * 0.8,
                     CrownClassCode %in% c(5, 6) ~ Avg_Height_Codom * 0.5 # subcanopy and gap exploiters
                     )) |>
  select(tree_id, Plot_Name, Network, ParkUnit, ParkSubUnit, SampleYear, cycle, SCIENTIFIC_NAME, USDA_SYMBOL = TaxonCode,
         SPCD, JENKINS_SPGRPCD, STATUSCD, STATUSclassifier, CrownClassCode, DBHcm, HEIGHT_CALC)

#----- Tricky heights to fix manually -----
# ACAD-001 2006 has no tree heights. Using 2010 measurements
treehts$HEIGHT_CALC[treehts$Plot_Name == "ACAD-001" & treehts$SampleYear == 2006 & treehts$CrownClassCode == 2] <- 15.46667 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "ACAD-001" & treehts$SampleYear == 2006 & treehts$CrownClassCode == 3] <- 15.46667
treehts$HEIGHT_CALC[treehts$Plot_Name == "ACAD-001" & treehts$SampleYear == 2006 & treehts$CrownClassCode == 4] <- 15.46667 * 0.8

# ACAD-095 2012 trees all classified as dominant with no tree heights. Better to class as codom. Using 2016 measurements for same trees
treehts$HEIGHT_CALC[treehts$tree_id == "ACAD-095-001" & treehts$SampleYear == 2012] <- 8.0
treehts$HEIGHT_CALC[treehts$tree_id == "ACAD-095-002" & treehts$SampleYear == 2012] <- 8.2

# MABI-014 2008 has no stand heights. Using 2012 measurements
treehts$HEIGHT_CALC[treehts$Plot_Name == "MABI-014" & treehts$SampleYear == 2008 & treehts$CrownClassCode == 2] <- 36.13333 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "MABI-014" & treehts$SampleYear == 2008 & treehts$CrownClassCode == 3] <- 36.13333
treehts$HEIGHT_CALC[treehts$Plot_Name == "MABI-014" & treehts$SampleYear == 2008 & treehts$CrownClassCode == 4] <- 36.13333 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "MABI-014" & treehts$SampleYear == 2008 & treehts$CrownClassCode == 5] <- 36.13333 * 0.5

# MIMA-020 is early successional with no heights for most years except 2008. Using 2008 values for remaining visits
treehts$HEIGHT_CALC[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 1] <- 18
treehts$HEIGHT_CALC[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 2] <- 18 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 3] <- 18
treehts$HEIGHT_CALC[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 4] <- 18 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 5] <- 18 * 0.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 6] <- 18 * 0.5

# MORR-002 2024 has no codominant trees with heights. Using 2019 values.
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 1] <- 25.9
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 2] <- 25.9 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 3] <- 25.9
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 5] <- 25.9 * 0.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 6] <- 25.9 * 0.5

# MORR-006 2007 has no tree heights. Using 2011 measurements
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 1] <- 28.8333
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 2] <- 28.8333 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 3] <- 28.8333
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 4] <- 28.8333 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 5] <- 28.8333 * 0.5

# MORR-012 2007 has no tree heights. Using 2011 measurements
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 1] <- 32.0333
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 2] <- 32.0333 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 3] <- 32.0333
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 4] <- 32.0333 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 5] <- 32.0333 * 0.5

# MORR-021 doesn't have codominant trees in 2013, 2017, and 2022. Using 2009 values
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-021" & treehts$SampleYear %in% c(2013, 2017, 2022) & treehts$CrownClassCode == 1] <- 27.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-021" & treehts$SampleYear %in% c(2013, 2017, 2022) & treehts$CrownClassCode == 2] <- 27.5 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-021" & treehts$SampleYear %in% c(2013, 2017, 2022) & treehts$CrownClassCode == 3] <- 27.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "MORR-021" & treehts$SampleYear %in% c(2013, 2017, 2022) & treehts$CrownClassCode == 5] <- 27.5 * 0.5

# ROVA-001 2007 has no tree heights. Using 2011 measurements
treehts$HEIGHT_CALC[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 1] <- 24.6667
treehts$HEIGHT_CALC[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 2] <- 24.6667 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 3] <- 24.6667
treehts$HEIGHT_CALC[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 4] <- 24.6667 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 5] <- 24.6667 * 0.5

# SARA-002 is early successional with no heights for most years except 2006. Using 2006 values for remaining visits
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 1] <- 7.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 2] <- 7.8 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 3] <- 7.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 4] <- 7.8 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 5] <- 7.8 * 0.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 6] <- 7.8 * 0.5

# SARA-009 is early successional with no heights for most years except 2006. Using 2006 values for remaining visits
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 1] <- 12.7
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 2] <- 12.7 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 3] <- 12.7
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 4] <- 12.7 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 5] <- 12.7 * 0.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 6] <- 12.7 * 0.5

# SARA-012 is early successional with no heights for most years except 2006. Using 2006 values for remaining visits
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 1] <- 17.4
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 2] <- 17.4 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 3] <- 17.4
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 4] <- 17.4 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 5] <- 17.4 * 0.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
                      treehts$CrownClassCode == 6] <- 17.4 * 0.5

# SARA-024 is early successional with no heights for most years except 2008. Using 2008 values for remaining visits
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
                      treehts$CrownClassCode == 1] <- 25
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
                      treehts$CrownClassCode == 2] <- 25 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
                      treehts$CrownClassCode == 3] <- 25
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
                      treehts$CrownClassCode == 4] <- 25 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
                      treehts$CrownClassCode == 5] <- 25 * 0.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
                      treehts$CrownClassCode == 6] <- 25 * 0.5

# SARA-029 has no codominant trees in 2012. Using 2016 values.
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 1] <- 18.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 2] <- 18.5 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 3] <- 18.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 4] <- 13.7
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 5] <- 18.5 * 0.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 6] <- 18.5 * 0.5

# SARA-032 is early successional with no heights for most years except 2006. Using 2006 values for remaining visits
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
                      treehts$CrownClassCode == 1] <- 10.2
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
                      treehts$CrownClassCode == 2] <- 10.2 * 1.1
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
                      treehts$CrownClassCode == 3] <- 10.2
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
                      treehts$CrownClassCode == 4] <- 10.2 * 0.8
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
                      treehts$CrownClassCode == 5] <- 10.2 * 0.5
treehts$HEIGHT_CALC[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
                      treehts$CrownClassCode == 6] <- 10.2 * 0.5

trhts_na <- treehts |> filter(is.na(HEIGHT_CALC)) |> filter(STATUSCD == "live") |>
  select(tree_id, Plot_Name, SampleYear, cycle, SCIENTIFIC_NAME, DBHcm, STATUSCD, STATUSclassifier,
         CrownClassCode, HEIGHT_CALC)

table(trhts_na$Plot_Name)

#----- CULL -----
head(treehts)
treecond1 <- joinTreeConditions() |> mutate(tree_id = paste0(Plot_Name, "-", sprintf("%03d", TagCode))) |>
  select(tree_id, SampleYear, AD, CAVL, CAVS, DBT)

treecond1$AD[is.na(treecond1$AD)] <- 0
treecond1$CAVL[is.na(treecond1$CAVL)] <- 0
treecond1$CAVS[is.na(treecond1$CAVS)] <- 0
treecond1$DBT[is.na(treecond1$DBT)] <- 0

treecond <- left_join(treehts, trcond, by = c("tree_id", "SampleYear")) |>
  mutate(CULL_br = ifelse(STATUSclassifier == "broken", 0.5, 0),
         CULL_AD = ifelse(AD == 1, 0.25, 0),
         CULL_CAVL = ifelse(AD == 0 & CAVL == 1, 0.1, 0),
         CULL_CAVS = ifelse(AD == 0 & CAVS == 1, 0.05, 0),
         CULL_DBT = ifelse(AD == 0 & DBT == 1, 0.1, 0),
         CULL = CULL_br + CULL_AD + CULL_CAVL + CULL_CAVS + CULL_DBT,
         Habit = "Tree",
         tpa_unadj = ifelse(ParkUnit == "ACAD", (225/4046.86)^-1, (400/4046.86)^-1)
  )

tree_final <- treecond |> select(tree_id, Plot_Name, Network, ParkUnit, ParkSubUnit, SampleYear, cycle,
                                 tpa_unadj
                                 SCIENTIFIC_NAME, USDA_SYMBOL, SPCD, JENKINS_SPGRPCD, STATUSCD, STATUSclassifier,
                                 CCLCD = CrownClassCode, DBH = DBHcm, THT = HEIGHT_CALC, CULL, Habit)

head(treecond)
# Nextreecond# Next Steps
  # Trees
    # Join Tree Height to dataset
    # Calculate CULL
    # Crown class
    # Add saplings
  # Events


