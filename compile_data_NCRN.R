#-----------------------------------------------------------------------------
# Compiling National Capital Region Network forest data for forest carbon project
#   Parks included: ANTI, CATO, CHOH, GWMP (includes GRFA), HAFE, MANA, MONO, NACE (includes GREE, PISC), PRWI, ROCR, WOTR
#-----------------------------------------------------------------------------

library(NPSForVeg) # can be installed via: devtools::install_github("NCRN/NPSForVeg")
library(tidyverse)
library(sf)
library(rgeoboundaries) # for state/county boundaries
library(tidycensus) # for FIPS state/county codes
# dir.create("./data")

# import latest NCRN data.

NCRN<-importNCRN("C:/Users/jschmit/OneDrive - DOI/Data/NCRN_Veg_2024/")

#--dat#---- Download and compile external data -----
# Download FIA REF_SPECIES.csv to get SPCD
ref_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/FIADB_REFERENCE.zip"
download.file(ref_url, "./data/FIADB_REFERENCE.zip")
refspp <- read.csv(unzip("./data/FIADB_REFERENCE.zip", "REF_SPECIES.csv")) |>
  select(SPCD, SPECIES_SYMBOL, SCIENTIFIC_NAME, GENUS, SPECIES, JENKINS_SPGRPCD)
#file.remove("REF_SPECIES.csv")
write.csv(refspp, "./data/REF_SPECIES.csv", row.names = F)

# Download Ecological Province shapefile
ecoprov_url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.EcoMapProvinces.zip"
download.file(ecoprov_url, "./data/S_USA.EcoMapProvinces.zip")
unzip("./data/S_USA.EcoMapProvinces.zip", exdir = "./data")
ecoprov <- read_sf("./data/S_USA.EcoMapProvinces.shp")|> st_transform(4326)
# st_crs(ecoprov) #EPSG 4269 NAD83 latlong

# Download Ecological Subsection shapefile
ecosub_url <- "https://data.fs.usda.gov/geodata/edw/edw_resources/shp/S_USA.EcomapSubsections.zip"
download.file(ecosub_url, "./data/S_USA.EcomapSubsections.zip")
unzip("./data/S_USA.EcomapSubsections.zip", exdir = "./data")
ecosub <- read_sf("./data/S_USA.EcomapSubsections.shp")|> st_transform(4326)
# st_crs(ecosub) #EPSG 4269 NAD83 latlong

#---- Spatial join NETN plots to Ecological Province and Subsection ----
plots <- getPlots(NCRN) |> mutate(Network="NCRN")
plots1 <- plots |> select(Plot_Name, Unit_Code, Subunit_Code, Latitude, Longitude) |>
  mutate(lat = Latitude, long = Longitude) |> unique()
plots_sf <- st_as_sf(plots1, coords = c("Longitude", "Latitude"), crs = 4326)#4269
sf_use_s2(FALSE) # troubleshooting st_join

# Province
plots_ecoprov <- st_join(plots_sf, ecoprov, left = TRUE) |>
  select(Plot_Name, Unit_Code, lat, long, PROVINCE = PROVINCE_, MAP_UNIT_S, MAP_UNIT_N) |>
  st_drop_geometry() |>
  as.data.frame() |>
  arrange(Plot_Name)

table(plots_ecoprov$MAP_UNIT_N, plots_ecoprov$Unit_Code) # No Water
table(plots_ecoprov$MAP_UNIT_S, plots_ecoprov$Unit_Code) # No Water
table(plots_ecoprov$PROVINCE, plots_ecoprov$Unit_Code) # No 816 (water)

# Subsection
plots_ecosub <- st_join(plots_sf, ecosub, left = TRUE) |>
  select(Plot_Name, Unit_Code, lat, long, MAP_UNIT_S, MAP_UNIT_N, SUBSECTION) |>
  st_drop_geometry() |>
  as.data.frame() |>
  arrange(Plot_Name)

table(plots_ecosub$MAP_UNIT_N, plots_ecosub$Unit_Code) # No Water
table(plots_ecosub$MAP_UNIT_S, plots_ecosub$Unit_Code) # No Water
table(plots_ecosub$SUBSECTION, plots_ecosub$Unit_Code) # No 816 (water)

# Join with eco prov/sub codes
plots_eco <- left_join(plots_ecoprov |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       plots_ecosub |> select(-MAP_UNIT_N, -MAP_UNIT_S),
                       by = c("Plot_Name", "Unit_Code", "lat", "long")) |>
  select(Plot_Name, ecodivision = PROVINCE, ecosubcd = SUBSECTION)

### COULD NOT DO THIS ON NPS NETWORK
#---- State, county and FIPS Codes ----

us_states <- st_transform(geoboundaries("USA", "adm1"), 4326)#4269 #worked on airgap with VPN
us_county <- st_transform(geoboundaries("USA", "adm2"), 4326)#4269
plots_state <- st_join(plots_sf, us_states, left = T) |> select(Plot_Name, state_name = shapeName) |>
  st_drop_geometry()
plots_county <- st_join(plots_sf, us_county, left = T) |>
  mutate(county = paste0(shapeName, " County")) |>
  select(Plot_Name, Unit_Code, county) |>
  st_drop_geometry()

plots_comb <- left_join(plots_state, plots_county, by = c("Plot_Name"))
data("fips_codes")
plots_comb2 <- left_join(plots_comb, fips_codes, by = c("state_name", "county"))
head(plots_comb2)

plots2 <- left_join(
  plots |> select(Plot_Name,  Network, Unit_Code, Subunit_Code, Latitude, Longitude) |> unique(),
  plots_eco, by = "Plot_Name"
)

plots_final <- left_join(plots2, plots_comb2, by = c("Plot_Name", "Unit_Code")) |>
  select(plt_cn = Plot_Name, park = Unit_Code, parksubunit = Subunit_Code,  network = Network,
         state_name, ecosubcd, lat = Latitude, long = Longitude, ecodivision, statecd = state_code,
         countycd = county_code)

head(plots_final)
# This is the left df to join tree and sapling data to.

#---- Compile NCRN tree data ----

##NETN code
# trees <- joinTreeData(park = "all", from = 2006, to = 2024, status = 'active') |>
#   mutate(tree_id = paste0(Plot_Name, "-", sprintf("%03d", TagCode))) |>
#   select(tree_id, Plot_Name, Network, ParkUnit, ParkSubUnit, SampleYear, cycle,
#          TSN, ScientificName, Fork, DBHcm, TreeStatusCode, CrownClassCode, DecayClassCode, num_stems)

live_trees<-getPlants(NCRN, "trees", years=2006:2024, status = "alive") |>
  mutate(tree_id=paste0("NCRN-" , Tag)) |>
  select(tree_id, Plot_Name, Unit_Code, Subunit_Code, Sample_Year, Cycle, TSN, Tag, Latin_Name, Equiv_Live_DBH_cm, Equiv_Dead_DBH_cm, Status, Crown_Class, Stems,
         Crown_Class, Crown_Description, Azimuth, Distance)

dead_trees<-getPlants(NCRN, "trees", years=2006:2024, status = "snag") |>
  mutate(tree_id=paste0("NCRN-" , Tag)) |>
  select(tree_id, Plot_Name, Unit_Code, Subunit_Code, Sample_Year, Cycle, TSN, Tag,Latin_Name, Equiv_Live_DBH_cm, Equiv_Dead_DBH_cm, Status, Crown_Class, Stems,
         Crown_Class, Crown_Description, Azimuth, Distance)


trees<-rbind(live_trees, dead_trees)

trees2<- trees|> mutate(Latin_Name=case_match(
  Latin_Name,
  "Quercus montana" ~ "Quercus prinus",
  "Carya tomentosa" ~ "Carya alba",
  "Quercus X benderi" ~ "Quercus rubra", # Q rubra X coccinea
  "Quercus X bushii" ~ "Quercus velutina", # Q velutina x marylandica
  "Quercus X fernowii" ~ "Quercus alba",# Q alba x stellata
  "Quercus X willdenowiana" ~ "Quercus velutina", # Q falcata x velutina
  c("Malus prunifolia", "Malus sieboldii") ~ "Malus spp.", #prunifolia and seiboldii not in reference list
  "Pyrus betulifolia" ~ "Pyrus spp.", #betulifolia not in ref list
  "Rosaceae Family" ~ "Pyrus calleryana", #Note indicate this may be the correct species
  "Unknown" ~"Tree unknown",
  .default = Latin_Name
))

trees2<-left_join(trees2, refspp, by=join_by(Latin_Name==SCIENTIFIC_NAME)) |>
  filter(!Status %in% c("Dead","Dead - Too Small", "Dead - Human Action", "Dead Missing"))

stems<-read_csv("C:/Users/jschmit/OneDrive - DOI/Data/NCRN_Veg_2024/Stems.csv") |> select(-Cycle)

stems2<-stems |> mutate(Latin_Name=case_match(
  Latin_Name,
  "Quercus montana" ~ "Quercus prinus",
  "Carya tomentosa" ~ "Carya alba",
  "Quercus X benderi" ~ "Quercus rubra", # Q rubra X coccinea
  "Quercus X bushii" ~ "Quercus velutina", # Q velutina x marylandica
  "Quercus X fernowii" ~ "Quercus alba",# Q alba x stellata
  "Quercus X willdenowiana" ~ "Quercus velutina", # Q falcata x velutina
  c("Malus prunifolia", "Malus sieboldii") ~ "Malus spp.", #prunifolia and seiboldii not in reference list
  "Pyrus betulifolia" ~ "Pyrus spp.", #betulifolia not in ref list
  "Rosaceae Family" ~ "Pyrus calleryana", #Note indicate this may be the correct species
  "Unknown" ~"Tree unknown",
  .default = Latin_Name )) |>
  filter(!Status %in% c("Dead","Dead - Too Small", "Dead - Human Action", "Dead Missing", "Dead Fallen"))


stems_trees<-stems2 |> filter(Class=="Tree", DBH>=10.0)

trees3<-left_join(stems_trees, trees2)

#
trees4<-trees3 |> mutate(Status2=case_match(
                         Status,
                         "Alive Broken"~ "broken",
                         "Alive Fallen" ~ "fallen",
                         c("Alive Leaning", "Dead Leaning")~"leaning",
                         c("Alive Standing", "Dead Standing") ~ "standing",
                         .default = "unknown"
                        ))
trees4<-trees4 |> mutate(StatusCode=case_match(
  Status,
  c("Alive Broken", "Alive Fallen" ,"Alive Leaning", "Alive Standing") ~"live",
  c("Dead Leaning", "Dead Standing") ~ "dead",
  .default = "unknown"
))



#---- Compile tree height ----
# NCRN does not measure this, so all are NA, NA_real_ is used to make it numeric
trees5<-trees4 |>
  mutate(ht=NA_real_, habit="Tree",tpa_unadj=(pi*(15^2)/4046.86)^-1,
                         decaycd=ifelse(StatusCode=="live",0,NA ), htcd=NA)


#----- CULL -----

Conditions<-read_csv("C:/Users/jschmit/OneDrive - DOI/Data/NCRN_Veg_2024/Tree_Sapling_Conditions.csv") |>
  select(-Cycle, - TSN) |>
  filter(Condition %in% c("Advanced decay", "Hollow", "Large dead branches"))

Conditions2<-semi_join(Conditions, trees5)


# treecond1 <- joinTreeConditions() |> mutate(tree_id = paste0(Plot_Name, "-", sprintf("%03d", TagCode))) |>
#   select(tree_id, SampleYear, AD, CAVL, CAVS, DBT)

# treecond1$AD[is.na(treecond1$AD)] <- 0
# treecond1$CAVL[is.na(treecond1$CAVL)] <- 0
# treecond1$CAVS[is.na(treecond1$CAVS)] <- 0
# treecond1$DBT[is.na(treecond1$DBT)] <- 0

Conditions3<- Conditions2 |> summarise(
  Cull_br = ifelse(all(Status=="Alive Broken"), .5, 0),
  Cull_AD = ifelse(any(Condition=="Advanced decay"), .25, 0),
  Cull_Ho = ifelse(!any(Condition=="Advanced decay") & any(Condition=="Hollow"), 0.1,0),
  Cull_DBT = ifelse(!any(Condition=="Advanced decay") & any(Condition=="Large dead branches"), 0.1,0),
  cull=Cull_br + Cull_AD + Cull_Ho + Cull_DBT,
  .by=c(Tag, Sample_Year))


#
# treecond <- left_join(treehts, treecond1, by = c("tree_id", "SampleYear")) |>
#   mutate(CULL_br = ifelse(STATUSclassifier == "broken", 0.5, 0),
#          CULL_AD = ifelse(AD == 1, 0.25, 0),
#          CULL_CAVL = ifelse(AD == 0 & CAVL == 1, 0.1, 0),
#          CULL_CAVS = ifelse(AD == 0 & CAVS == 1, 0.05, 0),
#          CULL_DBT = ifelse(AD == 0 & DBT == 1, 0.1, 0),
#          cull = CULL_br + CULL_AD + CULL_CAVL + CULL_CAVS + CULL_DBT,
#          habit = "Tree",
#          tpa_unadj = ifelse(ParkUnit == "ACAD", (225/4046.86)^-1, (400/4046.86)^-1))

# treecond$htcd[is.na(treecond$htcd) & !is.na(treecond$ht)] <- 4

trees5 <-trees5 |> left_join(Conditions3) |>
  mutate(cull=ifelse(is.na(cull), 0, cull))

tree6 <- trees5 |> select(plt_cn = Plot_Name, tre_cn = tree_id, year = Sample_Year, cycle=Cycle,
                            tpa_unadj, scientific_name = Latin_Name, usda_symbol =  SPECIES_SYMBOL,
                            spcd = SPCD, jenkins_spgrpcd = JENKINS_SPGRPCD, statuscd = StatusCode,
                            statusclassifier = Status2, treeclcd = Crown_Class,
                          dbhcm = DBH, ht, htcd, cull, habit, decaycd)


tree_plots <- left_join(plots_final, tree6, by = c("plt_cn")) |>
  select(plt_cn,  tre_cn, ecosubcd, ecodivision, year, cycle, tpa_unadj,
         lat, long, network, park, parksubunit, state_name, statecd, countycd,
         spcd, jenkins_spgrpcd, scientific_name, usda_symbol,
         statuscd, statusclassifier, treeclcd,
         dbhcm, ht, htcd, cull, habit, decaycd)
head(tree_plots)
names(tree_plots)

#write.csv(tree_plots, "./data/NCRN_tree_data.csv", row.names = F)

#---- Compile NETN sapling data ----
saps <- getPlants(NCRN, "saplings", years=2006:2024, status = "alive") |>
  mutate(tree_id=paste0("NCRN-" , Tag),
         TreeStatusCode = "live",
         CrownClassCode = 5,
         DecayClassCode = NA_real_,
         Fork = NA_character_) |>
  select(tree_id, Plot_Name, Unit_Code, Subunit_Code, Sample_Year, Cycle, TSN, Tag, Latin_Name, Equiv_Live_DBH_cm, Status,StemsLive,
         TreeStatusCode, CrownClassCode, DecayClassCode,
         Fork)


saps2<- saps |> mutate(Latin_Name=case_match(
  Latin_Name,
  "Quercus montana" ~ "Quercus prinus",
  "Carya tomentosa" ~ "Carya alba",
  "Malus sieboldii" ~ "Malus spp.", # Malus seiboldii not in reference list
  "Amelanchier canadensis" ~  "Amelanchier spp.", #A. canadensis not in reference list
  "Phellodendron lavallei"~"Phellodendron amurense",
  "Robinia viscosa" ~"Robinia pseudoacacia",
  "Sophora japonica"~"Styphnolobium japonicum",
  "Chionanthus virginicus" ~"Chionanthus axilliflorus", # not in list, and no spp. axilliflorus is similar size, but tropical
  .default = Latin_Name ))




stems_saps<-stems2 |> filter(DBH<=10.0) |>
  mutate(Latin_Name=case_match(
    Latin_Name,
  "Quercus montana" ~ "Quercus prinus",
  "Carya tomentosa" ~ "Carya alba",
  "Malus sieboldii" ~ "Malus spp.", # Malus seiboldii not in reference list
  "Amelanchier canadensis" ~  "Amelanchier spp.", #A. canadensis not in reference list
  "Phellodendron lavallei"~"Phellodendron amurense",
  "Robinia viscosa" ~"Robinia pseudoacacia",
  "Sophora japonica"~"Styphnolobium japonicum",
  "Chionanthus virginicus" ~"Chionanthus axilliflorus", # not in list, and no spp. axilliflorus is similar size, but tropical
  .default = Latin_Name ))


# Join REF_SPECIES SPCD and JENKINS_SPGRPCD to tree data

saps3<-left_join(saps2, refspp,  by=join_by(Latin_Name==SCIENTIFIC_NAME))

#correction for small stems attached to trees in microplots
#dist=sqrt (rad1^2+rad2^2a+2rad1*rad2*cos(theta2-theta1))
Micr1<-pi*60/180  # these are converting the Azimuth of the cenetrs for our microplots to radians
Micr2<-pi*180/180
Micr3<-pi*300/180


trees_saps<-trees2 |>
  mutate(Az_rad=Azimuth*pi/180, Dist2=Distance^2) |>
  filter(sqrt( Dist2 + 100 - (2*10*Distance * cos(Az_rad-Micr1 )))<=3 |
           sqrt( Dist2 + 100 - (2*10*Distance * cos(Az_rad-Micr2 )))<=3 |
           sqrt( Dist2 + 100 - (2*10*Distance * cos(Az_rad-Micr3 )))<=3 ) |>
  filter(Stems>1) #its a tree, so if only 1 stem its not a sapling

trees_saps<-trees_saps |>
    mutate(TreeStatusCode = "live",
         CrownClassCode = 5,
         DecayClassCode = NA_real_,
         Fork = NA_character_) |>
  select(tree_id, Plot_Name, Unit_Code, Subunit_Code, Sample_Year, Cycle, TSN,
         Tag, Latin_Name, Equiv_Live_DBH_cm, Status, StemsLive=Stems,
         TreeStatusCode, CrownClassCode, DecayClassCode, Fork, SPCD,
         SPECIES_SYMBOL, GENUS, SPECIES, JENKINS_SPGRPCD)

saps4<-rbind(saps3, trees_saps)


stems_saps2<-stems_saps |> semi_join(saps4, by=c("Tag", "Sample_Year"))


saps5<-stems_saps2 |> left_join(saps4)

micro_size_m2 = 3*3*pi

saps6 <- saps5 |> mutate(tpa_unadj = ((micro_size_m2 * 3)/4046.86)^-1,
                         network="NCRN",
                         ht = NA_real_,
                         htcd = NA_real_,
                         cull = 0,
                         habit = "Sapling",
                         statusclassifier = "unknown") |>
  select(plt_cn = Plot_Name, tre_cn = tree_id, network, park = Unit_Code, parksubunit = Subunit_Code,
         year = Sample_Year, cycle=Cycle, tpa_unadj,
         spcd = SPCD, jenkins_spgrpcd = JENKINS_SPGRPCD, scientific_name = Latin_Name, usda_symbol = SPECIES_SYMBOL,
         statuscd = TreeStatusCode, statusclassifier, treeclcd = CrownClassCode,
         dbhcm = DBH, ht, htcd, cull, habit, decaycd = DecayClassCode) #plots only paritally done - WOTR had copperheads, ROCR a homeless encampment


saps_plots <- right_join(plots_final, saps6, by = c("plt_cn", "park", "parksubunit", "network"))
head(data.frame(saps_plots))

setdiff(names(saps_plots), names(tree_plots))
setdiff(names(tree_plots), names(saps_plots))

tree_sap_data <- rbind(tree_plots, saps_plots) |> filter(!(plt_cn %in% c("WOTR-0003", "ROCR-0186") & year==2024)) #plots only paritally done - WOTR had copperheads, ROCR a homeless encampment

write.csv(tree_sap_data, "./data/NCRN_tree_sapling_data.csv", row.names = F)

#---- Metadata for tree and sapling data ----
# meta <- data.frame(column_name = names(tree_sap),
#                    description = NA_character_,
#                    units = NA_character_,
#                    NETN = NA_character_)
#
# meta$description[meta$column_name == "plt_cn"] <- "Unique plot identifier using 4-letter park code and 3-digit plot number"
# meta$units[meta$column_name == "plt_cn"] <- NA_character_
# meta$NETN[meta$column_name == "plt_cn"] <- "Plot numbers are unique to parks, but not the network"
#
# meta$description[meta$column_name == "tre_cn"] <- "Unique tree identifier using plot_cn and tree tag number"
# meta$units[meta$column_name == "tre_cn"] <- NA_character_
# meta$NETN[meta$column_name == "tre_cn"] <- "Tree tag numbers are unique to plots, but not the park or network. Saplings are not tagged, and are given a unique number based on when they were sampled. Sapling ids are therefore not linkable across time."
#
# meta$description[meta$column_name == "ecosubcd"] <- "Ecological subsection code"
# meta$description[meta$column_name == "ecodivision"] <- "Ecological subsection code"
#
# meta$description[meta$column_name == "year"] <- "Year plot was sampled, ranging from 2006-2024"
#
# meta$description[meta$column_name == "cycle"] <- "Number of times a plot has been visited"
# meta$units[meta$column_name == "cycle"] <- NA_character_
# meta$NETN[meta$column_name == "cycle"] <- "Plots are typically sampled on a 4-year rotation, with cycle 1 being 2006-2009. The sampling schedule was disrupted by COVID in 2020 and 2021, so that some parks/plots have 5 or 6 year intervals between visits."
#
# meta$description[meta$column_name == "tpa_unadj"] <- "Tree and sapling expansion factor for sampling area. Equates to 1/acres of sampling area."
# meta$units[meta$column_name == "tpa_unadj"] <- "1/acre"
# meta$NETN[meta$column_name == "tpa_unadj"] <- "In 2006, only 1 microplot was sampled for saplings. In 2007 and on, 3 microplots were sampled. The tpa_unadj reflects that change."
#
# meta$description[meta$column_name == "lat"] <- "Latitude of plot center in NAD83"
# meta$units[meta$column_name == "lat"] <- "decimal degrees"
#
# meta$description[meta$column_name == "long"] <- "Longitude of plot center in NAD83"
# meta$units[meta$column_name == "long"] <- "decimal degrees"
#
# meta$description[meta$column_name == "network"] <- "4-letter network code"
# meta$description[meta$column_name == "park"] <- "4-letter park code"
# meta$description[meta$column_name == "parksubunit"] <- "Subunit within a park as potential grouping variable."
# meta$description[meta$column_name == "state_name"] <- "State park occurs in."
# meta$description[meta$column_name == "statecd"] <- "State FIPS code"
# meta$description[meta$column_name == "countycd"] <- "County FIPS code"
# meta$description[meta$column_name == "spcd"] <- "Species code used by FIA taken from REF_SPECIES. When a species code doesn't exist, code is NA."
# meta$description[meta$column_name == "jenkins_spgrpcd"] <- "Jenkins species group used by FIA and taken from REF_SPECIES"
# meta$description[meta$column_name == "scientific_name"] <- "Latin name"
# meta$description[meta$column_name == "usda_symbol"] <- "USDA Plants Symbol"
# meta$description[meta$column_name == "statuscd"] <- "Tree status: dead or live"
#
# #meta$description[meta$column_name == "statusclassifier"] <-
# #  "Indicates whether stem is standing, leaning (30-45 degrees) or fallen (>45degrees)"
# #meta$NETN[meta$column_name == "statusclassifier"] <-
# #  "Only recorded for trees, not saplings. Classifier wasn't recorded until 2010 and later."
#
# meta$description[meta$column_name == "treeclcd"] <- "Crown class of tree. 1 = open grown; 2 = dominant; 3 = codominant; 4 = intermediate; 5 = subcanopy; 6 = gap exploiter"
# meta$NETN[meta$column_name == "treecld"] <- "Gap exploiter is either an intermediate or subcanopy tree expected to grow faster than normal because it is within a canopy gap."
#
# meta$description[meta$column_name == "dbhcm"] <- "DBH of tree or sapling."
# meta$units[meta$column_name == "dbhcm"] <- "cm"
#
# meta$description[meta$column_name == "ht"] <- "Height of tree"
# meta$units[meta$column_name == "ht"] <- "meters"
#
# meta$description[meta$column_name == "htcd"] <- "Indicates how tree height was assigned. 1 = the individual tree was measured for tree height. 4 = the height was derived."
#
# meta$description[meta$column_name == "cull"] <- "Percent of wood considered cull from 0 to 1 based on recorded tree conditions."
#
# meta$description[meta$column_name == "habit"] <- "Denotes if stem is a tree or sapling. Trees are defined as 1) any live stem >=10cm DBH regardless of whether it is standing, leaning or fallen, or 2) any dead stem >= 10cm DBH that is standing or leaning. Saplings are stems >= 1cm DBH and <10cm DBH and are only measured if live."
# meta$NETN[meta$column_name == "habit"] <- "Individual saplings are not tracked over time, and sometimes are missed across years because crews because QA/QC isn't as strict as with trees."
#
# meta$description[meta$column_name == "decaycd"] <- "FIA decay class for snags ranging from 1-5." #NCRN is NA
# write.csv(meta, "./data/metadata_NETN.csv", row.names = F)
#
# #++++ Questions for group ++++
# # 1.  Photinia villosa doesn't have a SPCD. The closest match is Photinia x fraseri or Photinia davidiana. Do I use
# #     one of those codes, or leave it blank? This is mainly for saplings.
# # 2. I set sapling crown class code as subcanopy. Is that okay? I did not add a height. Should?




