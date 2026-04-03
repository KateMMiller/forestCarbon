#--------------------------------------------------
# Compiling predictor variables for carbon analysis
#--------------------------------------------------

library(tidyverse)
library(forestMIDN)
importCSV(path = "../data/", zip_name = "MIDN_NCBN_Forest_20250926.zip")

# Plot events table for left join of data
plotev <- joinLocEvent(output = 'verbose') |>
  select(Plot_Name, Network, ParkUnit, ParkSubUnit,
         PlotID, EventID, Lat, Long, Aspect,
         PhysiographySummary, PhysiographyLabel,
         SampleYear, SampleDate)

# Summarize guild percent cover and frequency
# exotic species
guilds_exo <- sumQuadGuilds(speciesType = "exotic") |>
  mutate(Group = paste0(Group, "_exo")) |>
  rename(cov = quad_pct_cover, freq = quad_pct_freq)

# native species
guilds_nat <- sumQuadGuilds(speciesType = "native") |>
  mutate(Group = paste0(Group, "_nat")) |>
  rename(cov = quad_pct_cover, freq = quad_pct_freq)

# reshape to wide
guilds <- rbind(guilds_exo, guilds_nat) |>
  pivot_wider(names_from = Group, values_from = c(cov, freq),
              names_glue = "{Group}_{.value}") |>
  select(Plot_Name, PlotID, EventID, SampleYear, Tree_exo_cov:Fern_nat_freq) |>
  data.frame()
head(guilds)

rec_check <- data.frame(table(guilds$Plot_Name))
table(rec_check$Freq) # all 4 or 5, so no dupes, but could have missing

# compile stand data
stand <- joinStandData() |> select(Plot_Name, SampleYear, Pct_Crown_Closure, Txt_Crown_Closure,
                                   Deer_Browse_Index, PlotSlope, PlotID, EventID)

# compile pest detections
# trees
tree_pests <- joinTreeConditions() |> select(Plot_Name, ParkUnit, PlotID, EventID, SampleYear, TagCode,
                                             BBD, BLD, EAB, EHS, GM, HWA, RPS, SLF, SPB,
                                             VIN_B, VIN_C) |>

  summarize(BBD_trcnt = sum(BBD, na.rm = T),
            BLD_trcnt = sum(BLD, na.rm = T),
            EAB_trcnt = sum(EAB, na.rm = T),
            EHS_trcnt = sum(EHS, na.rm = T),
            GM_trcnt = sum(GM, na.rm = T),
            HWA_trcnt = sum(HWA, na.rm = T),
            RPS_trcnt = sum(RPS, na.rm = T),
            SLF_trcnt = sum(SLF, na.rm = T),
            SPB_trcnt = sum(SPB, na.rm = T),
            VINB_trcnt = sum(case_when(VIN_B > 0 ~ 1,
                                  is.na(VIN_B) ~ NA,
                                  TRUE ~ 0)),
            VINC_trcnt = sum(case_when(VIN_C > 0 ~ 1,
                                  is.na(VIN_C) ~ NA,
                                  TRUE ~ 0)),
            live_trcnt = sum(!is.na(TagCode)),
            .by = c(Plot_Name, ParkUnit, PlotID, EventID, SampleYear))

# plot-level disturbance
disturb <- joinStandDisturbance() |>
  filter(DisturbanceSummary != "None")  |>
  mutate(pest = case_when(grepl("BBD|beech bark disease", DisturbanceNote, ignore.case = T) ~ "BBD",
                          grepl("beech leaf disease|BLD", DisturbanceNote, ignore.case = T) ~ "BLD",
                          grepl("EHS|elongate hemlock scale",
                                DisturbanceNote, ignore.case = T) ~ "EHS",
                          grepl("emerald|EAB", DisturbanceNote, ignore.case = T) ~ "EAB",
                          grepl("GM|spongy|gypsy", DisturbanceNote) ~ "GM",
                          grepl("HWA|hemlock woolly adelgid| hemlock wooly",
                                DisturbanceNote, ignore.case = T) ~ "HWA",
                          grepl("red pine scale|RPS", DisturbanceNote, ignore.case = T) ~ "RPS",
                          grepl("spotted lantern|SLF", DisturbanceNote, ignore.case = T) ~ "SLF",
                          grepl("southern pine beetle|SPB", DisturbanceNote, ignore.case = T) ~ "SPB",
                          TRUE ~ NA_character_
  ))  |>  filter(!is.na(pest))  |>
  select(Plot_Name, ParkUnit, SampleYear, pest) |> mutate(present = 1) |> unique()

disturb_wide <- disturb |>
  arrange(pest, Plot_Name, SampleYear) |>
  pivot_wider(names_from = pest, values_from = present, values_fill = 0)

# visit notes
vnotes <- joinVisitNotes() |> filter(!Note_Type %in% "Tree_Notes") |>
  mutate(pest = case_when(grepl("BBD|beech bark disease", Notes, ignore.case = T) ~ "BBD",
                          grepl("beech leaf disease|BLD", Notes, ignore.case = T) ~ "BLD",
                          grepl("EHS|elongate hemlock scale",
                                Notes, ignore.case = T) ~ "EHS",
                          grepl("emerald|EAB", Notes, ignore.case = T) ~ "EAB",
                          grepl("GM|spongy|gypsy", Notes) ~ "GM",
                          grepl("HWA|hemlock woolly adelgid| hemlock wooly",
                                Notes, ignore.case = T) ~ "HWA",
                          grepl("red pine scale|RPS", Notes, ignore.case = T) ~ "RPS",
                          grepl("spotted lantern|SLF", Notes, ignore.case = T) ~ "SLF",
                          grepl("southern pine beetle|SPB", Notes, ignore.case = T) ~ "SPB",
                          TRUE ~ NA_character_)) |>
  filter(!is.na(pest)) |>
  filter(!grepl("no sign|No BBD|not BBD|than it does BBD|didn't see obvious|No definitive|
                BBD code 2 removed|BBD value and foliage conditions|sureabout|no direct evidence|
                Southern Pine Beetle in area near plot|unsure if due to EAB|possible BBD|
                did not see|no distinguisheable|suspicious of EAB|does not look like|but not on plot|
                couldn't find clear evidence of EAB|but do not indicate|did not see sign|
                didn't see sign|Suspicious of|freshwater|didn't see signs.|not sighted|bushwack|
                Foliage too high|No obvious signs of|Canopy too high|Fagus Granifolia|Seabreeze",
                Notes, ignore.case = T)) |>
  select(Plot_Name, ParkUnit, SampleYear, pest) |> mutate(present = 1) |> unique()

vnotes_wide <- vnotes |> arrange(pest) |>
  pivot_wider(names_from = pest, values_from = present,
              values_fill = 0, names_glue = "{pest}_note") |>
  summarize(BBD_note = ifelse(sum(BBD_note) > 0, 1, 0),
            BLD_note = ifelse(sum(BLD_note) > 0, 1, 0),
            EAB_note = ifelse(sum(EAB_note) > 0, 1, 0),
            EHS_note = 0, #ifelse(sum(EHS_note) > 0, 1, 0),
            GM_note = 0, #ifelse(sum(GM_note) > 0, 1, 0),
            HWA_note = 0, #ifelse(sum(HWA_note) > 0, 1, 0),
            RPS_note = 0, #ifelse(sum(RPS_note) > 0, 1, 0),
            SLF_note = ifelse(sum(SLF_note) > 0, 1, 0),
            SPB_note = ifelse(sum(SPB_note) > 0, 1, 0),
            .by = c(Plot_Name, ParkUnit, SampleYear))

head(vnotes_wide)

pest_comb1 <- left_join(tree_pests, disturb_wide, by = c("Plot_Name", "ParkUnit", "SampleYear"))
pest_comb2 <- left_join(pest_comb1, vnotes_wide, by = c("Plot_Name", "ParkUnit", "SampleYear"))

head(pest_comb2)

pest_cols <- names(pest_comb2[,6:ncol(pest_comb2)])
pest_comb2[,pest_cols][is.na(pest_comb2[,pest_cols])] <- 0
head(pest_comb2)
# Create pest_plot column for any instance a pest was detected
pest_comb3 <- pest_comb2 |> mutate(BBD = ifelse(BBD_trcnt > 0 | BBD_note > 0, 1, 0),
                                   BLD = ifelse(BLD_trcnt > 0 | BLD_note > 0, 1, 0),
                                   EAB = ifelse(EAB_trcnt > 0 | EAB > 0 | EAB_note > 0, 1, 0),
                                   EHS = ifelse(EHS_trcnt > 0 | EHS_note > 0, 1, 0),
                                   GM = ifelse(GM_trcnt > 0 | GM_note > 0, 1, 0),
                                   HWA = ifelse(HWA_trcnt > 0 | HWA_note > 0, 1, 0),
                                   RPS = ifelse((RPS_trcnt > 0 | RPS_note > 0) &
                                                 SampleYear > 2012, 1, 0),
                                   SLF = ifelse(SLF_trcnt > 0 | SLF > 0 | SLF_note > 0, 1, 0),
                                   SPB = ifelse(SPB_trcnt > 0 | SPB > 0 | SPB_note > 0, 1, 0)) |>
  select(Plot_Name:SampleYear, VINE_trcnt = VINC_trcnt, BBD, BLD, EAB, EHS, GM, HWA, RPS, SLF, SPB)

head(pest_comb3)

# Check for missing records between the plot info and data
anti_join(plotev, guilds, by = c("Plot_Name", "SampleYear", "PlotID", "EventID")) # ACAD-060-2019
anti_join(plotev, stand, by = c("Plot_Name", "SampleYear", "PlotID", "EventID")) # none
anti_join(plotev, pest_comb3) # none

# join plot and stand and guild data
intersect(names(plotev), names(stand))
midn_comb1 <- left_join(plotev, stand, by = c("Plot_Name", "SampleYear", "PlotID", "EventID"))
midn_comb2 <- left_join(midn_comb1, guilds, by = c("Plot_Name", "SampleYear", "PlotID", "EventID")) |>
  arrange(Plot_Name, SampleYear)
midn_comb3 <- left_join(midn_comb2, pest_comb3,
                        by = c("Plot_Name", "ParkUnit", "PlotID", "EventID", "SampleYear"))
names(midn_comb3)
head(midn_comb3)
# set up group columns to convert NA to 0
cols <- names(midn_comb3)[18:ncol(midn_comb3)]
midn_comb3[,cols][is.na(midn_comb3[,cols])] <- 0

# Need to rename the columns more like FIA
midn_final <- midn_comb3 |>
  select(plt_cn = Plot_Name, year = SampleYear, lat = Lat, long = Long,
         network = Network, park = ParkUnit, parksubunit = ParkSubUnit,
         physio_class = PhysiographySummary, Pct_Crown_Closure:SPB )

head(midn_final)
write.csv(midn_final, "./data/MIDN_veg_and_pest_data.csv", row.names = F)

