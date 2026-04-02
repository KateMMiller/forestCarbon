#--------------------------------------------------
# Compiling predictor variables for carbon analysis
#--------------------------------------------------

library(tidyverse)
library(forestNETN)
importCSV(path = "../data/", zip_name = "NETN_Forest_20250926.zip")

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
                                             BBD, BLD, EAB, EHS, GM, HWA, RPS, SPB,
                                             VIN_B, VIN_C) |>

  summarize(BBD_trcnt = sum(BBD, na.rm = T),
            BLD_trcnt = sum(BLD, na.rm = T),
            EAB_trcnt = sum(EAB, na.rm = T),
            EHS_trcnt = sum(EHS, na.rm = T),
            GM_trcnt = sum(GM, na.rm = T),
            HWA_trcnt = sum(HWA, na.rm = T),
            RPS_trcnt = sum(RPS, na.rm = T),
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
head(disturb)
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
                          TRUE ~ NA_character_
  ))  |>  filter(!is.na(pest))  |>
  select(Plot_Name, ParkUnit, SampleYear, pest) |> mutate(present = 1) |> unique()

disturb_wide <- disturb |>
  arrange(pest, Plot_Name, SampleYear) |>
  pivot_wider(names_from = pest, values_from = present, values_fill = 0) |>
  mutate(SPB = 0)
names(tree_pests)

# visit notes
vnotes <- joinVisitNotes() |>
  mutate(pest = case_when(grepl("BBD|beech bark disease", Notes, ignore.case = T) ~ "BBD",
                          grepl("beech leaf disease|BLD", Notes, ignore.case = T) ~ "BLD",
                          grepl("EHS|elongate hemlock scale",
                          Notes, ignore.case = T) ~ "EHS",
                          grepl("emerald|EAB", Notes, ignore.case = T) ~ "EAB",
                          grepl("GM|spongy|gypsy", Notes) ~ "GM",
                          grepl("HWA|hemlock woolly adelgid| hemlock wooly",
                                Notes, ignore.case = T) ~ "HWA",
                          grepl("red pine scale|RPS", Notes, ignore.case = T) ~ "RPS",
                          TRUE ~ NA_character_)) |>
  filter(!is.na(pest)) |>
  filter(!grepl("no sign|didn't see obvious|No definitive|sureabout|no direct evidence|
                did not see|no distinguisheable|suspicious of EAB|does not look like|
                couldn't find clear evidence of EAB|but do not indicate|did not see sign|
                didn't see sign|Suspicious of|freshwater|didn't see signs.|not sighted|bushwack|
                Foliage too high|No obvious signs of|Canopy too high",
                Notes, ignore.case = T)) |>
  select(Plot_Name, ParkUnit, SampleYear, pest) |> mutate(present = 1) |> unique()

vnotes_wide <- vnotes |> pivot_wider(names_from = pest, values_from = present,
                                     values_fill = 0, names_glue = "{pest}_note") |>
  summarize(BBD_note = ifelse(sum(BBD_note) > 1, 1, 0),
            BLD_note = ifelse(sum(BLD_note) > 1, 1, 0),
            EAB_note = ifelse(sum(EAB_note) > 1, 1, 0),
            EHS_note = ifelse(sum(EHS_note) > 1, 1, 0),
            GM_note = ifelse(sum(GM_note) > 1, 1, 0),
            HWA_note = ifelse(sum(HWA_note) > 1, 1, 0),
            RPS_note = ifelse(sum(RPS_note) > 1, 1, 0),
            .by = c(Plot_Name, ParkUnit, SampleYear))

head(vnotes_wide)

pest_comb1 <- left_join(tree_pests, disturb_wide, by = c("Plot_Name", "ParkUnit", "SampleYear"))
pest_comb2 <- left_join(pest_comb1, vnotes_wide, by = c("Plot_Name", "ParkUnit", "SampleYear"))

head(pest_comb2)

pest_cols <- names(pest_comb2[,6:ncol(pest_comb2)])
pest_comb2[,pest_cols][is.na(pest_comb2[,pest_cols])] <- 0

# Create pest_plot column for any instance a pest was detected
pest_comb3 <- pest_comb2 |> mutate(BBD_plot = ifelse(BBD_trcnt > 0 | BBD_plot > 0 | BBD_note > 0, 1, 0),
                                   BLD_plot = ifelse(BLD_trcnt > 0 | BLD_plot > 0 | BLD_note > 0, 1, 0),
                                   EAB_plot = ifelse(EAB_trcnt > 0 | EAB_plot > 0 | EAB_note > 0, 1, 0),
                                   EHS_plot = ifelse(EHS_trcnt > 0 | EHS_plot > 0 | EHS_note > 0, 1, 0),
                                   GM_plot = ifelse(GM_trcnt > 0 | GM_plot > 0 | GM_note > 0, 1, 0),
                                   HWA_plot = ifelse(HWA_trcnt > 0 | HWA_plot > 0 | HWA_note > 0, 1, 0),
                                   RPS_plot = ifelse((RPS_trcnt > 0 | RPS_plot > 0 | RPS_note > 0) &
                                                       SampleYear > 2012, 1, 0),
                                   SPB_plot = 0) |>
  select(Plot_Name:SampleYear, VINE_trcnt = VINC_trcnt, live_trcnt, BBD_trcnt:SPB_trcnt, BBD_plot:SPB_plot)

head(pest_comb3)

table(pest_comb3$ParkUnit, pest_comb3$SampleYear, pest_comb3$RPS_plot)



# Check for missing records between the plot info and data
anti_join(plotev, guilds, by = c("Plot_Name", "SampleYear", "PlotID", "EventID")) # ACAD-060-2019
anti_join(plotev, stand, by = c("Plot_Name", "SampleYear", "PlotID", "EventID")) # none
anti_join(plotev, pest_comb3) # none

# join plot and stand and guild data
intersect(names(plotev), names(stand))
netn_comb1 <- left_join(plotev, stand, by = c("Plot_Name", "SampleYear", "PlotID", "EventID"))
netn_comb2 <- left_join(netn_comb1, guilds, by = c("Plot_Name", "SampleYear", "PlotID", "EventID")) |>
  arrange(Plot_Name, SampleYear)
netn_comb3 <- left_join(netn_comb2, pest_comb3,
                        by = c("Plot_Name", "ParkUnit", "PlotID", "EventID", "SampleYear"))

head(netn_comb3)
# set up group columns to convert NA to 0
cols <- names(netn_comb3)[18:ncol(netn_comb3)]
netn_comb3[,cols][is.na(netn_comb3[,cols])] <- 0

#++++ ENDED HERE +++++
# Need to rename the columns more like FIA
netn_final <- netn_comb |>
  select(plt_cn = Plot_Name, year = SampleYear, lat = Lat, long = Long,
         network = Network, park = ParkUnit, parksubunit = ParkSubUnit,
         )
