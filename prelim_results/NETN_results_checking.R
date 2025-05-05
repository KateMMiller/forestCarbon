#--------------------------------
# checking prelim results
#--------------------------------
library(forestNETN)
library(tidyverse)
library(plotly)

importCSV("../data/", zip_name = "records-2306029.zip")

tree_dat <- joinTreeData(status = 'active')
table(tree_dat$TreeStatusCode)
live = c("1", "AB", "AF", "AL", "AM", "AS", "RB", "RF", 'RL', "RS")
dead = c("2", "DB", "DL", "DM", "DS")

head(tree_dat)

sort(unique(tree_dat$ScientificName))
exo <- c("Aesculus", "Ailanthus", "Catalpa", "Cladrastis", "Crataegus", "Malus", "Pyrus", "Rhamnus")
oth_nat <- c("Alnus", "Amelanchier", "Cornus", "Juglans", "Juniperus", "Larix",
             "Nyssa", "Ostrya", "Platanus", "Prunus", "Salix", "Tilia",
             "Unknown")

tree_sum <- tree_dat |>
  mutate(genus = word(ScientificName, 1),
         status = case_when(TreeStatusCode %in% live ~ "L",
                            TreeStatusCode %in% dead ~ 'D',
                            TRUE ~ 'excl'),
         sppgrp = case_when(genus %in% exo ~ "oth_exotic",
                            genus %in% oth_nat ~ "oth_native",
                            TRUE ~ genus)) |>
  filter(status != "excl") |>
  group_by(ParkUnit, Plot_Name, cycle, sppgrp, status) |>
  summarize(stems_ha = sum(num_stems) *10000/ifelse(first(ParkUnit) == "ACAD", 225, 400),
            ba_cm2ha = round(sum(BA_cm2) / ifelse(first(ParkUnit) == "ACAD", 225, 400), 2),
            .groups = 'drop') |>
  arrange(sppgrp, Plot_Name, cycle, desc(status))

tree_ba_wide <- tree_sum |> select(ParkUnit, Plot_Name, cycle, sppgrp, status, ba_cm2ha) |>
  pivot_wider(names_from = c(sppgrp, status), values_from = ba_cm2ha) |>
  arrange(Plot_Name, cycle)

head(tree_sum)
head(tree_ba_wide)

tree_ba_sum <- tree_sum |> data.frame() |>
  dplyr::group_by(ParkUnit, Plot_Name, cycle, status) |>
  dplyr::summarize(ba_total = sum(ba_cm2ha), .groups = 'drop')

tree_ba_sum$plot = as.numeric(substr(tree_ba_sum$Plot_Name, 6, 8))


write.csv(tree_ba_wide, "./prelim_results/NETN_BA_changes.csv", row.names = F)

netnC <- read.csv("./prelim_results/NETN_test.csv")

table(netnC$STATUSCD, useNA = 'always')
head(netnC)
#++++ Only live trees currently included in carbon estimates +++++
netn_plotC <- netnC |>
  mutate(park = substr(UNITCD, 1, 4)) |>
  group_by(park, PLT_CN, YEAR, STATUSCD, CYCLE) |>
  summarize(totC = sum(CARBON, na.rm = T),
            totAGB = sum(AGB, na.rm = T),
            num_stems = sum(!is.na(CARBON)),
            .groups = 'drop')
head(netnC)
table(netn_plotC$park)

netn_plotC_sub <- netnC |>
  mutate(park = substr(UNITCD, 1, 4)) |>
  group_by(park, UNITCD, PLT_CN, YEAR, STATUSCD, CYCLE) |>
  summarize(totC = sum(CARBON, na.rm = T),
            totAGB = sum(AGB, na.rm = T),
            num_stems = sum(!is.na(CARBON)),
            .groups = 'drop')

netn_plotC_sub$plot <- as.numeric(gsub("\\D", "", netn_plotC_sub$PLT_CN))

# ggplot(netn_plot, aes(x = park, y = totAGB, fill = park)) +
#   geom_violin() + forestNETN::theme_FHM()
table(netn_plotC$YEAR)

ggplot(netn_plotC, aes(x = park, y = totC, fill = park, group = park)) +
  geom_violin() + theme_classic()

ggplot(netn_plotC, aes(x = CYCLE, y = totC, fill = park, group = CYCLE)) +
  geom_violin() + theme_classic() + facet_wrap(~park)

ggplot(netn_plotC |> filter(YEAR > 2020), aes(x = park, y = totC, fill = park, group = park)) +
  geom_violin() + theme_classic()

plot_check <- function(parkcode){
 p <-
   ggplot(netn_plotC_sub |> filter(park == parkcode),
          aes(x = CYCLE, y = totC, group = factor(plot),
              label = plot, color = factor(plot)))+
    geom_point() +
    geom_line(linewidth = 1) +
    geom_label() +
    theme_classic() +
    scale_color_viridis_d() +
    labs(x = NULL, y = "Total live C", title = parkcode)

ba_df <- tree_ba_sum |> filter(ParkUnit == parkcode) |> filter(status == "L")

 q <-
   ggplot(ba_df,
             aes(x = cycle, y = ba_total, group = factor(plot),
                 color = factor(plot), label = plot)) +
   geom_point() +
   geom_line(linewidth = 1) +
   geom_label() +
   theme_classic() +
   scale_color_viridis_d() +
   labs(x = NULL, y = "Total live BA (cm2/ha)", title = parkcode)

 comb <- plotly::subplot(p, q, titleY = T)
 return(comb)
}

plot_check("ACAD") + facet_wrap(~UNITCD) + theme(legend.position = 'none')
plot_check("MABI") |> ggplotly()
plot_check("MIMA") |> ggplotly()
plot_check("MORR") |> ggplotly()
plot_check("ROVA") + facet_wrap(~UNITCD)
plot_check("ROVA") |> ggplotly()
plot_check("SAGA") |> ggplotly()
plot_check("SARA") |> ggplotly()
plot_check("WEFA") |> ggplotly()
