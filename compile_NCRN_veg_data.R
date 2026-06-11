#--------------------------------------------------
# Compiling predictor variables for carbon analysis
#--------------------------------------------------

library(tidyverse)
library(NPSForVeg)
NCRN<-importNCRN("./data/NCRN/")

PhysioData<-read_csv("./data/regression_data_2016-2019.csv") |>
  filter(Network=="NCRN") |>
  select(Plot_Name, physio_class=Physiographic_Class) |>
  mutate(physio_class=replace_values(physio_class,
                                     "dry"~"Xeric",
                                     "dry-mesic"~"Dry-Mesic",
                                     "hydric"~"Hydric",
                                     "mesic"~"Mesic"))

# Events table for left join of data

Plots<-getPlots(NCRN) |>
  select(Plot_Name, ParkUnit=Unit_Code, ParkSubUnit=Subunit_Code, Lat=Latitude, Long=Longitude, Aspect, PlotSlope=Slope) |>
  mutate(Network='NCRN', ParkSubUnit=replace_values(ParkSubUnit,"na"~NA)) |> left_join(PhysioData)

plotev<-getEvents(NCRN) |>
  select(Plot_Name,SampleYear=Event_Year, SampleDate=Event_Date, Deer_Browse_Index=Deer_Impact,  ) |>
  left_join(Plots)

# Summarize guild percent cover and frequency
# exotic species
herbs<-getPlants(NCRN, "herbs") |> select (Plot_Name, SampleYear=Sample_Year, Latin_Name, Percent_Cover, Exotic) |>
  filter(Exotic==1) |>
  summarize(cov=sum(Percent_Cover/12), freq=n()/12, .by=c(Plot_Name, SampleYear, Latin_Name))

Commons <- getCommons(NCRN) |> select(Latin_Name,Herbaceous, Tree, Vine, Shrub)
Commons <- Commons |> mutate(Herbaceous=ifelse(Vine==1 ,1, Herbaceous), #all vines count as herbs
                             Herbaceous=ifelse(Shrub==1,0, Herbaceous), # for Lonicera spp
                             Shrub=ifelse(Herbaceous+Tree+Vine+Shrub==0,1,Shrub),# for rubus spp
                             Graminoid=ifelse(Latin_Name %in% c("Microstegium vimineum","Oplismenus undulatifolius" ), 1,0),
                             Herbaceous=ifelse(Graminoid==1,0, Herbaceous))
Commons<-semi_join(Commons, herbs) |> select(-Vine)


Guild_Assign<- Commons |>
  pivot_longer(cols=c(Herbaceous,Tree, Shrub, Graminoid)) |>
  filter(value==1) |>
  mutate(name=paste0(name,"_exo")) |> select(Latin_Name, Group=name)


guilds_exo<-left_join(herbs, Guild_Assign) |>
  summarise(quad_pct_cover=sum(cov), quad_pct_freq=sum(freq), .by=c(Plot_Name, SampleYear, Group))


guilds <- guilds_exo |>
  pivot_wider(names_from = Group, values_from = c(quad_pct_cover, quad_pct_freq),
              names_glue = "{Group}_{.value}",
              values_fill = list(quad_pct_cover=0, quad_pct_freq=0))


# compile pest detections
# trees
tree_pests <- read.csv("C:/Users/jschmit/OneDrive - DOI/Data/NCRN_Veg_2025/Tree_Sapling_Conditions.csv") |>
  select(ParkUnit=Unit_Code, Plot_Name, SampleYear=Sample_Year, TagCode=Tag, Condition, Pest) |>
  filter((Pest==1 | Condition%in% c("Gypsy moth", "Vines in the crown")), !Condition %in% c("Beech bark disease", "Other significant insect damage", "Tent caterpillars", "Dogwood anthracnose"))

tree_pests<-tree_pests |> mutate(Condition=Condition |> replace_values(
  "Beech leaf disease"~"BLD",
  "Emerald ash borer"~"EAB",
  "Gypsy moth"~"GM",
  "Hemlock Scale"~"EHS",
  "Hemlock wooly adelgid"~"HWA",
  "Spotted Lanternfly"~"SLF",
  "Vines in the crown" ~"VIN_C"

))

tree_pests<-  tree_pests |>
  summarise(BLD_trcnt = any(Condition=="BLD"),
            EAB_trcnt = any(Condition=="EAB"),
            EHS_trcnt = any(Condition=="EHS"),
            GM_trcnt = any(Condition=="GM"),
            HWA_trcnt = any(Condition=="HWA"),
            SLF_trcnt = any(Condition=="SLF"),
            VINC_trcnt = sum(Condition=="VIN_C"),
            .by=c(Plot_Name,SampleYear ))



tree_pests<-tree_pests |> mutate(across(BLD_trcnt:SLF_trcnt, ~ as.numeric(.x)))

Final_Data<-left_join(plotev,guilds ) |> left_join(tree_pests)

## Make the column names match other data sets.
Final_Data<-Final_Data |>
  mutate(across(Herbaceous_exo_quad_pct_cover:VINC_trcnt, ~ replace_na(.x,0) )) |>
  arrange(Plot_Name, SampleYear) |>
  select(plt_cn=Plot_Name, year=SampleYear, lat=Lat, long=Long, network=Network,
         park=ParkUnit, parksubunit=ParkSubUnit, physio_class,
         Deer_Browse_Index, PlotSlope, Aspect,
         Shrub_exo_cov=Shrub_exo_quad_pct_cover,
         Herbaceous_exo_cover=Herbaceous_exo_quad_pct_cover,
         Graminoid_exo_cover=Graminoid_exo_quad_pct_cover,
         Shrub_exo_freq=Shrub_exo_quad_pct_freq,
         Herbaceous_exo_freq=Herbaceous_exo_quad_pct_freq,
         Graminoid_exo_freq=Graminoid_exo_quad_pct_freq,
         VINE_trcnt=VINC_trcnt,
         BLD=BLD_trcnt,
         EAB=EAB_trcnt,
         EHS=EHS_trcnt,
         GM=GM_trcnt,
         HWA=HWA_trcnt,
         SLF=SLF_trcnt
)

### needs replace_NA
### add in mesic, hyrdic etc.


write.csv(Final_Data, "./data/NCRN_veg_and_pest_data.csv", row.names = F)

