#----- Tricky heights to fix manually -----
# ASIS-407-2024 had all open grown or subcanopy trees. Based on crew leader's suggestion, using height from a similar plot.
treehts$ht[treehts$Plot_Name == "ASIS-407" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 1] <- 9.3
treehts$ht[treehts$Plot_Name == "ASIS-407" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 2] <- 9.3 * 1.1
treehts$ht[treehts$Plot_Name == "ASIS-407" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 4] <- 9.3 * 0.8
treehts$ht[treehts$Plot_Name == "ASIS-407" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 5] <- 9.3 * 0.5
treehts$ht[treehts$Plot_Name == "ASIS-407" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 6] <- 9.3 * 0.5

# HOFU-247 is a barberry thicket with few trees, all subcanopy. Using average tree heights from nearest 2 plots with
# similar forest type
treehts$ht[treehts$Plot_Name == "HOFU-247" & treehts$CrownClassCode == 1] <- 30.9
treehts$ht[treehts$Plot_Name == "HOFU-247" & treehts$CrownClassCode == 2] <- 30.9 * 1.1
treehts$ht[treehts$Plot_Name == "HOFU-247" & treehts$CrownClassCode == 3] <- 30.9
treehts$ht[treehts$Plot_Name == "HOFU-247" & treehts$CrownClassCode == 4] <- 30.9 * 0.8
treehts$ht[treehts$Plot_Name == "HOFU-247" & treehts$CrownClassCode == 5] <- 30.9 * 0.5
treehts$ht[treehts$Plot_Name == "HOFU-247" & treehts$CrownClassCode == 6] <- 30.9 * 0.5

# PETE-172-2009 is missing tree heights. Using averages from 2013 visit
treehts$ht[treehts$Plot_Name == "PETE-172" & treehts$SampleYear == 2009 & treehts$CrownClassCode == 1] <- 17.93
treehts$ht[treehts$Plot_Name == "PETE-172" & treehts$SampleYear == 2009 & treehts$CrownClassCode == 2] <- 17.93 * 1.1
treehts$ht[treehts$Plot_Name == "PETE-172" & treehts$SampleYear == 2009 & treehts$CrownClassCode == 3] <- 17.93
treehts$ht[treehts$Plot_Name == "PETE-172" & treehts$SampleYear == 2009 & treehts$CrownClassCode == 4] <- 14.47
treehts$ht[treehts$Plot_Name == "PETE-172" & treehts$SampleYear == 2009 & treehts$CrownClassCode == 5] <- 17.93 * 0.5
treehts$ht[treehts$Plot_Name == "PETE-172" & treehts$SampleYear == 2009 & treehts$CrownClassCode == 6] <- 17.93 * 0.5

# THST-293 2018 and 2023 don't have codom tree heights- only intermediate. Using intermediate*1.2 as codom for
# Subcanopy calculation.
treehts$ht[treehts$Plot_Name == "THST-293" & treehts$SampleYear == 2018 &
             treehts$CrownClassCode == 5] <- (24.4*1.2)*0.5
treehts$ht[treehts$Plot_Name == "THST-293" & treehts$SampleYear == 2022 &
             treehts$CrownClassCode == 5] <- (18.9*1.2)*0.5
