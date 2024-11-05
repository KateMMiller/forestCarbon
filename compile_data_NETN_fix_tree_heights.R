#----- Tricky heights to fix manually -----
# ACAD-001 2006 has no tree heights. Using 2010 measurements
treehts$ht[treehts$Plot_Name == "ACAD-001" & treehts$SampleYear == 2006 & treehts$CrownClassCode == 2] <- 15.46667 * 1.1
treehts$ht[treehts$Plot_Name == "ACAD-001" & treehts$SampleYear == 2006 & treehts$CrownClassCode == 3] <- 15.46667
treehts$ht[treehts$Plot_Name == "ACAD-001" & treehts$SampleYear == 2006 & treehts$CrownClassCode == 4] <- 15.46667 * 0.8

# ACAD-095 2012 trees all classified as dominant with no tree heights. Better to class as codom. Using 2016 measurements for same trees
treehts$ht[treehts$tree_id == "ACAD-095-001" & treehts$SampleYear == 2012] <- 8.0
treehts$ht[treehts$tree_id == "ACAD-095-002" & treehts$SampleYear == 2012] <- 8.2

# MABI-014 2008 has no stand heights. Using 2012 measurements
treehts$ht[treehts$Plot_Name == "MABI-014" & treehts$SampleYear == 2008 & treehts$CrownClassCode == 2] <- 36.13333 * 1.1
treehts$ht[treehts$Plot_Name == "MABI-014" & treehts$SampleYear == 2008 & treehts$CrownClassCode == 3] <- 36.13333
treehts$ht[treehts$Plot_Name == "MABI-014" & treehts$SampleYear == 2008 & treehts$CrownClassCode == 4] <- 36.13333 * 0.8
treehts$ht[treehts$Plot_Name == "MABI-014" & treehts$SampleYear == 2008 & treehts$CrownClassCode == 5] <- 36.13333 * 0.5

# MIMA-020 is early successional with no heights for most years except 2008. Using 2008 values for remaining visits
treehts$ht[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 1] <- 18
treehts$ht[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 2] <- 18 * 1.1
treehts$ht[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 3] <- 18
treehts$ht[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 4] <- 18 * 0.8
treehts$ht[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 5] <- 18 * 0.5
treehts$ht[treehts$Plot_Name == "MIMA-020" & treehts$SampleYear %in% c(2012, 2016, 2022) & treehts$CrownClassCode == 6] <- 18 * 0.5

# MORR-002 2024 has no codominant trees with heights. Using 2019 values.
treehts$ht[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 1] <- 25.9
treehts$ht[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 2] <- 25.9 * 1.1
treehts$ht[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 3] <- 25.9
treehts$ht[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 5] <- 25.9 * 0.5
treehts$ht[treehts$Plot_Name == "MORR-002" & treehts$SampleYear == 2024 & treehts$CrownClassCode == 6] <- 25.9 * 0.5

# MORR-006 2007 has no tree heights. Using 2011 measurements
treehts$ht[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 1] <- 28.8333
treehts$ht[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 2] <- 28.8333 * 1.1
treehts$ht[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 3] <- 28.8333
treehts$ht[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 4] <- 28.8333 * 0.8
treehts$ht[treehts$Plot_Name == "MORR-006" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 5] <- 28.8333 * 0.5

# MORR-012 2007 has no tree heights. Using 2011 measurements
treehts$ht[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 1] <- 32.0333
treehts$ht[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 2] <- 32.0333 * 1.1
treehts$ht[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 3] <- 32.0333
treehts$ht[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 4] <- 32.0333 * 0.8
treehts$ht[treehts$Plot_Name == "MORR-012" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 5] <- 32.0333 * 0.5

# MORR-021 doesn't have codominant trees in 2013, 2017, and 2022. Using 2009 values
treehts$ht[treehts$Plot_Name == "MORR-021" & treehts$SampleYear %in% c(2013, 2017, 2022) & treehts$CrownClassCode == 1] <- 27.5
treehts$ht[treehts$Plot_Name == "MORR-021" & treehts$SampleYear %in% c(2013, 2017, 2022) & treehts$CrownClassCode == 2] <- 27.5 * 1.1
treehts$ht[treehts$Plot_Name == "MORR-021" & treehts$SampleYear %in% c(2013, 2017, 2022) & treehts$CrownClassCode == 3] <- 27.5
treehts$ht[treehts$Plot_Name == "MORR-021" & treehts$SampleYear %in% c(2013, 2017, 2022) & treehts$CrownClassCode == 5] <- 27.5 * 0.5

# ROVA-001 2007 has no tree heights. Using 2011 measurements
treehts$ht[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 1] <- 24.6667
treehts$ht[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 2] <- 24.6667 * 1.1
treehts$ht[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 3] <- 24.6667
treehts$ht[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 4] <- 24.6667 * 0.8
treehts$ht[treehts$Plot_Name == "ROVA-001" & treehts$SampleYear == 2007 & treehts$CrownClassCode == 5] <- 24.6667 * 0.5

# SARA-002 is early successional with no heights for most years except 2006. Using 2006 values for remaining visits
treehts$ht[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 1] <- 7.8
treehts$ht[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 2] <- 7.8 * 1.1
treehts$ht[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 3] <- 7.8
treehts$ht[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 4] <- 7.8 * 0.8
treehts$ht[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 5] <- 7.8 * 0.5
treehts$ht[treehts$Plot_Name == "SARA-004" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 6] <- 7.8 * 0.5

# SARA-009 is early successional with no heights for most years except 2006. Using 2006 values for remaining visits
treehts$ht[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 1] <- 12.7
treehts$ht[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 2] <- 12.7 * 1.1
treehts$ht[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 3] <- 12.7
treehts$ht[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 4] <- 12.7 * 0.8
treehts$ht[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 5] <- 12.7 * 0.5
treehts$ht[treehts$Plot_Name == "SARA-009" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 6] <- 12.7 * 0.5

# SARA-012 is early successional with no heights for most years except 2006. Using 2006 values for remaining visits
treehts$ht[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 1] <- 17.4
treehts$ht[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 2] <- 17.4 * 1.1
treehts$ht[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 3] <- 17.4
treehts$ht[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 4] <- 17.4 * 0.8
treehts$ht[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 5] <- 17.4 * 0.5
treehts$ht[treehts$Plot_Name == "SARA-012" & treehts$SampleYear %in% c(2010, 2014, 2018, 2023) &
             treehts$CrownClassCode == 6] <- 17.4 * 0.5

# SARA-024 is early successional with no heights for most years except 2008. Using 2008 values for remaining visits
treehts$ht[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
             treehts$CrownClassCode == 1] <- 25
treehts$ht[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
             treehts$CrownClassCode == 2] <- 25 * 1.1
treehts$ht[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
             treehts$CrownClassCode == 3] <- 25
treehts$ht[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
             treehts$CrownClassCode == 4] <- 25 * 0.8
treehts$ht[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
             treehts$CrownClassCode == 5] <- 25 * 0.5
treehts$ht[treehts$Plot_Name == "SARA-024" & treehts$SampleYear %in% c(2012, 2016, 2022) &
             treehts$CrownClassCode == 6] <- 25 * 0.5

# SARA-029 has no codominant trees in 2012. Using 2016 values.
treehts$ht[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 1] <- 18.5
treehts$ht[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 2] <- 18.5 * 1.1
treehts$ht[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 3] <- 18.5
treehts$ht[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 4] <- 13.7
treehts$ht[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 5] <- 18.5 * 0.5
treehts$ht[treehts$Plot_Name == "SARA-029" & treehts$SampleYear == 2012 & treehts$CrownClassCode == 6] <- 18.5 * 0.5

# SARA-032 is early successional with no heights for most years except 2006. Using 2006 values for remaining visits
treehts$ht[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
             treehts$CrownClassCode == 1] <- 10.2
treehts$ht[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
             treehts$CrownClassCode == 2] <- 10.2 * 1.1
treehts$ht[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
             treehts$CrownClassCode == 3] <- 10.2
treehts$ht[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
             treehts$CrownClassCode == 4] <- 10.2 * 0.8
treehts$ht[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
             treehts$CrownClassCode == 5] <- 10.2 * 0.5
treehts$ht[treehts$Plot_Name == "SARA-032" & treehts$SampleYear %in% c(2008, 2012, 2016, 2022) &
             treehts$CrownClassCode == 6] <- 10.2 * 0.5
