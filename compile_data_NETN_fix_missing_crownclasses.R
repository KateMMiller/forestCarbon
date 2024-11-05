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
trees3$CrownClassCode[trees3$tree_id == "ACAD-120-011" & trees3$SampleYear == 2021] <- 4 #zombie tree
trees3$DBHcm[trees3$tree_id == "ACAD-120-011" & trees3$SampleYear == 2021] <- (19.6 + 20.8)/2 #zombie

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
