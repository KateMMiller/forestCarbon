#----- Fix missing tree status and crown class codes -----
# For live trees missing a crown class or status qualifier, using the class from the next consecutive measurement
trees3$CrownClassCode[trees3$tree_id == "APCO-010-363" & trees3$SampleYear == 2011] <- 2
trees3$CrownClassCode[trees3$tree_id == "APCO-011-452" & trees3$SampleYear %in% c(2007, 2011)] <- 5
trees3$CrownClassCode[trees3$tree_id == "APCO-184-6387" & trees3$SampleYear == 2009] <- 3

trees3$CrownClassCode[trees3$tree_id == "BOWA-095-3062" & trees3$SampleYear == 2008] <- 5

trees3$CrownClassCode[trees3$tree_id == "FRSP-002-061" & trees3$SampleYear == 2007] <- 3
trees3$CrownClassCode[trees3$tree_id == "FRSP-002-066" & trees3$SampleYear == 2007] <- 3
trees3$CrownClassCode[trees3$tree_id == "FRSP-017-693" & trees3$SampleYear == 2015] <- 5
trees3$CrownClassCode[trees3$tree_id == "FRSP-075-2782" & trees3$SampleYear == 2011] <- 5
trees3$CrownClassCode[trees3$tree_id == "FRSP-075-2783" & trees3$SampleYear == 2011] <- 4
trees3$CrownClassCode[trees3$tree_id == "FRSP-149-5263" & trees3$SampleYear == 2016] <- 5
trees3$CrownClassCode[trees3$tree_id == "FRSP-208-11192" & trees3$SampleYear == 2009] <- 3
trees3$CrownClassCode[trees3$tree_id == "FRSP-274-9431" & trees3$SampleYear == 2010] <- 5

trees3$CrownClassCode[trees3$tree_id == "GETT-090-3104" & trees3$SampleYear == 2008] <- 5
trees3$CrownClassCode[trees3$tree_id == "GETT-090-3119" & trees3$SampleYear == 2008] <- 5
trees3$CrownClassCode[trees3$tree_id == "GETT-174-6642" & trees3$SampleYear == 2009] <- 5

trees3$CrownClassCode[trees3$tree_id == "PETE-026-987" & trees3$SampleYear == 2007] <- 4
trees3$CrownClassCode[trees3$tree_id == "PETE-065-2218" & trees3$SampleYear == 2015] <- 4
trees3$CrownClassCode[trees3$tree_id == "PETE-109-3240" & trees3$SampleYear == 2008] <- 5
trees3$CrownClassCode[trees3$tree_id == "PETE-109-3241" & trees3$SampleYear == 2008] <- 3

trees3$CrownClassCode[trees3$tree_id == "RICH-015-8416" & trees3$SampleYear == 2019] <- 3
trees3$CrownClassCode[trees3$tree_id == "RICH-072-2635" & trees3$SampleYear == 2011] <- 5
trees3$CrownClassCode[trees3$tree_id == "RICH-073-14003" & trees3$SampleYear == 2015] <- 5
trees3$CrownClassCode[trees3$tree_id == "RICH-148-13407" & trees3$SampleYear == 2008] <- 3
trees3$CrownClassCode[trees3$tree_id == "RICH-203-8642" & trees3$SampleYear == 2009] <- 4
trees3$CrownClassCode[trees3$tree_id == "RICH-238-8901" & trees3$SampleYear == 2009] <- 3

trees3$CrownClassCode[trees3$tree_id == "THST-293-10229" & trees3$SampleYear == 2011] <- 5

trees3$CrownClassCode[trees3$tree_id == "VAFO-076-17821" & trees3$SampleYear == 2008] <- 5









