# Quality assessment to compare differences in taxonomy among data sources
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2018-08-30

rm(list = ls())

################################################################################
bioscan <- read.csv("data/BioScanData.csv")
inaturalist <- read.csv("data/iNaturalist-clean.csv")
inaturalist <- inaturalist[!is.na(inaturalist$species), ]

bioscan.species.columns <- c(5:33)
bioscan.species <- colnames(bioscan)[bioscan.species.columns]
inaturalist.species <- unique(as.character(inaturalist$species))
inaturalist.species <- gsub(pattern = " ", 
                            replacement = "_", 
                            x = inaturalist.species)

bioscan.missing <- setdiff(bioscan.species, inaturalist.species)
bioscan.missing

#' bioscan             iNaturalist
#' Plebejus acmon      Icaricia acmon
#' Pyrgus albescens    Pyrgus albescens [same]
#' Poanes melane       Paratrytone melane
#' Papilio cresphontes Zerynthia rumina [bad GBIF import!]

################################################################################
# Now looking the other way, at any species that are in iNaturalist that are 
# not in BioSCAN using the reduced, but unclean iNaturalist data
inaturalist.unclean <- read.csv("data/iNaturalist-unclean-reduced.csv")
inaturalist.unclean <- inaturalist.unclean[!is.na(inaturalist.unclean$species), ]

inaturalist.unclean.species <- unique(as.character(inaturalist.unclean$species))
inaturalist.unclean.species <- gsub(pattern = " ", 
                                    replacement = "_", 
                                    x = inaturalist.unclean.species)

inaturalist.missing <- setdiff(inaturalist.unclean.species, bioscan.species)
#' Paratrytone_melane
#' Zerynthia_rumina
#' Limenitis_lorquini
#' Limenitis_bredowii
#' Atlides_halesus

write.csv(x = inaturalist.unclean[inaturalist.unclean$species %in% inaturalist.missing, ],
          file = "output/unique-iNaturalist.csv", 
          row.names = FALSE)
