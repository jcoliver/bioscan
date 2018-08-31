# Reconcile taxonomic differences between bioscan & iNaturalist
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

#' bioscan           iNaturalist
#' Plebejus acmon    Icaricia acmon
#' Pyrgus albescens  Pyrgus albescens [same]
#' Poanes melane     Paratrytone melane
