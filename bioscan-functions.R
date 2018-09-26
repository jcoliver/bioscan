# Functions for the bioscan project; largely data pre-processing
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2018-09-13

################################################################################
#' Create bioscan data that only includes sites which have both Malaise trap and 
#' Pollard walk data
#' 
#' @param input character filename of original bioscan data
#' @param output character filename of bioscan data including only sites with 
#' data for both collection methods
#' @return data.frame with bioscan data that only includes sites which have both 
#' Malaise trap and Pollard walk data
CompleteBioscan <- function(input = "data/BioScanData.csv", 
                            output = "data/BioScanDataComplete.csv") {
  bioscan.complete <- NULL
  if (file.exists(output)) {
    bioscan.complete <- read.csv(file = output)
  } else {
    bioscan <- read.csv(input)
    # Drop any rows missing data
    bioscan <- na.omit(bioscan)
    
    # Identify sites with data for each of the two collection methods
    pollard.sites <- bioscan$Site.Number[bioscan$Collection.Method == "Pollard Walk"]
    malaise.sites <- bioscan$Site.Number[bioscan$Collection.Method == "Malaise"]
    
    # Identify sites with data for *both* collection methods
    sites.with.both <- intersect(x = pollard.sites, y = malaise.sites)
    
    # Reduce dataset to only those sites with both types of data
    bioscan.complete <- bioscan[bioscan$Site.Number %in% sites.with.both, ]
    rownames(bioscan.complete) <- NULL
    rm(bioscan, pollard.sites, malaise.sites, sites.with.both)
    
    # Save this to a file, too
    write.csv(x = bioscan.complete, file = output, row.names = FALSE)
  }
  
  # # Drop any species colums with zero observations [INCOMPLETE]
  # # Identify those columns with species data
  # species.cols <- c(5:33)
  # 
  # species.totals <- colSums(bioscan.complete[, species.cols])
  # zero.obs <- names(species.totals)[species.totals < 1]

  return(bioscan.complete)
}

################################################################################
#' Create iNaturalist data that is restricted to geographic area of bioscan data
#' and uses the same taxonomy
#' 
#' @param input character iNaturalist data file name
#' @param output character file name for restricted iNaturalist data
#' @param bioscan character filename of original bioscan data
#' @param bioscan.complete character filname of bioscan data including only 
#' sites with data for both collection methods
#' 
#' @return data.frame only including those 
CleanINaturalist <- function(input = "data/iNaturalist-clean.csv",
                             output = "data/iNaturalist-clean-reduced.csv",
                             bioscan.file = "data/BioScanData.csv",
                             bioscan.complete.file = "data/BioScanDataComplete.csv",
                             bioscan.df = NULL) {
  inaturalist.reduced <- NULL
  if (file.exists(output)) {
    inaturalist.reduced <- read.csv(file = output)
  } else {
    inaturalist <- read.csv(file = input)
    
    # Drop NAs (should have been done already, but just in case)
    inaturalist <- na.omit(inaturalist)
    
    # Read in the bioscan data (or clean it up if it hasn't already been done)
    if (is.null(bioscan.df)) {
      bioscan <- CompleteBioscan(input = bioscan.file, 
                                 output = bioscan.complete.file)
    } else {
      bioscan <- bioscan.df
    }
    # Determine boundaries of rectangle from Bioscan data
    max.lon <- max(bioscan$Longitude)
    min.lon <- min(bioscan$Longitude)
    max.lat <- max(bioscan$Latitude)
    min.lat <- min(bioscan$Latitude)
    
    # Restrict iNaturalist data to that rectangle
    inaturalist <- inaturalist[inaturalist$longitude >= min.lon &
                                 inaturalist$longitude <= max.lon &
                                 inaturalist$latitude >= min.lat &
                                 inaturalist$latitude <= max.lat, ]
    
    # We want to make species column in iNaturalist match the format as in bioscan 
    # data: Genus_species
    inaturalist$species <- gsub(pattern = " ", 
                                replacement = "_", 
                                x = as.character(inaturalist$species))
    
    # Before taxonomic reconciliation, save a version
    write.csv(x = inaturalist, 
              file = gsub(pattern = "clean",
                          replacement = "unclean",
                          x = output), 
              row.names = FALSE)

    # And need to make two adjustments to iNaturalist species so they match with 
    # taxonomy used by bioscan
    inaturalist$species[inaturalist$species == "Icaricia_acmon"] <- "Plebejus_acmon"
    inaturalist$species[inaturalist$species == "Paratrytone_melane"] <- "Poanes_melane"
    inaturalist$species[inaturalist$species == "Zerynthia_rumina"] <- "Papilio_cresphontes"
    inaturalist$species[inaturalist$species == "Limenitis_bredowii"] <- "Adelpha_bredowii"
    
    # Turn it back into a factor, which also means we've dropped unused levels
    inaturalist$species <- as.factor(inaturalist$species)
    
    # And drop one record of Atlides halesus that we don't trust
    inaturalist.reduced <- inaturalist[inaturalist$catalognumber != 14464139, ]
    rm(bioscan, min.lon, max.lon, min.lat, max.lat, inaturalist)
    
    # Go ahead and save this so we can use it elsewhere (like a map)
    write.csv(x = inaturalist.reduced, 
              file = output, 
              row.names = FALSE)
  }
  return(inaturalist.reduced)
}