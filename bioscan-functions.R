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
  return(bioscan.complete)
}