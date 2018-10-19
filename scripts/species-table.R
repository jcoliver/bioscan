# Table with species found in each survey type
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2018-09-10

rm(list = ls())

################################################################################
# DATA WRANGLING
library("tidyverse")
source(file = "bioscan-functions.R")
bioscan <- CompleteBioscan()
inaturalist <- CleanINaturalist()

# Identify those columns with species data
species.cols <- c(5:33)

################################################################################
# TABLE
#' Want (but family will get added on later)
#' Family  Species  Pollard  Malaise  iNaturalist

################################################################################
# Third version
# Identify the column with collection method
collection.column <- which(colnames(bioscan) == "Collection.Method")
# Transform data to long format, with just the method, species, and abundance
counts <- bioscan[, c(collection.column, species.cols)] %>%
  gather(key = "Species", value = "Count", -Collection.Method)
# Drop all rows where species not observed
counts <- counts[counts$Count > 0, ]
# Now add iNaturalist data as additional rows
inaturalist.counts <- data.frame(Collection.Method = "iNaturalist",
                                 Species = inaturalist$species,
                                 Count = 1) # We don't actually have the count
counts <- rbind(counts, inaturalist.counts)

# Use table to do the counts for us
output.long <- as.data.frame(table(counts[, c("Species", "Collection.Method")]))

output.df <- spread(data = output.long, key = Collection.Method, value = Freq)

write.csv(x = output.df,
          file = "output/species-method.txt", 
          row.names = FALSE,
          quote = FALSE)
