# Table with species found in each survey type
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2018-09-10

rm(list = ls())

################################################################################
# DATA WRANGLING
library("tidyverse")
source(file = "../bioscan-functions.R")
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

################################################################################
# Second version, INCOMPLETE
# Just do presence/absence
pa <- bioscan[, species.cols]
pa[pa > 0] <- 1

# Do counts for each method
malaise.counts <- colSums(x = pa[bioscan$Collection.Method == "Malaise", ])
pollard.counts <- colSums(x = pa[bioscan$Collection.Method == "Pollard Walk", ])

# Combine these two
output.df <- data.frame(Pollard = pollard.counts,
                        Malaise = malaise.counts)
# Drop any species that weren't observed in either method
output.df <- output.df[rowSums(output.df) > 0, ]

# Move the rownames to a column
output.df$Species <- rownames(output.df)
rownames(output.df) <- NULL
output.df <- output.df[, c("Species", "Pollard", "Malaise")]

# Add the iNaturalist data
inaturalist.counts <- table(inaturalist$species)

# There may be species in iNaturalist that we don't have yet, so add those
# Identify species in iNaturalist not in the output.df already
missing.species <- setdiff(x = names(inaturalist.counts), y = output.df$Species)
# Add rows for those missing species. So UGLY.
output.df[c((nrow(output.df) + 1):(nrow(output.df) + length(missing.species))), "Species"] <- missing.species
output.df$Pollard[output.df$Species %in% missing.species] <- 0
output.df$Malaise[output.df$Species %in% missing.species] <- 0

output.df$iNaturalist <- 0
output.df$iNaturalist[output.df$Species]

inaturalist.column <- table(inaturalist.species, output.df$Species)

################################################################################
# First version, just "X" for p/a
malaise.totals <- colSums(x = bioscan[bioscan$Collection.Method == "Malaise", species.cols])
malaise.species <- names(x = malaise.totals)[malaise.totals > 0]

pollard.totals <- colSums(x = bioscan[bioscan$Collection.Method == "Pollard Walk", species.cols])
pollard.species <- names(x = pollard.totals)[pollard.totals > 0]

inaturalist.species <- unique(as.character(inaturalist$species))

species <- union(x = union(x = malaise.species, y = pollard.species), 
                 y = inaturalist.species)

output.df <- data.frame(Species = sort(species),
                        Pollard = NA,
                        Malaise = NA,
                        iNaturalist = NA)


output.df$Pollard[output.df$Species %in% pollard.species] <- "X"
output.df$Malaise[output.df$Species %in% malaise.species] <- "X"
output.df$iNaturalist[output.df$Species %in% inaturalist.species] <- "X"

write.csv(x = output.df,
          file = "output/species-method.txt", 
          row.names = FALSE)
