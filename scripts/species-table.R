# Table for paper with species found in each survey type
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2018-09-10

rm(list = ls())

################################################################################
# DATA WRANGLING
# Read in data
bioscan <- read.csv(file = "data/BioScanData.csv")
inaturalist <- read.csv(file = "data/iNaturalist-clean-reduced.csv")

# Drop any rows missing data
bioscan <- na.omit(bioscan)

# Identify sites with data for each of the two collection methods
pollard.sites <- bioscan$Site.Number[bioscan$Collection.Method == "Pollard Walk"]
malaise.sites <- bioscan$Site.Number[bioscan$Collection.Method == "Malaise"]

# Identify sites with data for *both* collection methods
sites.with.both <- intersect(x = pollard.sites, y = malaise.sites)

# Reduce dataset to only those sites with both types of data
bioscan <- bioscan[bioscan$Site.Number %in% sites.with.both, ]
rownames(bioscan) <- NULL

# Identify those columns with species data
species.cols <- c(5:33)

################################################################################
# TABLE
#' Want (but family will get added on later)
#' Family  Species  Pollard  Malaise  iNaturalist

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
