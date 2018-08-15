# Test for abundance differences between survey types
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2018-08-09

rm(list = ls())
library("tidyr")

################################################################################
bioscan <- read.csv(file = "data/BioScanData.csv")

# Drop any rows missing data
bioscan <- na.omit(bioscan)

# Drop any rows with data for only one survey type
pollard.sites <- bioscan$Site.Number[bioscan$Collection.Method == "Pollard Walk"]
malaise.sites <- bioscan$Site.Number[bioscan$Collection.Method == "Malaise"]
sites.with.both <- intersect(x = pollard.sites, y = malaise.sites)
bioscan <- bioscan[bioscan$Site.Number %in% sites.with.both, ]

# Want data in format:
# Site.Number Collection.Type Species Count
bioscan.long <- gather(data = bioscan,
                       key = "Species",
                       value = "Count",
                       -Site.Number, -Collection.Method, -Latitude, -Longitude)

# Calculate D for each species

for (site in unique(bioscan.long$Site.Number)){
  for (species in unique(bioscan.long$Species)) {
    abundance.d = bioscan.long$Count[bioscan.long$Site.Number == site && bioscan.long$Species == species && bioscan.long$Collection.Method == "Pollard Walk"] -
      bioscan.long$Count[bioscan.long$Site.Number == site && bioscan.long$Species == species && bioscan.long$Collection.Method == "Malaise"]
  }
}

