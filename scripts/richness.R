# Test for richness differences between survey types
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2018-08-09

rm(list = ls())

################################################################################
library("tidyr")
bioscan <- read.csv(file = "data/BioScanData.csv")

# Drop any rows missing data
bioscan <- na.omit(bioscan)

# Drop any rows with data for only one survey type
pollard.sites <- bioscan$Site.Number[bioscan$Collection.Method == "Pollard Walk"]
malaise.sites <- bioscan$Site.Number[bioscan$Collection.Method == "Malaise"]
sites.with.both <- intersect(x = pollard.sites, y = malaise.sites)
bioscan <- bioscan[bioscan$Site.Number %in% sites.with.both, ]
rownames(bioscan) <- NULL

# Identify those columns with species data
species.cols <- c(5:33)

# Calculate richness for each row (total number of species with at least one 
# individual observed)
bioscan$richness <- apply(X = bioscan[, species.cols],
                          MARGIN = 1,
                          FUN = function(x) {
                            sum(x > 0)
                          })

# Create data frame for t-test. One column for Pollard, one for Malaise
richness.df <- bioscan[, c("Site.Number", "Collection.Method", "richness")]
richness.df <- richness.df %>%
  spread(Collection.Method, richness)

richness.t <- t.test(x = richness.df$Malaise, 
                     y = richness.df$`Pollard Walk`,
                     paired = TRUE)
richness.t

boxplot(richness ~ Collection.Method, data = bioscan,
        xlab = "Collection Method",
        ylab = "Species Richness",
        las = 1)
