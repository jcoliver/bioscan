# Test for richness differences between survey types
# Jeffrey C. Oliver
# jcoliver@email.arizona.edu
# 2018-08-09

rm(list = ls())

################################################################################
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

simple.lm <- lm(richness ~ Collection.Method, data = bioscan)
summary(simple.lm)

boxplot(richness ~ Collection.Method, data = bioscan)
