# Plot sites on a map
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2018-08-24

rm(list = ls())

################################################################################
# DEPENDENCIES
# Load dependencies
library("ggplot2")
library("ggmap")

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

latlongs <- unique(bioscan[, c("Longitude", "Latitude")])

latlongs.inat <- unique(inaturalist[, c("longitude", "latitude")])
################################################################################
# MAP
map.bounds <- c(floor(min(bioscan$Longitude)),
                floor(min(bioscan$Latitude)),
                ceiling(max(bioscan$Longitude)),
                ceiling(max(bioscan$Latitude)))

la.map <- get_map(location = map.bounds, source = "google", maptype = "satellite")

bioscan.map <- ggmap(la.map) +
  geom_point(data = latlongs,
             mapping = aes(x = Longitude, y = Latitude),
             shape = 21,
             fill = "#ff8c1a",
             color = "black",
             size = 3) +
  theme(legend.position = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_map(xlim = c(min(bioscan$Longitude) - 0.1, max(bioscan$Longitude) + 0.1),
            ylim = c(min(bioscan$Latitude) - 0.1, max(bioscan$Latitude) + 0.1))
print(bioscan.map)
ggsave(filename = "output/site-map.png", plot = bioscan.map)

bounds <- data.frame(x = c(rep(min(latlongs$Longitude), times = 2), rep(max(latlongs$Longitude), times = 2)),
                     y = c(min(latlongs$Latitude), max(latlongs$Latitude), max(latlongs$Latitude), min(latlongs$Latitude)))

both.map <- ggmap(la.map) +
  geom_polygon(data = bounds, 
               mapping = aes(x = x, y = y),
               fill = "#ffffff",  # color of rectangle
               color = "#222222", # color of line around rectangle
               alpha = 0.25) +
  geom_point(data = latlongs,
             mapping = aes(x = Longitude, y = Latitude),
             shape = 21,
             fill = "#ff8c1a",
             color = "black",
             size = 3) +
  geom_point(data = latlongs.inat,
             mapping = aes(x = longitude, y = latitude),
             shape = 4,
             color = "#1133ff",
             size = 3) +
  theme(legend.position = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_map(xlim = c(min(bioscan$Longitude) - 0.1, max(bioscan$Longitude) + 0.1),
            ylim = c(min(bioscan$Latitude) - 0.1, max(bioscan$Latitude) + 0.1))
print(both.map)
ggsave(filename = "output/site-map-combined.png", plot = bioscan.map)
