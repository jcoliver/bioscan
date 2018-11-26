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
library("ggsn")
source(file = "bioscan-functions.R")

################################################################################
# DATA WRANGLING
# Read in data
bioscan <- CompleteBioscan()
inaturalist <- CleanINaturalist()

# Drop any rows missing data
bioscan <- na.omit(bioscan)

latlongs <- unique(bioscan[, c("Longitude", "Latitude")])

latlongs.inat <- unique(inaturalist[, c("longitude", "latitude")])
colnames(latlongs.inat) <- c("Longitude", "Latitude")

# Create data frame with both data sources for easier graphing
latlongs.both <- rbind(latlongs, latlongs.inat)
latlongs.both$Source <- "iNaturalist"
latlongs.both$Source[1:nrow(latlongs)] <- "SCAN surveys"

################################################################################
# MAP
map.bounds <- c(floor(min(bioscan$Longitude)),
                floor(min(bioscan$Latitude)),
                ceiling(max(bioscan$Longitude)),
                ceiling(max(bioscan$Latitude)))
names(map.bounds) <- c("left", "bottom", "right", "top")

la.map <- get_map(location = map.bounds, 
                  source = "stamen", 
                  maptype = "terrain-lines", # toner-lite
                  color = "bw")

bounds <- data.frame(x = c(rep(min(latlongs$Longitude), times = 2), rep(max(latlongs$Longitude), times = 2)),
                     y = c(min(latlongs$Latitude), max(latlongs$Latitude), max(latlongs$Latitude), min(latlongs$Latitude)))

# Add a dummy data frame because ggsn::scalebar only works with a data frame 
# with columns named "long" and "lat"; also add dummy row to get bar positioned 
# correctly
latlongs.scale <- latlongs
colnames(latlongs.scale) <- c("long", "lat")
latlongs.scale[nrow(latlongs.scale) + 1, ] <- list("long" = -118.425, "lat" = 33.84)

city.labels <- data.frame(names = c("Los Angeles", "Del Aire", "East Compton"),
                          longitude = c(-118.2437, -118.3695, -118.1953),
                          latitude = c(34.0522, 33.9161, 33.8981),
                          sizes = c(7, 5, 5))

both.map <- ggmap(la.map) +
  geom_polygon(data = bounds, 
               mapping = aes(x = x, y = y),
               fill = "#ffffff",  # color of rectangle
               color = "#dd2222", # color of line around rectangle
               alpha = 0.45) +
  annotate("text",
           label = city.labels$name, 
           x = city.labels$longitude, 
           y = city.labels$latitude, 
           size = city.labels$sizes, 
           color = "black") +
  geom_point(data = latlongs.both,
             mapping = aes(x = Longitude, y = Latitude, shape = Source, color = Source, fill = Source),
             size = 3) +
  scale_shape_manual(values = c(4, 21)) + # X, filled circle
  scale_color_manual(values = c("#1133ff", "#000000")) + # blue, orange
  scale_fill_manual(values = c("#ffffff", "#ff8c1a")) + # first argument is dummy
  # geom_point(data = latlongs,
  #            mapping = aes(x = Longitude, y = Latitude),
  #            shape = 21,
  #            fill = "#ff8c1a",
  #            color = "black",
  #            size = 3) +
  # geom_point(data = latlongs.inat,
  #            mapping = aes(x = longitude, y = latitude),
  #            shape = 4,
  #            color = "#1133ff",
  #            size = 3) +
  scalebar(data = latlongs.scale,
           dist = 5,
           location = "bottomleft",
           dd2km = TRUE,
           height = 0.03,
           st.dist = 0.04,
           st.size = 4.5,
           model = "WGS84") +
  # theme(legend.position = "none") +
  theme(legend.position = c(0.80, 0.11)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_map(xlim = c(min(bioscan$Longitude) - 0.1, max(bioscan$Longitude) + 0.1),
            ylim = c(min(bioscan$Latitude) - 0.1, max(bioscan$Latitude) + 0.1))
print(both.map)
ggsave(filename = "output/site-map.png", plot = both.map)
