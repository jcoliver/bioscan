# Cleaning up iNaturalist data to restrict to area of interest
# Jeff Oliver
# jcoliver@email.arizona.edu
# 2018-08-28

################################################################################
inaturalist <- read.delim(file = "data/iNaturalist.txt")

# Restrict to dates of interest 15 March 2015 - 15 July 2015
inat <- inaturalist
inat$sampledate <- as.Date(inat$eventdate)
inat <- inat[inat$sampledate >= "2015-03-15" & inat$sampledate <= "2015-07-15", ]

# Retain only a few columns of interest
inat <- inat[, c("gbifid", "species", "decimallatitude", "decimallongitude", "sampledate")]
# Rename those lat/long columns
colnames(inat)[which(colnames(inat) == "decimallatitude")] <- "latitude"
colnames(inat)[which(colnames(inat) == "decimallongitude")] <- "longitude"

# And drop any with NA values in remaining columns - they won't be much use
inat <- na.omit(inat)

# Save and do remaining geographical subsetting in script
write.csv(x = inat, file = "data/iNaturalist-clean.csv", row.names = FALSE)
