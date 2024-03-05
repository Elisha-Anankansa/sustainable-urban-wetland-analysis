# Load necessary libraries
library(sf)
library(raster)
library(ggplot2)
library(rasterVis)
library(gstat)

# Read in shapefile of Oforikrom Municipality
oforikrom_shape <- st_read("path/to/oforikrom_shapefile.shp")

# Read in data frame of wetland locations
wetlands <- read.csv("path/to/wetlands_data.csv")

# Convert data frame to spatial data frame
coordinates(wetlands) <- ~ x + y
proj4string(wetlands) <- proj4string(oforikrom_shape)

# Perform spatial analysis of wetland encroachment
encroachment_data <- st_intersection(oforikrom_shape, wetlands)

# Calculate the number of encroached wetlands
encroached_wetlands <- st_sf(data.frame(n = st_n(encroachment_data)), crs = st_crs(oforikrom_shape))

# Calculate the percentage of encroached wetlands
total_wetlands <- st_sf(data.frame(n = st_n(wetlands)), crs = st_crs(oforikrom_shape))
percentage_encroached <- encroached_wetlands$n / total_wetlands$n

# Plot the spatial distribution of encroached wetlands
ggplot() +
  geom_sf(data = oforikrom_shape, fill = "gray") +
  geom_sf(data = encroachment_data, fill = "red") +
  labs(x = "Longitude", y = "Latitude", fill = "Encroached Wetlands") +
  theme_bw()

# Plot the percentage of encroached wetlands
ggplot() +
  geom_bar(data = data.frame(percentage_encroached), aes(x = "", y = percentage_encroached)) +
  labs(x = "", y = "Percentage of Encroached Wetlands", title = "Percentage of Encroached Wetlands in Oforikrom Municipality") +
  theme_bw()

# Analyze the relationship between wetland encroachment and urbanization
urban_data <- st_read("path/to/urban_data.shp")

# Merge the urban data with the encroachment data
encroachment_urban_data <- merge(encroachment_data, urban_data, by.x = "ID", by.y = "ID")

# Perform Gaussian regression model to Map the spatial distribution of urbanization and its impact on wetland encroachment
urban_model <- gstat(formula = encroachment_urban_data$encroachment ~ urban_data$urbanization, locations = ~x+y, data = encroachment_urban_data)

# Predict urbanization and its impact on wetland encroachment for Oforikrom Municipality
predicted_encroachment <- predict(urban_model, oforikrom_shape)

# Plot the spatial distribution of urbanization and its impact on wetland encroachment
ggplot() +
  geom_sf(data = oforikrom_shape, fill = "gray") +
  geom_raster(data = predicted_encroachment, aes(fill = encroachment)) +
  scale_fill_gradientn(colours = c("green", "yellow", "red"),
                       values = c(0, 0.5, 1),
                       guide = "colorbar", name = "Encroachment") +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()

# Perform spatial analysis to identify areas at risk of further encroachment
at_risk_data <- st_intersection(oforikrom_shape, st_buffer(encroachment_data, dist = 1000)) # adjust buffer distance as needed
# Plot the areas at risk of further encroachment
ggplot() +
  geom_polygon(data = areas_at_risk, aes(x = longitude, y = latitude, group = group), fill = "red", alpha = 0.5) +
  geom_raster(data = wetland_raster, aes(fill = wetland_condition)) +
  labs(title = "Areas at Risk of Wetland Encroachment",
       x = "Longitude", y = "Latitude",
       fill = "Wetland Condition") +
  scale_fill_manual(values = c("green" = "green", "yellow" = "yellow", "red" = "red"),
                    labels = c("Healthy", "Degraded", "Highly Degraded")) +
  theme_minimal()