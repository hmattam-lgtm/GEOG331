library(lubridate)
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(tidyr)
library(ncdf4)

#load in data
datH <- vect("Z:\\hmattam\\Data\\Fire\\Cal_Data.shp",)
year_2000_onward <- datH[datH$YEAR_ >= 2000 & datH$YEAR_ != 2025]
year_freq <- table(year_2000_onward$YEAR_)
year_freq_df <- as.data.frame(year_freq)
colnames(year_freq_df) <- c("Year", "Frequency")

#sort all the years
years <- sort(unique(year_2000_onward$YEAR_)) 
#number of colors
n <- length(years)

#plot visually using terra plot of just the fires
terra::plot(
  year_2000_onward, 
  "YEAR_",
  main = "Fire Locations (2000–Present)",
  col = terrain.colors(n), 
  breaks = c(years, base::max(years) + 1)
)

#ggplot histogram
ggplot(year_freq_df, aes(x = Year, y = Frequency)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  theme_minimal() +
  labs(
    title = "Fire Frequency per Year (2000–2024)",
    x = "Year",
    y = "Number of Fires"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##PDSI
##load PDSI data folder
path <- "Z:\\hmattam\\Data\\terraclim"
##load in all the files
files <- list.files(path,
                    pattern = "^TerraClimate_PDSI_20[0-2][0-9]\\.nc$",
                    full.names = TRUE)
##rasterize the files
pdsi <- rast(files)

#project the California boundaries
ca_counties <- vect("Z:\\hmattam\\Data\\CA\\CA_Counties.shp")
ca_counties_proj <- project(ca_counties, crs(pdsi))
ca_boundary <- aggregate(ca_counties_proj)

#Use the boundaries of California to crop and mask PDSI
pdsi_ca <- mask(crop(pdsi, ca_boundary), ca_boundary)

#get individual county mean and put into df
county_pdsi <- zonal(pdsi_ca, ca_counties_proj, fun = "mean", na.rm = TRUE)
county_pdsi$County <- 1:nrow(county_pdsi)  

#get values for counties and months numbers to use
n_counties <- nrow(county_pdsi)
n_months_total <- ncol(county_pdsi) - 1
n_years <- n_months_total / 12
years <- 2000:(2000 + n_years - 1)

#Change format of data to have month to be month index
county_pdsi_long <- county_pdsi %>%
  pivot_longer(
    cols = starts_with("PDSI_"),
    names_to = "Month_Index",
    values_to = "PDSI"
  ) %>%
  mutate(
    Month = as.numeric(sub("PDSI_(\\d+)", "\\1", Month_Index)),
    Year  = 2000 + ((row_number() - 1) %/% 12)  # calculates year automatically
  ) %>%
  select(County, Year, Month, PDSI)

#properly assign month, year, and county
county_pdsi_long$County <- rep(1:n_counties, each = n_months_total)
county_pdsi_long$Month  <- rep(rep(1:12, times = n_years), times = n_counties)
county_pdsi_long$Year   <- rep(years, times = n_counties, each = 12)

#averaging across each year over all counties
state_annual <- county_pdsi_long %>%
  group_by(Year) %>%
  summarise(Avg_PDSI = mean(PDSI, na.rm = TRUE), .groups = "drop")

#plot the PDSI annually using data found
ggplot(state_annual, aes(x = Year, y = Avg_PDSI)) +
  geom_col(fill = "skyblue") +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  theme_minimal() +
  labs(
    title = "Average Annual PDSI in California",
    x = "Year",
    y = "Average PDSI"
  )

# NDVI 
folder_path <- "Z:\\hmattam\\Data\\ModisData" 
# List HDF files 
hdf_files <- list.files(folder_path, pattern = "\\.hdf$", full.names = TRUE) 


#create function toget the ndvi data from each file and then find mean
get_ndvi_info <- function(file) { 
  sds <- gdal_subdatasets(file) 
  ndvi_sds <- sds[grep("NDVI", sds, ignore.case = TRUE)] 
  if(length(ndvi_sds) == 0) return(NULL) # skip files with no NDVI 
  ndvi_rast <- rast(unname(ndvi_sds[[1]])) # Apply MODIS scale factor 
  ndvi_scaled <- ndvi_rast * 0.0001 
  mean_ndvi <- global(ndvi_rast * 0.0001, "mean", na.rm = TRUE)[1] 
  year <- as.numeric(regmatches(file, regexpr("\\d{4}", file))) 
  data.frame(year = year, mean_ndvi = mean_ndvi) } 

#Actually apply the functions to each file 
ndvi_list <- lapply(hdf_files, get_ndvi_info) 

#Make and df using all the files (combine it)
ndvi_df <- do.call(rbind, ndvi_list) 
#scale the NDVI
ndvi_df$mean_ndvi_scaled <- ndvi_df$mean * 0.0001

#Ensure we get one yearly NDVI
ndvi_yearly <- ndvi_df %>% 
  group_by(year) %>% 
  summarise(mean_ndvi = mean(mean_ndvi_scaled, na.rm = TRUE)) 

#Plot the trends as a line plot
ggplot(ndvi_yearly, aes(x = year, y = mean_ndvi)) + 
  geom_line(color = "darkgreen") + 
  geom_point(color = "darkgreen") + 
  labs(title = "NDVI Trends (April 15–30) in California", 
       x = "Year", y = "Mean NDVI") + 
  theme_minimal()

#create a new year col with it as numeric
fire_yearly <- year_freq_df %>%
  mutate(Year = as.numeric(Year))

#change it to same as PDSI
fire_yearly$Year <- 2000:(2000 + nrow(fire_yearly) - 1)

#combine data
fire_pdsi <- fire_yearly %>%
  left_join(state_annual, by = "Year")
#remove NA
fire_pdsi_clean <- fire_pdsi %>%
  filter(!is.na(Frequency), !is.na(Avg_PDSI))

#plot relationship between wildfires and PDI
plot(fire_pdsi_clean$Avg_PDSI, fire_pdsi_clean$Frequency,
     pch = 19,
     xlab = "Average Annual PDSI",
     ylab = "Number of Wildfires",
     main = "Wildfire Frequency vs Drought Severity in California")
fit <- lm(Frequency ~ Avg_PDSI, data = fire_pdsi_clean)
abline(fit, col = "red", lwd = 2)

#plot residuals
plot(
  fire_pdsi_clean$Avg_PDSI,
  residuals(fit),
  pch = 19,
  xlab = "Average Annual PDSI",
  ylab = "Residuals",
  main = "Residuals of Fire–PDSI Regression"
)
abline(h = 0, col = "red")

##all three data sets
#create combined data frame with all relevant values
combined_df <- fire_yearly %>%
  left_join(state_annual, by = "Year") %>%
  left_join(ndvi_yearly, by = c("Year" = "year")) %>%
  filter(!is.na(Frequency), !is.na(Avg_PDSI), !is.na(mean_ndvi))

#plot the three on one graph
ggplot(combined_df, aes(x = Year)) +
  geom_line(aes(y = Frequency, color = "Wildfires"), size = 1) +
  geom_line(aes(y = Avg_PDSI * 100, color = "PDSI (scaled)"), size = 1) +
  geom_line(aes(y = mean_ndvi * 1000, color = "NDVI (scaled)"), size = 1) +
  theme_minimal() +
  labs(
    title = "Wildfires, Drought, and Vegetation in California",
    y = "Relative Values (scaled)",
    color = "Variable"
  )


