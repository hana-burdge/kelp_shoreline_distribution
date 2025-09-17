# ##############################################################################

# Authoured by Romina Barbosa
# Modified by Hana Burdge
# Map the distribution of kelp overtime in relation to local sea surface temperature

# ##############################################################################

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(sf)
library(tidyverse)
# devtools::install_github("r-lib/conflicted")
library(mapview)
library(readxl)

mypath<- "data/"

data_2022_coordinatesXY <- read.csv(paste(mypath, "Kelp_SCFS_survey_data_2022_coordinatesXY.csv", sep="/"))#, sep=";")
colnames(data_2022_coordinatesXY)
data_2022_coordinatesXY<- data_2022_coordinatesXY[,-1]


# 2023 
data_2023_jul <- read_excel(paste(mypath, "Kelp_SCFS_survey_data_2023_shoreline_distribution.xlsx", sep="/"))
data_2023_aug <- read_excel(paste(mypath, "Kelp_SCFS_survey_data_2023_shoreline_distribution.xlsx", sep="/"),
                                                                     sheet = "August2023")
data_2023_sep <- read_excel(paste(mypath, "Kelp_SCFS_survey_data_2023_shoreline_distribution.xlsx", sep="/"),
                                                                    sheet = "September2023")
colnames(data_2023_aug)
colnames(data_2023_jul)
colnames(data_2023_sep)

# 2024
data_2024_jul <- read_excel(paste(mypath, "2024 SCFS shoreline.xlsx", sep="/"),
                            sheet = "july2024")
data_2024_aug <- read_excel(paste(mypath, "2024 SCFS shoreline.xlsx", sep="/"),
                            sheet = "august2024")
data_2024_sep <- read_excel(paste(mypath, "2024 SCFS shoreline.xlsx", sep="/"),
                            sheet = "sept2024")
colnames(data_2024_aug)
colnames(data_2024_jul)
colnames(data_2024_sep)

# 2025
data_2025_jul <- read_excel(paste(mypath, "2025 SCFS shoreline.xlsx", sep="/"),
                            sheet = "july2025")
colnames(data_2025_jul)
data_2025_aug <- read_excel(paste(mypath, "2025 SCFS shoreline.xlsx", sep="/"),
                            sheet = "august2025")
colnames(data_2025_aug)
data_2025_sep <- read_excel(paste(mypath, "2025 SCFS shoreline.xlsx", sep="/"),
                            sheet = "sept2025")
colnames(data_2025_sep)


## Merge 3 months of surveys
# 2023
data_2023<- rbind(data_2023_jul[,1:15], data_2023_aug[,1:15], data_2023_sep[,1:15])
# 2024
data_2024 <- rbind(data_2024_jul[,1:15], data_2024_aug[,1:15], data_2024_sep[,1:15])
# 2025
data_2025 <- rbind(data_2025_jul[,1:15], data_2025_aug[,1:15], data_2025_sep[,1:15])

## Select some of the columns (only the ones needed) and add a column with year of survey
# 2023
colnames(data_2023)[c(1:8,11:15)]<- paste(colnames(data_2023[,c(1:8,11:15)]), "2023", sep="_")
colnames(data_2023)[10]<- "Site" # replace column name
# 2024
colnames(data_2024)[c(1:8,11:15)]<- paste(colnames(data_2024[,c(1:8,11:15)]), "2024", sep="_")
colnames(data_2024)[10]<- "Site" # replace column name
#2025
colnames(data_2025)[c(1:8,11:15)]<- paste(colnames(data_2025[,c(1:8,11:15)]), "2025", sep="_")
colnames(data_2025)[10]<- "Site" # replace column name


## Change the code of column "No kelp"; no kelp "1" is replaced for "2" (this will be used for the analysis of dynamics) 
# 2023
data_2023[is.na(data_2023$`No kelp_2023`), "No kelp_2023"]<- 0 # Replace NAs with 0
data_2023[data_2023$`No kelp_2023` == 1, "No kelp_2023"]<- 2   # replace values 1 for 2
# 2024
data_2024[is.na(data_2024$`No kelp_2024`), "No kelp_2024"]<- 0 # Replace NAs with 0
data_2024[data_2024$`No kelp_2024` == 1, "No kelp_2024"]<- 4   # replace values 1 for 4
# 2025
data_2025[is.na(data_2025$`No kelp_2025`), "No kelp_2025"]<- 0 # Replace NAs with 0
data_2025[data_2025$`No kelp_2025` == 1, "No kelp_2025"]<- 8   # replace values 1 for 8

## Create a column of kelp presence (the inverse of No kelp column), by reversing the levels
# 2023
data_2023$kelp_2023<- as.factor(data_2023$`No kelp_2023`)
levels(data_2023$kelp_2023)
# [1] "0" "2"
levels(data_2023$kelp_2023)<- c("2","0")# (0 was kelp presence and 2 kelp absence in the original No kelp column)
levels(data_2023$kelp_2023)
# [1] "2" "0"
summary(data_2023$kelp_2023)
# 2    0 
# 2915 2126 

# 2024
data_2024$kelp_2024<- as.factor(data_2024$`No kelp_2024`)
levels(data_2024$kelp_2024)
# [1] "0" "4"
levels(data_2024$kelp_2024)<- c("4","0")# (0 was kelp presence and 4 kelp absence in the original No kelp column)
levels(data_2024$kelp_2024)
# [1] "4" "0"
summary(data_2024$kelp_2024)
# 4    0 
# 2963 2085 

# 2025
data_2025$kelp_2025<- as.factor(data_2025$`No kelp_2025`)
levels(data_2025$kelp_2025)
# [1] "0" "6"
levels(data_2025$kelp_2025)<- c("8","0")# (0 was kelp presence and 8 kelp absence in the original No kelp column)
levels(data_2025$kelp_2025)
# [1] "6" "0"
summary(data_2025$kelp_2025)
# 6    0 
# 3916 510


## Replace NAs per "0" and "1" per "2" 
# 2023
data_2023[is.na(data_2023$`less_than_10%_2023`), "less_than_10%_2023"]<- 0
data_2023[data_2023$`less_than_10%_2023` == 1, "less_than_10%_2023"]<- 2

data_2023[is.na(data_2023$`greater_then_10%_2023`), "greater_then_10%_2023"]<- 0
data_2023[data_2023$`greater_then_10%_2023` == 1, "greater_then_10%_2023"]<- 2

data_2023[is.na(data_2023$Width_less_2m_2023), "Width_less_2m_2023"]<- 0
data_2023[data_2023$Width_less_2m_2023 == 1, "Width_less_2m_2023"]<- 2

data_2023[is.na(data_2023$Width_greater_2m_2023), "Width_greater_2m_2023"]<- 0
data_2023[data_2023$Width_greater_2m_2023 == 1, "Width_greater_2m_2023"]<- 2
# 2024
data_2024[is.na(data_2024$`less_than_10%_2024`), "less_than_10%_2024"]<- 0
data_2024[data_2024$`less_than_10%_2024` == 1, "less_than_10%_2024"]<- 2

data_2024[is.na(data_2024$`greater_then_10%_2024`), "greater_then_10%_2024"]<- 0
data_2024[data_2024$`greater_then_10%_2024` == 1, "greater_then_10%_2024"]<- 2

data_2024[is.na(data_2024$Width_less_2m_2024), "Width_less_2m_2024"]<- 0
data_2024[data_2024$Width_less_2m_2024 == 1, "Width_less_2m_2024"]<- 2

data_2024[is.na(data_2024$Width_greater_2m_2024), "Width_greater_2m_2024"]<- 0
data_2024[data_2024$Width_greater_2m_2024 == 1, "Width_greater_2m_2024"]<- 2
#2025
data_2025[is.na(data_2025$`less_than_10%_2025`), "less_than_10%_2025"]<- 0
data_2025[data_2025$`less_than_10%_2025` == 1, "less_than_10%_2025"]<- 2

data_2025[is.na(data_2025$`greater_then_10%_2025`), "greater_then_10%_2025"]<- 0
data_2025[data_2025$`greater_then_10%_2025` == 1, "greater_then_10%_2025"]<- 2

data_2025[is.na(data_2025$Width_less_2m_2025), "Width_less_2m_2025"]<- 0
data_2025[data_2025$Width_less_2m_2025 == 1, "Width_less_2m_2025"]<- 2

data_2025[is.na(data_2025$Width_greater_2m_2025), "Width_greater_2m_2025"]<- 0
data_2025[data_2025$Width_greater_2m_2025 == 1, "Width_greater_2m_2025"]<- 2


## Plot dataset of each year ====================================================
colnames(data_2022_coordinatesXY)
map_2022<- mapview(data_2022_coordinatesXY, xcol = "x", ycol = "y", 
        crs = 4269, grid = FALSE, cex= 1, legend = TRUE, zcol = "No.kelp", 
        col.regions = c("green", "blue"),  alpha = 0) ##16B4D6
map_2022


data_2022_coordinatesXY$Location<- as.factor(data_2022_coordinatesXY$Location)
mapview(data_2022_coordinatesXY, xcol = "x", ycol = "y", 
        crs = 4269, grid = FALSE, cex= 1, legend = TRUE, zcol = "Location", 
        col.regions = c("green", "blue", "red", "orange", "lightgrey", "cyan",
                        "darkgreen", "yellow", "gold", "darkgrey", "black",
                        "pink", "brown3", "purple", "magenta", "darkred"),  alpha = 0)

# library(utils)
# html_fl = tempfile(fileext = ".html")
# png_fl = tempfile(fileext = ".png")
# mapshot(map_2022, file = png_fl)


## Check that Arrow and South Arrow are the same; they have same number of segments
# 2023
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "South_Arrow"),"Site"])
data_2023$Location<- as.factor(data_2023$Location)
summary(data_2023[which(data_2023$Location== "Arrow"),"Site"])
# 2024
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "South_Arrow"),"Site"])
data_2024$Location<- as.factor(data_2024$Location)
summary(data_2024[which(data_2024$Location== "Arrow"),"Site"])
#2025
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "South_Arrow"),"Site"])
data_2025$Location<- as.factor(data_2025$Location)
summary(data_2025[which(data_2025$Location== "Arrow"),"Site"])

## Check that Cramer and Crammer South are the same; they have same number of segments
# 2023
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "Crammer_South"),"Site"])
summary(data_2023[which(data_2023$Location== "Cramer"),"Site"])
# 2024
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "Crammer_South"),"Site"])
summary(data_2024[which(data_2024$Location== "Cramer"),"Site"])
#2025
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "Crammer_South"),"Site"])
summary(data_2025[which(data_2025$Location== "Cramer"),"Site"])

## Check that Gilford_Point 2023 is not contained in Port_Elisabeth segments 2022
# 2023
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "Gilford_Point"),"Site"])
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "Port_Elisabeth"),"Site"])
summary(data_2023[which(data_2023$Location== "Port_Elisabeth"),"Site"])
# 2024
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "Gilford_Point"),"Site"])
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "Port_Elisabeth"),"Site"])
summary(data_2024[which(data_2024$Location== "Port_Elisabeth"),"Site"])
#2025
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "Gilford_Point"),"Site"])
unique(data_2022_coordinatesXY[which(data_2022_coordinatesXY$Location== "Port_Elisabeth"),"Site"])
summary(data_2025[which(data_2025$Location== "Port_Elisabeth"),"Site"])


##
# 2023
data_2023$Location<- as.factor(data_2023$Location)
levels(as.factor(data_2022_coordinatesXY$Location))
levels(data_2023$Location)
# 2024
data_2024$Location<- as.factor(data_2024$Location)
levels(as.factor(data_2022_coordinatesXY$Location))
levels(data_2024$Location)
#2025
data_2025$Location<- as.factor(data_2025$Location)
levels(as.factor(data_2022_coordinatesXY$Location))
levels(data_2025$Location)

## Rename location to match data_2022_coorinatesXY *this is very important*
# 2023
levels(data_2023$Location)[1] <- "South_Arrow"
levels(data_2023$Location)[2] <- "South_Bonwick"
levels(data_2023$Location)[3] <- "Crammer_South"
levels(data_2023$Location)[6] <- "Gilford_Point"
levels(data_2023$Location)[11] <- "Port_Elisabeth"
levels(data_2023$Location)[13] <- "Turnour_North"
levels(data_2023$Location)

# 2024
levels(data_2024$Location)[1] <- "South_Bonwick"
levels(data_2024$Location)[2] <- "Crammer_South"
levels(data_2024$Location)[5] <- "Gilford_LowerKnight"
levels(data_2024$Location)[6] <- "Gilford_Point"
levels(data_2024$Location)[7] <- "Gwayasdums"
levels(data_2024$Location)[10] <- "Port_Elisabeth"
levels(data_2024$Location)[11] <- "South_Arrow"
levels(data_2024$Location)[13] <- "Turnour_North"
levels(data_2024$Location)[14] <- "Turnour_South"
levels(data_2024$Location)

#2025
levels(data_2025$Location)[1] <- "Crammer_South"
levels(data_2025$Location)

##
# 2023
colnames(data_2023)
colnames(data_2022_coordinatesXY[,c(1,2,5,6)])
data_2023_coord<- merge( data_2023, data_2022_coordinatesXY[,c(1,2,5,6)], by= c("Location", "Site"))
# 2024
colnames(data_2024)
colnames(data_2022_coordinatesXY[,c(1,2,5,6)])
data_2024_coord<- merge( data_2024, data_2022_coordinatesXY[,c(1,2,5,6)], by= c("Location", "Site"))
# 2025
colnames(data_2025)
colnames(data_2022_coordinatesXY[,c(1,2,5,6)])
data_2025_coord<- merge( data_2025, data_2022_coordinatesXY[,c(1,2,5,6)], by= c("Location", "Site"))



# 2023 
colnames(data_2023_coord)
m_2023<- mapview(data_2023_coord, xcol = "x", ycol = "y", 
        crs = 4269, grid = FALSE, cex= 1, legend = TRUE, zcol = "No kelp_2023", 
        col.regions = c("green", "blue"),  alpha = 0) ##16B4D6

m_2023

# 2024
colnames(data_2024_coord)
m_2024<- mapview(data_2024_coord, xcol = "x", ycol = "y", 
                 crs = 4269, grid = FALSE, cex= 1, legend = TRUE, zcol = "No kelp_2024", 
                 col.regions = c("green", "blue"),  alpha = 0) ##16B4D6

m_2024

#2025
colnames(data_2025_coord)
m_2025<- mapview(data_2025_coord, xcol = "x", ycol = "y", 
                 crs = 4269, grid = FALSE, cex= 1, legend = TRUE, zcol = "No kelp_2025", 
                 col.regions = c("green", "blue"),  alpha = 0) ##16B4D6

m_2025


# 2023
mapview(data_2023_coord, xcol = "x", ycol = "y", 
                 crs = 4269, grid = FALSE, cex= 1, legend = TRUE, zcol = "Location", 
        col.regions = c("green", "blue", "red", "orange", "lightgrey", "cyan",
                        "darkgreen", "yellow", "gold", "darkgrey", "black",
                        "pink", "brown", "purple", "magenta"),  alpha = 0)
# 2024
mapview(data_2024_coord, xcol = "x", ycol = "y", 
        crs = 4269, grid = FALSE, cex= 1, legend = TRUE, zcol = "Location", 
        col.regions = c("green", "blue", "red", "orange", "lightgrey", "cyan",
                        "darkgreen", "yellow", "gold", "darkgrey", "black",
                        "pink", "brown", "purple", "magenta"),  alpha = 0)
# 2025
mapview(data_2025_coord, xcol = "x", ycol = "y", 
        crs = 4269, grid = FALSE, cex= 1, legend = TRUE, zcol = "Location", 
        col.regions = c("green", "blue", "red", "orange", "lightgrey", "cyan",
                        "darkgreen", "yellow", "gold", "darkgrey", "black",
                        "pink", "brown", "purple", "magenta"),  alpha = 0)

# Kelp Report Segment Map Figure 
# Example: assign colors explicitly to each Location
location_colors <- c(
  "Swanson" = "blue",
  "Crease" = "red",
  "Village" = "green",
  "Gilford_Point" = "orange",
  "Turnour_North" = "cyan",
  "Turnour_South" = "yellow",
  "Gwayasdums" = "purple",
  "Crammer_South" = "pink",
  "Minstrel" = "gold",
  "South_Bonwick" = "darkgreen",
  "Doctor" = "black",
  "Gilford_LowerKnight" = "brown",
  "South_Arrow" = "magenta",
  "Port_Elisabeth" = "darkblue"
)

# Create a friendly version of Location for legend
data_2025_coord <- data_2025_coord %>%
  mutate(Location_friendly = gsub("_", " ", Location))

# make rendered html self-contained
mapviewOptions(fgb = FALSE)

mapviewOptions(viewBounds = list(
  x = c(-126.76, -126.25),  # longitudes
  y = c(50.54, 50.78)       # latitudes
))

segment_map_2025 <- mapview(
  data_2025_coord,
  xcol = "x", ycol = "y",
  crs = 4269, grid = FALSE,
  cex = 2, legend = TRUE,
  zcol = "Location_friendly",
  col.regions = location_colors,  
  alpha = 0,
  layer.name = "Survey Locations",
  legend.title.cex = 0.5,     
  legend.text.cex = 0.5
)
segment_map_2025

mapshot(segment_map_2025, file = "figures/segment_map_2025.png",vwidth = 700, vheight = 600, zoom = 10)

#### Merge both years' dataset =======================
colnames(data_2022_coordinatesXY)
colnames(data_2022_coordinatesXY)[c(7:18)]<- paste(colnames(data_2022_coordinatesXY[,c(7:18)]), "2022", sep="_")

# I inverse the code for presence and absence (0 was kelp presence and 1 kelp absence in the original data)
data_2022_coordinatesXY$kelp_2022<- as.factor(data_2022_coordinatesXY$No.kelp_2022)
levels(data_2022_coordinatesXY$kelp_2022)<- c("1","0") # make sure that nomenclature is the same for both years 

# Merge datasets!
# First merge 2022 and 2023
merged_22_23 <- merge(data_2022_coordinatesXY, data_2023, by = c("Location", "Site"))

# Then merge the result with 2024
merged_data_22_23_24 <- merge(merged_22_23, data_2024, by = c("Location", "Site"))

# Then merge with 2025
merged_data_22_23_24_25 <- merge(merged_data_22_23_24, data_2025, by = c("Location", "Site"))

levels(merged_data_22_23_24_25$kelp_2022)
# [1] "1" "0"
levels(merged_data_22_23_24_25$kelp_2023)
# [1] "2" "0"
levels(merged_data_22_23_24_25$kelp_2024)
# [1] "4" "0"
levels(merged_data_22_23_24_25$kelp_2025)
# [1] "8" "0"

# Sum all dynamics over years to have one dynamic comlumned that shows over time
merged_data_22_23_24_25$kelp_dynamic<- as.numeric(as.character(merged_data_22_23_24_25$kelp_2022)) + 
  as.numeric(as.character(merged_data_22_23_24_25$kelp_2023)) + 
  as.numeric(as.character(merged_data_22_23_24_25$kelp_2024)) + 
  as.numeric(as.character(merged_data_22_23_24_25$kelp_2025))

# Renaming kelp dynamic numbers to actual dynamic
# sum = 0 (0+0+0+0) -> absent 
# sum = 1 (1+0+0+0) -> loss
# sum = 2 (0+2+0+0) -> gain/loss
# sum = 3 (1+2+0+0) -> loss
# sum = 4 (0+0+4+0) -> gain/loss
# sum = 5 (1+0+4+0) -> gain/loss
# sum = 6 (0+2+4+0) -> gain/loss
# sum = 7 (1+2+4+0) -> loss
# sum = 8 (0+0+0+8) -> gain
# sum = 9 (1+0+0+8) -> gain/loss
# sum = 10 (0+2+0+8) -> gain/loss
# sum = 11 (1+2+0+8) -> gain/loss
# sum = 12 (0+0+4+8) -> gain
# sum = 13 (1+0+4+8) -> gain/loss
# sum = 14 (0+2+4+8) -> gain
# sum = 15 (1+2+4+8) -> persistent

merged_data_22_23_24_25$kelp_dynamic <- recode(
  as.character(merged_data_22_23_24_25$kelp_dynamic),
  "0" = "Absent",
  "1" = "Lost in 2023",
  "2" = "Gained in 2023",
  "2" = "Lost in 2024",
  "3" = "Lost in 2024",
  "4" = "Gained in 2024",
  "4" = "Lost in 2025",
  "5" = "Lost in 2023",
  "5" = "Gained in 2024",
  "5" = "Lost in 2025",
  "6" = "Gained in 2023",
  "6" = "Lost in 2025",
  "7" = "Lost in 2025",
  "8" = "Gained in 2025",
  "9" = "Lost in 2023",
  "9" = "Gained in 2025",
  "10" = "Gained in 2023",
  "10" = "Lost in 2024",
  "10" = "Gained in 2025",
  "11" = "Lost in 2024",
  "11" = "Gained in 2025",
  "12" = "Gained in 2024",
  "13" = "Lost in 2023",
  "13" = "Gained in 2024",
  "14" = "Gained in 2023",
  "15" = "Persistent"
)


#-------------------------------------------------------------------------------

# PLOTTING MAPS 

#-------------------------------------------------------------------------------
# Plotting Persistence vs Absence
# filter your data
persistent_subset_data <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic %in% c("Persistent", "Absent"))

# plot only the filtered data
persistent_map <- mapview(
  persistent_subset_data,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic",
  legend = TRUE,
  col.regions = c("Absent" = "red", "Persistent" = "forestgreen"), # colors for these categories
  alpha = 0.8,
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)
# Set center and set zoom
#persistent_map@map <- persistent_map@map %>%
  #leaflet::setView(lng = -126.5, lat = 50.66, zoom = 11.2)

persistent_map

webshot::install_phantomjs()
mapshot(persistent_map, file = "figures/persistence_map.png", vwidth = 700, vheight = 500, zoom = 10)


# LOOKING AT DIFFERENT COMBINATIONS FOR PLOTS ----------------------------------
# Plotting Gain vs Loss vs Gain/Loss

# 2023 DYNAMICS-----------------------------------------------------------------
# filter your data for dynamics in 2023
gain_loss_23_subset <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic %in% c("Gained in 2023", "Lost in 2023"))

# plot only the filtered data
gain_loss_23_map <- mapview(
  gain_loss_23_subset,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic",
  legend = TRUE,
  col.regions = c("Gained in 2023" = "forestgreen", "Lost in 2023" = "red"), # colors for these categories
  alpha = 0.2,
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)

gain_loss_23_map

mapshot(gain_loss_23_map, file = "figures/gain_loss_23.png",vwidth = 700, vheight = 500, zoom = 10)

# 2024 DYNAMICS-----------------------------------------------------------------
# filter your data for dynamics in 2024
gain_loss_24_subset <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic %in% c("Gained in 2024", "Lost in 2024"))

# plot only the filtered data
gain_loss_24_map <- mapview(
  gain_loss_24_subset,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic",
  legend = TRUE,
  col.regions = c("Gained in 2024" = "forestgreen", "Lost in 2024" = "red"), # colors for these categories
  alpha = 0.2,
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)

gain_loss_24_map

mapshot(gain_loss_24_map, file = "figures/gain_loss_24.png",vwidth = 700, vheight = 500, zoom = 10)

# 2025 DYNAMICS-----------------------------------------------------------------
# filter your data for dynamics in 2025
gain_loss_25_subset <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic %in% c("Gained in 2025", "Lost in 2025"))

# plot only the filtered data
gain_loss_25_map <- mapview(
  gain_loss_25_subset,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic",
  legend = TRUE,
  col.regions = c("Gained in 2025" = "forestgreen", "Lost in 2025" = "red"), # colors for these categories
  alpha = 0.2,
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)

gain_loss_25_map

mapshot(gain_loss_25_map, file = "figures/gain_loss_25.png",vwidth = 700, vheight = 500, zoom = 10)


# Arrange maps into one plot ---------------------------------------------------
library(magick)

# Read images
img1 <- image_read("figures/gain_loss_23.png")
img2 <- image_read("figures/gain_loss_24.png")
img3 <- image_read("figures/gain_loss_25.png")

# Label each image
img1 <- image_annotate(img1, "A", size = 300, color = "black", gravity = "NorthWest")
img2 <- image_annotate(img2, "B", size = 300, color = "black", gravity = "NorthWest")
img3 <- image_annotate(img3, "C", size = 300, color = "black", gravity = "NorthWest")

# Combine vertically
combined <- image_append(c(img1, img2, img3), stack = TRUE)

# Save output
image_write(combined, "figures/combined_gain_loss_maps.png")

# ONLY GAINED AND ONLY LOST IN ALL YEARS----------------------------------------
# filter your data for gained dynamics 
gain_subset <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic %in% c("Gained in 2025", "Gained in 2024", "Gained in 2023"))

# plot only the filtered data
gain_map <- mapview(
  gain_subset,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic",
  legend = TRUE,
  col.regions = c("Gained in 2025" = "green", "Gained in 2024" = "blue", "Gained in 2023" = "cyan"), # colors for these categories
  alpha = 0,
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)


gain_map

# filter your data for lost dynamics 
lost_subset <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic %in% c("Lost in 2025", "Lost in 2024", "Lost in 2023"))

# plot only the filtered data
lost_map <- mapview(
  lost_subset,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic",
  legend = TRUE,
  col.regions = c("Lost in 2025" = "red", "Lost in 2024" = "orange", "Lost in 2023" = "pink"), # colors for these categories
  alpha = 0,
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)

lost_map

# Plot maps showing change in dynamics between years??



#-------------------------------------------------------------------------------

# LOOK AT PERCENT GAIN AND LOSS OVER TIME

#-------------------------------------------------------------------------------

## By Location
library(dplyr)
library(ggplot2)
library(forcats)

# Assign Locations to inlet or dynamic region
region_summary <- merged_data_22_23_24_25 %>%
  filter(!Location %in% c("Hanson", "Midsummer")) %>% 
  mutate(merged_data_22_23_24_25, region = case_when(
    Location %in% c("Gilford_Point", "Port_Elisabeth", "Turnour_North", "Turnour_South", 
                    "Gwayasdums", "Crammer_South","Minstrel", "South_Bonwick", "Doctor") ~ "Inlet",
    Location %in% c("Village", "Swanson", "Crease", "Gilford_LowerKnight", "South_Arrow") ~ "Dynamic"
  ))

# make rendered html self-contained
mapviewOptions(fgb = FALSE)

# Plotting a map for the 2 regions 
region_map <- mapview(
  region_summary,
  xcol = "x", ycol = "y",
  crs = 4269, grid = FALSE,
  cex= 2, legend = TRUE, zcol = "region",
  col.regions = c("blue", "red"),  alpha = 0,
  layer.name = "Region")

region_map

mapview::mapshot(region_map, file = "figures/region_map.png", vwidth = 700, vheight = 500, zoom = 10)


# Read images
img4 <- image_read("figures/segment_map_2025.png")
img5 <- image_read("figures/region_map.png")

# Label each image
img4 <- image_annotate(img4, "A", size = 300, color = "black", gravity = "NorthWest")
img5 <- image_annotate(img5, "B", size = 300, color = "black", gravity = "NorthWest")


# Combine vertically
combined <- image_append(c(img4, img5))

# Save output
image_write(combined, "figures/combined_segment_region_maps.png")


# Gain and Loss Percent --------------------------------------------------------
# filter for gain and loss and calculate %
gain_loss_summary <- region_summary %>%
  filter(kelp_dynamic %in% c("Gained in 2025", "Gained in 2024", "Gained in 2023",
                             "Lost in 2025", "Lost in 2024", "Lost in 2023")) %>%                    
  group_by(region, kelp_dynamic) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(total = sum(count),
         percent = (count / total) * 100) %>%
  ungroup() %>%
  mutate(percent_signed = case_when(
    kelp_dynamic %in% c("Gained in 2025", "Gained in 2024", "Gained in 2023") ~  percent,
    kelp_dynamic %in% c("Lost in 2025", "Lost in 2024", "Lost in 2023")       ~ -percent
  ))

# create a column with year 
gain_loss_summary <- gain_loss_summary %>% 
  mutate(year = case_when(
    kelp_dynamic %in% c("Gained in 2025", "Lost in 2025") ~ "2025",
    kelp_dynamic %in% c("Gained in 2024", "Lost in 2024") ~ "2024",
    kelp_dynamic %in% c("Gained in 2023", "Lost in 2023") ~ "2023"
  ))
  
# plot gain and loss %
gain_loss_percent <- ggplot(gain_loss_summary, aes(x = year, y = percent_signed, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +   # zero line
  scale_fill_manual(values = c("Dynamic" = "blue", "Inlet" = "red")) + # custom colors
  labs(
    x = "Year",
    y = "Net Percentage of Segments",
    fill = "Region"
  ) +
  scale_y_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30, 40)) +
  theme_classic()
gain_loss_percent

ggsave("figures/gain_loss_percet.png", plot = gain_loss_percent, width = 6, height = 4, dpi = 300)

#-------------------------------------------------------------------------------

# RUN SOME MODELS

#-------------------------------------------------------------------------------

# Look at the data 
kelp_models <- region_summary %>% 
  mutate(kelp_dynamic = as.factor(kelp_dynamic)) %>% 
  mutate(year = case_when(
    kelp_dynamic %in% c("Gained in 2025", "Lost in 2025") ~ "2025",
    kelp_dynamic %in% c("Gained in 2024", "Lost in 2024") ~ "2024",
    kelp_dynamic %in% c("Gained in 2023", "Lost in 2023") ~ "2023"
  ))
 
#----------------------
# LOOK AT 2023 DYNAMICS
#----------------------

ggplot(kelp_models, aes(x = kelp_2023)) +
  geom_bar()

kelp_models <- 

lm_2023 <- lm(kelp_2023 ~ region, kelp_models)
summary(lm_2023)


