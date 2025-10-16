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
# assign colors explicitly to each Location
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

#### Merge all years' datasets =======================
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

merged_data_22_23_24_25 <- merged_data_22_23_24_25 %>%
  mutate(
    kelp_dynamic_label = case_when(
      kelp_dynamic == 0 ~ "Absent",
      
      # 2023
      kelp_dynamic == 1 & Year_2023 == 2023 ~ "Lost in 2023",
      kelp_dynamic == 2 & Year_2023 == 2023 ~ "Gained in 2023",
      kelp_dynamic == 5 & Year_2023 == 2023 ~ "Lost in 2023",
      kelp_dynamic == 6 & Year_2023 == 2023 ~ "Gained in 2023",
      kelp_dynamic == 9 & Year_2023 == 2023 ~ "Lost in 2023",
      kelp_dynamic == 10 & Year_2023 == 2023 ~ "Gained in 2023",
      kelp_dynamic == 14 & Year_2023 == 2023 ~ "Gained in 2023",
      
      # 2024
      kelp_dynamic == 2 & Year_2024 == 2024 ~ "Lost in 2024",
      kelp_dynamic == 3 & Year_2024 == 2024 ~ "Lost in 2024",
      kelp_dynamic == 4 & Year_2024 == 2024 ~ "Gained in 2024",
      kelp_dynamic == 5 & Year_2024 == 2024 ~ "Gained in 2024",
      kelp_dynamic == 10 & Year_2024 == 2024 ~ "Lost in 2024",
      kelp_dynamic == 11 & Year_2024 == 2024 ~ "Lost in 2024",
      kelp_dynamic == 12 & Year_2024 == 2024 ~ "Gained in 2024",
      kelp_dynamic == 13 & Year_2024 == 2024 ~ "Gained in 2024",
      
      # 2025
      kelp_dynamic == 4 & Year_2025 == 2025 ~ "Lost in 2025",
      kelp_dynamic == 5 & Year_2025 == 2025 ~ "Lost in 2025",
      kelp_dynamic == 6 & Year_2025 == 2025 ~ "Lost in 2025",
      kelp_dynamic == 7 & Year_2025 == 2025 ~ "Lost in 2025",
      kelp_dynamic == 8 & Year_2025 == 2025 ~ "Gained in 2025",
      kelp_dynamic == 9 & Year_2025 == 2025 ~ "Gained in 2025",
      kelp_dynamic == 10 & Year_2025 == 2025 ~ "Gained in 2025",
      kelp_dynamic == 11 & Year_2025 == 2025 ~ "Gained in 2025",
      
      # Persistent
      kelp_dynamic == 15 ~ "Persistent",
      
      TRUE ~ as.character(kelp_dynamic)  # fallback
    )
  )

#-------------------------------------------------------------------------------

# PLOTTING MAPS 

#-------------------------------------------------------------------------------
# Plotting Persistence vs Absence
# filter your data
persistent_subset_data <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic_label %in% c("Persistent", "Absent"))

# plot only the filtered data
persistent_map <- mapview(
  persistent_subset_data,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic_label",
  legend = TRUE,
  col.regions = c("Absent" = "#F4A460", "Persistent" ="#00BFFF" ), # colors for these categories
  alpha = 0.8,
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)

persistent_map

webshot::install_phantomjs()
mapshot(persistent_map, file = "figures/persistence_map.png", vwidth = 700, vheight = 450, zoom = 20)


# LOOKING AT DIFFERENT COMBINATIONS FOR PLOTS ----------------------------------
# Plotting Gain vs Loss vs Gain/Loss

# 2023 DYNAMICS-----------------------------------------------------------------
# filter your data for dynamics in 2023
gain_loss_23_subset <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic_label %in% c("Gained in 2023", 
                                   "Lost in 2023", 
                                   "Persistent", 
                                   "Absent")) %>%
  mutate(kelp_dynamic_label = factor(
    kelp_dynamic_label,
    levels = c("Gained in 2023", "Lost in 2023", "Persistent", "Absent")
  ))

# plot only the filtered data
gain_loss_23_map <- mapview(
  gain_loss_23_subset,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic_label",
  legend = TRUE,
  col.regions = c("Gained in 2023" = "forestgreen", "Lost in 2023" = "red", "Persistent"  = "#00BFFF", "Absent" = "#F4A460"), # colors for these categories
  alpha = 0.2,
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)

gain_loss_23_map

mapshot(gain_loss_23_map, file = "figures/gain_loss_23.png",vwidth = 700, vheight = 450, zoom = 20)

## Trying to plot persistent and absent as more see through
# Prepare data with fill colors and alpha
gain_loss_23_subset_new <- gain_loss_23_subset %>%
  mutate(
    fill_color = case_when(
      kelp_dynamic_label == "Gained in 2023" ~ "forestgreen",
      kelp_dynamic_label == "Lost in 2023" ~ "red",
      kelp_dynamic_label == "Persistent" ~ "#00BFFF",
      kelp_dynamic_label == "Absent" ~ "#F4A460"
    ),
    fill_alpha = case_when(
      kelp_dynamic_label %in% c("Persistent", "Absent") ~ 0.2,  # faint
      TRUE ~ 1   # solid
    )
  )

# Create map
gain_loss_map_23_new <- leaflet(gain_loss_23_subset_new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    ~x, ~y,
    radius = 3,
    fillColor = ~fill_color,
    color = ~fill_color,     # match fill for no outline effect
    weight = 0,              # no stroke
    fillOpacity = ~fill_alpha,
    label = ~kelp_dynamic_label
  ) %>%
  addLegend(
    "topright",
    colors = c("forestgreen", "red", "#00BFFF", "#F4A460"),
    labels = c("Gained in 2023", "Lost in 2023", "Persistent", "Absent"),
    title = "Kelp Dynamics", opacity = 1
  )

gain_loss_map_23_new

mapshot(gain_loss_map_23_new, file = "figures/gain_loss_23_new.png",vwidth = 700, vheight = 450, zoom = 20)

# 2024 DYNAMICS-----------------------------------------------------------------
# filter your data for dynamics in 2024
gain_loss_24_subset <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic_label %in% c("Gained in 2024", 
                                   "Lost in 2024", 
                                   "Persistent", 
                                   "Absent")) %>%
  mutate(kelp_dynamic_label = factor(
    kelp_dynamic_label,
    levels = c("Gained in 2024", "Lost in 2024", "Persistent", "Absent")
  ))

# plot only the filtered data
gain_loss_24_map <- mapview(
  gain_loss_24_subset,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic_label",
  legend = TRUE,
  col.regions = c("Gained in 2024" = "forestgreen", "Lost in 2024" = "red", "Persistent"  = "#00BFFF", "Absent" = "#F4A460"),  alpha = 0.2,
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)

gain_loss_24_map

mapshot(gain_loss_24_map, file = "figures/gain_loss_24.png",vwidth = 700, vheight = 450, zoom = 20)

## Trying to plot persistent and absent as more see through
# Prepare data with fill colors and alpha
gain_loss_24_subset_new <- gain_loss_24_subset %>%
  mutate(
    fill_color = case_when(
      kelp_dynamic_label == "Gained in 2024" ~ "forestgreen",
      kelp_dynamic_label == "Lost in 2024" ~ "red",
      kelp_dynamic_label == "Persistent" ~ "#00BFFF",
      kelp_dynamic_label == "Absent" ~ "#F4A460"
    ),
    fill_alpha = case_when(
      kelp_dynamic_label %in% c("Persistent", "Absent") ~ 0.2,  # faint
      TRUE ~ 1   # solid
    )
  )

# Create map
gain_loss_map_24_new <- leaflet(gain_loss_24_subset_new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    ~x, ~y,
    radius = 3,
    fillColor = ~fill_color,
    color = ~fill_color,     # match fill for no outline effect
    weight = 0,              # no stroke
    fillOpacity = ~fill_alpha,
    label = ~kelp_dynamic_label
  ) %>%
  addLegend(
    "topright",
    colors = c("forestgreen", "red", "#00BFFF", "#F4A460"),
    labels = c("Gained in 2024", "Lost in 2024", "Persistent", "Absent"),
    title = "Kelp Dynamics", opacity = 1
  )

gain_loss_map_24_new

mapshot(gain_loss_map_24_new, file = "figures/gain_loss_24_new.png",vwidth = 700, vheight = 450, zoom = 20)


# 2025 DYNAMICS-----------------------------------------------------------------
# filter your data for dynamics in 2025
gain_loss_25_subset <- merged_data_22_23_24_25 %>%
  filter(kelp_dynamic_label %in% c("Gained in 2025", 
                                   "Lost in 2025", 
                                   "Persistent", 
                                   "Absent")) %>%
  mutate(kelp_dynamic_label = factor(
    kelp_dynamic_label,
    levels = c("Gained in 2025", "Lost in 2025", "Persistent", "Absent")
  ))

# plot only the filtered data
gain_loss_25_map <- mapview(
  gain_loss_25_subset,
  xcol = "x", ycol = "y",
  crs = 4269,
  grid = FALSE,
  cex = 2,
  zcol = "kelp_dynamic_label",
  legend = TRUE,
  col.regions = c("Gained in 2025" = "forestgreen", "Lost in 2025" = "red", "Persistent"  = "#00BFFF", "Absent" = "#F4A460"),  alpha = 0.2, 
  lwd = 0,
  col = NA,
  layer.name = "Kelp Dynamics"
)

gain_loss_25_map

mapshot(gain_loss_25_map, file = "figures/gain_loss_25.png",vwidth = 700, vheight = 0, zoom = 20)

## Trying to plot persistent and absent as more see through
# Prepare data with fill colors and alpha
gain_loss_25_subset_new <- gain_loss_25_subset %>%
  mutate(
    fill_color = case_when(
      kelp_dynamic_label == "Gained in 2025" ~ "forestgreen",
      kelp_dynamic_label == "Lost in 2025" ~ "red",
      kelp_dynamic_label == "Persistent" ~ "#00BFFF",
      kelp_dynamic_label == "Absent" ~ "#F4A460"
    ),
    fill_alpha = case_when(
      kelp_dynamic_label %in% c("Persistent", "Absent") ~ 0.2,  # faint
      TRUE ~ 1   # solid
    )
  )

# Create map
gain_loss_map_25_new <- leaflet(gain_loss_25_subset_new) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    ~x, ~y,
    radius = 3,
    fillColor = ~fill_color,
    color = ~fill_color,     # match fill for no outline effect
    weight = 0,              # no stroke
    fillOpacity = ~fill_alpha,
    label = ~kelp_dynamic_label
  ) %>%
  addLegend(
    "topright",
    colors = c("forestgreen", "red", "#00BFFF", "#F4A460"),
    labels = c("Gained in 2025", "Lost in 2025", "Persistent", "Absent"),
    title = "Kelp Dynamics", opacity = 1
  )

gain_loss_map_25_new

mapshot(gain_loss_map_25_new, file = "figures/gain_loss_25_new.png",vwidth = 700, vheight = 450, zoom = 20)


# Arrange maps into one plot ---------------------------------------------------
library(magick)

# Read images
img1 <- image_read("figures/gain_loss_23_new.png")
img2 <- image_read("figures/gain_loss_24_new.png")
img3 <- image_read("figures/gain_loss_25_new.png")

# Label each image
img1 <- image_annotate(img1, "A", size = 700, color = "black", gravity = "NorthWest")
img2 <- image_annotate(img2, "B", size = 700, color = "black", gravity = "NorthWest")
img3 <- image_annotate(img3, "C", size = 700, color = "black", gravity = "NorthWest")

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

# Assign Locations to regions
region_summary <- merged_data_22_23_24_25 %>%
  filter(!Location %in% c("Hanson", "Midsummer")) %>% 
  mutate(merged_data_22_23_24_25, region = case_when(
    Location %in% c("Crammer_South", "South_Arrow",  "Gwayasdums", "South_Bonwick") ~ "1",
     Location %in% c("Swanson", "Crease", "Gilford_LowerKnight") ~ "2",
      Location %in% c("Village", "Gilford_Point", "Port_Elisabeth", "Turnour_North", "Turnour_South") ~ "3",
        Location %in% c("Minstrel", "Doctor") ~ "4"
  ))

# Read in logger locations 
logger_locations <- read_csv("data/logger_locations.csv") %>%
  mutate(site_id = ifelse(site_id == "BATI12 (BATI14)", "BATI12", site_id)) %>%
  filter(!site_id %in% c("SC5", "SC6", "SC21", "SC7", "SC20", "BATI29", "SC11", "BATI2")) %>%
  mutate(region = case_when(
    site_id %in% c("SC3", "SC18", "SC17") ~ "1",
    site_id %in% c("SC4", "BATI12", "BATI3") ~ "2",
    site_id %in% c("SC1", "SC2", "BATI5") ~ "3",
    site_id %in% c("SC9", "BATI9", "SC10") ~ "4"
  ))

# Split labels for top/bottom
top_labels <- logger_locations %>% filter(site_id != "BATI5")
bottom_label <- logger_locations %>% filter(site_id == "BATI5")

region_cols <- c(
  "1" = "#66B2FF",  # bright pastel blue
  "2" = "#FF6666",  # bright pastel red
  "3" = "lightgreen",  # bright pastel green
  "4" = "#FFB266"   # bright pastel orange
)

pal <- colorFactor(palette = region_cols, domain = c(region_summary$region, logger_locations$region))

pal_logger <- colorFactor(palette = region_cols, domain = logger_locations$region)

# Make a map with both logger locations and surveys by region
survey_logger_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # Survey points: large, semi-transparent
  addCircleMarkers(
    data = region_summary,
    ~x, ~y,
    radius = 3,
    color = ~pal(region),
    fillOpacity = 0.25,
    stroke = FALSE,
    label = ~region
  ) %>%
  
  # Logger points: smaller, solid, black outline
  addCircleMarkers(
    data = logger_locations,
    ~lon, ~lat,
    radius = 5,
    fillColor = ~pal(region),  # region color inside
    color = "black",           # black outline
    weight = 2,                # outline thickness
    fillOpacity = 1
  ) %>%
  
  # Logger labels
  addLabelOnlyMarkers(
    data = top_labels,
    ~lon, ~lat,
    label = ~site_id,
    labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE,
                                style = list("color" = "black", "font-size" = "14px", "padding" = "2px"))
  ) %>%
  addLabelOnlyMarkers(
    data = bottom_label,
    ~lon, ~lat,
    label = ~site_id,
    labelOptions = labelOptions(noHide = TRUE, direction = "bottom", textOnly = TRUE,
                                style = list("color" = "black", "font-size" = "14px", "padding" = "2px"))
  ) %>%
  
  # Legend
  addLegend(
    "topright",
    pal = pal_logger,
    values = logger_locations$region,
    title = "Region", 
    opacity = 1
  )

survey_logger_map

mapview::mapshot(survey_logger_map, file = "figures/survey_logger_map.png", vwidth = 860, vheight = 500, zoom = 20)


# Gain and Loss Percent --------------------------------------------------------
# filter for gain and loss and calculate %
gain_loss_summary <- region_summary %>%
  filter(kelp_dynamic_label %in% c("Gained in 2025", "Gained in 2024", "Gained in 2023",
                             "Lost in 2025", "Lost in 2024", "Lost in 2023")) %>%                    
  group_by(region, kelp_dynamic_label) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(total = sum(count),
         percent = (count / total) * 100) %>%
  ungroup() %>%
  mutate(percent_signed = case_when(
    kelp_dynamic_label %in% c("Gained in 2025", "Gained in 2024", "Gained in 2023") ~  percent,
    kelp_dynamic_label %in% c("Lost in 2025", "Lost in 2024", "Lost in 2023")       ~ -percent
  ))

# create a column with year 
gain_loss_summary <- gain_loss_summary %>% 
  mutate(year = case_when(
    kelp_dynamic_label %in% c("Gained in 2025", "Lost in 2025") ~ "2025",
    kelp_dynamic_label %in% c("Gained in 2024", "Lost in 2024") ~ "2024",
    kelp_dynamic_label %in% c("Gained in 2023", "Lost in 2023") ~ "2023"
  ))

# Make a table of percents 
gain_loss_yearly <- gain_loss_summary %>%
  group_by(year) %>%
  summarise(
    total_gain = sum(count[kelp_dynamic_label %in% c("Gained in 2025","Gained in 2024","Gained in 2023")]),
    total_loss = sum(count[kelp_dynamic_label %in% c("Lost in 2025","Lost in 2024","Lost in 2023")]),
    .groups = "drop"
  ) %>%
  mutate(
    total_segments = total_gain + total_loss,
    percent_gain = (total_gain / total_segments) * 100,
    percent_loss = (total_loss / total_segments) * 100
  )

# plot gain and loss %
gain_loss_percent <- ggplot(gain_loss_summary, aes(x = year, y = percent_signed, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +   # zero line
  scale_fill_manual(values = c(region_cols)) + # custom colors
  labs(
    x = "Year",
    y = "Net Percentage of Segments",
    fill = "Region"
  ) +
  scale_y_continuous(breaks = c(-60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)) +
  theme_classic()
gain_loss_percent

ggsave("figures/gain_loss_percet.png", plot = gain_loss_percent, width = 6, height = 4, dpi = 300)

#-------------------------------------------------------------------------------

# LOOKING AT DYNAMICS AND MAX ANNUAL TEMP

#-------------------------------------------------------------------------------

source("code/envlogger_temp.R")

# Making a df with kelp presence in each year
library(dplyr)
library(tidyr)

kelp_presence <- region_summary %>%
  # Keep only the region and yearly kelp columns
  select(region, kelp_2022, kelp_2023, kelp_2024, kelp_2025) %>%
  
  
  # Pivot the yearly kelp columns into two columns: 'year' and 'kelp_presence'
  pivot_longer(
    cols = starts_with("kelp_"),   
    names_to = "year",             
    values_to = "kelp_presence",   
    values_drop_na = TRUE          
  ) %>%
  
  # Convert year to numeric
  mutate(year = as.integer(sub("kelp_", "", year))) %>%
  
  # Convert kelp_presence to numeric and recode to 1/0
  mutate(
    kelp_presence = as.numeric(as.character(kelp_presence)),  
    kelp_presence = ifelse(kelp_presence > 0, 1, 0)           
  ) 

  # Calculate percent presence by region and year
kelp_presence_summary <- kelp_presence %>%
  group_by(year, region, kelp_presence) %>%    
  summarise(count = n(), .groups = "drop") # count segments by presence

kelp_presence_summary <- kelp_presence_summary %>% 
  group_by(year, region) %>%                     # group by year and region for total
  mutate(total_segments = sum(count)) %>% # sum over 0 and 1 to get total
  mutate(percent = (count / total_segments) * 100) %>%
  ungroup()

kelp_presence_summary <- kelp_presence_summary %>% 
  filter(kelp_presence == 1)

# Combine daily temperature data with yearly kelp presence
kelp_presence_temp <- all_data_high_tide_region_avg %>%
  ungroup() %>%                  # remove any grouping
  mutate(year = as.integer(format(date, "%Y")))

# calculate the max temps for each year
temp_summary <- all_data_high_tide_region_avg %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%   # extract year
  group_by(year, region) %>%
  summarise(
    max_temp = max(avg_temp, na.rm = TRUE),           # maximum temperature
    p90_temp  = quantile(avg_temp, 0.9, na.rm = TRUE), # 90th percentile temperature
    .groups = "drop"
  )
  
  # Left join with kelp presence data by 'year' and 'region'
  # This replicates the yearly kelp presence across all daily measurements
kelp_presence_temp <- kelp_presence_summary %>%  # your yearly percent presence data
  left_join(temp_summary, by = c("year", "region"))

# plotting percent presence with temp
ggplot(kelp_presence_temp, aes(x = max_temp, y = percent, colour = region)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = "Yearly Maximum Temperature (°C)",
    y = "Percent Kelp Presence") +
  scale_color_manual(name = "Region",
                     values = c(region_cols)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("figures/presence_temp.png", plot = last_plot(), width = 12, height = 9, dpi = 300)

# plotting percent presence over time
ggplot(kelp_presence_temp, aes(x = year, y = percent, colour = region)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = "",
    y = "Percent Kelp Presence") +
  scale_color_manual(name = "Region",
                     values = c(region_cols)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("figures/presence_time.png", plot = last_plot(), width = 12, height = 9, dpi = 300)


# plotting max temp over time 
ggplot(kelp_presence_temp, aes(x = year, y = max_temp, colour = region)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = "",
    y = "Yearly Maximum Temperature (°C)") +
  scale_color_manual(name = "Region",
                     values = c(region_cols))

ggsave("figures/temp_time.png", plot = last_plot(), width = 12, height = 9, dpi = 300)


# ------------------------------------------------------------------------------

# TAKE A LOOK AT SOME MODELS FOR KELP PRESENCE AND MAX YEARLY TEMP

# ------------------------------------------------------------------------------
# Scale only the predictor
kelp_presence_scaled <- kelp_presence_temp %>%
  mutate(max_temp_scaled = scale(max_temp))

# Fit the linear model using scaled temperature but original percent
lm_scaled <- lm(percent ~ max_temp_scaled, data = kelp_presence_scaled)
summary(lm_scaled)

# checking for normally distributed residuals with hist
lm_scaled$lm_scaled_resids <-resid(lm_scaled) 

hist(lm_scaled$lm_scaled_resids) 

# look again with QQ Plot
library(car)
qqPlot(lm_scaled$lm_scaled_resids) 

# checking for relationships between residuals and predictors
residualPlot(lm_scaled, tests = FALSE) 
# Plot
library(dplyr)
library(ggplot2)

# Generate predictions with confidence intervals
kelp_presence_scaled <- kelp_presence_scaled %>%
  # predict with interval = "confidence"
  mutate(
    fit = predict(lm_scaled, newdata = kelp_presence_scaled, interval = "confidence")[, "fit"],
    lwr = predict(lm_scaled, newdata = kelp_presence_scaled, interval = "confidence")[, "lwr"],
    upr = predict(lm_scaled, newdata = kelp_presence_scaled, interval = "confidence")[, "upr"]
  )

# For showing the p value in the plot 
p_val <- summary(lm_scaled)$coefficients["max_temp_scaled", "Pr(>|t|)"]
p_label <- paste0("p = ", signif(p_val, 3))

# Plot with model 
lm_maxtemp_presence <- ggplot(kelp_presence_scaled, aes(x = max_temp, y = percent)) +
  geom_point(size = 3, alpha = 0.8, aes(colour = region)) +  # actual data
  geom_line(aes(y = fit), size = 1, alpha = 0.8) +  # regression line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1, color = NA) +  # CI shaded
  annotate("text",
           x = Inf,           # right edge
           y = Inf,           # top edge
           label = p_label,
           hjust = 1.1,       # slightly inset from right
           vjust = 3.1,       # slightly inset from top
           size = 5) +
   theme_classic() +
  #facet_wrap(~ region, scales = "free_y", ncol = 1) +
  labs(
    x = "Maximum Annual Temperature (°C)",
    y = "Kelp Presence (%)",
    color = "Region",
    fill = "Region"
  ) +
  scale_color_manual(values = c(region_cols)) +
  scale_fill_manual(values = c(region_cols)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(
    axis.title.x = element_text(size = 16),  # x-axis label
    axis.title.y = element_text(size = 16),  # y-axis label
    axis.text.x  = element_text(size = 14),  # x-axis tick labels
    axis.text.y  = element_text(size = 14),   # y-axis tick labels
    legend.title = element_text(size = 16),  # legend title font size
    legend.text = element_text(size = 14)    # legend item font size
  ) +
  theme(strip.text = element_text(size = 14))  # increase facet label size

lm_maxtemp_presence

ggsave("figures/lm_presence_maxtemp.png", plot = lm_maxtemp_presence, width = 9, height = 6, dpi = 300)

library(ggpubr)  # for stat_regline_equation and stat_cor

lm_maxtemp_presence <- ggplot(kelp_presence_scaled, aes(x = max_temp, y = percent)) +
  geom_point(size = 3, alpha = 0.8, aes(colour = region)) +  
  geom_line(aes(y = fit), size = 1, alpha = 0.8) +  
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1, color = NA) +  
  theme_classic() +
  labs(
    x = "Maximum Annual Temperature (°C)",
    y = "Kelp Presence (%)",
    color = "Region",
    fill = "Region"
  ) +
  scale_color_manual(values = c(region_cols)) +
  scale_fill_manual(values = c(region_cols)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  theme(strip.text = element_text(size = 14)) +
  # Add p-value and equation
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.r.squared.., sep = "~~~")), 
                        label.x.npc = "left", label.y.npc = 0.95, size = 5) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x.npc = "left", label.y.npc = 0.9, size = 5)

lm_maxtemp_presence
# ------------------------------------------------------------------------------

# NOW LOOK AT KELP PRESENCE AND MEAN ANNUAL TEMP, SUMMER, AND SPRING TEMPS

# ------------------------------------------------------------------------------

# MEAN ANNUAL ------------------------------------------------------------------
mean_annual_temp <- all_data_high_tide_region_avg %>% 
  mutate(year = as.integer(format(date, "%Y"))) %>%   
  group_by(year, region) %>%
  summarise(
    mean_temp = weighted.mean(avg_temp, w = rep(1, n()), na.rm = TRUE),
    .groups = "drop"
  )

kelp_mean_temp <- kelp_presence_summary %>%  # your yearly percent presence data
  left_join(mean_annual_temp, by = c("year", "region"))

# look at what it looks like
ggplot(kelp_mean_temp, aes(x = mean_temp, y = percent, colour = region)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = "Weighted Mean Annual Temperature (°C)",
    y = "Percent Kelp Presence") +
  scale_color_manual(name = "Region",
                     values = c(region_cols)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("figures/presence_annual_temp.png", plot = last_plot(), width = 12, height =9, dpi = 300)

# MEAN SPRING (MAY TO JUNE) ----------------------------------------------------
mean_spring_temp <- all_data_high_tide_region_avg %>%
  filter(month(date) %in% 5:6) %>%
  mutate(
    year = year(date),              
    month = month(date)             
  ) %>%
  group_by(year, region) %>%
  summarise(
    mean_spring_temp = weighted.mean(avg_temp, w = rep(1, n()), na.rm = TRUE),
    .groups = "drop"
  )

kelp_mean_spring_temp <- kelp_presence_summary %>%  # your yearly percent presence data
  left_join(mean_spring_temp, by = c("year", "region")) %>% 
  drop_na(mean_spring_temp)

# look at what it looks like
ggplot(kelp_mean_spring_temp, aes(x = mean_spring_temp, y = percent, colour = region)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = "Weighted Mean Spring Temperature (°C)",
    y = "Percent Kelp Presence") +
  scale_color_manual(name = "Region",
                     values = c(region_cols)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("figures/presence_spring_temp.png", plot = last_plot(), width = 12, height =9, dpi = 300)

# MEAN SUMMER (JULY TO AUGUST) -------------------------------------------------
mean_summer_temp <- all_data_high_tide_region_avg %>%
  filter(month(date) %in% 7:8) %>%
  mutate(
    year = year(date),              
    month = month(date)             
  ) %>%
  group_by(year, region) %>%
  summarise(
    mean_summer_temp = weighted.mean(avg_temp, w = rep(1, n()), na.rm = TRUE),
    .groups = "drop"
  )

kelp_mean_summer_temp <- kelp_presence_summary %>%  # your yearly percent presence data
  left_join(mean_summer_temp, by = c("year", "region")) %>% 
  drop_na(mean_summer_temp)

# look at what it looks like
ggplot(kelp_mean_summer_temp, aes(x = mean_summer_temp, y = percent, colour = region)) +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Percent Kelp Presence") +
  scale_color_manual(name = "Region",
                     values = c(region_cols)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("figures/presnce_summer_temp.png", plot = last_plot(), width = 12, height =9, dpi = 300)

# ------------------------------------------------------------------------------

# MODEL PRESENCE WITH MEAN ANNUAL TEMP, SUMMER, AND SPRING TEMPS

# ------------------------------------------------------------------------------

# SPRING -----------------------------------------------------------------------
# Scale only the predictor
kelp_spring_scaled <- kelp_mean_spring_temp %>%
  mutate(mean_spring_temp_scaled = scale(mean_spring_temp))

# Fit the linear model using scaled temperature but original percent
lm_spring_scaled <- lm(percent ~ mean_spring_temp_scaled, data = kelp_spring_scaled)
summary(lm_spring_scaled)

# checking for normally distributed residuals with hist
lm_spring_scaled$lm_spring_scaled_resids <-resid(lm_spring_scaled) 

hist(lm_spring_scaled$lm_spring_scaled_resids) 

# look again with QQ Plot
qqPlot(lm_spring_scaled$lm_spring_scaled_resids) 

# checking for relationships between residuals and predictors
residualPlot(lm_spring_scaled, tests = FALSE) 

# Generate predictions with confidence intervals
kelp_spring_scaled <- kelp_spring_scaled %>%
  # predict with interval = "confidence"
  mutate(
    fit = predict(lm_spring_scaled, newdata = kelp_spring_scaled, interval = "confidence")[, "fit"],
    lwr = predict(lm_spring_scaled, newdata = kelp_spring_scaled, interval = "confidence")[, "lwr"],
    upr = predict(lm_spring_scaled, newdata = kelp_spring_scaled, interval = "confidence")[, "upr"]
  )

# Plot with model 
spring_kelp_plot <- ggplot(kelp_spring_scaled, aes(x = mean_spring_temp, y = percent)) +
  geom_point(size = 3, aes(colour = region)) +  # actual data
  geom_line(aes(y = fit), size = 1) +  # regression line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +  # CI shaded
  theme_classic() +
  labs(
    x = "Mean Spring Temperature (°C)",
    y = "Kelp Presence (%)",
    color = "Region",
    fill = "Region"
  ) +
  scale_color_manual(values = c(region_cols)) +
  scale_fill_manual(values = c(region_cols)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(
    axis.title.x = element_text(size = 16),  # x-axis label
    axis.title.y = element_text(size = 16),  # y-axis label
    axis.text.x  = element_text(size = 14),  # x-axis tick labels
    axis.text.y  = element_text(size = 14),   # y-axis tick labels
    legend.title = element_text(size = 16),  # legend title font size
    legend.text = element_text(size = 14)    # legend item font size
  )

spring_kelp_plot

ggsave("figures/lm_spring_temp.png", plot = last_plot(), width = 9, height = 6, dpi = 300)

# SUMMER -----------------------------------------------------------------------
# Scale only the predictor
kelp_summer_scaled <- kelp_mean_summer_temp %>%
  mutate(mean_summer_temp_scaled = scale(mean_summer_temp))

# Fit the linear model using scaled temperature but original percent
lm_summer_scaled <- lm(percent ~ mean_summer_temp_scaled, data = kelp_summer_scaled)
summary(lm_summer_scaled)

# checking for normally distributed residuals with hist
lm_summer_scaled$lm_summer_scaled_resids <-resid(lm_summer_scaled) 

hist(lm_summer_scaled$lm_summer_scaled_resids) 

# look again with QQ Plot
qqPlot(lm_summer_scaled$lm_summer_scaled_resids) 

# checking for relationships between residuals and predictors
residualPlot(lm_summer_scaled, tests = FALSE) 

# Generate predictions with confidence intervals
kelp_summer_scaled <- kelp_summer_scaled %>%
  # predict with interval = "confidence"
  mutate(
    fit = predict(lm_summer_scaled, newdata = kelp_summer_scaled, interval = "confidence")[, "fit"],
    lwr = predict(lm_summer_scaled, newdata = kelp_summer_scaled, interval = "confidence")[, "lwr"],
    upr = predict(lm_summer_scaled, newdata = kelp_summer_scaled, interval = "confidence")[, "upr"]
  )

# Plot with model 
summer_kelp_plot <- ggplot(kelp_summer_scaled, aes(x = mean_summer_temp, y = percent)) +
  geom_point(size = 3, aes(colour = region)) +  # actual data
  geom_line(aes(y = fit), size = 1) +  # regression line
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, color = NA) +  # CI shaded
  theme_classic() +
  labs(
    x = "Mean Summer Temperature (°C)",
    y = "Kelp Presence (%)",
    color = "Region",
    fill = "Region"
  ) +
  scale_color_manual(values = c(region_cols)) +
  scale_fill_manual(values = c(region_cols)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(
    axis.title.x = element_text(size = 16),  # x-axis label
    axis.title.y = element_text(size = 16),  # y-axis label
    axis.text.x  = element_text(size = 14),  # x-axis tick labels
    axis.text.y  = element_text(size = 14),   # y-axis tick labels
    legend.title = element_text(size = 16),  # legend title font size
    legend.text = element_text(size = 14)    # legend item font size
  )

summer_kelp_plot

ggsave("figures/lm_summer_temp.png", plot = last_plot(), width = 9, height = 6, dpi = 300)



# Remove y axis and move right plot slightly to the right
summer_kelp_plot_no_y <- summer_kelp_plot +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0, 0, 0.3, 2), "lines")  # top, right, bottom, left
  )

spring_kelp_plot <- spring_kelp_plot +
  theme(
    axis.title = element_text(size = 18),       # axis titles
    axis.text = element_text(size = 16),        # axis labels
    legend.title = element_text(size = 18),     # legend title
    legend.text = element_text(size = 16)       # legend items
  )

summer_kelp_plot_no_y <- summer_kelp_plot_no_y +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16)
  )

# Combine plots
library(ggpubr)
lm_spring_summer_plot <- ggarrange(
  spring_kelp_plot,
  summer_kelp_plot_no_y,
  ncol = 2,
  nrow = 1,
  common.legend = TRUE,
  legend = "right",
  labels = c("A", "B"),
  font.label = list(size = 20, face = "bold")
)
lm_spring_summer_plot
ggsave("figures/lm_spring_summer.png", plot = lm_spring_summer_plot , width = 14, height = 7, dpi = 300)

### SPRING VS SUMMER MODELS ###
kelp_spring_scaled <- kelp_spring_scaled %>%
  mutate(
    fit = predict(lm_spring_scaled, newdata = ., interval = "confidence")[, "fit"],
    lwr = predict(lm_spring_scaled, newdata = ., interval = "confidence")[, "lwr"],
    upr = predict(lm_spring_scaled, newdata = ., interval = "confidence")[, "upr"],
    season = "Spring"
  ) %>%
  rename(temp_scaled = mean_spring_temp_scaled)

kelp_summer_scaled <- kelp_summer_scaled %>%
  mutate(
    fit = predict(lm_summer_scaled, newdata = ., interval = "confidence")[, "fit"],
    lwr = predict(lm_summer_scaled, newdata = ., interval = "confidence")[, "lwr"],
    upr = predict(lm_summer_scaled, newdata = ., interval = "confidence")[, "upr"],
    season = "Summer"
  ) %>%
  rename(temp_scaled = mean_summer_temp_scaled)

# Combine
kelp_both <- bind_rows(kelp_spring_scaled, kelp_summer_scaled)

# Plot
ggplot(kelp_both, aes(x = temp_scaled, y = percent, color = region)) +
  geom_point() +
  geom_line(aes(y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = region), alpha = 0.2, color = NA) +
  facet_wrap(~season) +
  labs(x = "Scaled Temperature", y = "Kelp Percent Presence") +
  theme_classic() +
  scale_color_manual(values = c("Dynamic" = "blue", "Inlet" = "red")) +
  scale_fill_manual(values = c("Dynamic" = "blue", "Inlet" = "red")) 

# ------------------------------------------------------------------------------

# LOOKING AT MEAN TEMPERATURES OVER TIME

# ------------------------------------------------------------------------------
