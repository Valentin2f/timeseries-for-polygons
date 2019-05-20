library(rgdal)
library(spdplyr)
library(raster)
library(tidyverse)
library(lubridate)
library(sf)



##### Data processing and creation of time series #####


##functions##

#function extracts gregorian date from name of modis file (refers to the file name accessed from appEEARS)
modis_date <- function (x) {
  temp <- gsub("^.*doy", "", x)
  temp <- gsub("_aid.*$", "", temp)
  year <- gsub("([0-9]{4})([0-9]{3})", "\\1", temp)
  origin <- ymd(paste0(year, "-01-01"))
  julian <- gsub("([0-9]{4})([0-9]{3})", "\\2", temp)
  date <- origin + (as.numeric(julian)-1)
  return(date)
}


#load raster
load_raster <- function (rasterlist){
  #creates tibble with date and additional name to modis files
  temp_raster <- as_tibble(rasterlist) %>%
    filter(grepl(".tif$", value)) %>%
    rename(path = value) %>%
    mutate(date = modis_date(path),
           obj_name = paste0("ndvi_", as.character(date)))
  
  #open rasters in lists
  rasters_indic <-list()
  for (i in 1:nrow(temp_raster)){
    temp <- raster(temp_raster$path[i])
    rasters_indic[[i]] <- temp
  }
  return(rasters_indic)
}

#load and reproject shape
load_repr_shape <- function(shape,rasterlist){
  directory <- getwd()
  shp_temp <- readOGR(dsn=path.expand(directory), layer = shape )
  #reproject crs of shape
  shp_temp <- spTransform(shp_temp,crs(rasterlist[[1]]))
  return(shp_temp)
}

crop_mask_stack <-function(rasterlist,shape){
  #crop and mask raster from list
  aoi_raster_indicator <- list()
  for (i in 1:length(rasterlist)){
    temp <- crop(rasterlist[[i]],shape)
    temp <- mask(temp,shape)
    aoi_raster_indicator[[i]] <- temp
  }
  # create a stack
  aoi_raster_indicator <- stack(aoi_raster_indicator)
  
  #rescale ndvi values
  aoi_raster_indicator <- aoi_raster_indicator/10000
  return(aoi_raster_indicator)
}


get_df_time_series <- function(rasterlist){
  
  #create vector with average ndvi for each raster
  avg_indicator <- cellStats(rasterlist,mean)
  
  #create data.frame from list
  avg_indicator_df <- as.data.frame(avg_indicator)
  names(avg_indicator_df) <- "meanNDVI"
  
  #extract years of acquisition
  date_ndvi <- modis_date(row.names(avg_indicator_df))
  
  avg_indicator_df$date <- date_ndvi
  return(avg_indicator_df)
}


plot_time_series <- function(df, title, subtitle){
  #plot mean ndvi values
  ggplot(df, aes(date,meanNDVI))+
    geom_line()+
    ggtitle(title, subtitle = subtitle)+
    xlab("Year") + ylab("Average NDVI")+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold"))+
    theme(plot.subtitle = element_text(hjust = 0.5, vjust = 1))+
    scale_x_date(breaks = "years", date_labels = "'%y", minor_breaks = "years")
}

##combine functions, except graph plotting

NDVI_analysis <- function(rasterlist, shape){
  ndvi_2000_2018 <- load_raster(rasterlist)
  aoi_boundary <-load_repr_shape(shape,ndvi_2000_2018)
  cms_ndvi_2000_2018 <- crop_mask_stack(ndvi_2000_2018,aoi_boundary)
  ndvi_df <- get_df_time_series(cms_ndvi_2000_2018)
  
}
