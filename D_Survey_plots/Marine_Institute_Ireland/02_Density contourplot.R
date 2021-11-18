# Input:    - ggin historical data (krigged densities). One file per year

# Output:   - density contourplot

## There are some sections you need to change depending upon your file names: ##
## search for the word CHANGE ##


# Setup----
rm(list = ls())
gc()

# Set up FU.
## CHANGE fu name, lat and long *********************************************************###
fu.name <- "fu2021"
latlimits <- c(49.505, 51.1) 
longlimits <- c(-9.495, -6.7)
###**************************************************************************************###

# Set up directories

## CHANGE THE DIRECTORIES if needed *****************************************************###
getwd()
wdir <- "/D_Survey_plots/Marine_Institute_Ireland"
input.dir_1 <- paste0(getwd(), wdir, "/input_1/")
input.dir_2 <- paste0(getwd(), wdir, "/input_2/")
output.dir <- paste0(getwd(), wdir,"/output/")
###**************************************************************************************###
# Create folders if they don't exist
if(!dir.exists(input.dir_1)) { dir.create(input.dir_1) }
if(!dir.exists(input.dir_2)) { dir.create(input.dir_2) }
if(!dir.exists(output.dir)) { dir.create(output.dir) }

# Libraries
library(ggplot2)
library(RColorBrewer) 
library(maptools)
library(rgdal)
library(sp)




# Read data ----

  # Read stations data

    dat_stations <- read.csv(paste0(input.dir_1, fu.name, "_tv_final_2020.csv"))
    dat_stations <- subset(dat_stations, Year == 2020)



  # Read ggin historical data (krigged densities)
    
    Files <- list.files(path=input.dir_2 , pattern = 'ggin', full.names = T)
    #select ggin files
    ggin=NULL                               #set up a blank object
    for(File in Files) {                        #loop through all files
      x=read.csv(File)                          #read the file
      FileName=sub(paste0(input.dir_2, "ggin"),'', sub('.csv','', File))  #get the filename and remove the space and .csv
      x$year=FileName                       #add a column with the filename to the dataframe
      ggin=rbind(ggin,x)            #combine the dataframe with the previous ones
    }
    
    # rename
    names(ggin)[3] <-"longitude"
    names(ggin)[4] <-"latitude"
    names(ggin)[6] <-"Density"
    names(ggin)[7] <-"stdev"
    # convert to spatial database
    coordinates(ggin)<-~longitude+latitude
    class(ggin)
    # check projection/coordinate system 
    proj4string(ggin)
    # assign
    proj4string(ggin)<-CRS("+proj=longlat +datum=WGS84")

    
    
  # Read shapefile as a SpatialPolygonsDataframe
    FG <- readShapePoly(paste0(input.dir_2, "FU20-21_Labadie"), proj4string=CRS('+proj=longlat +datum=WGS84'))
    IRE <- readShapePoly(paste0(input.dir_2, "EIRE"), proj4string=CRS('+proj=longlat +datum=WGS84'))
  
  # Merge data
    # double check that they match
    identical(proj4string(ggin),proj4string(IRE))
    identical(proj4string(ggin),proj4string(FG))
    
    # ggplot can't deal with a SpatialPointsDataFrame so we can convert back to a data.frame
    dat <- data.frame(ggin)
    str(dat)
    class(dat)
    summary(dat)
    
    
    p <- ggplot() +  
      geom_polygon(data=IRE, aes(x=long, y=lat, group=group),fill = "#006837") +  
      geom_polygon(data=FG, aes(x=long, y=lat, group=group), fill= "light grey")
    
    
    ##data checks for station spacing
    dat$Density[is.na(dat$Density)==T]
    dat$Density[dat$Density<0]<-0.0
    range(dat$Density)
    range(dat$longitude)
    range(dat$latitude)
    
    #station spacing
    x <- sort(unique(dat$longitude))
    y <- sort(unique(dat$latitude))
    summary(head(x, -1) - tail(x, -1))
    summary(head(y, -1) - tail(y, -1))



# Plot ----

p +
  geom_tile(data=subset(dat, Polygon==T), aes(x=round(longitude, 2), y=round(latitude, 2), fill=Density)) +
  scale_fill_gradientn(name = "Krigged density", colours = brewer.pal(9,"YlOrRd"), guide = "legend") +
  geom_point(data=dat_stations, aes(x=MidDeglong, y=MidDegLat, size=Density_Adjusted), shape =1, colour="black") +
  scale_size_continuous(name="Adjusted density\nin station") +
  theme_bw() +
  coord_cartesian(xlim = longlimits, ylim = latlimits) +
  labs(y="Latitude", x="Longitude") +
  facet_wrap(~year, ncol=2) +
      theme(panel.grid = element_blank())
    
ggsave(paste0(output.dir, "Contourplot_", fu.name, ".png"), width = 8, height = 10)




