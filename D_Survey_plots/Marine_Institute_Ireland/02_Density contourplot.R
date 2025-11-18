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
library(sf)
library(tidyverse)
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
      x <- read.csv(File)                          #read the file
      FileName <- sub(paste0(input.dir_2, "ggin"),'', sub('.csv','', File))  #get the filename and remove the space and .csv
      x$year <- FileName                       #add a column with the filename to the dataframe
      ggin <- rbind(ggin,x)            #combine the dataframe with the previous ones
    }

    # rename
    names(ggin)[3] <-"longitude"
    names(ggin)[4] <-"latitude"
    names(ggin)[6] <-"Density"
    names(ggin)[7] <-"stdev"
    # convert to spatial database
    ggin <- st_as_sf(ggin, coords = c("longitude", "latitude"), crs = 4326)

  # Read shapefile as a SpatialPolygonsDataframe
    FG <- st_read(paste0(input.dir_2, "FU20-21_Labadie.shp"))
    FG <- st_transform(FG, crs = 4326)

    IRE <- st_read(paste0(input.dir_2, "eire.shp"))
    IRE <- st_transform(IRE, crs = 4326)

  # Merge data
    # double check that they match
    identical(st_crs(ggin), st_crs(IRE))
    identical(st_crs(ggin), st_crs(FG))


    # Convert sf object to dataframe (keeping geometry)
    dat <- ggin %>%
        mutate(longitude = st_coordinates(.)[,1],
               latitude = st_coordinates(.)[,2])
    glimpse(dat)
    class(dat)
    summary(dat)


    p <- ggplot() +
        geom_sf(data = IRE, fill = "#006837") +
        geom_sf(data = FG, fill = "light grey")


    ##data checks for station spacing
    dat <- dat %>%
        mutate(Density = if_else(Density < 0, 0.0, Density))

    dat %>%
        summarise(
            na_count = sum(is.na(Density)),
            density_range = list(range(Density, na.rm = TRUE)),
            lon_range = list(range(longitude)),
            lat_range = list(range(latitude))
        )


    spacing_summary <- dat %>%
        summarise(
            lon_spacing = list(diff(sort(unique(longitude)))),
            lat_spacing = list(diff(sort(unique(latitude))))
        ) %>%
        mutate(
            lon_summary = map(lon_spacing, summary),
            lat_summary = map(lat_spacing, summary)
        )

# View spacing summaries
    spacing_summary$lon_summary[[1]]
    spacing_summary$lat_summary[[1]]



# Plot ----

p +
    geom_tile(data = dat %>% filter(Polygon == TRUE),
              aes(x = round(longitude, 2),
                  y = round(latitude, 2),
                  fill = Density)) +
    scale_fill_gradientn(name = "Krigged density",
                         colours = brewer.pal(9, "YlOrRd"),
                         guide = "legend") +
    geom_point(data = dat_stations,
               aes(x = MidDeglong,
                   y = MidDegLat,
                   size = Density_Adjusted),
               shape = 1,
               colour = "black") +
    scale_size_continuous(name = "Adjusted density\nin station") +
    theme_bw() +
    coord_sf(xlim = longlimits, ylim = latlimits, expand = FALSE) +
    labs(y = "Latitude", x = "Longitude") +
    facet_wrap(~year, ncol = 2) +
    theme(panel.grid = element_blank())

ggsave(paste0(output.dir, "Contourplot_", fu.name, ".png"), width = 8, height = 10)




