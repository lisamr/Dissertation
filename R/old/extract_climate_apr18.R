# This script extracts climate data from PRISM to data frames
# Author: Adam Mahood

# libraries --------------------------------------------------------------------
library(rgdal)
library(raster)

# parameters, data -------------------------------------------------------------
months_vec = c("01","02","03","04","05","06","07","08","09","10","11","12")
years = c(2011,2012,2013,2014,2015,2016)

plots = read.csv("data/locations.csv")
coordinates(plots) = ~Longitude+Latitude
prism_crs = "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
crs(plots) = prism_crs
pts = SpatialPoints(coordinates(plots)[,c(1,2)])


# extract ----------------------------------------------------------------------
# this assumes you have downloaded prism data into the prism_path folder and
# extracted it already into folders renamed in the way the variable is named
# in the bil files

prism_path = "/Users/computeruser/DATA/PRISM/" #for mac
# path = "/home/computer/DATA/PRISM/ppt/" #for linux

variables <- list.files(prism_path) 

t0 = Sys.time()
output_data = data.frame(plot = NA, 
                         date = NA, 
                         variable = NA, 
                         value = NA)
counter = 1

for(v in 1:length(variables)){
  for(p in 1:length(plots)){
    for(y in 1:length(years)){
      for(m in 1:length(months_vec)){
        # I know there's a better way, but this was easy
        if(variables[v] == "vpdmax"){
          filename_year <- paste0(prism_path,
                                  variables[v],
                                  "/PRISM_",variables[v],"_stable_4kmM1_",years[y],months_vec[m],"_bil.bil")
        }
        if(variables[v] == "ppt"){
          filename_year <- paste0(prism_path,
                                variables[v],
                                "/PRISM_",variables[v],"_stable_4kmM3_",years[y],months_vec[m],"_bil.bil")
        }
        if(variables[v] == "tmin"){
          filename_year <- paste0(prism_path,
                                  variables[v],
                                  "/PRISM_",variables[v],"_stable_4kmM2_",years[y],months_vec[m],"_bil.bil")
        }
        if(variables[v] == "tmax"){
          filename_year <- paste0(prism_path,
                                  variables[v],
                                  "/PRISM_",variables[v],"_stable_4kmM2_",years[y],months_vec[m],"_bil.bil")
        }
        raster_year <- raster(filename_year)
        output_data[counter,1] <- plots@data$Site.number[p]
        output_data[counter,2] <- as.numeric(paste0(years[y],months_vec[m]))
        output_data[counter,3] <- paste(variables[v])
        output_data[counter,4] <- raster::extract(raster_year, pts[p])
        
        print(paste(variables[v],months_vec[m], years[y], plots@data$Site.number[p]))
        
        counter = counter + 1
      }
    }
  }
}
print(Sys.time()-t0)

write.csv(output_data, "data/monthly_climate.csv")