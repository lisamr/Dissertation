# create 30 year, monthly normals for def, aet, and tmin
library(raster)
library(tidyverse)
library(foreach)
library(doParallel)

years <- 1979:2008
variables <- c("aet", "def") # from nate's gridmet stuff from john A


s3_path <- "s3://earthlab-modeling-human-ignitions/climate/mean"
local_data_path <- "/home/a/data/gridmet"
dir.create(file.path(local_data_path, "normals"))

# first, download the data =====================================================
system(paste("aws s3 cp "))

for(v in 1:length(variables)){
  for(y in 1:length(years)){
    system(paste("aws s3 cp",
                 file.path(s3_path, paste0(variables[v],"_", years[y], "_mean.tif")),
                 file.path(local_data_path, paste0(variables[v],"_", years[y], "_mean.tif")),
                 "--only-show-errors"
                 ))
  }
}

# create normal tifs ===========================================================
cc<-detectCores()-1
registerDoParallel(cc)
for(v in 1:length(variables)){
  foreach(m = 1:12)%dopar%{
    ll <- list()
    
    for(y in 1:length(years)){
     the_file <- paste0(variables[v],"_",years[y],"_mean.tif")
     ll[[y]] <- raster::stack(file.path(local_data_path, the_file))[[m]]
    }
    
    final_file <- file.path(local_data_path, "normals",
                            paste(variables[v], m, "79-08","normal.tif", sep="_"))
    ll %>%
      raster::stack() %>%
      raster::calc(mean) %>%
      writeRaster(final_file, overwrite=TRUE)
    
    system(paste("echo", final_file))
  }
}

# Extract normals ==============================================================
plots <- read.csv("data/locations.csv")
coordinates(plots) <- ~Longitude+Latitude
gm_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(plots) <- gm_crs
pts <- SpatialPoints(coordinates(plots)[,c(1,2)])

output_data_norms <- data.frame(plot = NA, 
                                variable = NA, 
                                month = NA,
                                value = NA)

variables <- c("def", "aet", "tmin")
counter=1
for(v in 1:length(variables)){
  if(variables[v] != "tmin") aet_files <- Sys.glob(file.path(local_data_path,"normals",paste0(variables[v], "*.tif")))
  if(variables[v] == "tmin") aet_files <- Sys.glob(file.path(local_data_path,"normals",paste0("*.bil")))
    rr <- stack(aet_files)
    # year <- str_extract(aet_files[i], "\\d{4}")
    names(rr) %>% substr(1,6) 
    
    print(paste(variables[v]))
    ex <- raster::extract(rr, pts) %>% as.data.frame()
    names(ex) %>% strsplit("_") -> xx
    mm=c()
    for(i in 1:length(xx)) {
      if(variables[v] != "tmin") mm[i] <- as.numeric(xx[[i]][2])
      if(variables[v] == "tmin") mm[i] <- as.numeric(xx[[i]][6])
    }
    
    for(m in 1:12){
      for(p in 1:25){
        nn <- ex[p,m]
        
        output_data_norms[counter,1] <- plots@data$Site.number[p]
        output_data_norms[counter,2] <- variables[v]
        output_data_norms[counter,3] <- mm[m]
        output_data_norms[counter,4] <- nn
        counter<-counter+1
        
      }
    
  }
}

write.csv(output_data_norms, "data/def_aet_norms.csv")


