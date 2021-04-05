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
stupidfuckingrows = as.vector(plots@data$Site.number)[-c(28,12,8,2,5)]

# ppt --------------------------------------------------------------------------
path = "/Users/computeruser/DATA/PRISM/ppt/" #for mac
# path = "/home/computer/DATA/PRISM/ppt/" #for linux

t0 = Sys.time()
outputlist = list()
for(p in 1:length((plots))){

  output_plot = data.frame(matrix(ncol = 12, nrow = 6), row.names=years)
  colnames(output_plot) = months_vec
  
  for(y in 1:length(years)){
    for(m in 1:length(months_vec)){
      filename_year = paste0(path,"PRISM_ppt_stable_4kmM3_",years[y],months_vec[m],"_bil.bil")
      raster_year = raster(filename_year)
      output_plot[y,m] = raster::extract(raster_year, pts[p])
      print(paste(months_vec[m], years[y], plots@data$Site.number[p]))
    }
  }
  output_plot$plot = plots@data$Site.number[p]
  outputlist[[p]] = output_plot
}
print(Sys.time()-t0)

pptsums = data.frame(matrix(ncol = 2, nrow = 25), 
                     row.names=plots@data$Site.number)
colnames(pptsums) = c("2011-2013_ppt","2014-2016_ppt")


for(j in 1:length(outputlist)){
  first_year = sum(outputlist[[j]][1,c(9:12)])
  second_year = sum(outputlist[[j]][2,c(1:12)])
  third_year = sum(outputlist[[j]][3,c(1:3)])
  pptsums[j,1] = sum(first_year,second_year,third_year)
}

for(j in 1:length(outputlist)){
  first_year = sum(outputlist[[j]][4,c(9:12)])
  second_year = sum(outputlist[[j]][5,c(1:12)])
  third_year = sum(outputlist[[j]][6,c(1:3)])
  pptsums[j,2] = sum(first_year,second_year,third_year)
}

pptsums$plot = rownames(pptsums)
if(!file.exists("data/ppt_sums.csv")){write.csv(pptsums, "data/ppt_sums.csv")}
