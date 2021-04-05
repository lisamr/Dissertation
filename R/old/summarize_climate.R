# This summarizes climate data extracted from PRISM data with the 
# extract_climate_apr18.R script

# setup ------------------------------------------------------------------------
library(zoo)
library(dplyr)

climate_file <- "data/monthly_climate.csv"

# data import ------------------------------------------------------------------

climate_data <- read.csv(climate_file, stringsAsFactors = FALSE)
#climate_data$date <- as.Date((as.yearmon(climate_data$date, "%Y/%m")))

# precip -----------------------------------------------------------------------

ppt2011_2012 <- filter(climate_data, variable == "ppt") %>%
  filter(date < 201206) %>%
  filter(date > 201105)
ppt2011_2012 <- aggregate(value~plot, data = ppt2011_2012, 
            FUN = sum)
colnames(ppt2011_2012) <- c("plot", "ppt11_12")


ppt2012_2013 <- filter(climate_data, variable == "ppt") %>%
  filter(date < 201306) %>%
  filter(date > 201205) 
ppt2012_2013 <- aggregate(value~plot, data = ppt2012_2013, 
                          FUN = sum)
colnames(ppt2012_2013) <- c("plot", "ppt12_13")

ppt2014_2015 <- filter(climate_data, variable == "ppt") %>%
  filter(date < 201506) %>%
  filter(date > 201405)
ppt2014_2015 <- aggregate(value~plot, data = ppt2014_2015, 
                          FUN = sum)
colnames(ppt2014_2015) <- c("plot", "ppt14_15")


ppt2015_2016 <- filter(climate_data, variable == "ppt") %>%
  filter(date < 201606) %>%
  filter(date > 201505) 
ppt2015_2016 <- aggregate(value~plot, data = ppt2015_2016, 
                          FUN = sum)
colnames(ppt2015_2016) <- c("plot", "ppt15_16")

# tmax -------------------------------------------------------------------------

tmax_13 <- filter(climate_data, variable == "tmax") %>%
  filter(date < 201306) %>%
  filter(date > 201212)
tmax_13 <- aggregate(value~plot, data = tmax_13, FUN = max)
colnames(tmax_13) <- c("plot", "tmax_13")

tmax_16 <- filter(climate_data, variable == "tmax") %>%
  filter(date < 201606) %>%
  filter(date > 201512)
tmax_16 <- aggregate(value~plot, data = tmax_16, FUN = max)
colnames(tmax_16) <- c("plot", "tmax16")

# tmin -------------------------------------------------------------------------

tmin_13 <- filter(climate_data, variable == "tmin") %>%
  filter(date < 201305) %>%
  filter(date > 201209)
tmin_13 <- aggregate(value~plot, data = tmin_13, FUN = min)
colnames(tmin_13) <- c("plot", "tmin_13")

tmin_16 <- filter(climate_data, variable == "tmin") %>%
  filter(date < 201605) %>%
  filter(date > 201509)
tmin_16 <- aggregate(value~plot, data = tmin_16, FUN = min)
colnames(tmin_16) <- c("plot", "tmin_16")

# vpdmax -----------------------------------------------------------------------
# 
vpdmax_13 <- filter(climate_data, variable == "vpdmax") %>%
  filter(date < 201306) %>%
  filter(date > 201212)
vpdmax_13 <- aggregate(value~plot, data = vpdmax_13, FUN = max)
colnames(vpdmax_13) <- c("plot", "vpdmax_13")

vpdmax_16 <- filter(climate_data, variable == "vpdmax") %>%
  filter(date < 201606) %>%
  filter(date > 201512)
vpdmax_16 <- aggregate(value~plot, data = vpdmax_16, FUN = max)
colnames(vpdmax_16) <- c("plot", "vpdmax_16")

# joining and exporting to csv -------------------------------------------------

climate_vars <- left_join(ppt2011_2012,ppt2012_2013) %>%
  left_join(ppt2014_2015) %>%
  left_join(ppt2015_2016) %>%
  left_join(tmax_13) %>%
  left_join(tmax_16) %>%
  left_join(tmin_13) %>%
  left_join(tmin_16) %>%
  left_join(vpdmax_13) %>%
  left_join(vpdmax_16)

write.csv(climate_vars, "data/climate_summaries.csv")

# visualizing just for fun -----------------------------------------------------
climate_data <- read.csv(climate_file, stringsAsFactors = FALSE)
climate_data$date <- as.Date((as.yearmon(as.character(climate_data$date), "%Y%m")))

ggplot(climate_data[climate_data$variable == "ppt",], aes(x = date, y = value)) +
  geom_point(aes(colour = as.factor(plot))) +
  geom_smooth() +
  ggtitle("Precipitation")

ggplot(climate_data[climate_data$variable == "tmin",], aes(x = date, y = value)) +
  geom_point(aes(colour = as.factor(plot))) +
  geom_smooth() +
  ggtitle("Min Temp")

ggplot(climate_data[climate_data$variable == "tmax",], aes(x = date, y = value)) +
  geom_point(aes(colour = as.factor(plot))) +
  geom_smooth() +
  ggtitle("Max Temp")

ggplot(climate_data[climate_data$variable == "vpdmax",], aes(x = date, y = value)) +
  geom_point(aes(colour = as.factor(plot))) +
  geom_smooth() +
  ggtitle("Max Vapor Pressure Deficit")

