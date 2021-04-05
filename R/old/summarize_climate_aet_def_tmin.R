# issue -- def, aet are not calculated from 30-year normals
# aspiration - get # days above 32 (or whatever)
library(tidyverse)

climate_data <- read_csv("data/def_aet_tmin.csv") %>% 
  dplyr::select(-X1) %>%
  mutate(year = as.numeric(substr(as.character(date),1,4)),
         month = as.numeric(substr(as.character(date),6,7)),
         plot = as.factor(plot),
         yearmon = as.numeric(paste0(substr(as.character(date),1,4),
                              substr(as.character(date),6,7))))
  

spr_2013_def <- climate_data %>%
  filter(variable == "def") %>%
  filter(year < 2014 & year > 2012 & month < 7) %>%
  group_by(plot) %>%
  summarise(ja_ju_def = mean(value),
            ja_ju_def_z = mean(z),
            year = 2013)

spr_def <- climate_data %>%
  filter(variable == "def") %>%
  filter(year < 2017 & year > 2015 & month < 7) %>%
  group_by(plot) %>%
  summarise(ja_ju_def = mean(value),
            ja_ju_def_z = mean(z),
            year = 2016) %>%
  ungroup() %>%
  rbind(spr_2013_def)

spr_2013_tmin <- climate_data %>%
  filter(variable == "tmin") %>%
  filter(year < 2014 & year > 2012 & month < 7) %>%
  group_by(plot) %>%
  summarise(ja_ju_tmin_min = min(value),
            ja_ju_tmin_z = mean(z),
            year = 2013)

spr_tmin <- climate_data %>%
  filter(variable == "tmin") %>%
  filter(year < 2017 & year > 2015 & month < 7) %>%
  group_by(plot) %>%
  summarise(ja_ju_tmin_min = min(value),
            ja_ju_tmin_z = mean(z),
            year = 2016) %>%
  rbind(spr_2013_tmin) %>%
  left_join(spr_def)

spr_2013_aet <- climate_data %>%
  filter(variable == "aet") %>%
  filter(year < 2014 & year > 2012 & month < 7) %>%
  group_by(plot) %>%
  summarise(ja_ju_aet = mean(value),
            ja_ju_aet_z = mean(z),
            year = 2013)

spr_aet <- climate_data %>%
  filter(variable == "aet") %>%
  filter(year < 2017 & year > 2015 & month < 7) %>%
  group_by(plot) %>%
  summarise(ja_ju_aet = mean(value),
            ja_ju_aet_z = mean(z),
            year = 2016) %>%
  rbind(spr_2013_aet) %>%
  left_join(spr_tmin)

twoyr_aet13 <- climate_data %>%
  filter(variable == "aet") %>%
  filter(yearmon < 201307 & yearmon > 201108) %>%
  group_by(plot) %>%
  summarise(twoyr_aet = mean(value),
            twoyr_aet_z = mean(z),
            year = 2013)

twoyr_aet <- climate_data %>%
  filter(variable == "aet") %>%
  filter(yearmon < 201607 & yearmon > 201408) %>%
  group_by(plot) %>%
  summarise(twoyr_aet = mean(value),
            twoyr_aet_z = mean(z),
            year = 2016) %>%
  rbind(twoyr_aet13) %>%
  left_join(spr_aet, by = c("plot", "year"))

twoyr_def13 <- climate_data %>%
  filter(variable == "def") %>%
  filter(yearmon < 201307 & yearmon > 201108) %>%
  group_by(plot) %>%
  summarise(twoyr_def = mean(value),
            twoyr_def_z = mean(z),
            year = 2013)

twoyr_def <- climate_data %>%
  filter(variable == "def") %>%
  filter(yearmon < 201607 & yearmon > 201408) %>%
  group_by(plot) %>%
  summarise(twoyr_def = mean(value),
            twoyr_def_z = mean(z),
            year = 2016) %>%
  rbind(twoyr_def13) %>%
  left_join(twoyr_aet, by = c("plot", "year"))

write_csv(twoyr_def, "data/climate_summaries_def_aet_tmin.csv")
