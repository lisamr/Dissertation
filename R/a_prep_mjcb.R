## libraries -------------------------------------------------------------------

libs <- c("nlme", "doBy", "dplyr", "plyr", "lmerTest", "sjPlot", "ggplot2",
          "tidyverse", "car", "mosaic", "rlist", "reshape2", "effects", "tidyr",
          "lattice", "cowplot", "MuMIn", "grid", "gridExtra", "ggpubr","Rmisc",
          "vegan", "agricolae", "raster")
iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)
source("R/functions.R")

# look up tables =========================================================
lut_variables <- c("Bromus_TN_pct" = "Bromus N (%)", "Bromus_TC_pct" = "Bromus C (%)",
                   "Bromus_CN" = "Bromus C:N", "Other_TN_pct" = "Other N (%)",
                   "Other_TC_pct" = "Other C (%)", "Other_CN" = "Other C:N", 
                   "Poa_TN_pct" = "Poa N (%)", "Poa_TC_pct" = "Poa C (%)",
                   "Poa_CN" = "Poa C:N", "Litter_TN_pct" = "Litter N (%)",
                   "Litter_TC_pct" = "Litter C (%)", "Litter_CN" = "Litter C:N",
                   "SOIL_SurSo4_kg_ha" = "Soil SO<sub>4</sub> (kg/ha)",
                   "SOIL_Ca_kg_ha" = "Soil Ca (kg/ha)", 
                   "SOIL_Mg_kg_ha" = "Soil Mg (kg/ha)",
                   "SOIL_CN" = "Soil C:N", "soil_n_kg_ha" = "Soil Total N (kg/ha)",
                   "soil_c_kg_ha" = "Soil Total C (kg/ha)",
                   "total_mineral_n" = "Soil Mineral N (kg/ha)",
                   "NO3_kg_ha" = "Soil Nitrate (kg/ha)",
                   "NH4_kg_ha" = "Soil Ammonium (kg/ha)",
                   "ja_ju_def" = "CWD",
                   "ja_ju_aet" = "AET",
                   "tmin" = "T_min",
                   "Annuals" = "Annuals",
                   "Perennials" = "Perennials",
                   "Forbs" = "Forbs",
                   "Grasses" = "Grasses")

lut_variables_nounit <- c("Bromus_TN_pct" = "Bromus N", "Bromus_TC_pct" = "Bromus C",
                          "Bromus_CN" = "Bromus C:N", "Other_TN_pct" = "Other N",
                          "Other_TC_pct" = "Other C", "Other_CN" = "Other C:N", 
                          "Poa_TN_pct" = "Poa N", "Poa_TC_pct" = "Poa C",
                          "Poa_CN" = "Poa C:N", "Litter_TN_pct" = "Litter N",
                          "Litter_TC_pct" = "Litter C", "Litter_CN" = "Litter C:N",
                          "SOIL_SurSo4_kg_ha" = "Soil SurSo4",
                          "SOIL_Ca_kg_ha" = "Soil Ca", 
                          "SOIL_Mg_kg_ha" = "Soil Mg",
                          "SOIL_CN" = "Soil C:N", "soil_n_kg_ha" = "Soil Total N",
                          "soil_c_kg_ha" = "Soil Total C",
                          "total_mineral_n" = "Soil Mineral N",
                          "NO3_kg_ha" = "Soil Nitrate",
                          "NH4_kg_ha" = "Soil Ammonium",
                          "ja_ju_def" = "CWD",
                          "ja_ju_aet" = "AET",
                          "tmin" = "T_min",
                          "Annuals" = "Annuals",
                          "Perennials" = "Perennials",
                          "Forbs" = "Forbs",
                          "Grasses" = "Grasses")

site_colors = c("springgreen4","deepskyblue2",
           "darkgoldenrod2","chocolate4")
## data prep -------------------------------------------------------------------
# adding precip with previously generated climate data from prism
# and year as a factor, then joining the two data frames

ppt <- read.csv("data/ppt_sums.csv") %>%
  dplyr::select(-X, 
                ppt2011_2013 = X2011.2013_ppt,
                ppt2014_2016 = X2014.2016_ppt) # %>%
  # filter(plot != 4, 
  #        plot != 5,
  #        plot != 19,
  #        plot != 20,
  #        plot != 21)

# adding more climate variables
climate_sums <- read.csv("data/climate_summaries.csv") %>%
  dplyr::select(-X) #%>%
  # filter(plot != 4, 
  #        plot != 5,
  #        plot != 19,
  #        plot != 20,
  #        plot != 21)

# cleaned and matched data frames
glmm13 <- read.csv("data/Jones_2013_transect_dec17.csv") %>%
  # filter(Site_number != 4, 
  #        Site_number != 5,
  #        Site_number != 19,
  #        Site_number != 20,
  #        Site_number != 21) %>%
  mutate(Year = 2013) %>%
  left_join(ppt,by=c("Site_number"="plot")) %>%
  left_join(climate_sums, by=c("Site_number"="plot")) %>%
  dplyr::select(-ppt2014_2016, -ppt14_15,-ppt15_16,-tmax16,-tmin_16,-vpdmax_16,
                #-ANF, -PNF,
                ppt_2yrtot = ppt2011_2013,
                ppt_2yr = ppt11_12,
                ppt_1yr = ppt12_13,
                tmax = tmax_13,
                tmin = tmin_13,
                vpdmx = vpdmax_13)

glmm16 <- read.csv("data/jones_all_nov_2017.csv") %>%
  mutate(Year = 2016) %>%
  left_join(ppt, by = c("Site_number"="plot")) %>%
  left_join(climate_sums, by=c("Site_number"="plot")) %>%
  dplyr::select(-ppt2011_2013, -ppt11_12,-ppt12_13,-tmax_13,-tmin_13,-vpdmax_13,
                # -ANF, -PNF,
                ppt_2yrtot = ppt2014_2016,
                ppt_2yr = ppt14_15,
                ppt_1yr = ppt15_16,
                tmax = tmax16,
                tmin = tmin_16,
                vpdmx = vpdmax_16)


# making Shrubs use the same data from 2013 (that was line
# intercept, I just included artr in my quadrats)

glmm16$Shrubs <- filter(glmm13,
                        Site_number != 4, 
                        Site_number != 5,
                        Site_number != 19,
                        Site_number != 20,
                        Site_number != 21)$Shrubs

# # center and scale soil SO4 by year
# glmm16$SOIL_SurSo4_kg_ha <- as.numeric(scale(glmm16$SOIL_SurSo4_kg_ha))
# glmm13$SOIL_SurSo4_kg_ha <- as.numeric(scale(glmm13$SOIL_SurSo4_kg_ha))

# relative cover of Crypto by year
glmm16$Crypto <- glmm16$Crypto / max(glmm16$Crypto)
glmm13$Crypto <- glmm13$Crypto / max(glmm13$Crypto)

# grabbing some soil stuff that's inconsistently collected but still data

soil_stuff16 <- read_csv("data/jones_all_oct5.csv") %>%
  dplyr::select(Plot_TP, NO3_ppm = "NO3.N..ppm.", 
                bulk_density = "bulkDensity.g.cm3.", NH4_ppm = "NH4.N..ppm.",
                net_mineralization = net.mineralization,
                soil_moisture = soilMoisture) %>%
  mutate(Year = "2016",
         NO3_kg_ha = NO3_ppm * bulk_density,
         NH4_kg_ha = NH4_ppm * bulk_density,
         Site_number = as.character(as.numeric(substr(Plot_TP, 2,3))),
         Transect = substr(Plot_TP,4,4))

# soil_stuff16 <- read_csv("data/jones_all_oct5.csv") %>%
#   dplyr::select(Plot_TP, NO3_ppm = "NO3.N..ppm.", 
#                 bulk_density = "bulkDensity.g.cm3.", NH4_ppm = "NH4.N..ppm.",
#                 net_mineralization = net.mineralization,
#                 soil_moisture = soilMoisture) %>%
#   mutate(Year = "2016",
#          NO3_kg_ha = NO3_ppm * 2,
#          NH4_kg_ha = NH4_ppm * 2,
#          Site_number = as.character(as.numeric(substr(Plot_TP, 2,3))),
#          Transect = substr(Plot_TP,4,4))

soil_stuff13 <- read_csv("data/data_2013/Soils 10-23-13.csv") %>%
  dplyr::select(Site_number = `Site number`, Transect, NO3_kg_ha = `Surno3 (kg/ha)`, 
                NH4_kg_ha = `NH4 (kg/ha)`) %>%
  mutate(net_mineralization = NA,
         Year = "2013")

bulk_dens<- soil_stuff16%>%
  dplyr::select(Site_number, Transect, bulk_density) %>%
  group_by(Site_number) %>%
  dplyr::summarise(bulk_d = mean(bulk_density)) %>%
  ungroup()%>%
  mutate(Site_number = as.numeric(Site_number))

dp = 10
# bd = median(soil_stuff16$bulk_density)

lut_st <- c("M" = "Invaded Sagebrush", "I" = "Intact Sagebrush",
            "C" = "Cheatgrass-dominated", "D" = "Cheatgrass Dieoff")

all <- rbind(glmm13, glmm16) %>%
  left_join(bulk_dens) %>%
  mutate(Year = as.factor(Year),
         shrub_b = as.factor(ifelse(Site.type == "I" | Site.type == "M", "Shrub", "Grass")),
         Site_number = as.factor(Site_number),
         soil_n_kg_ha = pct_to_kg_ha(pct = SOIL_TN_pct, depth = dp, bulk_density = bulk_d),
         soil_c_kg_ha = pct_to_kg_ha(pct = SOIL_OM_pct/1.724, depth = dp, bulk_density = bulk_d),
         Site.type = factor(lut_st[as.character(Site.type)], 
                            levels = c("Intact Sagebrush","Invaded Sagebrush",
                                       "Cheatgrass-dominated", "Cheatgrass Dieoff")))%>%
  as_tibble()


pca_all = princomp(decostand(dplyr::select(all,
                                           Bare,
                                           Litter,
                                           Crypto,
                                           AIG,
                                           AIF,
                                           # ANF,
                                           # PNF,
                                           PNG,
                                           Shrubs
), "standardize"), scores=T, cor = FALSE)
all$inv <- pca_all$scores[,1] * -1 # times -1 to be positive with invasion

pca_13 = princomp(decostand(dplyr::select(glmm13,
                                           Bare,
                                           Litter,
                                           Crypto,
                                           AIG,
                                           AIF,
                                           ANF,
                                           PNF,
                                           PNG,
                                           Shrubs
), "standardize"), scores=T, cor = FALSE)

pca_16 = princomp(decostand(dplyr::select(glmm16,
                                          Bare,
                                          Litter,
                                          Crypto,
                                          AIG,
                                          AIF,
                                          ANF,
                                          PNF,
                                          PNG,
                                          Shrubs
), "standardize"), scores=T, cor = FALSE)

# grabbing some individual species ---------------------------------------------
d16 <- read_csv("data/Jones_Cover - ArealCover.csv") %>%
  dplyr::select(B_tectorum = BRTE_A, E_cicutarium = ERCI_A, 
                S_altissimum = SIAL_A, P_secunda = POSE_A,
                L_perfoliatum = LEPE_A, Site_number = `Plot#`,
                Transect = TransectPair) %>%
  mutate(Site_number = as.character(as.numeric(substr(Site_number,2,3)))) %>%
  group_by(Site_number,Transect) %>%
  summarise_all(mean,na.rm=T) %>%
  mutate(Year = "2016")

d13 <- read_csv("data/Plant_cover_11-23-13_nov_17.csv")
d13[is.na(d13)] <- 0
d13 <- dplyr::select(d13, Site_number = Site, B_tectorum = "Bromus tectorum",
                     E_cicutarium = "Erodium cicutarium",
                     S_altissimum = "Sisymbrium altissimum",
                     P_secunda = "Poa secunda", 
                     L_perfoliatum = "Lepidium perfoliatum", Transect) %>%
  mutate(Site_number = replace(Site_number, Site_number == "Curlew", "25")) %>%
  mutate(Site_number = replace(Site_number, Site_number == "EVDO", "26")) %>%
  mutate(Site_number = replace(Site_number,Site_number == "GV", "27")) %>%
  mutate(Site_number = replace(Site_number, Site_number == "Stewart", "28")) %>%
  group_by(Site_number, Transect) %>%
  summarise_all(mean, na.rm=T) %>%
  mutate(Year = "2013")

d <- rbind(d13,d16)



soil_stuff <- rbind(soil_stuff13, 
                    dplyr::select(soil_stuff16,
                                  Site_number,
                                  Transect,
                                  Year,
                                  NO3_kg_ha,
                                  NH4_kg_ha,
                                  net_mineralization)) %>%
  mutate(total_mineral_n = NO3_kg_ha +NH4_kg_ha)


all <- all %>%
  left_join(dplyr::select(soil_stuff, Site_number,
                          Transect,
                          Year,
                          NO3_kg_ha,
                          NH4_kg_ha,
                          total_mineral_n), by = c("Year", "Site_number", "Transect"))


mod_mineralization <- all %>%
  left_join(dplyr::select(soil_stuff, Site_number,
                          Transect,
                          Year,
                          net_mineralization,
                          NO3_kg_ha,
                          NH4_kg_ha,
                          total_mineral_n), 
            by = c("Year", "Site_number", "Transect")) %>%
  filter(Year == 2016)%>%
  # group_by(Site_number,Year) %>%
  # summarise_all(mean, na.rm=T) %>%
  # ungroup%>% 
  # lm(net_mineralization ~ AIG, data=.)
  lmer(net_mineralization ~ AIG + (1|Site_number), data=.)

all_p <- all %>%
  dplyr::select(-Site.type, -shrub_b) %>%
  left_join(d,by=c("Site_number","Transect","Year")) %>%
  group_by(Site_number,Year) %>%
  summarise_all(mean, na.rm=T) %>%
  left_join(unique(dplyr::select(all, Site_number, Site.type, shrub_b))) %>%
  mutate(Site.type = as.character(Site.type),
         shrub_b = as.character(shrub_b)) %>%
  dplyr::select(-Transect) %>%
  ungroup()%>%
  left_join(read_csv("data/climate_summaries_def_aet_tmin.csv") %>%
              mutate(Year = as.character(year), Site_number = as.character(plot)) %>%
              dplyr::select(-year,-plot)
            ) %>%
  left_join(read_csv("data/growing_degree_days.csv", col_types = "ffd")) %>%
  ungroup() %>%
  as_tibble()

all_se <- all %>%
  dplyr::select(-Site.type, -shrub_b) %>%
  left_join(d,by=c("Site_number","Transect","Year")) %>%
  group_by(Site_number,Year) %>%
  summarise_all(function(x,...) sd(x, na.rm=T)/sqrt(length(x)), na.rm=T) %>%
  left_join(unique(dplyr::select(all, Site_number, Site.type, shrub_b))) %>%
  mutate(Site.type = as.character(Site.type),
         shrub_b = as.character(shrub_b)) %>%
  dplyr::select(-Transect) %>%
  ungroup()

all_sd <- all %>%
  dplyr::select(-Site.type, -shrub_b) %>%
  left_join(d,by=c("Site_number","Transect","Year")) %>%
  group_by(Site_number,Year) %>%
  summarise_all(function(x,...) sd(x, na.rm=T)) %>%
  left_join(unique(dplyr::select(all, Site_number, Site.type, shrub_b))) %>%
  mutate(Site.type = as.character(Site.type),
         shrub_b = as.character(shrub_b)) %>%
  dplyr::select(-Transect) %>%
  ungroup()

# all_p[is.na(all_p)] <- NA
# 
# allsoil <- all%>%
#   left_join(soil_stuff) %>%
#   mutate(total_mineral_n = NO3_kg_ha +NH4_kg_ha)

# summary(lm(net_mineralization ~ AIG, data=allsoil[allsoil$shrub_b == "Grass",])) #lower aic 
# summary(lm(net_mineralization ~ AIG, data=allsoil[allsoil$shrub_b == "Shrub",]))
# 
# allsoil %>%
#   filter(net_mineralization > 0,
#          as.character(Year) == "2016") %>%
#   ggplot(aes(x=AIG, y=net_mineralization, color = shrub_b)) +
#     geom_point(aes(shape = Site.type), size=2) +
#     geom_smooth(method = "lm") +
#     facet_wrap(~shrub_b)


# ## raster data 
# plots <- read.csv("data/locations.csv")
# coordinates(plots) <- ~Longitude+Latitude
# gm_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# crs(plots) <- gm_crs
# pts <- SpatialPoints(coordinates(plots)[,c(1,2)])
# 
# if(!file.exists("data/def_aet_tmin.csv")){
#   output_data <- data.frame(plot = NA, 
#                             date = NA, 
#                             variable = NA, 
#                             value = NA)
#   
#   variables <- c("def", "aet")
#   counter=1
#   for(v in 1:length(variables)){
#     aet_files <- list.files(paste0("/home/a/data/climate/",variables[v]), 
#                             full.names = TRUE)[33:38]
#     for (i in 1:length(aet_files)){
#       rr <- stack(aet_files[i])
#       print(paste(variables[v], substr(aet_files[i],30,33)))
#       ex <- raster::extract(rr, pts) %>% as.data.frame()
#       for(m in 1:12){
#         for(p in 1:25){
#           nn <- ex[p,m]
#           
#           output_data[counter,1] <- plots@data$Site.number[p]
#           output_data[counter,2] <- paste(substr(aet_files[i],30,33), 
#                                           if(m<9){paste0("0",as.character(m))}else{m}, 
#                                           "01",
#                                           sep="-")
#           output_data[counter,3] <- variables[v]
#           output_data[counter,4] <- nn
#           counter<-counter+1
#           
#         }
#       }
#     }
#   }
#   
#   output_data$date <- as.Date(output_data$date)
#   
#   mean_def <- mean(output_data[output_data$variable=="def",]$value)
#   sd_def <- sd(output_data[output_data$variable=="def",]$value)
#   mean_aet <- mean(output_data[output_data$variable=="aet",]$value)
#   sd_aet <- sd(output_data[output_data$variable=="aet",]$value)
#   
#   norms1<- data.frame(variable = c("def", "aet"),
#                       mean = c(mean_def, mean_aet),
#                       sd = c(sd_def, sd_aet))
#   output_data <- left_join(output_data,norms1)
#   output_data <- mutate(output_data, z = (value-mean)/sd)
#   
#   write.csv(output_data, "data/def_aet.csv")
#   
#   
#   prism<-read.csv("data/monthly_climate.csv") %>%
#     select(-X) %>%
#     mutate(date = paste0(date, "01"))%>%
#     mutate(date = as.Date(date, "%Y%m%d")) %>%
#     filter(variable == "tmin")
#   
#   mean_tmin <- mean(prism[prism$variable=="tmin",]$value)
#   sd_tmin <- sd(prism[prism$variable=="tmin",]$value)
#   mean_tmax <- mean(prism[prism$variable=="tmax",]$value)
#   sd_tmax <- sd(prism[prism$variable=="tmax",]$value)
#   
#   norms2<- data.frame(variable = c("tmin", "tmax"),
#                       mean = c(mean_tmin, mean_tmax),
#                       sd = c(sd_tmin, sd_tmax))
#   prism <- left_join(prism,norms2)
#   prism <- mutate(prism, z = (value-mean)/sd)
#   
#   output_data <- rbind(output_data,prism)
#   
#   write.csv(output_data, "data/def_aet_tmin.csv")
# }
# 
# def_aet_norms <- read_csv("data/def_aet_norms.csv") %>%
#   mutate(plot = as.character(plot)) %>%
#   dplyr::select(-X1) %>%
#   dplyr::rename(norm=value)
# 
# output_data <- read_csv("data/def_aet_tmin.csv")%>%
#   dplyr::select(-X1)%>%
#   mutate(month = as.numeric(substr(date, 6,7)), 
#          plot = as.character(plot)) %>%
#   left_join(def_aet_norms, by=c("plot","variable","month")) %>%
#   mutate(anomaly = value - norm)
# 
# mean_tmin <- mean(output_data[output_data$variable=="tmin",]$anomaly)
# sd_tmin <- sd(output_data[output_data$variable=="tmin",]$anomaly)
# mean_def <- mean(output_data[output_data$variable=="def",]$anomaly)
# sd_def <- sd(output_data[output_data$variable=="def",]$anomaly)
# mean_aet <- mean(output_data[output_data$variable=="aet",]$anomaly)
# sd_aet <- sd(output_data[output_data$variable=="aet",]$anomaly)
# 
# norms3<- data.frame(variable = c("tmin", "def", "aet"),
#                     amean = c(mean_tmin, mean_def, mean_aet),
#                     asd = c(sd_tmin, sd_def, sd_aet))
# 
# output_data <- left_join(output_data, norms3, by = "variable") %>%
#   mutate(z_anom = (anomaly - amean)/asd)
# 
# # sidetrack, writing out a climate summary for sems
# # 2013
# sample13 <- as.Date("2013-06-01")
# sample16 <- as.Date("2013-06-01")
# 
# rbind(
#   output_data %>%
#     filter(date < sample13 & date > sample13-180) %>%
#     group_by(plot,variable) %>%
#     summarise(mx = max(z_anom),
#               mn = min(z_anom),
#               mean = mean(z_anom),
#               sd=sd(z_anom))%>%
#     ungroup() %>%
#     mutate(Year=2013)
#   ,
#   output_data %>%
#     filter(date < sample16 & date > sample16-180) %>%
#     group_by(plot,variable) %>%
#     summarise(mx = max(z_anom),
#               mn = min(z_anom),
#               mean = mean(z_anom),
#               sd=sd(z_anom)) %>%
#     ungroup()%>%
#     mutate(Year=2016)
# )%>%pivot_wider(names_from ="variable", values_from = c(mx, mn, mean,sd)) %>%
#   dplyr::rename(Site_number=plot) %>%
# write_csv("data/climate_anomalies_summary.csv")
