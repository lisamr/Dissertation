source("R/functions.R")
source("R/a_prep_mjcb.R")


## basic data exploration ------------------------------------------------------
d <- melt(dplyr::select(all_se,-Site_number,-Year,-shrub_b, -Site.type))
d <- ggplot(d,aes(x = value)) +
  facet_wrap(~variable, scales = "free") +
  geom_density()
d
# ggsave("Images/density_plots.png", limitsize = FALSE)

#cleveland dot plots - color is site type, shape is plot #

# for(i in 3:length(all_se)){
#   dotchart(as.numeric(all_se[,i]),
#            color = as.numeric(all$Site.type),
#            groups = all$Year,
#            main = names(all)[i])
# }

# sulfur - is it trustworthy?
# hypothesis - the soil lab people gave me results in ppm, 
# and accidentally labelled them lbs/acre
# plot(all$SOIL_SurSo4_kg_ha ~ all$ppt, col = all$Year, pch = as.numeric(all$Site_number))


# winnemucca weather - precip is in inches -------------------------------------

weather = read.csv("data/winnemucca_airport_weather_10_17.csv", stringsAsFactors = FALSE)
weather = weather[,c(1:7,9,12,14,17:19,21,24:25)]
weather$date = as.Date(weather$DATE, "%m/%d/%y %H")
weather$year_month = substr(as.character(weather$date), 1,7)
for(i in 8:16){
  weather[,i] = as.numeric(weather[,i])
}
weather$SVP = 610.7* (10^((7.5*weather$HOURLYDRYBULBTEMPC)/(237.3 + weather$HOURLYDRYBULBTEMPC)))
weather$VPD = ((100 - weather$HOURLYRelativeHumidity)/100) * weather$SVP


monthly_weather_medians = summaryBy(. ~ year_month, data = na.omit(weather), FUN = median)
monthly_weather_maximumns = summaryBy(. ~ year_month, data = na.omit(weather), FUN = max)
monthly_weather_minimumns = summaryBy(. ~ year_month, data = na.omit(weather), FUN = min)
monthly_precip_sums = summaryBy(HOURLYPrecip ~ year_month, data = na.omit(weather), FUN = sum)
monthly_precip_sums$date = paste0(monthly_precip_sums$year_month, "-01")
monthly_precip_sums$date = as.Date(monthly_precip_sums$date)
monthly_precip_sums$month = substr(monthly_precip_sums$year_month,6,7)
monthly_precip_sums$year = substr(monthly_precip_sums$year_month,1,4)
yearly_precip_sums = summaryBy(HOURLYPrecip.sum ~ year, data = monthly_precip_sums, FUN = sum)
yearly_precip_sums$Precip_mm <- yearly_precip_sums$HOURLYPrecip.sum.sum *25.4
yearly_precip_sums$HOURLYPrecip.sum.sum = NULL
yearly_precip_sums <-yearly_precip_sums[c(2:7),] #gettting rid of incomplete years
yearly_precip_sums$year = as.numeric(yearly_precip_sums$year)

ggplot(data=yearly_precip_sums, aes(x=year, y=Precip_mm)) +
  geom_hline(yintercept = 210.566, col="grey60") +
  geom_point() +
  geom_bar(stat="identity") +
  ylab("Precipitation (mm)") +
  xlab("Year") +
  annotate("text",x= 2013, y=213, label = "30 year normal = 210 mm") +
  theme_minimal()

# correlation matrix -----------------------------------------------------------
# copied from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization


#### creating and reshaping the correlation matrix 
pred <- dplyr::select(all,
                      Bare, Litter, Crypto, Rock,
                      AIG, AIF, PNF, PNG, ANF, Shrubs,
                      ppt_2yrtot, ppt_2yr, ppt_1yr, tmax, tmin, vpdmx, pca1_inv 
)
pred13 <- dplyr::filter(all, Year == "2013") %>% 
  dplyr::select(
                 Bare, Litter, Crypto, Rock,
                 AIG, AIF, PNF, PNG, ANF, Shrubs,
                 ppt_2yrtot, ppt_2yr, ppt_1yr, tmax, tmin, vpdmx, pca1_inv 
)
pred16 <- dplyr::filter(all, Year == "2016") %>% 
  dplyr::select(
    Bare, Litter, Crypto, Rock,
    AIG, AIF, PNF, PNG, ANF, Shrubs,
    ppt_2yrtot, ppt_2yr, ppt_1yr, tmax, tmin, vpdmx, pca1_inv 
  )

resp <- dplyr::select(all,
                      Bromus_TN_pct, Bromus_TC_pct, #Bromus_CN,
                      Other_TN_pct, Other_TC_pct, #Other_CN,
                      Poa_TN_pct, Poa_TC_pct, #Poa_CN,
                      Litter_TN_pct, Litter_TC_pct, #Litter_CN,
                      SOIL_SurSo4_kg_ha, SOIL_Ca_kg_ha, SOIL_Mg_kg_ha,
                      SOIL_OM_pct, SOIL_TN_pct, SOIL_CN
)
resp13 <- dplyr::filter(all, Year == "2013") %>% 
  dplyr::select(
                Bromus_TN_pct, Bromus_TC_pct, Bromus_CN,
                Other_TN_pct, Other_TC_pct, Other_CN,
                Poa_TN_pct, Poa_TC_pct, Poa_CN,
                Litter_TN_pct, Litter_TC_pct, Litter_CN,
                SOIL_SurSo4_kg_ha, SOIL_Ca_kg_ha, SOIL_Mg_kg_ha,
                SOIL_OM_pct, SOIL_TN_pct, SOIL_CN
  )

resp16 <- dplyr::filter(all, Year == "2016") %>% 
  dplyr::select(
    Bromus_TN_pct, Bromus_TC_pct, Bromus_CN,
    Other_TN_pct, Other_TC_pct, Other_CN,
    Poa_TN_pct, Poa_TC_pct, Poa_CN,
    Litter_TN_pct, Litter_TC_pct, Litter_CN,
    SOIL_SurSo4_kg_ha, SOIL_Ca_kg_ha, SOIL_Mg_kg_ha,
    SOIL_OM_pct, SOIL_TN_pct, SOIL_CN
  )

square_cor(pred16,resp16) #### veg correlated with soil/plant
tri_cor(resp) #### just the soil and plants in a triangle matrix

# kw tests ---------------------------------------------------------------------
k13 = mass_krus(glmm13[,!names(glmm13) %in% c("Site.type", "Site_number", "Transect", "Year")], glmm13$Site.type)
k16 = mass_krus(glmm16[,!names(glmm16) %in% c("Site.type", "Site_number", "Transect", "Year")], glmm16$Site.type)
plotit(k13, "2013T")
plotit(k16, "2016T")

# exploring climate variables --------------------------------------------------
resp <- names(all_p)[c(3:20,37,38)]
clm <- names(all_p)[c(31:33, 50,52,54,56,58,60)]
res_df <- data.frame(resp = NA, pred = NA, year = NA, R2 = NA, p = NA, est = NA, site_type = NA )
counter <- 1
for(r in resp){
  for(c in clm){
      f <- formula(paste0(r, "~", c))
      x <- summary(lm(f, data = all_p, na.action = na.omit))
      res_df[counter, 1] <- r
      res_df[counter, 2] <- c
      res_df[counter, 3] <- NA# y
      res_df[counter, 4] <- round(x$r.squared, 4)
      res_df[counter, 5] <- round(x$coefficients[2,4], 4)
      res_df[counter, 6] <- round(x$coefficients[2,1], 4)
      #res_df[counter, 7] <- t
      counter = counter +1
      
  }
}

res_df1 <- arrange(res_df, desc(R2)) %>% filter(resp == "SOIL_OM_pct")

best <- res_df %>%
  group_by(resp) %>%
  dplyr::summarise(best = first(pred),
                   r2 = first(R2),
                   second_best = nth(x=pred, n = 2),
                   second_best_r2 = nth(x=R2, n=2)) %>%
  ungroup() %>%
  arrange(best)

table(best$best)
table(best$second_best)

ggplot(all_p, aes(x=ja_ju_tmin_min, y=Poa_CN, color = Site.type)) +
  geom_point(aes(shape = Year), size=2) #+
  #geom_smooth(method = "lm",se=F)
ggplot(all_p, aes(x=twoyr_aet, y=Poa_TN_pct, color = Site.type)) +
  geom_point(aes(shape=Year), size=2) #+
 #geom_smooth(method="lm", se=F)

ggplot(all_p, aes(x=Poa_CN, y=ja_ju_tmin_min, color = Site.type, shape = Year)) +
  geom_point()
ggplot(all_p, aes(x=Poa_CN, y=twoyr_aet, color = Site.type)) +
  geom_point(aes(shape=Year)) +
  geom_smooth(method="lm", se=F)

ggplot(all_p, aes(x=Other_CN, y=ja_ju_tmin_min, color = Site.type, shape = Year)) +
  geom_point()

ggplot(all_p, aes(x=SOIL_CN, y=twoyr_aet, color = Site.type)) +
  geom_point(aes(shape=Year)) +
  geom_smooth(method="lm", se=F)

ggplot(all_p, aes(x=SOIL_Ca_kg_ha, y=twoyr_aet, color = Site.type)) +
  geom_point(aes(shape=Year)) +
  geom_smooth(method="lm", se=F)

ggplot(res_df, aes(site_type, R2)) +
  geom_violin(scale="area")

ggplot(all_p, aes(y=Litter_TN_pct, x = tmax, color=Site.type)) +
  geom_smooth(method = "lm") +
  geom_errorbar(data = all_se, aes(x = all_p$tmax, ymin = all_p$Litter_TN_pct -Litter_TN_pct, ymax = all_p$Litter_TN_pct +Litter_TN_pct,y = all_p$Litter_TN_pct, xmin = all_p$tmax - tmax, xmax = all_p$tmax + tmax), col="grey60") +
  geom_point(aes(size=1.5))

ggplot(all_p, aes(y=SOIL_TN_pct, x = ppt_1yr, color = Site.type)) +
  geom_point(aes(shape = Year, size=1.5)) 

ggplot(all_p, aes(y=SOIL_TN_pct, x = tmax, shape = Year, color = Site.type)) +
  geom_point(aes(size=1.5))

ggplot(all_p, aes(y=SOIL_OM_pct, x = tmax, color = paste(Year, shrub_b))) +
  geom_smooth(method="lm", fill="grey90", se=F) +
  geom_errorbar(data = all_se, 
                alpha = 0.5,
                aes(x = all_p$tmax, ymin = all_p$SOIL_OM_pct - SOIL_OM_pct, ymax = all_p$SOIL_OM_pct +SOIL_OM_pct,y = all_p$SOIL_OM_pct, xmin = all_p$tmax - tmax, xmax = all_p$tmax + tmax)) +
  geom_point(aes(size=1.5))

ggplot(all_p, aes(y=SOIL_TN_pct, x = tmax, color = paste(Year, shrub_b))) +
  geom_smooth(method="lm", fill="grey90", se=F) +
  geom_errorbar(data = all_se, alpha=0.5, aes(x = all_p$tmax, ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, ymax = all_p$SOIL_TN_pct +SOIL_TN_pct,y = all_p$SOIL_TN_pct, xmin = all_p$tmax - tmax, xmax = all_p$tmax + tmax)) +
  geom_point(size=3.5)


ggplot(all_p, aes(y=Bromus_TN_pct, x = ppt_2yrtot, color = shrub_b)) +
  geom_point(size=3.5,aes(shape = Year)) +
  geom_smooth(method = "lm")

# climate vs predictors --------------------------------------------------------
ggplot(all_p, aes(y=sqrt(AIF), x = ja_ju_aet, color = Site.type)) +
  geom_errorbar(data = all_se,
                alpha = 0.5,
                aes(x = all_p$ja_ju_aet, 
                    ymin = sqrt(all_p$AIF) - sqrt(AIF), 
                    ymax = sqrt(all_p$AIF) + sqrt(AIF),
                    y = sqrt(all_p$AIF), 
                    xmin = all_p$tmax - tmax, 
                    xmax = all_p$tmax + tmax)) +
  geom_smooth(method="lm", se=F) +
  geom_point(aes(size=1.5, shape=Year), stroke=1.5)

ggplot(all_p, aes(y=sqrt(AIG), x = ja_ju_aet, color =Site.type)) +
  geom_errorbar(data = all_se,
                alpha = 0.5,
                aes(x = all_p$ja_ju_aet, ymin = sqrt(all_p$AIG) - sqrt(AIG), ymax = sqrt(all_p$AIG) + sqrt(AIG),y = sqrt(all_p$AIG), xmin = all_p$tmax - tmax, xmax = all_p$tmax + tmax)) +
  geom_smooth(method="lm", se=F) +
  geom_point(aes(size=1.5, shape = ), stroke=1.5) +
  scale_shape_manual(values = c(1:20))


# exploring other stuff --------------------------------------------------------
resp <- names(all)[4:21]
clm <- names(all)[c(22:29,31)]
clm <- names(all)[c(22:31)] #only for shrub plots

cover_df <- data.frame(resp = NA, pred = NA, year = NA, R2 = NA, p = NA, est = NA, site_type = NA )
counter <- 1
for(y in c("2013","2016")){
  for(r in resp){
    for(c in clm){
      for(t in c("Shrub")){
        f <- formula(paste0(r, "~", c))
        x <- summary(lm(f, data = all[all$Year == y & all$shrub_b == t,]))
        cover_df[counter, 1] <- paste(r)
        cover_df[counter, 2] <- paste(c)
        cover_df[counter, 3] <- paste(y)
        cover_df[counter, 4] <- round(x$r.squared, 4)
        cover_df[counter, 5] <- round(x$coefficients[2,4], 4)
        cover_df[counter, 6] <- round(x$coefficients[2,1], 4)
        cover_df[counter, 7] <- paste(t)
        counter = counter +1
      }
    }
  }
}

cover_df <- arrange(cover_df, desc(R2)); cover_df[1:30,]

# exploration by plot ----------------------------------------------------------
resp <- names(all_p)[3:20]
clm <- names(all_p)[c(21:28,30,37)]
cover_df <- data.frame(resp = NA, pred = NA, year = NA, R2 = NA, p = NA, est = NA, site_type = NA )
counter <- 1
for(y in c("2013","2016")){
  for(r in resp){
    for(c in clm){
      for(t in c("Grass", "Shrub")){
        f <- formula(paste0(r, "~", c))
        x <- summary(lm(f, data = all_p[all_p$Year == y & all_p$shrub_b == t,]))
        cover_df[counter, 1] <- paste(r)
        cover_df[counter, 2] <- paste(c)
        cover_df[counter, 3] <- paste(y)
        cover_df[counter, 4] <- round(x$r.squared, 4)
        cover_df[counter, 5] <- round(x$coefficients[2,4], 4)
        cover_df[counter, 6] <- round(x$coefficients[2,1], 4)
        cover_df[counter, 7] <- paste(t)
        counter = counter +1
      }
    }
  }
}

cover_df <- arrange(cover_df, desc(R2)); cover_df[1:30,]

# predictors of cover ----------------------------------------------------------------

aif_ppt <- ggplot(all_p, aes(x=ja_ju_aet, y=AIF, color = Site.type)) + 
  geom_smooth(method = "lm", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$ja_ju_aet, ymin = all_p$AIF - AIF, ymax = all_p$AIF + AIF, y = all_p$Bromus_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha = 0.5) +
  geom_point(aes(shape = Year), size=2) +
  ylab("Annual Introduced Forb Cover (%)") +
  xlab("2 Year Precipitation (mm)") +
  ggsave("Images/single_term/aif_ppt.png", limitsize = F)

aif_n <- ggplot(all_p, aes(x=SOIL_TN_pct, y=AIF, color = Site.type)) + 
  geom_smooth(method = "lm", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$SOIL_TN_pct, ymin = all_p$AIF - AIF, ymax = all_p$AIF + AIF, y = all_p$Bromus_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha = 0.5) +
  geom_point(aes(shape = Year)) +
  ylab("Annual Introduced Forb Cover (%)") +
  xlab("Soil Total N (%)") +
  ggsave("Images/single_term/aif_n.png", limitsize = F)

aig_ppt <- ggplot(all_p, aes(x=ppt_2yrtot, y=AIG, color = Site.type)) + 
  geom_smooth(method = "lm", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$ppt_2yrtot, ymin = all_p$AIG - AIG, ymax = all_p$AIG + AIG, y = all_p$Bromus_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF),alpha = 0.5) +
  geom_point(aes(shape = Year)) +
  ylab("Annual Introduced Grass Cover (%)") +
  xlab("2 Year Precipitation (mm)") +
  ggsave("Images/single_term/aig_ppt.png", limitsize = F)

aig_n <- ggplot(all_p, aes(x=SOIL_TN_pct, y=AIG, color = Site.type)) + 
  geom_smooth(method = "lm", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$SOIL_TN_pct, ymin = all_p$AIG - AIG, ymax = all_p$AIG + AIG, y = all_p$Bromus_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha = 0.5) +
  geom_point(aes(shape = Year)) +
  ylab("Annual Introduced Grass Cover (%)") +
  xlab("Soil Total N (%)") +
  ggsave("Images/single_term/aig_n.png", limitsize = F)


ggarrange(aig_ppt, aif_ppt, common.legend = T, legend = "bottom") +
  ggsave("Images/single_term/cover_ppt.png")

# Bromus TN --------------------

ggplot(all_p) + 
  geom_smooth(data = all_p[all_p$shrub_b == "Grass" & all_p$AIF>0.5,],aes(x=(AIF), y=Bromus_TN_pct), span=1.5, col="grey40", se=F)+
  geom_errorbar(data = all_se, aes(x = all_p$AIF, ymin = all_p$Bromus_TN_pct - Bromus_TN_pct, ymax = all_p$Bromus_TN_pct +Bromus_TN_pct,y = all_p$Bromus_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha=0.25) +
  geom_point(aes(x=(AIF), y=Bromus_TN_pct, color = Site.type, shape=Year)) +
  ylab("Bromus Total N (%)") +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank()) +
  #xlab("Annual Introduced Forb Cover (%)") +
  ggsave("Images/single_term/bn_aif.png", limitsize = F) -> bn_aif

ggplot(all_p) + 
  geom_smooth(data = all_p[all_p$shrub_b == "Grass",], aes(x=(AIG), y=Bromus_TN_pct), span=1.5, color="grey40", se=F)+
  geom_errorbar(data = all_se, aes(x = all_p$AIG, ymin = all_p$Bromus_TN_pct - Bromus_TN_pct, ymax = all_p$Bromus_TN_pct +Bromus_TN_pct,y = all_p$Bromus_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha=0.25) +
  geom_point(aes(x=(AIG), y=Bromus_TN_pct, color = Site.type, shape=Year)) +
  #ylab("Bromus Total N (%)") +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_blank()) +
  #xlab("Annual Introduced Forb Cover (%)") +
  ggsave("Images/single_term/bn_aig.png", limitsize = F) -> bn_aig


ggplot(all_p, aes(x=ppt_2yrtot, y=Bromus_TN_pct, color = Site.type)) + 
   geom_smooth(method = "lm", se=F) +
   geom_errorbar(data = all_se, aes(x = all_p$ppt_2yrtot, ymin = all_p$Bromus_TN_pct - Bromus_TN_pct, ymax = all_p$Bromus_TN_pct +Bromus_TN_pct,y = all_p$Bromus_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha = 0.5) +
  geom_point(aes(shape = Year)) +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y=element_blank()) +
#  ylab("Bromus Total Nitrogen (%)") +
#  xlab("2 Year Precipitation (mm)") +
  ggsave("Images/single_term/bn_ppt.png", limitsize = F) -> bn_ppt



# Poa TN -------------------------------------------

ggplot(all_p) +
  geom_smooth(data = all_p[all_p$shrub_b == "Grass" & all_p$AIF>0.5,],aes(x=(AIF), y=Poa_TN_pct), span=1.5, col="grey40", se=F)+
  geom_errorbar(data = all_se, aes(x = all_p$AIF, ymin = all_p$Poa_TN_pct - Poa_TN_pct, ymax = all_p$Poa_TN_pct +Poa_TN_pct,y = all_p$Poa_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha=0.25) +
  geom_point(aes(x=AIF, y=Poa_TN_pct, color = Site.type, shape=Year)) +
  ylab("Poa Total N (%)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
 # xlab("Annual Introduced Forb Cover (%)") +
  ggsave("Images/single_term/pn_aif.png", limitsize = F) -> pn_aif

ggplot(all_p) + 
  geom_smooth(data = all_p[all_p$shrub_b == "Grass",], aes(x=(AIG), y=Poa_TN_pct), span=1.5, col="grey40", se=F)+
  geom_errorbar(data = all_se, aes(x = all_p$AIG, ymin = all_p$Poa_TN_pct - Poa_TN_pct, ymax = all_p$Poa_TN_pct +Poa_TN_pct,y = all_p$Bromus_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha=0.25) +
  geom_point(aes(x=(AIG), y=Poa_TN_pct, color = Site.type, shape=Year)) +
  #ylab("Bromus Total N (%)") +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_blank()) +
  #xlab("Annual Introduced Forb Cover (%)") +
  ggsave("Images/single_term/pn_aig.png", limitsize = F) -> pn_aig

ggplot(all_p, aes(x=ppt_2yrtot, y=Poa_TN_pct, color = Site.type)) +
  geom_smooth(method = "lm", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$ppt_2yrtot, ymin = all_p$Poa_TN_pct - Poa_TN_pct, ymax = all_p$Poa_TN_pct +Poa_TN_pct,y = all_p$Poa_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF),alpha = 0.5) +
  geom_point() +
  theme(axis.title.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_blank()) +
 # ylab("Poa Total N (%)") +
 # xlab("2 Year Precipitation (mm)") +
  ggsave("Images/single_term/pn_ppt.png", limitsize = F) -> pn_ppt

# Other TN ---------------------------------------

ggplot(all_p) + 
  geom_smooth(data = all_p[all_p$shrub_b == "Grass" & all_p$AIF>0.5,],aes(x=(AIF), y=Other_TN_pct), span=1.5, col="grey40", se=F)+
  geom_errorbar(data = all_se, aes(x = all_p$AIF, ymin = all_p$Other_TN_pct - Other_TN_pct, ymax = all_p$Other_TN_pct +Other_TN_pct,y = all_p$Other_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type), alpha = 0.25) +
  geom_point(aes(x=AIF, y=Other_TN_pct, color = Site.type, shape=Year)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  ylab("Other Total Nitrogen (%)") +
  xlab("Annual Introduced Forb Cover (%)") +
 ggsave("Images/single_term/on_aif.png", limitsize = F) -> on_aif

ggplot(all_p) + 
  geom_smooth(data = all_p[all_p$shrub_b == "Grass",], aes(x=(AIG), y=Other_TN_pct), span=1.5, col="grey40", se=F)+
  geom_errorbar(data = all_se, aes(x = all_p$AIG, ymin = all_p$Other_TN_pct - Other_TN_pct, ymax = all_p$Other_TN_pct + Other_TN_pct,y = all_p$Bromus_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha=0.25) +
  geom_point(aes(x=(AIG), y=Other_TN_pct, color = Site.type, shape=Year)) +
  #ylab("Bromus Total N (%)") +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank()) +
  xlab("Annual Introduced Grass Cover (%)") +
  ggsave("Images/single_term/on_aig.png", limitsize = F) -> on_aig

ggplot(all_p, aes(x=ppt_2yrtot, y=Other_TN_pct, color = Site.type)) +
  geom_smooth(method = "lm", se=F)+
  geom_errorbar(data = all_se, aes(x=all_p$ppt_2yrtot, ymin = all_p$Other_TN_pct - Other_TN_pct, ymax = all_p$Other_TN_pct +Other_TN_pct,y = all_p$Other_TN_pct, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type),alpha=0.25) +
  geom_point(aes(shape=Year)) +
 # ylab("Other Total N (%)") +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank()) +
  xlab("2 Year Precipitation (mm)") +
  ggsave("Images/single_term/on_ppt.png", limitsize = F) -> on_ppt


ggarrange(plotlist=list(bn_aif, bn_aig, bn_ppt, 
                        pn_aif, pn_aig, pn_ppt, 
                        on_aif, on_aig, on_ppt), 
          ncol=3, nrow=3,
          common.legend = T,
          font.label = 'bold',
          legend = "bottom") +
  ggsave("Images/single_term/plantn.png", limitsize = F, height = 11, width = 11)


# Soil Mg ----------------------

ggplot(all_p) + 
  geom_smooth(method = "lm",aes(x=ppt_2yrtot, y=SOIL_Mg_kg_ha, color = Year)) +
  # geom_errorbar(data = all_se, aes(x = all_p$AIF, ymin = all_p$SOIL_Mg_kg_ha - SOIL_Mg_kg_ha, ymax = all_p$SOIL_Mg_kg_ha +SOIL_Mg_kg_ha,y = all_p$SOIL_Mg_kg_ha, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF), col="grey60") +
  geom_point(aes(x=ppt_2yrtot, y=SOIL_Mg_kg_ha, color = Year, shape=Site.type, size=1.5)) +
  ylab("Soil Mg (kg/ha)") +
  xlab("Annual Introduced Forb Cover (%)")
ggsave("Images/single_term/smg_aif_yr.png", limitsize = F)

ggplot(all_p) + 
  geom_smooth(method = "lm",aes(x=ppt_2yrtot, y=SOIL_Mg_kg_ha, color = Site.type), se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$ppt_2yrtot, ymin = all_p$SOIL_Mg_kg_ha - SOIL_Mg_kg_ha, ymax = all_p$SOIL_Mg_kg_ha +SOIL_Mg_kg_ha,y = all_p$SOIL_Mg_kg_ha, xmin = all_p$AIF - AIF, xmax = all_p$AIF + AIF, col=Site.type), alpha = 0.25) +
  geom_point(aes(x=ppt_2yrtot, y=SOIL_Mg_kg_ha, color = Site.type, shape=Year, size=1.5)) +
  ylab("Soil Mg (kg/ha)") +
  xlab("Annual Introduced Forb Cover (%)")
ggsave("Images/single_term/smg_aif_st.png", limitsize = F)

# Soil N ------------------------

ggplot(all_p, aes(x=Bare, y=SOIL_TN_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$Bare, ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, ymax = all_p$SOIL_TN_pct +SOIL_TN_pct,y = all_p$SOIL_TN_pct, xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare, col=Site.type), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  ylab("Soil Total Nitrogen (%)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=9)) +
  # xlab("Bare Ground Cover (%)") +
  ggsave("Images/single_term/sn_bare_st.png", limitsize = F) -> sn_bare_st

ggplot(all_p, aes(x=AIG, y=SOIL_TN_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$AIG, ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, ymax = all_p$SOIL_TN_pct +SOIL_TN_pct,y = all_p$SOIL_TN_pct, xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare, col=Site.type), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  #ylab("Soil Total Nitrogen (%)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  xlab("Annual Introduced Grass Cover (%)") +
  ggsave("Images/single_term/sn_aig_st.png", limitsize = F) -> sn_aig

ggplot(all_p, aes(x=AIF, y=SOIL_TN_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$AIF, ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, ymax = all_p$SOIL_TN_pct +SOIL_TN_pct,y = all_p$SOIL_TN_pct, xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare, col=Site.type), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  #ylab("Soil Total Nitrogen (%)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
  # xlab("Annual Introduced Forb Cover (%)") +
  ggsave("Images/single_term/sn_aif_st.png", limitsize = F) -> sn_aif

ggplot(all_p, aes(x=ppt_2yrtot, y=SOIL_TN_pct,color = Site.type)) + 
  geom_smooth(method="lm", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$ppt_2yrtot, ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, ymax = all_p$SOIL_TN_pct +SOIL_TN_pct,y = all_p$SOIL_TN_pct, xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare),alpha=0.25) +
  geom_point(aes(shape = Year)) +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme(axis.title.y=element_blank()) +
  # ylab("Soil Total Nitrogen (%)") +
  # xlab("Bare Ground Cover (%)") +
  ggsave("Images/single_term/sn_bare_sh.png", limitsize = F) -> sn_ppt

# soil C -----------------------------------------------------------------------

ggplot(all_p, aes(x=ppt_2yrtot, y=SOIL_OM_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$ppt_2yrtot, ymin = all_p$SOIL_OM_pct - SOIL_OM_pct, ymax = all_p$SOIL_OM_pct +SOIL_OM_pct,y = all_p$SOIL_TN_pct, xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  # ylab("Soil Organic Matter (%)") +
  theme(axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.title.x = element_text(size=10)) +
  xlab("2 Year Precipitation (mm)") +
  ggsave("Images/single_term/sc_bare_sh.png", limitsize = F) -> sc_ppt

ggplot(all_p, aes(x=AIG, y=SOIL_OM_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$AIG, ymin = all_p$SOIL_OM_pct - SOIL_OM_pct, ymax = all_p$SOIL_OM_pct +SOIL_OM_pct,y = all_p$SOIL_TN_pct, xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare, col=Site.type), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  #ylab("Soil Total Carbon (%)") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x = element_text(size=9),
        axis.title.x = element_text(size=10)) +
  xlab("AI Grass Cover (%)") +
  ggsave("Images/single_term/sc_aig_st.png", limitsize = F) -> sc_aig

ggplot(all_p, aes(x=AIF, y=SOIL_OM_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$AIF, ymin = all_p$SOIL_OM_pct - SOIL_OM_pct, ymax = all_p$SOIL_OM_pct +SOIL_OM_pct,y = all_p$SOIL_TN_pct, xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare, col=Site.type), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  #ylab("Soil Total Carbon (%)") +
  theme(axis.title.y=element_blank(),
       axis.text.y=element_blank(),
       axis.text.x = element_text(size=9),
       axis.title.x = element_text(size=10)) +
   xlab("AI Forb Cover (%)") +
  ggsave("Images/single_term/sc_aif_st.png", limitsize = F) -> sc_aif

ggplot(all_p, aes(x=Bare, y=SOIL_OM_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$Bare, ymin = all_p$SOIL_OM_pct - SOIL_OM_pct, ymax = all_p$SOIL_OM_pct +SOIL_OM_pct,y = all_p$SOIL_TN_pct, xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare, col=Site.type),alpha=0.25) +
  geom_point(aes(shape = Year)) +
  scale_y_continuous(labels = function(x) sprintf("%.3f", x)) +
  ylab("Soil Organic Matter (%)") +
  xlab("Bare Ground Cover (%)") +
  theme(axis.text.x = element_text(size=9),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        axis.text.y = element_text(size=9))+
  ggsave("Images/single_term/sc_bare_st.png", limitsize = F) -> sc_bare_st

ggarrange(plotlist=list(sn_bare_st, sn_ppt, sn_aif, sn_aig, sc_bare_st, sc_ppt,
                        sc_aif, sc_aig),
          nrow=2, ncol=4, widths=c(1.3,1,1,1),
          common.legend = TRUE)+
  ggsave("Images/single_term/soil_n_c.png",limitsize =FALSE,
         width = 7.5, height =6)

# Soil Ca ---------------

ggplot(all_p, aes(x=PNG, y=SOIL_Ca_kg_ha, color = shrub_b)) + 
  geom_point(aes(shape=Year, size=1.5))
ggsave("Images/single_term/sca_bare.png", limitsize = F)

ggplot(all_p, aes(y=SOIL_Ca_kg_ha, x=Site.type, shape=Year)) + 
  geom_jitter(width=.1,aes(color=Crypto, size=2))
ggsave("Images/single_term/sca_bare.png", limitsize = F)

# Soil SurSo4 -----------------------
ggplot(all_p, aes(x=Crypto, y=SOIL_SurSo4_kg_ha, color = shrub_b)) + 
  geom_point(aes(shape=Year, size=1.5))
ggsave("Images/single_term/sca_bare.png", limitsize = F)

ggplot(all_p[all_p$SOIL_SurSo4_kg_ha<0.7,], aes(x=shrub_b, y=SOIL_SurSo4_kg_ha, color = Crypto)) + 
  geom_jitter(width = 0.1, aes(shape=Year, size=1.5)) 
ggsave("Images/single_term/ss_bare.png", limitsize = F)

# Litter TN -----------------

ggplot(all_p, aes(x=AIG, y=Litter_TN_pct)) + 
  geom_smooth(method = "lm") +
  geom_errorbar(data = all_se, aes(x = all_p$AIG, ymin = all_p$Litter_TN_pct - Litter_TN_pct, ymax = all_p$Litter_TN_pct +Litter_TN_pct,y = all_p$SOIL_TN_pct, xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare), alpha=0.25) +
  geom_point(aes(shape=Year, color=Site.type), size=2.5)
ggsave("Images/single_term/ln_aig.png", limitsize = F)

ggplot(all_p, aes(x=AIF, y=Litter_TN_pct)) + 
  geom_smooth() +
  geom_errorbar(data = all_se, aes(x = all_p$AIF, 
                                   ymin = all_p$Litter_TN_pct - Litter_TN_pct, 
                                   ymax = all_p$Litter_TN_pct +Litter_TN_pct,
                                   y = all_p$SOIL_TN_pct, 
                                   xmin = all_p$Bare - Bare, 
                                   xmax = all_p$Bare + Bare, color=all_p$Site.type), alpha=0.25) +
  geom_point(aes(shape=Year, color = Site.type), size=2.5)
ggsave("Images/single_term/ln_aif.png", limitsize = F)

ggplot(all_p, aes(x=S_altissimum, y=Litter_TN_pct)) + 
  geom_smooth() +
  # geom_errorbar(data = all_se, aes(x = all_p$AIF, 
  #                                  ymin = all_p$Litter_TN_pct - Litter_TN_pct, 
  #                                  ymax = all_p$Litter_TN_pct +Litter_TN_pct,
  #                                  y = all_p$SOIL_TN_pct, 
  #                                  xmin = all_p$Bare - Bare, 
  #                                  xmax = all_p$Bare + Bare, color=all_p$Site.type), alpha=0.25) +
  geom_point(aes(shape=Year, color = Site.type), size=2.5)
ggsave("Images/single_term/ln_aif.png", limitsize = F)


# looking at variation ---------------------------------------------------------

resp <- names(all_p)[3:20]
clm <- names(all_p)[c(21:28,30,37)]
cover_sd <- data.frame(resp = NA, pred = NA, year = NA, R2 = NA, p = NA, est = NA, site_type = NA )
counter <- 1
for(y in c("2013","2016")){
  for(r in resp){
    for(c in clm){
      for(t in c("Grass", "Shrub")){
        f <- formula(paste0(r, "~", c))
        x <- summary(lm(f, data = all_se[all_se$Year == y & all_se$shrub_b == t,]))
        cover_sd[counter, 1] <- paste(r)
        cover_sd[counter, 2] <- paste(c)
        cover_sd[counter, 3] <- paste(y)
        cover_sd[counter, 4] <- round(x$r.squared, 4)
        cover_sd[counter, 5] <- round(x$coefficients[2,4], 4)
        cover_sd[counter, 6] <- round(x$coefficients[2,1], 4)
        cover_sd[counter, 7] <- paste(t)
        counter = counter +1
      }
    }
  }
}

cover_sd <- arrange(cover_sd, desc(R2)); cover_sd[31:60,]

# SOIL_CN

ggplot(all_se, aes(y=AIG)) +
  geom_point(aes(x=all_p$ppt_2yrtot, col=Site.type))




# single species ---------------------------------------------------------------

ggplot(all_p, aes(x=S_altissimum, y=SOIL_TN_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$S_altissimum, 
                                   ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, 
                                   ymax = all_p$SOIL_TN_pct + SOIL_TN_pct,
                                   y = all_p$SOIL_TN_pct, 
                                   xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  ylab("Soil Total Nitrogen (%)") +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       axis.text.y=element_blank()) +
  xlab("Sisymbrium altissimum Cover (%)") +
  ggsave("Images/single_term/sn_sial_st.png", limitsize = F) -> sn_sial

ggplot(all_p, aes(x=L_perfoliatum, y=SOIL_TN_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$L_perfoliatum, 
                                   ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, 
                                   ymax = all_p$SOIL_TN_pct + SOIL_TN_pct,
                                   y = all_p$SOIL_TN_pct, 
                                   xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  ylab("Soil Total Nitrogen (%)") +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       axis.text.y=element_blank()) +
  xlab("Lepidium perfoliatum Cover (%)") +
  ggsave("Images/single_term/sn_lepe_st.png", limitsize = F) -> sn_lepe

ggplot(all_p, aes(x=E_cicutarium, y=SOIL_TN_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$E_cicutarium, 
                                   ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, 
                                   ymax = all_p$SOIL_TN_pct + SOIL_TN_pct,
                                   y = all_p$SOIL_TN_pct, 
                                   xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  ylab("Soil Total Nitrogen (%)") +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       axis.text.y=element_blank()) +
  xlab("Erodium cicutarium Cover (%)") +
  ggsave("Images/single_term/sn_erci_st.png", limitsize = F) -> sn_erci

ggplot(all_p, aes(x=P_secunda, y=SOIL_TN_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$P_secunda, 
                                   ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, 
                                   ymax = all_p$SOIL_TN_pct + SOIL_TN_pct,
                                   y = all_p$SOIL_TN_pct, 
                                   xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  ylab("Soil Total Nitrogen (%)") +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       axis.text.y=element_blank()) +
  xlab("Poa Secunda Cover (%)") +
  ggsave("Images/single_term/sn_pose_st.png", limitsize = F) -> sn_pose

ggplot(all_p, aes(x=B_tectorum, y=SOIL_TN_pct,color = Site.type)) + 
  geom_smooth(method="lm",fill="grey70", se=F) +
  geom_errorbar(data = all_se, aes(x = all_p$B_tectorum, 
                                   ymin = all_p$SOIL_TN_pct - SOIL_TN_pct, 
                                   ymax = all_p$SOIL_TN_pct + SOIL_TN_pct,
                                   y = all_p$SOIL_TN_pct, 
                                   xmin = all_p$Bare - Bare, xmax = all_p$Bare + Bare), alpha=0.25) +
  geom_point(aes(shape = Year)) +
  ylab("Soil Total Nitrogen (%)") +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       axis.text.y=element_blank()) +
  xlab("Bromus tectorum Cover (%)") +
  ggsave("Images/single_term/sn_brte_st.png", limitsize = F) -> sn_brte

