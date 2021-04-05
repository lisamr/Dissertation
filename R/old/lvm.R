# lvm modelling

# note, jags needs to be installed (on debian linux, "sudo apt-get install jags")
# for windows or mac, sourceforge.net has jags

# setup ========================================================================
rm(list = ls())
source("R/a_prep_mjcb.R")
source("R/hui_2015_suppl/coral.R")

fg_abund <- dplyr::select(all_p, AIG, AIF, PNG, ANF, PNF, Shrubs)
fg_abund[fg_abund <0 & fg_abund>1] <- 1
fg_abund <- round(fg_abund,0)

pure_ord <- fit.coral(y = fg_abund, family = "negative.binomial", num.lv = 2, num.clus = 1, site.eff = TRUE, save.model = T)
icslv2clus1 <- try(get.ics(coralfit = pure_ord),silent=F)

plot(pure_ord$lv.mean, type = "n")
text(pure_ord$lv.mean, label = all_p$Site.type, col = all_p$Year)

all_p <- mutate(all_p, coral_1 = pure_ord$lv.med[,1],
                coral_2 = pure_ord$lv.med[,2])

cm <- cor(dplyr::select(all_p, -Site_number, -Year, -shrub_b, -Site.type)) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("response") 

cm1 <- dplyr::select(cm,coral_1, response) %>%
  arrange(desc(coral_1))

cm2 <- dplyr::select(cm, coral_2, response) %>%
  arrange(desc(coral_2))

ggplot(df, aes(x=lv_1, y = lv_2)) +
  geom_point(size = 2,aes(color = invasion_stage, shape = year)) +
  geom_path(aes(group = c(site_number)), alpha=0.5,arrow = grid::arrow()) +
  xlab("Latent Variable 1: High Litter and AIG / Low PNG, PNF and Crypto") +
  ylab("Latent Variable 2: High AIF and Litter / Low Shrubs and Bare Ground")

# pure ordination, no shrubs ===================================================
fg_abund <- dplyr::select(all_p, AIG, AIF, PNG, ANF, PNF)
fg_abund[fg_abund <0 & fg_abund>1] <- 1
fg_abund <- round(fg_abund,0)

pure_ord <- fit.coral(y = fg_abund, family = "negative.binomial", num.lv = 2, num.clus = 1, site.eff = TRUE, save.model = T)
icslv2clus1 <- try(get.ics(coralfit = pure_ord),silent=F)

plot(pure_ord$lv.mean, type = "n")
text(pure_ord$lv.mean, label = all_p$Site.type, col = all_p$Year)

all_p <- mutate(all_p, coral_1 = pure_ord$lv.med[,1],
                coral_2 = pure_ord$lv.med[,2])

evf <- envfit(dplyr::select(all_p, coral_1, coral_2), dplyr::select(all_p, -Site_number, 
                                                             -Year, -shrub_b, 
                                                             -Site.type, -coral_1,
                                                             -coral_2, -AIG,
                                                             ), na.rm=T)
eft <- tibble(coral_1 = evf$vectors$arrows[,1], 
              coral_2 = evf$vectors$arrows[,2],
              p = evf$vectors$pvals,
              variables = names(dplyr::select(all_p, -Site_number, 
                                            -Year, -shrub_b, 
                                            -Site.type, -coral_1,
                                            -coral_2, -AIG)),
              r2 = evf$vectors$r)
  # as.data.frame()
  # tibble::rownames_to_column("variable")

# movement1 <- all_p %>%
#   dplyr::select(coral_1, Site_number, Year) %>%
#   spread(key = Year, value = coral_1) %>%
#   dplyr::rename(c1_2013 = "2013", c1_2016 = "2016")
# movement <- all_p %>%
#   dplyr::select(coral_2, Site_number, Year, Site.type) %>%
#   spread(key = Year, value = coral_2)%>%
#   dplyr::rename(c2_2013 = "2013", c2_2016 = "2016") %>%
#   left_join(movement1) %>%
#   mutate(movement=sqrt(((abs(c1_2016)-abs(c1_2013))^2)+((abs(c2_2016)-abs(c2_2013))^2))) 

cm <- cor(dplyr::select(all_p, -Site_number, -Year, -shrub_b, -Site.type), use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  tibble::rownames_to_column("response") 

cm1 <- dplyr::select(cm,coral_1, response) %>%
  arrange(desc(coral_1))

cm2 <- dplyr::select(cm, coral_2, response) %>%
  arrange(desc(coral_2))

# plotting =====================================================================

lut_col <- c( "chocolate4", "darkgoldenrod2","springgreen4", "deepskyblue2")

ggplot(all_p, aes(x=coral_1, y = coral_2)) +
  geom_path(aes(group = c(Site_number)),
            arrow = grid::arrow(angle = 10), alpha = .5) +
  geom_point(size = 3,aes(color = Site.type, shape = Site.type)) +
  scale_color_manual(name = "Invasion Stage", values = lut_col)+
  scale_shape_manual(name = "Invasion Stage", values = 15:18)+
  geom_text(data = filter(eft, p<0.01), aes(x = coral_1, y = coral_2, label = variables, alpha = r2))
  
  # xlab("+ Litter, Cheagrass and Annual Invasive Forbs +\n - Native Grasses, Shrubs and Crypto -") +
  # ylab("+ Native and Non-native Forbs +\n - Cheatgrass -")

ggplot(movement, aes(x = Site.type, y=movement)) +
  geom_point()

ggplot(left_join(all_p, movement), aes(x = AIF, y = movement)) +
  geom_point()
