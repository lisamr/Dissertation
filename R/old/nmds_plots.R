# nmds change figure

source("R/a_prep_mjcb.R")

libs<- c("vegan")
lapply(libs, library, character.only=T)

# fg by plot ===================================================================
# both years
fgs <- all_p %>%
  dplyr::select(AIG, AIF, PNG, PNF, ANF, Shrubs) %>%
  wisconsin()

nms <- metaMDS(fgs, trymax = 10000, wascores = TRUE )
nms1 <- metaMDS(fgs, previous.best = nms, trymax = 10000, wascores = TRUE)

scores <- as.data.frame(scores(nms1)) %>%
  mutate(site_number = all_p$Site_number,
         year = all_p$Year,
         site_type = all_p$Site.type)

mean_change <- scores %>%
  group_by(site_type,year) %>%
  dplyr::summarise(NMDS1_mean = mean(NMDS1),
            NMDS2_mean = mean(NMDS2)
         ) %>%
  ungroup()

ggplot(data=scores) +
  geom_path(aes(group=site_number, x=NMDS1, y=NMDS2, color = site_type), arrow = arrow(), alpha = 0.5, show.legend = F)+
  geom_point(aes(x=NMDS1, y=NMDS2, color = site_type, shape = site_type), size= 4, stroke =1.5) +
  geom_path(data = mean_change,
            aes(group=site_type, x=NMDS1_mean, y=NMDS2_mean, color = site_type), 
            arrow = arrow(), alpha = 0.75, lwd = 2,show.legend = F)+
  scale_shape_manual(values = c(15:18),name = "Invasion Stage") +
  scale_color_manual(values = c( "chocolate4", "darkgoldenrod2","springgreen4", "deepskyblue2"),name = "Invasion Stage") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_fixed() +
  theme(legend.justification=c(0,1), legend.position=c(0,1),
        legend.background = element_rect(fill = 'transparent'))

ggsave("figures/NMDS_change.png", dpi=600)

# years separate on nmds, then procrustes, then plot ===========================
