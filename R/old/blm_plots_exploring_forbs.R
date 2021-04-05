library(sf)
library(tidyverse)
library(ggpubr)

plots <- st_read("/home/a/projects/wet_dry/data/plot_data/plots_with_landsat_buffed.gpkg")

plots <- mutate(plots,
                cg = ifelse(InvAnnGras <5,"U", "I"),
                sagebrush = ifelse(SagebrushC > 1, "S", "H"),
                state = paste(cg,sagebrush, sep="_"), 
                dup=duplicated(Latitude)) %>%
  filter(dup == FALSE)%>%
  select(-dup)

agricolae::kruskal(y=plots$InvAnnForb, trt=plots$state)->f
thing <- f$groups %>%
  rownames_to_column("state") %>%
  select(state,forb_groups = groups)

plots<-left_join(plots, thing)

ggplot(plots, aes(x=state, y=InvAnnForb, fill=forb_groups)) +
  geom_boxplot() +
  coord_trans(y="sqrt") +
  theme_pubr() +
  xlab(NULL) +
  ylab("Invasive Annual Forb Cover") -> p1

agricolae::kruskal(y=plots$InvAnnGras, trt=plots$state)->f
thing <- f$groups %>%
  rownames_to_column("state") %>%
  select(state,grass_groups=groups)

plots<-left_join(plots, thing)

ggplot(plots, aes(x=state, y=InvAnnGras, fill = grass_groups)) +
  geom_boxplot() +
  coord_trans(y="sqrt") +
  theme_pubr()+
  xlab(NULL) +
  ylab("Invasive Annual Grass Cover") -> p2

ggarrange(p1,p2) + ggsave("/home/a/projects/Jones_Study/figures/optional_grass_forb_fig.png", dpi=600)

ggplot(plots, aes(InvAnnForb)) +
  geom_density() +
  facet_wrap(aes(state)) +
  coord_trans(y="sqrt") +
  theme_pubr()
