# making a site differences plot with only plot averages

source("R/a_prep_mjcb.R")

lut_variables <- c("Bromus_TN_pct" = "Bromus N (%)", "Bromus_TC_pct" = "Bromus C (%)",
                   "Bromus_CN" = "Bromus C:N", "Other_TN_pct" = "Other N (%)",
                   "Other_TC_pct" = "Other C (%)", "Other_CN" = "Other C:N", 
                   "Poa_TN_pct" = "Poa N (%)", "Poa_TC_pct" = "Poa C (%)",
                   "Poa_CN" = "Poa C:N", "Litter_TN_pct" = "Litter N (%)",
                   "Litter_TC_pct" = "Litter C (%)", "Litter_CN" = "Litter C:N",
                   "SOIL_SurSo4_kg_ha" = "Soil SurSo4 (kg/ha)",
                   "SOIL_Ca_kg_ha" = "Soil Ca (kg/ha)", 
                   "SOIL_Mg_kg_ha" = "Soil Mg (kg/ha)",
                   "SOIL_CN" = "Soil C:N", "soil_n_kg_ha" = "Soil Total N (kg/ha)",
                   "soil_c_kg_ha" = "Soil OM (kg/ha)",
                   "total_mineral_n" = "Soil Mineral N (kg/ha)",
                   "NO3_kg_ha" = "Soil Nitrate (kg/ha)",
                   "NH4_kg_ha" = "Soil Ammonium (kg/ha)")

lut_col <- c("springgreen4", "deepskyblue2", "darkgoldenrod2", "chocolate4")


all_p %>%
  dplyr::select(Site_number, Year, Site.type,
                Bromus_TN_pct, Bromus_TC_pct, Bromus_CN,
                Poa_TN_pct, Poa_TC_pct, Poa_CN,
                Other_TN_pct, Other_TC_pct, Other_CN) %>%
  gather(key =variable,value = value, -Site.type, -Year, -Site_number) %>%
  mutate(variable = factor(lut_variables[variable],
                           levels = c("Bromus C (%)","Bromus N (%)","Bromus C:N",
                                      "Poa C (%)","Poa N (%)","Poa C:N",
                                      "Other C (%)","Other N (%)","Other C:N")),
         Site.type = factor(Site.type, 
                            levels = c("Intact Sagebrush", "Invaded Sagebrush",
                                       "Cheatgrass-dominated", "Cheatgrass Dieoff")))%>%
  ggplot(aes(x=Year, y = value, fill = Site.type)) +
    geom_boxplot() +
    scale_fill_manual(name="Invasion Stage", values = lut_col)+
    facet_wrap(~variable, scales = "free") +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    xlab(NULL)+
    ylab(NULL)+
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    ggsave("figures/figure_4_tissue_raw.pdf", width = 8.25, height = 8.25) 


all_p %>%
  dplyr::select(Site_number, Year, Site.type,
                soil_n_kg_ha, soil_c_kg_ha, SOIL_CN,
                total_mineral_n,
                Litter_TN_pct, Litter_TC_pct, Litter_CN) %>%
  gather(key =variable,value = value, -Site.type, -Year, -Site_number) %>%
  mutate(variable = factor(lut_variables[variable],
                           levels= c( "Soil OM (kg/ha)", "Soil Total N (kg/ha)",
                                      "Soil C:N",
                                      "Litter C (%)",  "Litter N (%)",
                                      "Litter C:N","Soil Mineral N (kg/ha)")),
         Site.type = factor(Site.type, 
                            levels = c("Intact Sagebrush", "Invaded Sagebrush",
                            "Cheatgrass-dominated", "Cheatgrass Dieoff")))%>%
  ggplot(aes(x=Year, y = value, fill = Site.type)) +
    geom_boxplot() +
    scale_fill_manual(name="Invasion Stage", values = lut_col)+
    facet_wrap(~variable, scales = "free") +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    xlab(NULL)+
    ylab(NULL)+
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    ggsave("figures/figure_3_soil_raw.pdf", width = 8.25, height = 8.25) 

