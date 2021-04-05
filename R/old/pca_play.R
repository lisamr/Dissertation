source("R/a_prep_mjcb.R")
source("R/functions.R")
library(ggbiplot)
library(factoextra)

#notes - keep eigenvalues >1, eigenvalue is sdev^2 for prcomp objects
# plant nutrients --------------------------------------------------------------

plant_nuts <- dplyr::select(all,
                            Bromus_TN_pct,
                            Bromus_TC_pct,
                            Other_TN_pct,
                            Other_TC_pct,
                            # Other_CN,
                            # Litter_CN,
                            # Poa_CN,     
                            # Bromus_CN,
                            Poa_TN_pct,
                            Poa_TC_pct,      
                            Litter_TN_pct,
                            Litter_TC_pct) %>% na.omit()

plant_nuts_13 <- dplyr::select(glmm13,
                            Bromus_TN_pct,
                            Bromus_TC_pct,
                            Other_TN_pct,
                            Other_TC_pct,
                            # Other_CN,
                            # Litter_CN,
                            # Poa_CN,     
                            # Bromus_CN,
                            Poa_TN_pct,
                            Poa_TC_pct,      
                            Litter_TN_pct,
                            Litter_TC_pct) %>% na.omit()

plant_nuts_16 <- dplyr::select(glmm16,
                               Bromus_TN_pct,
                               Bromus_TC_pct,
                               Other_TN_pct,
                               Other_TC_pct,
                               # Other_CN,
                               # Litter_CN,
                               # Poa_CN,     
                               # Bromus_CN,
                               Poa_TN_pct,
                               Poa_TC_pct,      
                               Litter_TN_pct,
                               Litter_TC_pct) %>% na.omit()

# correllation -----------------------------------------------------------------
tri_cor(plant_nuts)
tri_cor(plant_nuts_13)
tri_cor(plant_nuts_16) # litter Carbon and Other (AIF) C and N positive (in the .3 -.4 range)

# tutorial ---------------------------------------------------------------------
# from http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
pca_1 <- prcomp(plant_nuts, scale =T)
# pca_1$sdev^2 # keep the first 2
pca_2 <- psych::principal(plant_nuts, nfactors = 4, rotate = "varimax")

# queremos:
# Less than half of residuals with absolute values > 0.05
# Model fit > .9
# All communalities > .7

pca_2$residual %>% median() #good
pca_2$fit #0.86...almost!
pca_2$communality #fail...closer with 3 factors good with 4

dataset = cbind(plant_nuts, pca_2$scores)
summary(dataset$RC1, dataset$RC2)

ggplot(dataset,
       aes(RC1, RC2, label = as.character(na.omit(all)$Site.type))) +
  aes (x = RC1, y = RC2, by = na.omit(all)$Year) + stat_density2d(color = "gray87")+
  geom_text(size = 7, color = factor(na.omit(all)$Year)) +
  ggtitle ('Plant Nutrients') +
  theme_bw() +
  theme(  
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) + 
  theme(axis.title.x = element_text(colour = 'black', size = 23, 
                                    margin=margin(15,15,15,15)),
        axis.title.y = element_text(colour = 'black', size = 23, 
                                    margin=margin(15,15,15,15)),
        axis.text.x  = element_text(size=16),
        axis.text.y  = element_text(size=16)) +
  labs(x = "Bromus, Other, Poa N ->", y = "Poa/Other Carbon ->") +
  theme(plot.title = element_text(hjust = 0.5, size = 32, face = "bold",
                                  margin=margin(15,15,15,15)))



# visualizing prcomp objects ---------------------------------------------------
fviz_eig(pca_1)

fviz_pca_ind(pca_1,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pca_1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pca_1, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
 # predicting 2016 from 2013
pca_13 <- prcomp(plant_nuts_13, scale =T)
pr_16 <- predict(pca_13, newdata = plant_nuts_16)

# Plot of active individuals
p <- fviz_pca_ind(pca_13, repel = TRUE)
# Add supplementary individuals
fviz_add(p, pr_16, color ="blue")
fviz_pca_ind(pca_1,habillage = na.omit(all)$Year, repel =T)


# ------------------------------------------------------------------------------
pca_plants = princomp(plant_nuts, scores=T, covmat = MASS::cov.rob(plant_nuts))
pca_plants_s = princomp(plant_nuts_s, scores=T, covmat = MASS::cov.rob(plant_nuts_s))

ggbiplot(pca_plants, groups = na.omit(all)$Year)
ggbiplot(pca_plants_s, groups = na.omit(all)$Year)


# soil nutrients----------------------------------------------------------------

soil_all <- dplyr::select(all,
                          SOIL_SurSo4_kg_ha,
                          SOIL_Ca_kg_ha,
                          SOIL_Mg_kg_ha,
                          SOIL_OM_pct,
                          SOIL_TN_pct,   
                          SOIL_CN)

soil_13 <- dplyr::select(glmm13,
                          SOIL_SurSo4_kg_ha,
                          SOIL_Ca_kg_ha,
                          SOIL_Mg_kg_ha,
                          SOIL_OM_pct,
                          SOIL_TN_pct,   
                          SOIL_CN)

soil_16 <- dplyr::select(glmm16,
                         SOIL_SurSo4_kg_ha,
                         SOIL_Ca_kg_ha,
                         SOIL_Mg_kg_ha,
                         SOIL_OM_pct,
                         SOIL_TN_pct,   
                         SOIL_CN)

# correlation ------------------------------------------------------------------
tri_cor(soil_all); tri_cor(soil_13); tri_cor(soil_16)
# soil C:N/Soil C with Soil Ca both years


pca_soil_raw = princomp(soil_raw, scores=T,  covmat = MASS::cov.rob(soil_raw))

pca_soil_s = princomp(soil_scaled, scores=T, covmat = MASS::cov.rob(soil_scaled))

ggbiplot(pca_soil_raw, groups =  all$Year)
ggbiplot(pca_soil, groups =  all$Year)
ggbiplot(pca_soil_s, groups =  all$Year)

ggbiplot(pca_soil, groups =  all$Site_number, ellipse = T)

