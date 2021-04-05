#### libraries ####

library(doBy)
library(plyr)
library(picante)
library(ggbiplot) # must be installed via devtools::install_github("vqv/ggbiplot")
library(agricolae)
library(dplyr)
library(lme4)
library(car)
library(mosaic)
library(rlist) # for removing null list items
library(reshape2) # for something in the correlation matrix code
library(effects)

jones_master_cover = read.csv("data/Jones_Cover - ArealCover.csv", header = TRUE)

#### creating cover matrices ############################

cover = jones_master_cover
cover$plot_tran = paste0(cover$Plot.,cover$TransectPair)
cover_by_plot_tran = summaryBy(. ~ plot_tran, data=na.omit(cover), FUN = mean, keep.names = TRUE)
cover_by_plot = summaryBy(. ~ Plot., data = na.omit(cover), FUN=mean, keep.names = TRUE)

rownames(cover_by_plot_tran) = cover_by_plot_tran$plot_tran
rownames(cover_by_plot) = cover_by_plot$Plot.

basic_meta_plot = cover_by_plot[,c(2:7,11,22,29,35,41,57)]
basic_meta_plot_tran = cover_by_plot_tran[,c(2:7,11,22,29,35,41,57)]

cover_by_plot = cover_by_plot[,-c(1:7,11,22,29,35,41,57)]
cover_by_plot_tran = cover_by_plot_tran[,-c(1:7,11,22,29,35,41,57)]

life_forms_just_plants_byPlot = basic_meta_plot[7:12]


#### metadata #############

metadata = read.csv("data/jones_all_oct5.csv")
x = colnames(metadata)
x[13] = "PNG"
colnames(metadata) = x 
rm(x)

metadata$soil_carbon_gm2 = metadata$bulkDensity.g.cm3. * metadata$SOIL_TC * 100
metadata$soil_nitrogen_gm2 = metadata$bulkDensity.g.cm3. * metadata$SOIL_TN * 100

metadata1 = summaryBy(.~Plot, data = metadata, keep.names = TRUE, na.rm = TRUE)
metadata1$Type = metadata$Type[c(seq(1,60,3))]
metadata1$BC_cluster = metadata$Cluster[c(seq(1,60,3))]
rownames(metadata1) = metadata1$Plot
metadata2 = metadata1[,-c(1:2,8,33:34,39,44:45)]
metadata2$TN_POSE[is.nan(metadata2$TN_POSE)] = 0
metadata2$TC_POSE[is.nan(metadata2$TC_POSE)] = 0
metadata2$C.N_POSE[is.nan(metadata2$C.N_POSE)] = 0

#### 2013 data ###


Jones2013 = read.csv("data/Jones2013.csv")
names(Jones2013) = c("site_number", "site_type", "Litter_C:N", "POSE_C:N", "Other_C:N",
                     "BRTE_C:N", "SurSo4 (kg/ha)", "Ca (ppm)", "Mg (ppm)", "Soil_%C", 
                     "Soil_%N", "NH4 (kg/ha)",
                     "Surno3 (kg/ha)", "Total_mineral_N", "%TMN_that_is_NH4", 
                     "%TMN_that_is_NO3", "%_cover_total_shrubs", "PNG", "PNF","ANF",
                     "AIG", "AIF","total_veg_cover","Bare","Gravel", "Rock","Litter",
                     "Cryptogams", "Total_plant_N (g)", "Total_plant_C (g)", 
                     "Total_plant_weight (g)")
metadata_13 = Jones2013[-c(4:5, 14:16), -c(1:2, 17:23)]

Jones2013_lf = data.frame(Jones2013[,1], Jones2013[,17:22])
colnames(Jones2013_lf) = c("Plot", "Shrub", "PNG", "PNF", "ANF", "AIG", "AIF")
Jones2013_lf$Plot = paste("J",Jones2013_lf$Plot, sep="")
Jones2013_lf = Jones2013_lf[-c(4:5),]
rownames(Jones2013_lf) = Jones2013_lf$Plot
Jones2013_lf$Plot = NULL
Jones2013_lf = Jones2013_lf[-c(14:16),]



#### spatial ############

locations = read.csv("data/locations.csv")




#### basic linear models ########################

mod1=lm(net.mineralization ~ AIG, metadata2)
plot(net.mineralization ~ AIG, metadata2)
abline(mod1, col="red")

nitr_mod=lm(net.netrification ~ AIG, metadata2)
plot(net.netrification ~ AIG, metadata2)
abline(nitr_mod, col="red")

NO3_mod=lm(NO3.N..ppm. ~ AIG, metadata2)
plot(NO3.N..ppm. ~ AIG, metadata2)
abline(NO3_mod, col="red")

NH4_mod=lm(NH4.N..ppm. ~ AIG, metadata2)
plot(NH4.N..ppm. ~ AIG, metadata2)
abline(NH4_mod, col="red")


#### pca ########
# 
# rda1 = vegan::rda(cover_by_plot, basic_meta_plot[,1:6])
# rda2 = rda(metadata2[,1:11], na.action = na.omit)
# 
# plot(rda2)
# head(cover_by_plot)

standardized__ = decostand(metadata2[,1:11], "standardize")
pca1 = princomp(standardized__[,1:11])
#plot(pca1)
#biplot(pca1)

ggbiplot(pca1, labels =  factor(metadata1$BC_cluster)) +
  theme_bw() +
  ggtitle("2016 PCA")
# ggbiplot(pca1, labels =  factor(metadata1$Type)) +
#   theme_bw()
Jones2013_pca =Jones2013[-c(4,5,16,17,18),]
pca13 = princomp(decostand(Jones2013_pca[,c(17:22,24:28)], "standardize"), scores=T)
pca13 = prcomp(decostand(Jones2013_pca[,c(17:22,24:28)], "standardize"))

ggbiplot(pca13, labels = factor(Jones2013_pca$site_type)) +
  theme_bw() +
  ggtitle("2013 PCA")

pca1 = princomp(metadata2[,1:11], "standardize", scores = T)

ggbiplot(pca1, labels =  factor(metadata1$BC_cluster)) +
  theme_bw()

pca13 = princomp(decostand(Jones2013[-c(4,5,16,17,18),c(17:22,24:28)], "standardize"), scores=T)

ggbiplot(pca13, labels =  factor(Jones2013[-c(4,5,16,17,18),]$site_type)) +
  theme_bw()



pro = procrustes(pca13, pca1)
plot(pro, ar.col = metadata1$BC_cluster, main = "PCA Change from 2013 - 2016")
legend(-1.07, -0.36,
       title = "2016 Bray-Curtis Classification",
       c("Intact Sagebrush", "Invaded Sagebrush", "Cheatgrass-Dominated", "Cheatgrass Dieoff"), 
       col = c("green", "blue", "black", "red"), 
       pch = 19)
protest(comm.bc.mds, comm13.bc.mds)


#### BIODIVERSITY #################

# following the biodiversity analysis tutorial at:              #
# http://kembellab.ca/r-workshop/biodivR/SK_Biodiversity_R.html #



# Turn percent cover to relative abundance by dividing each value by sample
# total abundance
#comm <- decostand(cover_by_plot, method = "total")
comm <- decostand(life_forms_just_plants_byPlot, method = "total")
#colnames(comm) = c("AIG", "AIF", "PNG", "Shrub", "ANF", "PNF")

comm13 <- decostand(Jones2013_lf, method = "total")

# #### species accumulion curves 
# # all plots
# plot(specaccum(decostand(cover_by_plot, method = "total")), xlab = "# of samples", ylab = "# of species")
# 
# #by type (BC clust and field judgement)
# plot(specaccum(comm[metadata1$Type == "I",]), xlab = "# of samples", ylab = "# of species", col = "green")
# plot(specaccum(comm[metadata1$Type == "M",]), add = T, col = "blue")
# plot(specaccum(comm[metadata1$Type == "C",]), add = T, col = "gold")
# plot(specaccum(comm[metadata1$Type == "D",]), add = T, col = "red")
# 
# 
# plot(specaccum(comm[metadata1$BC_cluster == "I",]), xlab = "# of samples", ylab = "# of species", col = "green")
# plot(specaccum(comm[metadata1$BC_cluster == "M",]), add = T, col = "blue")
# plot(specaccum(comm[metadata1$BC_cluster == "C",]), add = T, col = "gold")
# plot(specaccum(comm[metadata1$BC_cluster == "D",]), add = T, col = "red")

#### classifying communities ##

rankindex(scale(metadata2), cover_by_plot, c("euc","man","bray","jac","kul")) #bray is good
# calculate Bray-Curtis distance among samples
comm.bc.dist <- vegdist(comm, method = "bray")
comm.bc.dist <- vegdist(comm13, method = "bray")

# cluster communities using average-linkage algorithm
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
comm.bc.clustc <- hclust(comm.bc.dist, method = "complete")
comm.bc.clusts <- hclust(comm.bc.dist, method = "single")

# Cophenetic correlation measures the similarity between original dissimilarities and dissimilarities
# estimated from the tree. (from vegantutor)

cor(comm.bc.dist, cophenetic(comm.bc.clust)) # 0.93
cor(comm.bc.dist, cophenetic(comm.bc.clustc)) # 0.88
cor(comm.bc.dist, cophenetic(comm.bc.clusts)) # 0.90

# plot cluster diagram
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity")
rect.hclust(comm.bc.clust,4)
text(2.5,.4, "Mixed Sagebrush \nand Cheatgrass", cex =1.2, col = "blue")
text(8,.4, "Intact Sagebrush", cex =1.2, col = "darkgreen")
text(13.5,.4, "Cheatgrass-\nDominated", cex =1.2, col = "gold")
text(18,.4, "Cheatgrass Dieoff", cex =1.2, col = "red")

#### NMDS ###

# The metaMDS function automatically transforms data and checks solution
# robustness

#### 2016 ###
comm.bc.mds <- metaMDS(comm, dist = "bray")
stressplot(comm.bc.mds) # Assess goodness of ordination fit (stress plot)
ordiplot(comm.bc.mds, display = "sites", type = "text") # plot site scores as text
ordipointlabel(comm.bc.mds) # automated plotting of results - tries to eliminate overlapping labels

#### 2013 ###
comm13.bc.mds <- metaMDS(comm13, dist = "bray")
stressplot(comm13.bc.mds) # Assess goodness of ordination fit (stress plot)
ordiplot(comm13.bc.mds, display = "sites", type = "text") # plot site scores as text
ordipointlabel(comm13.bc.mds) # automated plotting of results - tries to eliminate overlapping labels


#### 2016 ###

mds.fig <- ordiplot(comm.bc.mds, type = "none", main = "2016 data, Colors based on 2013 classification")
# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = metadata1$Type == "I")
points(mds.fig, "sites", pch = 19, col = "blue", select = metadata1$Type == "M")
points(mds.fig, "sites", pch = 19, col = "gold", select = metadata1$Type == "C")
points(mds.fig, "sites", pch = 19, col = "red", select = metadata1$Type == "D")
ordiellipse(comm.bc.mds, metadata1$Type, conf = 0.95, label = TRUE)

mds.fig <- ordiplot(comm.bc.mds, type = "none", main = "2016 data, Colors based on BC-Clustering")
# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = metadata1$BC_cluster == "I")
points(mds.fig, "sites", pch = 19, col = "blue", select = metadata1$BC_cluster == "M")
points(mds.fig, "sites", pch = 19, col = "gold", select = metadata1$BC_cluster == "C")
points(mds.fig, "sites", pch = 19, col = "red", select = metadata1$BC_cluster == "D")
ordiellipse(comm.bc.mds, metadata1$BC_cluster, conf = 0.95, label = TRUE)

plot(envfit(comm.bc.mds, metadata2[,-c(1:12)], na.rm = T), p.max=0.05)



# overlay the cluster results we calculated earlier
ordicluster(comm.bc.mds, comm.bc.clust, col = "gray")


#### 2013 ###

mds.fig <- ordiplot(comm13.bc.mds, type = "none", main = "2013 data")
# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = metadata1$Type == "I")
points(mds.fig, "sites", pch = 19, col = "blue", select = metadata1$Type == "M")
points(mds.fig, "sites", pch = 19, col = "gold", select = metadata1$Type == "C")
points(mds.fig, "sites", pch = 19, col = "red", select = metadata1$Type == "D")
ordiellipse(comm.bc.mds, metadata1$Type, conf = 0.95, label = TRUE)
plot(envfit(comm.bc.mds, metadata_13, na.rm = T), p.max=0.05)

mds.fig <- ordiplot(comm13.bc.mds, type = "none")

# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = metadata1$BC_cluster == "I")
points(mds.fig, "sites", pch = 19, col = "blue", select = metadata1$BC_cluster == "M")
points(mds.fig, "sites", pch = 19, col = "gold", select = metadata1$BC_cluster == "C")
points(mds.fig, "sites", pch = 19, col = "red", select = metadata1$BC_cluster == "D")
ordiellipse(comm13.bc.mds, metadata1$BC_cluster, conf = 0.95, label = TRUE)

#### Change from 2013 to 2016 ###

pro = procrustes(comm.bc.mds, comm13.bc.mds)
plot(pro, ar.col = metadata1$BC_cluster, main = "NMDS Change from 2013 - 2016")
legend(-1.07, -0.36,
       title = "2016 Bray-Curtis Classification",
       c("Intact Sagebrush", "Invaded Sagebrush", "Cheatgrass-Dominated", "Cheatgrass Dieoff"), 
       col = c("green", "blue", "black", "red"), 
       pch = 19)
protest(comm.bc.mds, comm13.bc.mds)


#### Kruskal-Wallis ON EVERYTHING #####
#### Functions ####

mass_krus = function(x, y){
  # This function inputs a data frame, and outputs another data frame
  # of Kruskall-Wallis test results on that data. x is the many response
  # variables, y is one column - the treatment. It runs the K-W test 
  # column by column.
  
  krus = list()
  for(i in 1:(length(x))){
    means = as.data.frame(kruskal(x[,i], y)$means)
    groups = as.data.frame(kruskal(x[,i], y)$groups)
    means$Treatment = rownames(means)
    groups$Treatment = rownames(groups)
    krus[[i]] = plyr::join(means, groups, by = "Treatment")
    #colnames(krus[[i]]) = c("variable", "something","std", "r", "Min", "Max", "trt", "means", "Groups")
    names(krus[[i]])[1] = "means"
    krus[[i]]$variable = as.factor(colnames(x[i]))
  }
  tuks_df = as.data.frame(do.call("rbind", krus))
  tuks_df$medians = round(tuks_df$Q50, 2)
  tuks_df$Treatment = as.factor(tuks_df$Treatment)
  tuks_df$Treatment = factor(tuks_df$Treatment, levels(tuks_df$Treatment)[c(3,4,1,2)])
  tuks_df$median_std = paste0(tuks_df$medians, " (", round(tuks_df$std,2), ")")
  tuks_df$groups <- factor(tuks_df$groups, levels = c("a", "ab", "b", "bc", "c"))
  #colnames(tuks_df) = c("Variable", "something","std", "r", "min", "max", "Treatment", "means", "Group", "mean_std")
  return(tuks_df)
}

class_ = function(x){
  ifelse(x == "a", 0, 1 )
}

plotit = function(kdf, title){
  only_sigs = kdf %>%
    #mutate(M_id = class_(groups)) %>%
    group_by(variable) # %>%
    #summarise(min_ID = sum(M_id)) %>%
    #filter(min_ID >= 1 ) %>%
    #left_join(.,kdf, by="variable")

  ggplot(na.omit(only_sigs), aes(x=Treatment, y = variable)) +
    geom_tile(aes(fill = as.factor(groups)), color = "grey10") +
    theme_bw() + 
    scale_fill_grey(start = 1, end = 0.5) +
    geom_text(aes(label=median_std)) +
    ggtitle(paste(title))
}


#### metadata prep for KW test ####

Jones13kw = read.csv("data/Jones2013_nov_2017.csv")
Jones16 = read.csv("data/jones_all_nov_2017.csv")
Jones16kw = summaryBy(.~Site_number, data = Jones16, keep.names = TRUE, na.rm = TRUE)
Jones16kw$Site_type = Jones16$Site_type[c(seq(1,60,3))]
Jones16kw$Cluster = Jones16$Cluster[c(seq(1,60,3))]
rownames(Jones16kw) = Jones13kw$Plot
rownames(Jones13kw) = Jones13kw$Plot

Jones16kw = dplyr::select(Jones16kw,
                          SOIL_SurSo4_kg_ha,
                          SOIL_Ca_ppm,
                          SOIL_Mg_ppm,
                          SOIL_OM_pct,
                          SOIL_TN_pct,
                          SOIL_CN,
                          Litter_CN,
                          Poa_CN,           
                          Other_CN,
                          Bromus_CN,
                          PNG,       
                          PNF,
                          ANF,
                          AIG,                 
                          AIF,
                          Shrubs,
                          Bare,
                          Litter,
                          Crypto,              
                          Rock)

Jones13kw = dplyr::select(Jones13kw,
                          SOIL_SurSo4_kg_ha,
                          SOIL_Ca_ppm,
                          SOIL_Mg_ppm,
                          SOIL_OM_pct,
                          SOIL_TN_pct,
                          SOIL_CN,
                          Litter_CN,
                          Poa_CN,           
                          Other_CN,
                          Bromus_CN,
                          PNG,       
                          PNF,
                          ANF,
                          AIG,                 
                          AIF,
                          Shrubs,
                          Bare,
                          Litter,
                          Crypto,              
                          Rock
                          )

#### Run the tests and plot the results ####
k13T = mass_krus(Jones13T[,!names(Jones13T) %in% "Site_type"], Jones13T$Site_type)
k16T = mass_krus(Jones16T[,!names(Jones16T) %in% "Site_type"], Jones16T$Site_type)
plotit(k13T, "2013T")
plotit(k16T, "2016T")

k13 = mass_krus(glmm13[,!names(glmm13) %in% c("Site.type", "Site_number", "Transect", "Year")], glmm13$Site.type)
k16 = mass_krus(glmm16[,!names(glmm16) %in% c("Site.type", "Site_number", "Transect", "Year")], glmm16$Site.type)
plotit(k13, "2013T")
plotit(k16, "2016T")

#### pca redone with soil info incorporated using the comparable data sets ####
# replace 2016 shrubs with 2013 shrubs
Jones16kw$Shrubs = Jones13kw$Shrubs

pca13 = princomp(decostand(Jones13kw[,c(3:8,11,18:23, 25:28)], "standardize"), scores=T)
ggbiplot(pca13, labels =  factor(Jones13kw$Cluster)) +
  theme_bw() +
  ggtitle("2013")

pca16 = princomp(decostand(Jones16kw[,c(3:8,11,18:23, 25:28)], "standardize"), scores=T)
ggbiplot(pca16, labels =  factor(Jones16kw$Cluster)) +
  theme_bw() +
  ggtitle("2016")

plot(cca(X = Jones13kw[,c(18:23)], Y = Jones13kw[,c(3:8,11, 14:17)]), col = Jones13kw$Cluster, pch=19)
plot(cca(X = Jones16kw[,c(18:23)], Y = Jones16kw[,c(3:8,11, 14:17)]))

#### cca with formulas ####

cca16_basic = cca(cover_by_plot, Jones16kw[,c(3:8,11,14:17)])
plot(cca16_basic)

cca_form_13 = cca(Jones13kw[,c(18:23)] ~ 
                 #SOIL_TN_pct +
                 #SOIL_OM_pct +
                 SOIL_Mg_ppm +
                 SOIL_Ca_ppm +
                 SOIL_SurSo4_kg_ha +
                 SOIL_total_mineral_N +
                 SOIL_CN +
                 Bromus_CN +
                 Other_CN +
                 Litter_CN +
                 Poa_CN,
               data = Jones13kw);plot(cca_form_13,
                                      main = "2013 cca")

cca_form_16 = cca(Jones16kw[,c(18:23)] ~ 
                    #SOIL_TN_pct +
                   # SOIL_OM_pct +
                    SOIL_Mg_lbs_Acre +
                    SOIL_Ca_lbs_Acre +
                    SOIL_SurSo4_kg_ha +
                    SOIL_total_mineral_N +
                    SOIL_CN +
                    Bromus_CN +
                    Other_CN +
                    Litter_CN +
                    Poa_CN,
                  data = Jones16kw);plot(cca_form_16,
                                         main = "2016 cca")

anova(cca_form_13, by = "term", perm = 500)
anova(cca_form_16, by = "term", perm = 500)

anova(cca_form_13, by = "margin", perm = 5000)
anova(cca_form_16, by = "margin", perm = 5000)

mod0_16 = cca(Jones16kw[,c(18:23)] ~1, Jones16kw)
mod_16 = step(mod0_16, scope = formula(cca_form_16), test = "perm")

mod0_13 = cca(Jones13kw[,c(18:23)] ~1, Jones13kw)
mod_13 = step(mod0_13, scope = formula(cca_form_13), test = "perm")

vif.cca(mod_13)
vif.cca(mod_16)

plot(mod_13, main = "2013"); plot(mod_16, main = "2016")
plot(procrustes(mod_13, mod_16))

pro = plot(procrustes(cca_form_13, cca_form_16))
plot(pro, ar.col = Jones13kw$Cluster, main = "CCA Change from 2013 - 2016")

rda16_basic = rda(cover_by_plot, Jones16kw[,c(3:8,11,14:17)])
plot(rda16_basic)

rda_form = rda(Jones16kw[,c(18:23)] ~ 
                 SOIL_TN_pct +
                 SOIL_OM_pct +
                 SOIL_Mg_lbs_Acre +
                 SOIL_SurSo4_kg_ha +
                 SOIL_net_mineralization +
                 Other_CN +
                 Litter_CN +
                 Poa_CN +
                 Litter,
               data = Jones16kw);plot(rda_form)

#### redoing NMDS ####


#### 2016 ##
comm.bc.mds <- metaMDS(Jones16kw[,c(18:22)], dist = "bray")
# stressplot(comm.bc.mds) # Assess goodness of ordination fit (stress plot)
# ordiplot(comm.bc.mds, display = "sites", type = "text") # plot site scores as text
# ordipointlabel(comm.bc.mds) # automated plotting of results - tries to eliminate overlapping labels

#### 2013 ##
comm13.bc.mds <- metaMDS(Jones13kw[,c(18:22)], dist = "bray")
# stressplot(comm13.bc.mds) # Assess goodness of ordination fit (stress plot)
# ordiplot(comm13.bc.mds, display = "sites", type = "text") # plot site scores as text
# ordipointlabel(comm13.bc.mds) # automated plotting of results - tries to eliminate overlapping labels


#### 2016 ##

mds.fig <- ordiplot(comm.bc.mds, type = "none", main = "2016 data, Colors based on 2013 classification")
# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = Jones16kw$Site_type == "I")
points(mds.fig, "sites", pch = 19, col = "blue", select = Jones16kw$Site_type == "M")
points(mds.fig, "sites", pch = 19, col = "gold", select = Jones16kw$Site_type == "C")
points(mds.fig, "sites", pch = 19, col = "red", select = Jones16kw$Site_type == "D")
#ordiellipse(comm.bc.mds,Jones16kw$Site_type, conf = 0.95, label = TRUE)
plot(envfit(comm.bc.mds, Jones16kw[,c(3:8,11,14:17)], na.rm = T), p.max=0.05)


mds.fig <- ordiplot(comm.bc.mds, type = "none", main = "2016 data, Colors based on BC-Clustering")
# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = Jones16kw$Cluster == "I")
points(mds.fig, "sites", pch = 19, col = "blue", select = Jones16kw$Cluster == "M")
points(mds.fig, "sites", pch = 19, col = "gold", select = Jones16kw$Cluster == "C")
points(mds.fig, "sites", pch = 19, col = "red", select = Jones16kw$Cluster == "D")
#ordiellipse(comm.bc.mds,Jones16kw$Cluster, conf = 0.95, label = TRUE)
plot(envfit(comm.bc.mds, Jones16kw[,c(3:8,11,14:17)], na.rm = T), p.max=0.05)



# overlay the cluster results we calculated earlier
ordicluster(comm.bc.mds, comm.bc.clust, col = "gray")


#### 2013 ##

mds.fig <- ordiplot(comm13.bc.mds, type = "none", main = "2013 data")
# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = Jones16kw$Site_type == "I")
points(mds.fig, "sites", pch = 19, col = "blue", select = Jones16kw$Site_type == "M")
points(mds.fig, "sites", pch = 19, col = "gold", select = Jones16kw$Site_type == "C")
points(mds.fig, "sites", pch = 19, col = "red", select = Jones16kw$Site_type == "D")
#ordiellipse(comm13.bc.mds, Jones13kw$Site_type, conf = 0.95, label = TRUE)
plot(envfit(comm13.bc.mds, Jones13kw[,c(3:8,11,14:17)], na.rm = T), p.max=0.05)

mds.fig <- ordiplot(comm13.bc.mds, type = "none")

# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = Jones16kw$Cluster == "I")
points(mds.fig, "sites", pch = 19, col = "blue", select = Jones16kw$Cluster == "M")
points(mds.fig, "sites", pch = 19, col = "gold", select = Jones16kw$Cluster == "C")
points(mds.fig, "sites", pch = 19, col = "red", select = Jones16kw$Cluster == "D")
#ordiellipse(comm13.bc.mds, Jones13kw$Cluster, conf = 0.95, label = TRUE)
plot(envfit(comm13.bc.mds, Jones13kw[,c(3:8,11,14:17)], na.rm = T), p.max=0.05)

#### Change from 2013 to 2016 ##

pro = procrustes(comm.bc.mds, comm13.bc.mds)
plot(pro, ar.col = Jones13kw$Site_type, main = "NMDS Change from 2013 - 2016")
legend(-1.07, -0.36,
       title = "2016 Bray-Curtis Classification",
       c("Intact Sagebrush", "Invaded Sagebrush", "Cheatgrass-Dominated", "Cheatgrass Dieoff"), 
       col = c("green", "blue", "black", "red"), 
       pch = 19)
protest(comm.bc.mds, comm13.bc.mds)


##### weather data #######

# precip is in inches

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
write.csv(yearly_precip_sums,"data/ppt_sums.csv")

ggplot(data=yearly_precip_sums, aes(x=year, y=Precip_mm)) +
  geom_hline(yintercept = 210.566, col="grey60") +
  geom_point() +
  ylab("Precipitation (mm)") +
  xlab("Year") +
  annotate("text",x= 2013, y=213, label = "30 year normal = 210 mm")

# mean ppt = 8.29 in, 8.29*25.4 = 210.566


############### lmming #####
ppt = read.csv("data/ppt_sums.csv")

glmm13 = Jones13kw
glmm13$Year = 2013
glmm13$ppt = ppt$X2011.2013_ppt[-c(4,5,16:18)]
glmm13$Plot = as.factor(rownames(glmm13))
glmm16 = Jones16kw
glmm16$Year = 2016
glmm16$ppt = ppt$X2014.2016_ppt[-c(4,5,16:18)]
glmm16$Plot = as.factor(rownames(glmm16))

names(glmm13) = names(glmm16) #not legit

all = rbind(glmm13, glmm16)
all$Year = as.factor(all$Year)

#### linear modelling ####

best_models = list()
model_list = list()

pred = dplyr::select(all,
                     SOIL_SurSo4_kg_ha,
                     SOIL_Ca_ppm,
                     SOIL_Mg_ppm,
                     SOIL_OM_pct,
                     SOIL_TN_pct,
                     SOIL_CN
  )

resp = dplyr::select(all,
                     Poa_CN,
                     Other_CN,
                     Bromus_CN,
                     Litter_CN
                     )

Precip <- all$ppt
Year <- all$Year

for(i in 1:length(resp)){
  model_list[[i]] = list()
  
  m0 = lm(resp[,i] ~ 1, pred) # model with 0 predictors
  m1 = lm(resp[,i] ~ . * Precip, pred) # model with all possible predictors
  model_list[[i]][[1]] = names(resp[i]) # just getting the name of the resonse
  model_list[[i]][[2]] = step(m0, scope = formula(m1)) # building models starting with nothing then everything
  model_list[[i]][[3]] = step(m1, scope = list(lower = formula(m0), upper = formula(m1)), trace = 0)
  
  ml2 = model_list[[i]][[2]]
  ml3 = model_list[[i]][[3]]
  
  ## now defining the best model as that with the lowest VIF
  ## yes I know that's a totally oversimplified method
  
  if(length(ml2$coefficients) >2){
    model_list[[i]][[4]] = vif(ml2)}
  if(length(ml3$coefficients) >2){
    model_list[[i]][[5]] = vif(ml3)}
  
  if(length(ml3$coefficients) >2 && length(ml2$coefficients) >2){
  
  v2 = sum(model_list[[i]][[4]])
  v3 = sum(model_list[[i]][[5]])
  
    if(v2 < v3){
      best_models[i] = ifelse(v2 > 0, list(ml2),list(ml3))}
    else{
      best_models[i] = ifelse(v3 > 0, list(ml3),list(ml2))
    }
  }
}


#### Plotting the model coefficients ####

df_list = list()
best_models = list.clean(best_models, fun = is.null, recursive = FALSE)
for(i in 1:length(best_models)){
  s = summary(best_models[[i]])
  x = s$coefficients
  d = as.data.frame(x)
  d$predictor = rownames(d)
  d$response = as.character(s$call$formula[2])
  df_list[[i]] = d[-1,] # removing the intercept
  df_list[[i]][,6] = paste0(model_list[[i]][1], " (" ,round(s$r.squared,2),")")
}

full_df = as.data.frame(do.call("rbind", df_list))
# full_df$is_sig = ifelse(full_df$`Pr(>|t|)` < 0.05, 1,NA)
full_df$effect_size = abs(full_df$Estimate)
full_df$sign = as.factor(ifelse(full_df$Estimate > 0, "positive","negative"))

ggplot(na.omit(full_df), aes(x=response, y = predictor)) +
  geom_count(aes(size = sqrt(effect_size), color = sign), shape = 19) + 
  theme_bw() + 
  scale_color_manual(values = c("black", "grey65")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Significant (p < 0.05) Soil Predictors of Plant Nutrients")

#### effects ####
for(i in 1:length(best_models)){
  plot(allEffects(best_models[[i]]), ylab=model_list[[i]][[1]])
}
plot(allEffects(model_list[[4]][[3]]), ylab=model_list[[4]][[1]])
#### correlation matrix ####
# copied from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

#### creating and reshaping the correlation matrix
cormat <- round(cor(pred, resp, use="pairwise.complete.obs"),2)
cormat = round(cor(resp, use="pairwise.complete.obs"),2)

melted_cormat <- melt(cormat)

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# take out low correlation values in the interest of visualization
melted_cormat$vis = ifelse(abs(melted_cormat$value) >0.3, melted_cormat$value, NA ) 


# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "grey60", size = 4) +
  geom_text(aes(Var2, Var1, label = vis), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, 
                                 barheight = 1,
                                 title.position = "top", 
                                 title.hjust = 0.5))


#### data explore ####

library(reshape2)
library(ggplot2)
d <- melt(all[,-c(1:3,32)])
d = ggplot(d,aes(x = value)) + 
  facet_wrap(~variable, scales = "free") + 
  geom_density()

ggsave("Images/density_plots.png", limitsize = FALSE)
