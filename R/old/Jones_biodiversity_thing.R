library(doBy)
library(plyr)

jones_master_cover <- read.csv("data/Jones_Cover - ArealCover.csv", header = TRUE)

summary(jones_master_cover)

###########################
# creating a cover matrix #
###########################

cover = jones_master_cover
cover$plot_tran = paste0(cover$Plot.,cover$TransectPair)
cover_by_plot_tran = summaryBy(. ~ plot_tran, data=na.omit(cover), FUN = mean, keep.names = TRUE)
cover_by_plot = summaryBy(. ~ Plot., data = na.omit(cover), FUN=mean, keep.names = TRUE)

rownames(cover_by_plot_tran) = cover_by_plot_tran$plot_tran
rownames(cover_by_plot) = cover_by_plot$plot_tran

cover_by_plot = cover_by_plot[,-c(1:11,15,26,33,45,61)]
cover_by_plot_tran = cover_by_plot_tran[,-c(1:11,15,26,33,45,61)]


cover_by_plot_tran$plot_tran = NULL
cover_by_plot$plot_tran = NULL



locations = read.csv("locations.csv")

########################
##### plants n cover ###
########################

Jones_AC$Plt_Trn_SP = paste(Jones_AC$Plot_Tran, Jones_AC$subPlot, sep="")
rownames(Jones_AC) = Jones_AC$Plt_Trn_SP
Jones_AC_plants_n_more= data.frame(Jones_AC[,3], Jones_AC[6:61])
Jones_AC_plants_n_more$Inv_AG_A=NULL
Jones_AC_plants_n_more$Inv_AF_A=NULL
Jones_AC_plants_n_more$Nat_PG_A=NULL
Jones_AC_plants_n_more$Shrub_A=NULL
Jones_AC_plants_n_more$Nat_AF_A=NULL
Jones_AC_plants_n_more$Nat_PF_A=NULL
Jones_AC_plants_n_more$Jones_AC...3. = as.factor(Jones_AC_plants_n_more$Jones_AC...3.)
Jones_AC_plants_n_more$Dist_2_sage = Jones_AC_plants_n_more$Jones_AC...3.
Jones_AC_plants_n_more$Jones_AC...3.=NULL
Jones_AC_plants_n_more$SeedlingsPA = as.factor(ifelse(Jones_AC_plants_n_more$FirstYearSeedlingDensity>0,1,0))

x=summaryBy(.~Plot_Tran, FUN=mean, data=Jones_AC, na.rm=TRUE)
write.csv(x, "Jones_AC_by_plotTran.csv")

life_forms = data.frame(x[,1:7],x[,11],x[,22],x[29],x[,35],x[,41],x[,57])
colnames(life_forms) = c("Plot_TP", "Litter", "Bare", "Crypto", "Rock","Dung", "FirstYearSeedlings", "AIG", "AIF", "NPG", "Shrub", "ANF", "PNF")
write.csv(life_forms, "LifeForms_AC_by_plotTran.csv")
jones_all = join(life_forms, Jones_all_except_cover, by = "Plot_TP")
rownames(life_forms) = life_forms$Plot_TP

life_forms$Plot_TP = NULL
life_forms$SeedlingsPA = as.factor(ifelse(life_forms$FirstYearSeedlings>0,"present","absent"))

Jones_all_except_cover = read.csv("Jones_all_transect_pairs.csv", header = TRUE)
Jones_all_except_cover$Plot = paste(substr(Jones_all_except_cover$Plot_TP, 1,3))



Jones_all_plot = summaryBy(.~Plot, data = jones_all, keep.names = TRUE, na.rm = TRUE)
Jones_all_plot[11,23:25] = 0
Jones_all_plot[13,23:25] = 0
Jones_all_plot[16,23:25] = 0
Jones_all_plot[20,23:25] = 0

rownames(Jones_all_plot) = Jones_all_plot$Plot
Jones_all_plot$Plot = NULL

Jones_all_plot$SeedlingPresence = as.factor(ifelse(Jones_all_plot$FirstYearSeedlings > 0 , 1 , 0))

#### random forests: explore important variables

#library(randomForest)

#Jones_AC_plants_n_more_noNA = na.omit(Jones_AC_plants_n_more)

#SeedlingsRF = randomForest(SeedlingPresence ~ ., data = Jones_all_plot[,-6], ntree = 10000, importance = TRUE)
#varImpPlot(SeedlingsRF)


#SeedlingsRF_sp = randomForest(SeedlingsPA ~ ., data = Jones_AC_plants_n_more_noNA[,-6], ntree = 5000, importance = TRUE)
#varImpPlot(SeedlingsRF_sp)



### models

mod1=lm(net.mineralization ~ AIG, Jones_all_plot)
plot(net.mineralization ~ AIG, Jones_all_plot)
abline(mod1, col="red")

nitr_mod=lm(net.netrification ~ AIG, Jones_all_plot)
plot(net.netrification ~ AIG, Jones_all_plot)
abline(nitr_mod, col="red")

NO3_mod=lm(NO3.N..ppm. ~ AIG, Jones_all_plot)
plot(NO3.N..ppm. ~ AIG, Jones_all_plot)
abline(NO3_mod, col="red")

NH4_mod=lm(NH4.N..ppm. ~ AIG, Jones_all_plot)
plot(NH4.N..ppm. ~ AIG, Jones_all_plot)
abline(NH4_mod, col="red")


library(MASS)
library(foreign)

rmod1=rlm(net.mineralization ~ AIG, Jones_all_plot)
plot(net.mineralization ~ AIG, Jones_all_plot)
abline(rmod1, col="red")

rmod2=rlm(net.mineralization ~ AIG, jones_all)
plot(net.mineralization ~ AIG, jones_all)
abline(rmod2, col="red")

##adding in brte mass

BRTE_Mass = read.csv("BRTE_Mass_Jones - Sheet1.csv", header=F)
BRTE_Mass$Plot = paste(substr(BRTE_Mass$V1,1,4))
colnames(BRTE_Mass) = c("Plot_Bags", "Mass", "Plot")

BRTE_Mass_byPlot = summaryBy(Mass ~ Plot, data = BRTE_Mass, FUN=sum, keep.names = TRUE)
BRTE = data.frame(Jones_AC[,12], Jones_AC[,62])
colnames(BRTE) = c("BRTE", "Plot")
BRTE_byPlot = summaryBy(BRTE ~ Plot, data = BRTE, FUN=mean, keep.names = TRUE)

BRTE_cover_x_mass = join(BRTE_Mass_byPlot, BRTE_byPlot, by="Plot")

write.csv(BRTE_cover_x_mass, "BRTE_cover_mass.csv")

## data mongering

Jones_Cover = read.csv("Jones_AC.csv")

Jones_Cover$Pl_Tr_Sp = paste(Jones_Cover$Plot_Tran, "_", Jones_Cover$subPlot, sep="")

rownames(Jones_Cover)= Jones_Cover$Pl_Tr_Sp

Jones_Cover$X = NULL
Jones_Cover$subPlot= NULL
Jones_Cover$Plot. = NULL
Jones_Cover$NearestSage = NULL
Jones_Cover$TransectPair = NULL
Jones_Cover$Pl_Tr_Sp = NULL
Jones_Cover$Plot_Tran = NULL
Jones_Cover$Date = NULL
Jones_Cover$Litter = NULL
Jones_Cover$Bare = NULL
Jones_Cover$Rock = NULL
Jones_Cover$Crypto = NULL
Jones_Cover$Dung = NULL
Jones_Cover$FirstYearSeedlingDensity = NULL

Jones_Cover_Just_Plants = data.frame(Jones_Cover[,1:3], Jones_Cover[,5:14], Jones_Cover[,16:21],Jones_Cover[,23:27],Jones_Cover[,29:33], Jones_Cover[,35:49])

Jones_Cover_Just_Plants_byPlot = Jones_Cover_Just_Plants



Jones_Cover_Just_Plants_byPlot$Plot = paste(substr(rownames(Jones_Cover_Just_Plants_byPlot),1,3))

Jones_Cover_Just_Plants_byPlot = summaryBy(.~ Plot, data = Jones_Cover_Just_Plants_byPlot, FUN=mean, keep.names = TRUE, na.rm = TRUE)

rownames(Jones_Cover_Just_Plants_byPlot) = Jones_Cover_Just_Plants_byPlot$Plot

Jones_Cover_Just_Plants_byPlot$Plot = NULL

write.csv(Jones_Cover_Just_Plants, "Jones_Cover_Plants_only")

JCBP = Jones_Cover_Just_Plants_byPlot

life_forms_just_plants_byPlot = Jones_all_plot[7:12]
#rownames(life_forms_just_plants_byPlot) = life_forms_just_plants_byPlot[,1]
#life_forms_just_plants_byPlot$Jones_all_plot...1. = NULL

LFBP = life_forms_just_plants_byPlot

Jones2013 = read.csv("Jones2013.csv")
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

#################################################################
# following the biodiversity analysis tutorial at:              #
# http://kembellab.ca/r-workshop/biodivR/SK_Biodiversity_R.html #
#################################################################

library(picante)

# check total abundance in each sample
apply(JCBP, 1, sum)

# Turn percent cover to relative abundance by dividing each value by sample
# total abundance
comm <- decostand(JCBP, method = "total")
#comm <- decostand(LFBP, method = "total")
colnames(comm) = c("AIG", "AIF", "PNG", "Shrub", "ANF", "PNF")

comm13 <- decostand(Jones2013_lf, method = "total")


# check total abundance in each sample
apply(comm, 1, sum)

# look at the transformed data
comm[1:5, 1:5]

############ metadata #########################
metadata = read.csv("jones_all_oct5.csv")
x = colnames(metadata)
x[13] = "PNG"
colnames(metadata) = x 

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


############### species accumulion curves ###############################
# all plots
plot(specaccum(decostand(JCBP, method = "total")), xlab = "# of samples", ylab = "# of species")

#by type (BC clust and field judgement)
plot(specaccum(comm[metadata1$Type == "I",]), xlab = "# of samples", ylab = "# of species", col = "green")
plot(specaccum(comm[metadata1$Type == "M",]), add = T, col = "blue")
plot(specaccum(comm[metadata1$Type == "C",]), add = T, col = "gold")
plot(specaccum(comm[metadata1$Type == "D",]), add = T, col = "red")


plot(specaccum(comm[metadata1$BC_cluster == "I",]), xlab = "# of samples", ylab = "# of species", col = "green")
plot(specaccum(comm[metadata1$BC_cluster == "M",]), add = T, col = "blue")
plot(specaccum(comm[metadata1$BC_cluster == "C",]), add = T, col = "gold")
plot(specaccum(comm[metadata1$BC_cluster == "D",]), add = T, col = "red")
########### classifying communities ########################

rankindex(scale(metadata2), LFBP, c("euc","man","bray","jac","kul")) #bray is good
# calculate Bray-Curtis distance among samples
comm.bc.dist <- vegdist(comm, method = "bray")
# cluster communities using average-linkage algorithm
comm.bc.clust <- hclust(comm.bc.dist, method = "average")
comm.bc.clustc <- hclust(comm.bc.dist, method = "complete")
comm.bc.clusts <- hclust(comm.bc.dist, method = "single")

# Cophenetic correlation measures the similarity between original dissimilarities and dissimilarities
# estimated from the tree. (from vegantutor)

cor(comm.bc.dist, cophenetic(comm.bc.clust)) # 0.8798922 
cor(comm.bc.dist, cophenetic(comm.bc.clustc)) # 0.8684251
cor(comm.bc.dist, cophenetic(comm.bc.clusts)) # 0.8741128

# plot cluster diagram
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity")
rect.hclust(comm.bc.clust,10)
text(2.5,.4, "Mixed Sagebrush \nand Cheatgrass", cex =1.2, col = "blue")
text(8,.4, "Intact Sagebrush", cex =1.2, col = "darkgreen")
text(13.5,.4, "Cheatgrass-\nDominated", cex =1.2, col = "gold")
text(18,.4, "Cheatgrass Dieoff", cex =1.2, col = "red")


############ NMDS #########################

# The metaMDS function automatically transforms data and checks solution
# robustness

## 2016
comm.bc.mds <- metaMDS(comm, dist = "bray")
stressplot(comm.bc.mds) # Assess goodness of ordination fit (stress plot)
ordiplot(comm.bc.mds, display = "sites", type = "text") # plot site scores as text
ordipointlabel(comm.bc.mds) # automated plotting of results - tries to eliminate overlapping labels

## 2013
comm13.bc.mds <- metaMDS(comm13, dist = "bray")
stressplot(comm13.bc.mds) # Assess goodness of ordination fit (stress plot)
ordiplot(comm13.bc.mds, display = "sites", type = "text") # plot site scores as text
ordipointlabel(comm13.bc.mds) # automated plotting of results - tries to eliminate overlapping labels




#################### Incorporating metadata ###########################################################


##### 2016

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


### 2013

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

##### Change from 2013 to 2016

pro = procrustes(comm.bc.mds, comm13.bc.mds)
plot(pro, ar.col = metadata1$BC_cluster, main = "NMDS Change from 2013 - 2016")
legend(-1.07, -0.36,
       title = "2016 Bray-Curtis Classification",
       c("Intact Sagebrush", "Invaded Sagebrush", "Cheatgrass-Dominated", "Cheatgrass Dieoff"), 
       col = c("green", "blue", "black", "red"), 
       pch = 19)



#################################

# plot abundance. cex increases the size of bubbles.
ordisurf(comm.bc.mds, comm[, "AIG"], bubble = TRUE, main = "Annual Invasive Grass abundance", cex = 3)
ordisurf(comm.bc.mds, comm[, "Shrub"], bubble = TRUE, main = "Shrub abundance", cex = 3)
ordisurf(comm.bc.mds, metadata1[, "net.mineralization"], bubble = TRUE, main = "Net mineralization", cex = 3)
ordisurf(comm.bc.mds, metadata1[, "SurSo4_SOIL"], bubble = TRUE, main = "SurSo4_SOIL", cex = 3)
ordisurf(comm.bc.mds, metadata1[, "Ca_SOIL"], bubble = TRUE, main = "Ca_SOIL", cex = 3)
ordisurf(comm.bc.mds, metadata1[, "Mg_SOIL"], bubble = TRUE, main = "Mg_SOIL", cex = 3)
ordisurf(comm.bc.mds, metadata1[, "C.N_SOIL"], bubble = TRUE, main = "C.N_SOIL", cex = 3)
ordisurf(comm.bc.mds, metadata1[, "net.mineralization"], bubble = TRUE, main = "Net mineralization", cex = 3)
ordisurf(comm.bc.mds, metadata1[, "NO3.N..ppm."], bubble = TRUE, main = "Nitrate", cex = 3)
ordisurf(comm.bc.mds, metadata1[, "NO3.N..ppm."], bubble = TRUE, main = "Nitrate", cex = 3)



####### PCA

jones.pca = prcomp(metadata2)
plot(jones.pca)
biplot(jones.pca, scale = TRUE)

rda_2016 = rda(cover_by_plot, metadata2[,-c(21:23)])
plot(rda_2016)

##########################
### for leanne pallett ###
##########################

for_leanne = jones_all
for_leanne$plot_num = as.numeric(paste(substr(jones_all$Plot_TP, 2,3)))
for_leanne = for_leanne[c(31:33,46:48),]


Jones_AC_byPlottran = Jones_Cover_Just_Plants
Jones_AC_byPlottran$PlotTran = paste(substr(rownames(Jones_AC_byPlottran),1,4))
Jones_AC_byPlottran = summaryBy(.~PlotTran, data = Jones_AC_byPlottran, keep.names = TRUE, na.rm = TRUE)

for_leanne_plants = Jones_AC_byPlottran[c(31:33,46:48),]

colnames(locations) = c("plot_num", "site_type", "Last Year Burned", "Longitude", "Latitude", "Elevation (m)")
for_leanne = plyr::join(for_leanne, locations, by = "plot_num")



write.csv(for_leanne, "for_leanne_site_data.csv")
write.csv(for_leanne_plants, "for_leanne_plants.csv")

