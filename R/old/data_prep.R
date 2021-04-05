####################################
# Data Prep Script for Jones Study #
####################################

library(doBy)
library(plyr)

#########
# cover #
#########

jones_master_cover = read.csv("Jones_Cover - ArealCover.csv", header = TRUE)

cover = jones_master_cover
cover$plot_tran = paste0(cover$Plot.,cover$TransectPair)
cover_by_plot_tran = summaryBy(. ~ plot_tran, data=na.omit(cover), 
                               FUN = mean, keep.names = TRUE)
cover_by_plot = summaryBy(. ~ Plot., data = na.omit(cover), 
                          FUN=mean, keep.names = TRUE)

rownames(cover_by_plot_tran) = cover_by_plot_tran$plot_tran
rownames(cover_by_plot) = cover_by_plot$plot_tran

cover_by_plot = cover_by_plot[,-c(1:11,15,26,33,45,61)]
cover_by_plot_tran = cover_by_plot_tran[,-c(1:11,15,26,33,45,61)]


cover_by_plot_tran$plot_tran = NULL
cover_by_plot$plot_tran = NULL


############
# metadata #
############

metadata = read.csv("jones_all_oct5.csv")
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

###########
# spatial #
###########

locations = read.csv("locations.csv")
