######## plotting differences
boxplot(net.mineralization ~ BC_cluster, metadata1)
boxplot(net.mineralization ~ Type, metadata1)

metadata1$ShrubDV = as.factor(ifelse(metadata1$Shrub > 3, "Shrubs", "No_Shrubs"))

boxplot(net.mineralization ~ ShrubDV, metadata1)

########## HSD ############
library(agricolae)
library(dplyr)

S_CN_tuk = HSD.test(aov(C.N_SOIL ~ BC_cluster, metadata1), "BC_cluster")
S_CN_tuk$groups
boxplot(C.N_SOIL ~ BC_cluster, metadata1)
bar.group(S_CN_tuk$groups, density=4, ylim = c(0, 20), border="blue", ylab = "Soil C:N", xlab = "Invasion Stage")

netMin_tuk = HSD.test(aov(net.mineralization ~ BC_cluster, metadata1), "BC_cluster")
netMin_tuk$groups
boxplot(net.mineralization ~ BC_cluster, metadata1)
bar.group(netMin_tuk$groups,density=4,ylim = c(0,0.35), border="blue", ylab = "Net Mineralization", xlab = "Invasion Stage")

NO3_tuk = HSD.test(aov(NO3.N..ppm. ~ BC_cluster, metadata1), "BC_cluster")
NO3_tuk$groups
boxplot(NO3.N..ppm. ~ BC_cluster, metadata1)
bar.group(NO3_tuk$groups,density=4, ylim = c(0, 0.3), border="blue", ylab = "Nitrate", xlab =  "Invasion Stage")

NH4_tuk = HSD.test(aov(NH4.N..ppm. ~ BC_cluster, metadata1), "BC_cluster")
NH4_tuk$groups
boxplot(NH4.N..ppm. ~ BC_cluster, metadata1)
bar.group(NH4_tuk$groups,density=4, ylim = c(0,0.15), border="blue", ylab = "Ammonium", xlab =  "Invasion Stage")

SO4_tuk = HSD.test(aov(SurSo4_SOIL ~ BC_cluster, metadata1), "BC_cluster")
SO4_tuk$groups
boxplot(SurSo4_SOIL ~ BC_cluster, metadata1)
bar.group(SO4_tuk$groups, density=4, ylim = c(0,7), border="blue", ylab = "Sur_SO4", xlab =  "Invasion Stage")

Ca_tuk = HSD.test(aov(Ca_SOIL ~ BC_cluster, metadata1), "BC_cluster")
Ca_tuk$groups
boxplot(Ca_SOIL ~ BC_cluster, metadata1)
bar.group(Ca_tuk$groups, density=4, ylim = c(0, 5000), border="blue", ylab = "Soil Calcium", xlab =  "Invasion Stage")

Mg_tuk = HSD.test(aov(Mg_SOIL ~ BC_cluster, metadata1), "BC_cluster")
Mg_tuk$groups
boxplot(Mg_SOIL ~ BC_cluster, metadata1)
bar.group(Mg_tuk$groups, density=4, ylim = c(0, 900), border="blue", ylab = "Soil Mg", xlab =  "Invasion Stage")

#########################################################################
tuks = list()
for(i in 1:(length(metadata3)-1)){
  means = as.data.frame(HSD.test(aov(metadata3[,i] ~ Type, metadata3), "Type")$means)
  groups = as.data.frame(HSD.test(aov(metadata3[,i] ~ Type, metadata3), "Type")$groups)
  means$trt = rownames(means)
  tuks[[i]] = plyr::join(means, groups, by = "trt")
  colnames(tuks[[i]]) = c("variable", "std", "r", "Min", "Max", "trt", "means", "M")
  tuks[[i]]$variable = as.factor(colnames(metadata3[i]))
}

tuks = list()
for(i in 1:(length(metadata13)-1)){
  means = as.data.frame(HSD.test(aov(metadata13[,i] ~ Type, metadata13), "Type")$means)
  groups = as.data.frame(HSD.test(aov(metadata13[,i] ~ Type, metadata13), "Type")$groups)
  means$trt = rownames(means)
  tuks[[i]] = plyr::join(means, groups, by = "trt")
  colnames(tuks[[i]]) = c("variable", "std", "r", "Min", "Max", "trt", "means", "M")
  tuks[[i]]$variable = as.factor(colnames(metadata13[i]))
}

krus = list()
for(i in 1:(length(metadata13)-1)){
  means = as.data.frame(kruskal(metadata13[,i], metadata13[,length(metadata13)])$means)
  groups = as.data.frame(kruskal(metadata13[,i], metadata13[,length(metadata13)])$groups)
  means$trt = rownames(means)
  krus[[i]] = plyr::join(means, groups, by = "trt")
  colnames(krus[[i]]) = c("variable", "something","std", "r", "Min", "Max", "trt", "means", "Groups")
  krus[[i]]$variable = as.factor(colnames(metadata13[i]))
}

tuks = list()
for(i in 1:(length(metadata3)-1)){
  means = as.data.frame(HSD.test(aov(metadata3[,i] ~ BC_cluster, metadata3), "BC_cluster")$means)
  groups = as.data.frame(HSD.test(aov(metadata3[,i] ~ BC_cluster, metadata3), "BC_cluster")$groups)
  means$trt = rownames(means)
  tuks[[i]] = plyr::join(means, groups, by = "trt")
  colnames(tuks[[i]]) = c("variable", "std", "r", "Min", "Max", "trt", "means", "M")
  tuks[[i]]$variable = as.factor(colnames(metadata3[i]))
}

#######################################
# making the data frames for plotting #
#######################################

tuks_df = as.data.frame(do.call("rbind", tuks))
tuks_df$means = round(tuks_df$means, 2)
tuks_df$trt = as.factor(tuks_df$trt)
tuks_df$trt = factor(tuks_df$trt, levels(tuks_df$trt)[c(3,4,1,2)])
tuks_df$mean_std = paste0(tuks_df$means, " (", round(tuks_df$std,2), ")")
colnames(tuks_df) = c("Variable", "std", "r", "min", "max", "Treatment", "means", "Group", "mean_std")

tuks_df = as.data.frame(do.call("rbind", krus))
tuks_df$means = round(tuks_df$means, 2)
tuks_df$trt = as.factor(tuks_df$trt)
tuks_df$trt = factor(tuks_df$trt, levels(tuks_df$trt)[c(3,4,1,2)])
tuks_df$mean_std = paste0(tuks_df$means, " (", round(tuks_df$std,2), ")")
colnames(tuks_df) = c("Variable", "something","std", "r", "min", "max", "Treatment", "means", "Group", "mean_std")

