######### for loop for tukey HSD tests ###############
# basically, the first three chunks apply the same
# procedure to different dataframes,then the last two 
# process and plot the results.
#
# hopefully I will just make this into a function at 
# some point

library(ggplot2)
library(agricolae)
library(plyr)
library(dplyr)
library(vegan)


#############
# Functions #
#############

mass_krus = function(x, y){
# This function inputs a data frame, and outputs another data frame
# of Kruskall-Wallis test results on that data. x is the many response
# variables, y is one column - the treatment. It runs the K-W test 
# column by column.

  krus = list()
  for(i in 1:(length(x))){
    means = as.data.frame(kruskal(x[,i], y)$means)
    groups = as.data.frame(kruskal(x[,i], y)$groups)
    means$trt = rownames(means)
    groups$trt = rownames(groups)
    krus[[i]] = plyr::join(means, groups, by = "trt")
    #colnames(krus[[i]]) = c("variable", "something","std", "r", "Min", "Max", "trt", "means", "Groups")
    names(krus[[i]])[1] = "means"
    krus[[i]]$variable = as.factor(colnames(x[i]))
  }
  tuks_df = as.data.frame(do.call("rbind", krus))
  tuks_df$means = round(tuks_df$means, 2)
  tuks_df$trt = as.factor(tuks_df$trt)
  tuks_df$trt = factor(tuks_df$trt, levels(tuks_df$trt)[c(3,4,1,2)])
  tuks_df$mean_std = paste0(tuks_df$means, " (", round(tuks_df$std,2), ")")
  #colnames(tuks_df) = c("Variable", "something","std", "r", "min", "max", "Treatment", "means", "Group", "mean_std")
  return(tuks_df)
}

class_ = function(x){
  ifelse(x == "a", 0, 1 )
}

plotit = function(kdf, title){
  only_sigs = kdf %>%
    mutate(M_id = class_(groups)) %>%
    group_by(variable) %>%
    summarise(min_ID = sum(M_id)) %>%
    filter(min_ID >= 1 ) %>%
    left_join(.,kdf, by="variable")
  
  ##############
  # ggplotting #
  ##############
  
  ggplot(na.omit(only_sigs), aes(x=trt, y = variable)) +
    geom_tile(aes(fill = groups), color = "grey10") +
    theme_bw() + 
    scale_fill_grey(start = 1, end = 0.5) +
    geom_text(aes(label=mean_std)) +
    ggtitle(paste(title))
}

###################
# using 2013 data #
###################

metadata13 = Jones2013[, -1] 
metadataT = metadata1[, -c(1,2,8,33:35,39,45,46)] # by 2013 type
metadataT$TN_POSE[is.nan(metadataT$TN_POSE)] = 0
metadataT$TC_POSE[is.nan(metadataT$TC_POSE)] = 0
metadataT$C.N_POSE[is.nan(metadataT$C.N_POSE)] = 0
metadataBC = metadata1[, -c(1,2,8,33:35,39,44,46)]  # by 2016 BC_cluster
metadataBC$TN_POSE[is.nan(metadataBC$TN_POSE)] = 0
metadataBC$TC_POSE[is.nan(metadataBC$TC_POSE)] = 0
metadataBC$C.N_POSE[is.nan(metadataBC$C.N_POSE)] = 0

k13 = mass_krus(metadata13[,!names(metadata13) %in% "site_type"], metadata13$site_type)
k16T = mass_krus(metadataT[,!names(metadataT) %in% "Type"], metadataT$Type)
k16BC = mass_krus(metadataBC[,!names(metadataBC) %in% "BC_cluster"], metadataBC$BC_cluster)

plotit(k13, "2013")
plotit(k16BC, "2016 Bray-Curtis")
plotit(k16T, "2016 Same types as 2013")