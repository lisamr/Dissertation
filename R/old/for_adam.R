## Random Forests using Party
library(party)
library(caret)



controls = cforest_unbiased(ntree = 5000)

## Creating decision tree and forest models, replace "Class" with your dependent
## variable. You get a classification
## tree if this variable is categorical or a regression tree if it is 
## continuous. Both are fine, but classification
## trees are prerhaps a little easier to interpret

data <- all %>%
  dplyr::select(-ANF, -PNF)

data$shrub_b <- ifelse(data$Site.type == "I" | data$Site.type == "M", "Shrub", "Grass")


vars <- names(data)
climvars <- paste(names(data)[31:36], collapse = " + ")
preds <- paste(names(data)[c(22:37)], collapse = " + ")

ss = c(4:21)
tt = seq_along(c(4:21))

registerDoParallel(3)

resultadost <- list()
for(i in tt){
  resultadost[[i]] = list()
  f <- formula(paste(vars[ss[i]]," ~ ", preds))
  d <- data[!is.na(data[,ss[i]]),]
  
  #fit <- cforest(f, data=d, controls = controls) ## Fitting the RF model
  tree <- ctree(f, data=d)
  plot(tree, main = paste(vars[ss[i]]))
  # resultadost[[i]]$fitStats<-caret:::cforestStats(fit) ## Extracting accuracy statistics for forest model
  # 
  # imp<-varimp(fit, conditional=T) ## Determining variable importance, as measured by mean decrease in accuracy of model with elimination of variable
  # resultadost[[i]]$imp<-sort(imp)
}



ss <- seq_along(4:21)
tt <- 4:21
rr=list()
for(i in ss){rr[[tt[i]]]=dotplot(resultados[[i]]$imp, main = paste(names(data)[i]))}

scores=list()
for(i in ss){
  scores[[i]] <- as.data.frame(resultados[[tt[i]]]$imp)
  scores[[i]]$var <- rownames(scores[[i]])
  scores[[i]]$rank <- 1:16
  scores[[i]]$weighted_rank <- scores[[i]]$rank * resultados[[tt[i]]]$fitStats[[2]]
  scores[[i]]$resp <- names(data)[tt[i]]
}
scores <-do.call("rbind",scores)

avg_scores <- summaryBy(weighted_rank ~ var, data= scores, keep.names = TRUE)
max_w_scores <- summaryBy(weighted_rank ~ var, data= scores, keep.names = TRUE, FUN = max)
max_scores <- summaryBy(rank ~ var, data= scores, keep.names = TRUE, FUN = max)


sum_scores <- sum_scores[order(sum_scores$weighted_rank),]
