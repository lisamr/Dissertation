library(randomForest)
library(dismo)
source("R/a_prep_mjcb.R")

#mixed rf ======================================================================
#' Mixed Random Forest
#'
#' The function to fit a random forest with random effects.
#'
#' @param Y The outcome variable.
#' @param X A data frame or matrix contains the predictors.
#' @param random A string in lme4 format indicates the random effect model.
#' @param data The data set as a data frame.
#' @param initialRandomEffects The initial values for random effects.
#' @param ErrorTolerance The tolerance for log-likelihood.
#' @param MaxIterations The maximum iteration times.
#'
#' @return A list contains the random forest ($forest), mixed model ($MixedModel), and random effects ($RandomEffects).
#' See the example below for the usage.

#' @export
#' @import randomForest lme4
#' @examples
#'
#' # load the sleepstudy data set from the lme4 package
#' library(lme4)
#' data(sleepstudy)
#'
#' tmp = MixRF(Y=sleepstudy$Reaction, X=as.data.frame(sleepstudy$Days), random='(Days|Subject)',
#'             data=sleepstudy, initialRandomEffects=0, ErrorTolerance=0.01, MaxIterations=100)
#'
#' # tmp$forest
#' 
#' # tmp$MixedModel
#' 
#' # tmp$RandomEffects


MixRF = function(Y, X, random, data, initialRandomEffects = 0, ErrorTolerance = 0.001, MaxIterations = 1000, 
                 importance = FALSE, ntree = 500, mtry = max(floor(ncol(X)/3), 1), nodesize = 5, maxnodes = NULL) {
  
  Target = Y
  
  # Condition that indicates the loop has not converged or run out of iterations
  ContinueCondition = TRUE
  
  iterations <- 0
  
  # Get initial values
  AdjustedTarget <- Target - initialRandomEffects
  oldLogLik <- -Inf
  
  while(ContinueCondition){
    
    iterations <- iterations+1
    
    # randomForest
    rf = randomForest(X, AdjustedTarget, 
                      importance = importance, ntree = ntree, mtry = mtry, nodesize = nodesize, maxnodes = maxnodes)
    
    # y - X*beta (out-of-bag prediction)
    resi = Target - rf$predicted
    
    ## Estimate New Random Effects and Errors using lmer
    f0 = as.formula(paste0('resi ~ -1 + ',random))
    lmefit <- lmer(f0, data=data)
    
    # check convergence
    newLogLik <- as.numeric(logLik(lmefit))
    
    ContinueCondition <- (abs(newLogLik-oldLogLik)>ErrorTolerance & iterations < MaxIterations)
    oldLogLik <- newLogLik
    
    # Extract random effects to make the new adjusted target
    AllEffects <- predict(lmefit)
    
    #  y-Zb
    AdjustedTarget <- Target - AllEffects
  }
  
  result <- list(forest=rf, MixedModel=lmefit, RandomEffects=ranef(lmefit),
                 IterationsUsed=iterations)
  
  return(result)
}

# load data ====================================================================
all_rf<-dplyr::select(all_p, -ends_with("z"),-tmin)[,-c(36:43)] %>%
  mutate(Site.type = as.factor(Site.type),
         shrub_b = as.factor(shrub_b),
         Year = as.factor(Year),
         Site_number = as.factor(Site_number)) %>%
  as.data.frame

#random forest models ==========================================================
scn <- randomForest(all_rf$SOIL_CN ~ ., data=all_rf[,c(21:35, 46:51)])
varImpPlot(scn)

scn_r <- MixRF(all_rf$SOIL_CN, all_rf[,21:43], random = "(1|Site_number)", data = all_rf)

sn <- randomForest(all_rf$SOIL_TN_pct ~ ., data=all_rf[,c(21:35, 46:51)])
varImpPlot(sn)

sc <- randomForest(all_rf$SOIL_OM_pct ~ ., data=all_rf[,c(21:35, 46:51)])
varImpPlot(sc)

smg <- randomForest(all_rf$SOIL_Mg_kg_ha ~ ., data=all_rf[,c(21:35, 46:51)])
varImpPlot(smg)


lit <- randomForest(all_rf$Litter_CN ~ ., data=all_rf[,c(21:35, 46:51)])
varImpPlot(lit);lit

ln <- randomForest(all_rf$Litter_TN_pct ~ ., data=all_rf[,c(21:35, 46:51)])
varImpPlot(ln);ln

lc <- randomForest(all_rf$Litter_TC_pct ~ ., data=all_rf[,c(21:35, 46:51)])
varImpPlot(lc);lc

#boosted regression trees=======================================================

snb <- gbm.step(data = all_rf,
               gbm.x=c(1,2,21:43), 
               gbm.y=19, family = "gaussian", max.trees = 20000,learning.rate = 0.001);gbm.plot(snb)

scb <- gbm.step(data = all_rf,
                gbm.x=c(1,2,21:43), 
                gbm.y=18, family = "gaussian", max.trees = 20000,learning.rate = 0.001);gbm.plot(scb)

scnb <- gbm.step(data = all_rf,
                gbm.x=21:43, 
                gbm.y=20, family = "gaussian", max.trees = 20000,learning.rate = 0.001);gbm.plot(scnb)
