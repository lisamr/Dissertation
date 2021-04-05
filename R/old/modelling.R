####### modeling
library(randomForest)
library(car)
library(mosaic)
library(ggplot2)

### normalizing fields
### testing for normality
norms = list()
for(i in 1:length(metadata2)){
  norms[[i]] = c(colnames(metadata2[i]),shapiro.test(metadata2[,i])[[2]])
}  # litter and bare normal
norms = as.data.frame(do.call("rbind", norms))
norms$V2 = as.numeric(as.character(norms$V2))
not_norms = norms[norms$V2 < 0.1,]
dim(not_norms)

#####################################
### NO3 ppm vs plants and microsite #
#####################################

NO3mod0 = lm(NO3.N..ppm. ~ 1, metadata2[,c(2:11,32)])
NO3mod1 = lm(NO3.N..ppm. ~ ., metadata2[,c(2:11,32)])
NO3mod01 = step(NO3mod0, scope = formula(NO3mod1))
NO3mod10 <- step(NO3mod1, scope = list(lower = formula(NO3mod0), upper = formula(NO3mod1)), trace = 0)

vif(NO3mod01)
vif(NO3mod10)

NO3RF = randomForest(NO3.N..ppm. ~ ., data = metadata2[,c(2:11,32)], ntree = 10000, importance = TRUE)
varImpPlot(NO3RF)

#################################################
#### net mineralization vs plants and microsite #
#################################################

NM_mod0 = lm(exp(metadata2$net.mineralization) ~ 1, metadata2[,c(2:11)])
NM_mod1 = lm(exp(metadata2$net.mineralization) ~ ., metadata2[,c(2:11)])
NM_mod01 = step(NM_mod0, scope = formula(NM_mod1), test = "F")
NM_mod10 <- step(NM_mod1, scope = list(lower = formula(NM_mod0), upper = formula(NM_mod1)), trace = 0)

vif(NM_mod01)
vif(NM_mod10)

NM_RF = randomForest(metadata2$net.mineralization ~ ., data = metadata2[,c(2:11)], ntree = 10000, importance = TRUE)
varImpPlot(NM_RF)

###############################################
##### plant nutrient ~ soil nutrients #########
###############################################

#### Cheatgrass

CGN_mod0 = lm(log(metadata2$TN_BRTE) ~ 1, metadata2[,c(24:34,37)])
CGN_mod1 = lm(log(metadata2$TN_BRTE) ~ ., metadata2[,c(24:34,37)])
CGN_mod01 = step(CGN_mod0, scope = formula(CGN_mod1))
CGN_mod10 <- step(CGN_mod1, scope = list(lower = formula(CGN_mod0), upper = formula(CGN_mod1)), trace = 0)

vif(CGN_mod01)
vif(CGN_mod10)

CGN_RF = randomForest(metadata2$TN_BRTE ~ ., data = metadata2[,c(24:34,37)], ntree = 10000, importance = TRUE)
varImpPlot(CGN_RF)


#### Poa Secunda

PSn_mod0 = lm((metadata2$TN_POSE) ~ 1, metadata2[,c(24:34,37)])
PSn_mod1 = lm((metadata2$TN_POSE) ~ ., metadata2[,c(24:34,37)])
PSn_mod01 = step(PSn_mod0, scope = formula(PSn_mod1))
PSn_mod10 <- step(PSn_mod1, scope = list(lower = formula(PSn_mod0), upper = formula(PSn_mod1)), trace = 0)

vif(PSn_mod01)
vif(PSn_mod10)

PSn_RF = randomForest(metadata2$TN_POSE ~ ., data = metadata2[,c(24:34,37)], ntree = 10000, importance = TRUE)
varImpPlot(PSn_RF)

#### Other

on_mod0 = lm((metadata2$TN_OTHER) ~ 1, metadata2[,c(24:34,37)])
on_mod1 = lm((metadata2$TN_OTHER) ~ ., metadata2[,c(24:34,37)])
on_mod01 = step(on_mod0, scope = formula(on_mod1))
on_mod10 <- step(on_mod1, scope = list(lower = formula(on_mod0), upper = formula(on_mod1)), trace = 0)

vif(on_mod01)
vif(on_mod10)

on_RF = randomForest(metadata2$TN_OTHER ~ ., data = metadata2[,c(24:34,37)], ntree = 10000, importance = TRUE)
varImpPlot(on_RF)

#### Litter

LIT_mod0 = lm(log(metadata2$TN_LIT) ~ 1, metadata2[,c(24:34,37)])
LIT_mod1 = lm(log(metadata2$TN_LIT) ~ ., metadata2[,c(24:34,37)])
LIT_mod01 = step(LIT_mod0, scope = formula(CGN_mod1))
LIT_mod10 <- step(LIT_mod1, scope = list(lower = formula(LIT_mod0), upper = formula(LIT_mod1)), trace = 0)

vif(LIT_mod01)
vif(LIT_mod10)

LIT_RF = randomForest(metadata2$TN_LIT ~ ., data = metadata2[,c(24:34,37)], ntree = 10000, importance = TRUE)
varImpPlot(LIT_RF)


########################################
# plant nutrient ~ plant and microsite #
########################################

####### BRTE
CG_mod0 = lm(exp(metadata2$TN_BRTE) ~ 1, metadata2[,c(2:11)])
CG_mod1 = lm(exp(metadata2$TN_BRTE) ~ ., metadata2[,c(2:11)])
CG_mod01 = step(CG_mod0, scope = formula(CG_mod1), test = "F")
CG_mod10 <- step(CG_mod1, scope = list(lower = formula(CG_mod0), upper = formula(CG_mod1)), trace = 0)

vif(CG_mod01)
vif(CG_mod10)

CG_RF = randomForest(metadata2$TN_BRTE ~ ., data = metadata2[,c(2:11)], ntree = 10000, importance = TRUE)
varImpPlot(CG_RF)

############ POSE ~ microsite


PS_mod0 = lm(exp(metadata2$TN_POSE) ~ 1, metadata2[,c(2:11)])
PS_mod1 = lm(exp(metadata2$TN_POSE) ~ ., metadata2[,c(2:11)])
PS_mod01 = step(PS_mod0, scope = formula(PS_mod1), test = "F")
PS_mod10 <- step(PS_mod1, scope = list(lower = formula(PS_mod0), upper = formula(PS_mod1)), trace = 0)

vif(PS_mod01)
vif(PS_mod10)

PS_RF = randomForest(metadata2$TN_POSE ~ ., data = metadata2[,c(2:11)], ntree = 10000, importance = TRUE)
varImpPlot(PS_RF)


######### other ~ microsite

o_mod0 = lm(exp(metadata2$TN_OTHER) ~ 1, metadata2[,c(2:11)])
o_mod1 = lm(exp(metadata2$TN_OTHER) ~ ., metadata2[,c(2:11)])
o_mod01 = step(o_mod0, scope = formula(o_mod1), test = "F")
o_mod10 <- step(o_mod1, scope = list(lower = formula(o_mod0), upper = formula(o_mod1)), trace = 0)

vif(o_mod01)
vif(o_mod10)

o_RF = randomForest(metadata2$TN_OTHER ~ ., data = metadata2[,c(2:11)], ntree = 10000, importance = TRUE)
varImpPlot(o_RF)

# ### table that shit yo
# 
# library(stargazer)
# 
# stargazer(NO3mod01, NM_mod01, title = "Soil Nitrogen")

################################
### ggplots of model results ###
################################

### microsite predictors

microsite_mods =list(o_mod01, PS_mod01, CG_mod10, NM_mod10,NO3mod01)
df_list = list()
for(i in 1:length(microsite_mods)){
  s = summary(microsite_mods[[i]])
  x = s$coefficients
  d = as.data.frame(x)
  d$predictor = rownames(d)
  d$response = as.character(s$call$formula[2])
  df_list[[i]] = d[-1,] # removing the intercept
}

full_df = as.data.frame(do.call("rbind", df_list))
full_df$is_sig = ifelse(full_df$`Pr(>|t|)` < 0.05, 1,NA)
full_df$effect_size = full_df$is_sig * abs(full_df$Estimate)
full_df$sign = as.factor(ifelse(full_df$Estimate > 0, "positive","negative"))
full_df$response = ifelse(full_df$response == "exp(metadata2$TN_OTHER)", "%N_OTHER",full_df$response)
full_df$response = ifelse(full_df$response == "exp(metadata2$TN_POSE)", "%N_POSE",full_df$response)
full_df$response = ifelse(full_df$response == "exp(metadata2$TN_BRTE)", "%N_BRTE",full_df$response)
full_df$response = ifelse(full_df$response == "exp(metadata2$net.mineralization)", "Net_Mineralization",full_df$response)
full_df$response = ifelse(full_df$response == "NO3.N..ppm.", "NO3_SOIL", full_df$response)
full_df$response = as.factor(full_df$response)


ggplot(na.omit(full_df), aes(x=response, y = predictor)) +
  geom_count(aes(size = effect_size, color = sign), shape = 1) + 
  theme_bw() + 
  scale_color_manual(values = c("black", "grey60")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Significant (p < 0.05) Microsite Predictors of Plant Nutrients")
# +
#  scale_size_area()

## plant vs soil predictors

plant_soil_mods = list(CGN_mod01, PSn_mod01, on_mod01, LIT_mod10)
df_list = list()
for(i in 1:length(plant_soil_mods)){
  s = summary(plant_soil_mods[[i]])
  x = s$coefficients
  d = as.data.frame(x)
  d$predictor = rownames(d)
  d$response = as.character(s$call$formula[2])
  df_list[[i]] = d[-1,]
}

full_df = as.data.frame(do.call("rbind", df_list))
full_df$is_sig = ifelse(full_df$`Pr(>|t|)` < 0.05, 1,NA)
full_df$effect_size = full_df$is_sig * abs(full_df$Estimate)
full_df$sign = as.factor(ifelse(full_df$Estimate > 0, "positive","negative"))
full_df$response = ifelse(full_df$response == "(metadata2$TN_OTHER)", "%N_OTHER",full_df$response)
full_df$response = ifelse(full_df$response == "(metadata2$TN_POSE)", "%N_POSE",full_df$response)
full_df$response = ifelse(full_df$response == "log(metadata2$TN_BRTE)", "%N_BRTE",full_df$response)
full_df$response = ifelse(full_df$response == "log(metadata2$TN_LIT)", "%N_LIT",full_df$response)
full_df$response = as.factor(full_df$response)

ggplot(na.omit(full_df), aes(x=response, y = predictor)) +
  geom_count(aes(size = effect_size, color = sign), shape = 1) + 
  theme_bw() + 
  scale_color_manual(values = c("black", "grey60")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Significant (p < 0.05) Soil Predictors of Plant Nutrients")


