## libraries -------------------------------------------------------------------

library(nlme) # lmes
library(doBy) # for summaryBy - data mongering
library(dplyr) # piping operators etc, the select function
library(plyr) # join
library(lmerTest) # for the step function
library(sjPlot) #to visualize the lmes
library(ggplot2) # duh
library(tidyverse) # generally useful
library(car) #generally useful
library(mosaic) # generally useful
library(rlist) # for removing null list items
library(reshape2) # for something in the correlation matrix code
library(effects) # displaying effects of models
library(tidyr) # gather
library(lattice) # panel plots
library(cowplot) #for multipanel plots
library(MuMIn) #r-squared for lmes
library(grid)
library(gridExtra)
library(ggpubr) # for theme_pubr()

## data prep -------------------------------------------------------------------

# cleaned and matched data frames
glmm13 <- read.csv("data/Jones_2013_transect_dec17.csv")
glmm16 <- read.csv("data/jones_all_nov_2017.csv")
glmm13 <- glmm13[-c(10:15,46:54),] # getting rid of unrepeated sites

# adding precip with previously generated precip data from prism
# and year as a factor, then joining the two data frames
ppt = read.csv("data/ppt_sums.csv")
names(ppt) = c("x", "ppt_11_13", "ppt_14_16", "Site_number") 
glmm13$Year = 2013
glmm13 = plyr::join(glmm13, ppt[,c(2,4)]) # magic numbers
names(glmm13)[33] <- "ppt"
glmm16$Year = 2016
glmm16 <- plyr::join(glmm16, ppt[,c(3,4)])
names(glmm16)[33] <- "ppt"

# making Shrubs use the same data from 2013 (that was line
# intercept, I just included artr in my quadrats)

glmm16$Shrubs <- glmm13$Shrubs

# center and scale soil SO4 by year
glmm16$SOIL_SurSo4_kg_ha <- as.numeric(scale(glmm16$SOIL_SurSo4_kg_ha))
glmm13$SOIL_SurSo4_kg_ha <- as.numeric(scale(glmm13$SOIL_SurSo4_kg_ha))

# relative cover of Crypto by year
glmm16$Crypto <- glmm16$Crypto / max(glmm16$Crypto)
glmm13$Crypto <- glmm13$Crypto / max(glmm13$Crypto)



all = rbind(glmm13, glmm16)
all$Year = as.factor(all$Year)
all$Site_number = as.factor(all$Site_number)


## basic data exploration ------------------------------------------------------
# d <- melt(all[,-c(1:3,32)])
# d = ggplot(d,aes(x = value)) + 
#   facet_wrap(~variable, scales = "free") + 
#   geom_density()
# d
# ggsave("Images/density_plots.png", limitsize = FALSE)
# 
# # cleveland dot plots - color is site type, shape is plot #
# for(i in 1:length(all)){
#   dotchart(as.numeric(all[,i]), color = all$Site.type,
#            pch = as.numeric(all$Site_number),
#            groups = all$Year,
#            main = names(all)[i])
# }
# 
# # sulfur - is it trustworthy?
# # hypothesis - the soil lab people gave me results in ppm, 
# # and accidentally labelled them lbs/acre
# plot(all$SOIL_SurSo4_kg_ha ~ all$ppt, col = all$Year, pch = as.numeric(all$Site_number))

## functions -------------------------------------------------------------------

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

plotit = function(kdf, title){
  # this function plots the results of the kw tests
  
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

# these are from the internet... for the correlation matrices
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
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# these are to make the correlation matrices, they output
# a ggplot and also save as a .png and depend on the preceeding functions
tri_cor <-function(resp){
  
  cormat <- round(cor(resp, use="pairwise.complete.obs", method = "kendall"),2)
  
  # Get upper triangle of the correlation matrix
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  # take out low correlation values in the interest of visualization
  melted_cormat$vis = ifelse(abs(melted_cormat$value) >0.3, melted_cormat$value, NA ) 
  
  # Create a ggheatmap
  g <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Kendall\nCorrelation") +
    theme_minimal() + # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) +
    coord_fixed() + 
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
  ggsave("Images/cormat_soil_plant.png", scale=1.45,limitsize = FALSE)
  return(g)
}
square_cor <- function(pred, resp){
  cormat <- round(cor(pred, resp, use="pairwise.complete.obs",method="kendall"),2)
  melted_cormat <- melt(cormat)
  # take out low correlation values in the interest of visualization
  melted_cormat$vis = ifelse(abs(melted_cormat$value) >0.3, melted_cormat$value, NA ) 
  
  # Create a ggheatmap
  g <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()+ 
    geom_text(aes(Var2, Var1, label = value), color = "grey60", size = 4) +
    geom_text(aes(Var2, Var1, label = vis), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank())
  ggsave("Images/cor_veg_soil_plant.png", scale=1.2)
  return(g)
}

## real modelling --------------------------------------------------------------
# check correlation of explanatory variables
tri_cor(pred) #litter and bare correlated, nothing else problematic, removing bare

# general parameters
path <- "Images/model_viz_jan18/" # my path for saving plot images
ctrl <- lmeControl(opt = "optim", maxIter = 1000, msMaxIter = 1000)

coefs1 = c("Rock:ppt","Crypto:ppt", "PNG:ppt", 
           "PNF:ppt", "ANF:ppt", "AIG:ppt", "AIF:ppt", 
           "Litter:ppt", "Shrubs:ppt")

coefs2 <- c("Rock","Crypto","Shrubs","PNF","PNG",
            "ANF","AIG","AIF","Litter","ppt")
# functions for model building--------------------------------------------------
best_random <- function(df){
  # this tests lm vs lme with random slope vs lme with random slope and intercept
  # f is a formula using the formula() function NEEDS TO BE DEFINED IN THE GLOBAL ENVIRONMENT
  # df is the data frame
  ctrl <- lmeControl(opt = "optim", maxIter = 1000, msMaxIter = 1000)
  gls <- gls(model = f, data = df) # straight up lm - REML is default
  ri <- lme(fixed = f, 
            random = ~1|Site_number, 
            control = ctrl,
            method = "REML", data = df) # lme with random intercept
  ris <- lme(fixed = f, 
             random = ~1 + Year|Site_number, 
             method = "REML",
             control = ctrl,
             data = df) # random slope and intercept
  anova(gls,ri,ris)
}

plot_res_col <- function(mod, df, pred){
  # this function plots the residuals vs fitted, colored by all the variables
  Residuals <- resid(mod, type = "normalized") 
  Fitted <- fitted(mod)
  resp <- dplyr::select(df, Rock, Crypto, Shrubs, PNF, PNG, ANF, 
                        AIG, AIF, Litter, ppt)
  coefs2 <- c("Rock","Crypto","Shrubs","PNF","PNG",
              "ANF","AIG","AIF","Litter","ppt")
  dir.create(paste0("Images/model_viz_jan18/",pred))
  
  for(i in 1:length(coefs2)){
    ggplot(data=df, aes(x=Fitted,y=Residuals,colour=resp[i])) + 
      geom_point() +
      scale_color_continuous()+
      ggtitle(coefs2[i])
    
    ggsave(paste0("Images/model_viz_jan18/", pred,"/", coefs2[i], ".png"), limitsize = TRUE)
    
  }}

plot_res_size <- function(mod, df){
  Residuals <- resid(mod, type = "normalized") 
  Fitted <- fitted(mod)
  resp <- dplyr::select(df, Rock, Crypto, Shrubs, PNF, PNG, ANF, 
                        AIG, AIF, Litter, ppt)
  coefs2 <- c("Rock","Crypto","Shrubs","PNF","PNG",
              "ANF","AIG","AIF","Litter","ppt")
  
  op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
  MyYlab <- "Residuals"
  for(i in 1:length(resp)){
    plot(x = Fitted, y = Residuals, 
         xlab = "Fitted values", 
         ylab = MyYlab, 
         cex = scale(resp[,i]),
         main = coefs2[i])
  }
  par(op)
}

plot_res_vs_all <- function(df, mod){
  # plots model residuals vs all predictors
  E2 <- resid(mod, type = "normalized") 
  F2 <- fitted(mod)
  op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
  MyYlab <- "Residuals"
  plot(x = F2, y = E2, xlab = "Fitted values", ylab = MyYlab)
  plot(y = E2, x= df$AIG, main = "AIG", ylab = MyYlab)
  plot(y = E2, x= df$Litter, main = "Litter", ylab = MyYlab)
  plot(y = E2, x= df$AIF, main = "AIF", ylab = MyYlab)
  plot(y = E2, x= df$PNF, main = "PNF", ylab = MyYlab)
  plot(y = E2, x= df$Crypto, main = "Crypto", ylab = MyYlab)
  plot(y = E2, x= df$PNG, main = "PNG", ylab = MyYlab)
  plot(y = E2, x= df$ANF, main = "ANF", ylab = MyYlab)
  plot(y = E2, x= df$Shrubs, main = "Shrubs", ylab = MyYlab)
  plot(y = E2, x= df$Rock, main = "Rock", ylab = MyYlab)
  plot(y = E2, x= df$ppt, main = "precipitation", ylab = MyYlab)
  par(op)
  dev.off()
}

droptest <- function(mod, coefs){
  # this function runs an anova with every variable dropped
  for(i in 1:length(coefs)){
    x <- update(mod, as.formula(paste0(".~. -",coefs[i])))
    print(coefs[i])            
    print(anova(mod,x))
  }}

viz_mod <- function(mod, title, path, file){
  # plots the model coefficients and saves to png
  return(plot_model(mod, 
                    type = "est", 
                    title = title,
                    colors = "bw",
                    sort.est = TRUE,
                    show.values = TRUE,
                    show.p = TRUE))
  
  ggsave(paste0(path,file,".png"), limitsize = FALSE)
}

viz_int <- function(mod){
  return(
    sjp.int(mod, 
            swap.pred = T, 
            #int.term = "Shrubs:ppt", 
            mdrt.values = "quart",
            legend.title = "Precipitation",
            title = "",
            axis.title.y = "",
            show.ci = T,
            facet.grid = F,
            geom.colors = c("black","grey20", "grey40", "grey60", "grey80"))
  )
}
all$ppt <- as.numeric(scale(all$ppt, center = FALSE)) # so lmer doesn't get mad
all$ppt <- round(all$ppt, 2) # easier visualization - doesn't affect models

v_struct <- function(v = formula(~ppt),df,r){
  # tests different weight structures
  # f is formula for fixed effects - DEFINE IN GLOBAL ENV
  # v is formula for variable to test e.g. formula(~ppt)
  # df is data
  # r is formula for random structure i.e. formula(~1 + Year|Site_number)
  vcp <- varConstPower(form = v)
  ve <- varExp(form = v)
  vp <- varPower(form = v)
  vf <- varFixed(v)
  n <- lme(f, random = r, 
           method = "REML", 
           control = ctrl,
           data = df) 
  lvcp <- lme(f, random = r, 
              method = "REML", 
              control = ctrl,
              weights = vcp,
              data = df)
  lve <- lme(f, random = r, 
             method = "REML", 
             control = ctrl,
             weights = ve,
             data = df) 
  lvp <- lme(f, random = r, 
             method = "REML", 
             control = ctrl,
             weights = vp,
             data = df) 
  lvf <- lme(f, random = r, 
             method = "REML", 
             control = ctrl,
             weights = vf,
             data = df)
  print(anova(n,lvcp,lve,lvp,lvf))
}

set_theme(theme_pubr())
model_list = list()

# Bromus TN --------------------------------------------------------------------
btn.df <- all[!is.na(all$Bromus_TN_pct),] # getting rid of NA rows
# densityplot(btn.df$Bromus_TN_pct)
# 
# f <- formula(Bromus_TN_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                               Litter + Shrubs)* ppt)
# btn.lm <- lm(f, data = btn.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(btn.lm)
# par(op)

# determining best random structure - beyond optimal model
# best_random(df=btn.df) # random s & i is the best
# btn.lme <- lme(f, random = ~1 + Year|Site_number,
#                 control = ctrl,
#                 method = "ML",
#                 data = btn.df)
#plot_res_vs_all(df=btn.df, mod = btn.lme)


# finding the optimal fitted structure
# btn.lme1 <- lme(Bromus_TN_pct ~ PNG +  AIF + Shrubs * ppt, 
#                 random = ~1 + Year|Site_number,
#                 control = ctrl,
#                 method = "ML", 
#                 data = btn.df)
# droptest(mod = btn.lme1, coefs = coefs2)
# droptest(mod = btn.lme1, coefs = coefs1)
# summary(btn.lme1)
# drop1(btn.lme1)


# refit and validate
btn.lmef <- lme(Bromus_TN_pct ~ PNG +  AIF + Shrubs * ppt, 
                random = ~1 + Year|Site_number, 
                control = ctrl,method = "REML", 
                data = btn.df)
btn.lmer <- lmer(Bromus_TN_pct ~ PNG +  AIF + Shrubs * ppt+ 
                (1 + Year|Site_number), 
                data = btn.df)
# plot_res_vs_all(btn.df, btn.lmef)
# summary(btn.lmef)
model_list$bromus_tn <- btn.lmef

# Bromus TC ---------------------------------------------------------------------------
btc.df <- all[!is.na(all$Bromus_TC_pct),] # getting rid of NA rows
# densityplot((btc.df$Bromus_TC_pct)^10)
# 
# f <- formula(Bromus_TC_pct^10 ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                  Litter + Shrubs)* ppt)
# btc.lm <- lm(f, data = btc.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(btc.lm)
# par(op)

# determining best random structure - beyond optimal model

# best_random(df = btc.df) #random I
# btc.lme <- lme(f, random = ~1|Site_number, 
#                method = "ML", data = btc.df)
# plot residuals vs predictors
# plot_res_vs_all(df=btc.df, mod = btc.lme) #looks good

# optimal fixed structure
# btc.lme1 <- lme(Bromus_TC_pct^10 ~ Crypto + (AIG)* ppt,
#                 random = ~1|Site_number,
#                 method = "ML",
#                 control = ctrl,
#                 data = btc.df)
# droptest(btc.lme1, coefs1)
# droptest(btc.lme1, coefs2)
# summary(btc.lme1)
# drop1(btc.lme1)
# vif(btc.lme1)

#validation
btc.lmef <- lme(Bromus_TC_pct^10 ~ Crypto + AIG* ppt,
                random = ~1|Site_number,
                method = "REML",
                control = ctrl,
                data = btc.df)
btc.lmer <- lmer(Bromus_TC_pct^10 ~ Crypto + AIG* ppt+
                (1|Site_number),
                
                data = btc.df)
# summary(btc.lmef)
# plot_res_vs_all(btc.df, btc.lmef)

model_list$bromus_tc <- btc.lmef

# Bromus CN ---------------------------------------------------------------------------
btcn.df <- all[!is.na(all$Bromus_CN),] # getting rid of NA rows
# densityplot(log(btcn.df$Bromus_CN))
# 
# f <- formula(log(Bromus_CN) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                         Litter + Shrubs)* ppt)
# btcn.lm <- lm(f, data = btcn.df)
# 
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(btcn.lm)
# par(op)
# 
# #determining random structure
# best_random(btcn.df) # random i+s is the best
# btcn.lme <- lme(f, random = ~1 + Year|Site_number,
#                 control = ctrl,
#                 method = "REML",
#                 data = btcn.df)
# 
# # residuals vs predictors
# plot_res_vs_all(btcn.df, btcn.lme)
# 
# # there is a small difference in spread for precip
# #v_struct(formula(~ppt),df = btcn.df, r = formula(~1 + Year|Site_number))
# # vf is best
# #vf <- varFixed(~ppt)
# # max recommends avoiding this if possible in the interest of keeping the model simple
# 
# # finding the optimum fixed structure round 1
# btcn.lme2 <- lme(log(Bromus_CN) ~ Rock + Litter + PNG +( 
#                                      Shrubs)* ppt,
#                  random = ~1 + Year|Site_number,
#                  method = "ML",
#                  #weights = vf,
#                  control = ctrl,
#                  data = btcn.df)
# drop1(btcn.lme2)
# droptest(mod = btcn.lme2, coefs = coefs1)
# droptest(mod = btcn.lme2, coefs = coefs2)
# summary(btcn.lme2)

btcn.lmef <- lme(log(Bromus_CN) ~ Rock + Litter + PNG + Shrubs * ppt,
                 random = ~1 + Year|Site_number,
                 method = "REML",
                 #weights = vf,
                 control = ctrl,
                 data = btcn.df)
# summary(btcn.lmef)
# plot_res_vs_all(btcn.lmef, btcn.df)

model_list$bromus_tcn <- btcn.lmef

# Other TN ---------------------------------------------------------------------------
on.df <- all[!is.na(all$Other_TN_pct),] # getting rid of NA rows
# densityplot(on.df$Other_TN_pct)
# 
# f <- formula(Other_TN_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                   Litter + Shrubs)* ppt)
# on.lm <- lm(f, data = on.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(on.lm)
# par(op)
# 
# #determining random structure
# best_random(on.df)
# on.lme <- lme(f, 
#               random = ~1 + Year|Site_number, 
#               method = "REML",
#               control = ctrl,
#               data = on.df) 
# 
# plot_res_vs_all(on.df, on.lme)
# 
# # doesn't look to bad
# # finding the optimum fixed structure - round 1
# on.lme1 <- lme(Other_TN_pct ~ Shrubs+ (AIG +  AIF)* ppt, 
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = on.df)
# drop1(on.lme1)
# droptest(mod = on.lme1, coefs = coefs1)
# summary(on.lme1) 


# final model?
on.lmef <- lme(Other_TN_pct ~ (AIG +  AIF)* ppt + Shrubs, 
               random = ~1 + Year|Site_number,
               control = ctrl,
               method = "REML",
               data = on.df)
# summary(on.lmef)
# plot_res_vs_all(on.df,on.lmef)
# 
model_list$other_tn <- on.lmef


# Other TC ---------------------------------------------------------------------------
oc.df <- all[!is.na(all$Other_TC_pct),] # getting rid of NA rows
# densityplot(all$Other_TC_pct) # possible outlier
# oc.df <- oc.df[oc.df$Other_TC_pct >30,]
# densityplot(oc.df$Other_TC_pct) # 
# 
# 
# f <- formula(Other_TC_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                    Litter + Shrubs)* ppt)
# oc.lm <- lm(f, data = oc.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(oc.lm)
# par(op)
# 
# 
# #determining random structure
# best_random(oc.df)
# oc.lme <- lme(f, 
#               random = ~1|Site_number, 
#               method = "REML",
#               control = ctrl,
#               data = oc.df)
# plot_res_vs_all(oc.df, oc.lme)
# 
# # finding the optimum fixed structure
# oc.lme1 <- lme(Other_TC_pct ~ AIG + (PNG + ANF + Shrubs)* ppt, 
#                random = ~1|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = oc.df)
# drop1(oc.lme1)
# droptest(mod = oc.lme1, coefs = coefs1)
# droptest(mod = oc.lme1, coefs = coefs2)
# summary(oc.lme1) 
# vif(oc.lme1)

#validation
oc.lmef <- lme(Other_TC_pct ~ AIG + (PNG + ANF + Shrubs)* ppt, 
               random = ~1|Site_number,
               method = "REML",
               control = ctrl,
               data = oc.df)

# plot_res_vs_all(oc.df, oc.lmef)
# summary(oc.lmef)

model_list$other_tc <- oc.lmef


# Other C:N --------------------------------------------------------------------

ocn.df <- all[!is.na(all$Other_CN),] # getting rid of NA rows
# densityplot(log(ocn.df$Other_CN))
# 
# f <- formula(log(Other_CN) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                    Litter + Shrubs) * ppt)
# ocn.lm <- lm(f, data = ocn.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(ocn.lm)
# par(op)
# 
# #determining random structure
# best_random(df = ocn.df) # random i is best
# ocn.lme <- lme(f, 
#                random = ~1|Site_number, 
#                method = "REML", 
#                control = ctrl,
#                data = ocn.df) # lme with random intercept
# plot_res_vs_all(ocn.df, ocn.lme) 
# 
# # finding the optimum fixed structure - round 1
# ocn.lme1 <- lme(log(Other_CN) ~ Shrubs + (AIF + Litter) * ppt, 
#                 random = ~1|Site_number,
#                 method = "ML",
#                 control = ctrl,
#                 data = ocn.df)
# drop1(ocn.lme1)
# droptest(mod = ocn.lme1, coefs = coefs1)
# droptest(mod = ocn.lme1, coefs = coefs2)
# summary(ocn.lme1) 

# validation
ocn.lmef <- lme(log(Other_CN) ~ Shrubs + (AIF + Litter) * ppt, 
                random = ~1|Site_number,
                method = "REML",
                control = ctrl,
                data = ocn.df)
# plot_res_vs_all(ocn.df, ocn.lmef)
# summary(ocn.lmef)
model_list$other_cn <- ocn.lmef


# Poa TN -----------------------------------------------------------------------
pn.df <- all[!is.na(all$Poa_TN_pct),] # getting rid of NA rows
# densityplot(log(pn.df$Poa_TN_pct))
# 
# f <- formula(log(Poa_TN_pct) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                      Litter + Shrubs)* ppt)
# pn.lm <- lm(f, data = pn.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(pn.lm)
# par(op)
# 
# #determining random structure
# best_random(pn.df) #random is
# pn.lme <- lme(f, 
#                random = ~1 + Year|Site_number, 
#                method = "REML",
#                control = ctrl,
#                data = pn.df) # lme with random slope and intercept
# plot_res_vs_all(pn.df,pn.lme) #perhaps something with ppt
# 
# # 
# # v_struct(formula(~ppt),df = pn.df, r = formula(~1 + Year|Site_number))
# # vp = varPower(form = ~ppt)
# 
# # finding the optimum fixed structure
# pn.lme1 <- lme(log(Poa_TN_pct) ~ ANF + AIG + Shrubs* ppt, 
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = pn.df)
# drop1(pn.lme1) 
# droptest(mod = pn.lme1, coefs = coefs1)
# droptest(mod = pn.lme1, coefs = coefs2)
# summary(pn.lme1) 

# validate 
pn.lmef <- lme(log(Poa_TN_pct) ~ ANF + AIG + Shrubs* ppt, 
               random = ~1 + Year|Site_number,
               method = "REML",
               control = ctrl,
               data = pn.df)

# plot_res_vs_all(pn.df, pn.lmef)
# summary(pn.lmef)
model_list$poa_tn <- pn.lmef


# Poa TC -----------------------------------------------------------------------
pc.df <- all[!is.na(all$Poa_TC_pct),] # getting rid of NA rows
# densityplot(pc.df$Poa_TC_pct) # possible outlier
# pc.df <- pc.df[pc.df$Poa_TC_pct > 40,] # got rid of the outlier(s)
# densityplot(pc.df$Poa_TC_pct) # much better
# 
# f<- formula(Poa_TC_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                       Litter + Shrubs)* ppt)
# pc.lm <- lm(f, data = pc.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(pc.lm)
# par(op) # way better without outlier - but
# # 49 seems to cause problems - it has 75% rock maybe that is doing something
# 
# #determining random structure
# best_random(pc.df) # random i best
# pc.lme <- lme(f, 
#               random = ~1|Site_number, 
#               method = "REML", 
#               control = ctrl, 
#               data = pc.df) # lme with random intercept
# plot_res_vs_all(pc.df, pc.lme) # might need a better variance structure
# 
# # finding the optimum fixed structure
# pc.lme1 <- lme(Poa_TC_pct ~ AIF + Crypto + Litter + Rock + AIG* ppt, 
#                random = ~1|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = pc.df)
# drop1(pc.lme1) 
# droptest(mod = pc.lme1, coefs = coefs1)
# droptest(mod = pc.lme1, coefs2)
# summary(pc.lme1) 

# validation
pc.lmef <- lme(Poa_TC_pct ~ AIF + Crypto + Litter + Rock + AIG* ppt, 
               random = ~1|Site_number,
               method = "REML",
               control = ctrl,
               data = pc.df)
# plot_res_vs_all(pc.df,pc.lmef)
# summary(pc.lmef)
model_list$poa_tc <- pc.lmef


# Poa CN -----------------------------------------------------------------------
pcn.df <- all[!is.na(all$Poa_CN),] # getting rid of NA rows
# densityplot(sqrt(pcn.df$Poa_CN))
# 
# f <- formula(sqrt(Poa_CN) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                  Litter + Shrubs)* ppt)
# pcn.lm <- lm(f, data = pcn.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(pcn.lm)
# par(op) 
# 
# #determining random structure
# best_random(pcn.df)
# pcn.lme <- lme(f, 
#                random = ~1 + Year|Site_number, 
#                method = "REML", 
#                control = ctrl,
#                data = pcn.df) 
# plot_res_vs_all(pcn.df, pcn.lme)
# 
# # # might need to do weights by precip
# # v_struct(formula(~ppt),pcn.df,formula(~1+Year|Site_number)) # vp best
# # vp = varPower(form = ~ppt)
# 
# # finding the optimum fixed structure - round 1
# pcn.lme1 <- lme(sqrt(Poa_CN) ~ ANF + Shrubs * ppt, 
#                 random = ~1 + Year|Site_number,
#                 method = "ML",
#                 control = ctrl,
#                 #weights = vp,
#                 data = pcn.df)
# drop1(pcn.lme1)
# droptest(mod = pcn.lme1, coefs = coefs1)
# droptest(mod = pcn.lme1, coefs = coefs2)
# summary(pcn.lme1) 

# validate
pcn.lmef <- lme(sqrt(Poa_CN) ~ ANF + Shrubs * ppt, 
                random = ~1 + Year|Site_number,
                method = "REML",
                control = ctrl,
                #weights = vp,
                data = pcn.df)

# plot_res_vs_all(pcn.df, pcn.lmef)
# summary(pcn.lmef)
model_list$poa_cn <- pcn.lmef


# Litter TN ---------------------------------------------------------------------------
ln.df <- all[!is.na(all$Litter_TN_pct),] # getting rid of NA rows
# densityplot(sqrt(ln.df$Litter_TN_pct))
# 
# f <- formula(sqrt(Litter_TN_pct) ~ 
#                (Rock + Crypto + PNG + PNF + 
#                   ANF + AIG +  AIF + Litter + Shrubs)* ppt)
# ln.lm <- lm(f, data = ln.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(ln.lm)
# par(op) 
# 
# #determining random structure
# best_random(df=ln.df)
# ln.lme <- lme(f, 
#                random = ~1 + Year|Site_number, 
#                method = "REML", 
#                control = ctrl,
#                data = ln.df) # lme with random slope and intercept
# 
# plot_res_vs_all(ln.df, ln.lme) # might need weights by ppt
# # v_struct(df = ln.df, r = formula(~1 + Year|Site_number))
# # vf <- varFixed(~ppt)
# 
# # optimal fixed structure
# ln.lme1 <- lme(sqrt(Litter_TN_pct) ~ ppt,
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                #weights = vf,
#                data = ln.df)
# drop1(ln.lme1)
# droptest(mod = ln.lme1, coefs = coefs1)
# summary(ln.lme1) 

# validation
ln.lmef <- lme(sqrt(Litter_TN_pct) ~ ppt,
               random = ~1 + Year|Site_number,
               method = "REML",
               control = ctrl,
               #weights = vf,
               data = ln.df)

# plot_res_vs_all(ln.df, ln.lmef)
# summary(ln.lmef)
model_list$litter_tn <- ln.lmef


# Litter TC --------------------------------------------------------------------

lc.df <- all[!is.na(all$Litter_TC_pct),] # getting rid of NA rows
# densityplot((lc.df$Litter_TC_pct^2))
# 
# f <- formula(Litter_TC_pct^2 ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                           Litter + Shrubs)* ppt)
# lc.lm <- lm(f, data = lc.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(lc.lm)
# par(op) 
# 
# #determining random structure
# best_random(lc.df)
# lc.lme <- lme(f, 
#               random = ~1|Site_number, 
#               method = "REML", 
#               control = ctrl,
#               data = lc.df) # lme with random intercept
# 
# plot_res_vs_all(lc.df, lc.lme) 
# # v_struct(df = lc.df, r = formula(~1 + Year|Site_number))
# # vf <- varFixed(~ppt)
# 
# # round 1
# lc.lme1 <- lme(Litter_TC_pct^2 ~ Litter* ppt,
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = lc.df)
# drop1(lc.lme1)
# droptest(mod = lc.lme1, coefs = coefs1)
# droptest(mod = lc.lme1, coefs = coefs2)
# summary(lc.lme1) 


#validation
lc.lmef <- lme(Litter_TC_pct^2 ~ Litter* ppt,
               random = ~1 + Year|Site_number,
               method = "REML",
               control = ctrl,
               data = lc.df)
# plot_res_vs_all(lc.df, lc.lmef)
# summary(lc.lmef)

model_list$litter_tc <- lc.lmef


# Litter CN --------------------------------------------------------------------

lcn.df <- all[!is.na(all$Litter_CN),] # getting rid of NA rows
# densityplot(log(lcn.df$Litter_CN))
# 
# f <- formula(log(Litter_CN) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                     Litter + Shrubs)* ppt)
# lcn.lm <- lm(f, data = lcn.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(lcn.lm)
# par(op) 
# 
# #determining random structure
# best_random(lcn.df)
# 
# lcn.lme1 <- lme(f, 
#                 random = ~1|Site_number, 
#                 method = "REML", 
#                 data = lcn.df, 
#                 control = ctrl) # lme with random slope and intercept
# plot_res_vs_all(lcn.df, lcn.lme1) # no major problems with heterogeneity
# # v_struct(df = lcn.df, r = formula(~1|Site_number)) #none
# 
# # finding the fixed structure
# lcn.lme2 <- lme(log(Litter_CN) ~ Rock + ANF + (Crypto + Litter)* ppt, 
#                 random = ~1|Site_number,
#                 method = "ML",
#                 control = ctrl,
#                 data = lcn.df)
# drop1(lcn.lme2)
# droptest(lcn.lme2, coefs1);
# droptest(lcn.lme2, coefs2)
# summary(lcn.lme2)

#validation
lcn.lmef <- lme(log(Litter_CN) ~ Rock + ANF + (Crypto + Litter)* ppt, 
                random = ~1|Site_number,
                method = "REML",
                control = ctrl,
                data = lcn.df)
# summary(lcn.lmef)
# plot_res_vs_all(lcn.df,lcn.lmef)

model_list$litter_cn <- lcn.lmef


# Soil SO4 ---------------------------------------------------------------------
ss.df <- all[!is.na(all$SOIL_SurSo4_kg_ha),] # getting rid of NA rows
# densityplot((log(1 + ss.df$SOIL_SurSo4_kg_ha))) 
# # skewed biomodal with some possible outliers
# # fixed it by relativizing by year - possible lab error?
# 
# f <- formula(log(1+ SOIL_SurSo4_kg_ha) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                       Litter + Shrubs)* ppt)
# ss.lm <- lm(f, data = ss.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(ss.lm)
# par(op) 
# 
# #determining random structure
# best_random(ss.df)
# ss.lme <- lme(f, 
#                random = ~1| + Year|Site_number, 
#                method = "REML", 
#                data = ss.df, 
#                control = ctrl) # lme with random intercept
# 
# plot_res_vs_all(ss.df, ss.lme)
# 
# # v_struct(v=formula(~ppt), ss.df, r=formula(~1 + Year|Site_number)) #none
# 
# # round 1
# ss.lme1 <- lme(log(1+ SOIL_SurSo4_kg_ha) ~ ANF + (Shrubs)* ppt, 
#                random = ~1 + Year|Site_number, 
#                method = "ML", 
#                data = ss.df,
#                control = ctrl)
# drop1(ss.lme1) # keep ANF, AIF
# droptest(mod = ss.lme1, coefs = coefs1)
# droptest(mod = ss.lme1, coefs = coefs2)
# summary(ss.lme1) 

# validation
ss.lmef <- lme(log(1+ SOIL_SurSo4_kg_ha) ~ ANF + (Shrubs)* ppt, 
               random = ~1 + Year|Site_number, 
               method = "REML", 
               data = ss.df,
               control = ctrl)

# plot_res_vs_all(ss.df, ss.lmef)
# r.squaredGLMM(ss.lmef)
# summary(ss.lmef)

model_list$soil_so4 <- ss.lmef

# Soil CA ----------------------------------------------------------------------
sca.df <- all[!is.na(all$SOIL_Ca_kg_ha),] # getting rid of NA rows
# densityplot((log(sca.df$SOIL_Ca_kg_ha))) 
# 
# f <- formula(log(SOIL_Ca_kg_ha) ~ (Rock + Crypto + PNG + PNF + ANF + AIG + 
#                                     AIF + Litter + Shrubs)* ppt)
# sca.lm <- lm(f, data = sca.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(sca.lm)
# par(op) 
# 
# #determining random structure
# best_random(sca.df)
# sca.lme <- lme(f, 
#                random = ~1 + Year|Site_number, 
#                method = "REML", 
#                data = sca.df, 
#                control = ctrl) # lme with random slope and intercept
# plot_res_vs_all(sca.df, sca.lme)
# # v_struct(formula(~ppt), sca.df, r= formula(~1+Year|Site_number)) # none
# 
# # optimum fixed
# sca.lme1 <- lme(log(SOIL_Ca_kg_ha) ~ (Crypto + ANF + AIF + Shrubs)* ppt, 
#                 random = ~1 + Year|Site_number, 
#                 method = "ML", 
#                 data = sca.df,
#                 control = ctrl)
# drop1(sca.lme1) # keep Shrubs, AIF, AIG, ANF
# droptest(mod = sca.lme1, coefs = coefs1)
# droptest(mod = sca.lme1, coefs = coefs2)
# summary(sca.lme1) 



# Validation
sca.lmef <- lme(log(SOIL_Ca_kg_ha) ~ (Crypto + 
                  ANF + AIF + Shrubs) * ppt, 
                random = ~1 + Year|Site_number, 
                method = "REML", 
                data = sca.df,
                control = ctrl)

# plot_res_vs_all(df= sca.df, mod= sca.lmef)
# summary(sca.lmef)

model_list$soil_sca <- sca.lmef


# Soil Mg ----------------------------------------------------------------------

sm.df <- all[!is.na(all$SOIL_Mg_kg_ha),] # getting rid of NA rows
# densityplot(((sca.df$SOIL_Mg_kg_ha))) 
# 
# f <- formula(SOIL_Mg_kg_ha ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                      Litter + Shrubs)* ppt)
# sca.lm <- lm(f, data = sca.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(sca.lm)
# par(op) 
# 
# best_random(sm.df) #s+i
# sm.lme <- lme(f, random = ~1 + Year|Site_number,
#               method = "REML",
#               control = ctrl,
#               data = sm.df)
# plot_res_vs_all(sm.df, sm.lme) 
# # v_struct(v = formula(~ppt), df = sm.df, r= formula(~1 + Year|Site_number)) #ve
# # ve <- varExp(form = ~ppt)
# 
# # finding the optimum fixed structure
# 
# sm.lme1 <- lme(SOIL_Mg_kg_ha ~ Crypto + PNF + ppt, 
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                #weights = ve,
#                data = sm.df)
# drop1(sm.lme1)
# droptest(sm.lme1, coefs2)
# summary(sm.lme1)
# vif(sm.lme1)

#validation

sm.lmef <- lme(SOIL_Mg_kg_ha ~ Crypto + PNF + ppt, 
               random = ~1 + Year|Site_number,
               method = "REML",
               control = ctrl,
               #weights = ve,
               data = sm.df)

# plot_res_vs_all(sm.df, sm.lmef)
# summary(sm.lmef)

model_list$soil_smg <- sm.lmef


# Soil OM ----------------------------------------------------------------------
sc.df <- all[!is.na(all$SOIL_OM_pct),] # getting rid of NA rows
# densityplot(((sc.df$SOIL_OM_pct))) 
# 
# f <- formula(SOIL_OM_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                                 Litter + Shrubs)* ppt)
# sc.lm <- lm(f, data = sc.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(sc.lm)
# par(op) 
# 
# best_random(sc.df) #s+i
# sc.lme <- lme(f, random = ~1 + Year|Site_number,
#               method = "REML",
#               control = ctrl,
#               data = sc.df)
# plot_res_vs_all(sc.df, sc.lme) 
# v_struct(v = formula(~ppt), df = sc.df, r= formula(~1 + Year|Site_number)) #none
# 
# # finding the optimum fixed structure
# 
# sc.lme1 <- lme(SOIL_OM_pct ~ Shrubs + (Crypto)* ppt, 
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = sm.df)
# drop1(sc.lme1)
# droptest(sc.lme1, coefs2)
# summary(sc.lme1)
# vif(sc.lme1)

#validation
sc.lmef <- lme(SOIL_OM_pct ~ Shrubs + (Crypto)* ppt, 
               random = ~1 + Year|Site_number,
               method = "REML",
               control = ctrl,
               data = sc.df)

# plot_res_vs_all(sc.df, sc.lmef)
# summary(sc.lmef)

model_list$soil_tc <- sc.lmef


# Soil TN ----------------------------------------------------------------------
sn.df <- all[!is.na(all$SOIL_TN_pct),] # getting rid of NA rows
# densityplot(((sn.df$SOIL_TN_pct))) 
# 
# f <- formula(SOIL_TN_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
#                               Litter + Shrubs)* ppt)
# sn.lm <- lm(f, data = sn.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(sn.lm)
# par(op) 
# 
# best_random(sn.df) #s+i
# sn.lme <- lme(f, random = ~1 + Year|Site_number,
#               method = "REML",
#               control = ctrl,
#               data = sn.df)
# plot_res_vs_all(sn.df, sn.lme) 
# # v_struct(v = formula(~ppt), df = sn.df, r= formula(~1 + Year|Site_number)) #vp
# # vp = varPower(form = ~ppt)
# 
# # finding the optimum fixed structure
# 
# sn.lme1 <- lme(SOIL_TN_pct ~ AIG, 
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                #weights = vp,
#                control = ctrl,
#                data = sn.df)
# drop1(sn.lme1)
# droptest(sn.lme1, coefs1)
# droptest(sn.lme1, coefs2)
# summary(sn.lme1)
# vif(sn.lme1)

#validation
sn.lmef <- lme(SOIL_TN_pct ~ AIG, 
               random = ~1 + Year|Site_number,
               method = "REML",
               #weights = vp,
               control = ctrl,
               data = sn.df)

# plot_res_vs_all(sn.df, sn.lmef)
# summary(sn.lmef)

model_list$soil_stn <- sn.lmef


# Soil CN ----------------------------------------------------------------------

scn.df <- all[!is.na(all$SOIL_CN),] # getting rid of NA rows
# densityplot((log(scn.df$SOIL_CN)))  #possible outlier
# scn.df <- scn.df[scn.df$SOIL_CN < 25,]
# 
# f <- formula()
# scn.lm <- lm(f, data = scn.df)
# op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2)) 
# plot(scn.lm)
# par(op) 
# 
# best_random(scn.df) #ri
# scn.lme <- lme(f, random = ~1|Site_number,
#               method = "REML",
#               control = ctrl,
#               data = scn.df)
# plot_res_vs_all(scn.df, scn.lme)
# # v_struct(v = formula(~ppt), df = scn.df, r= formula(~1 + Year|Site_number)) #vp
# # vp <- varPower(form =~ppt)
# # finding the optimum fixed structure
# 
# scn.lme1 <- lme(log(SOIL_CN) ~  Rock + (Crypto + PNG + PNF + AIF)* ppt, 
#                random = ~1|Site_number,
#                method = "ML",
#                #weights = vp,
#                control = ctrl,
#                data = scn.df)
# drop1(scn.lme1)
# droptest(scn.lme1, coefs1)
# droptest(scn.lme1, coefs2)
# summary(scn.lme1)
# vif(scn.lme1)

#validation
scn.lmef <- lme(log(SOIL_CN) ~  Rock + (Crypto + PNG + PNF + AIF)* ppt, 
                random = ~1 + Year|Site_number,
                method = "REML",
                control = ctrl,
                #weights = vp,
                data = scn.df)

# plot_res_vs_all(scn.df, scn.lmef)
# summary(scn.lmef)

model_list$soil_scn <- scn.lmef



## lmm visualization ---------------------------------------------------------
# resp_names0 = c()
# for(i in 1:length(model_list)){
#   x <- model_list[[i]]$call$fixed[[2]]
#   resp_names0 <- append(resp_names0,x)
# }

resp_names <- c("Bromus Total N (%)",
                "Bromus Total C (%) (^10)",
                "Bromus Total C:N (log)",
                "Other Total N (%)",
                "Other Total C (%)",
                "Other Total C:N (log)",
                "Poa Total N (%) (log)",
                "Poa Total C (%)",
                "Poa Total C:N (sqrt)",
                "Litter Total N (%) (sqrt)",
                "Litter Total C (%) (^2)",
                "Litter Total C:N (log)",
                "Soil So4 (kg/ha) (log + 1)",
                "Soil Ca (kg/ha) (log)",
                "Soil Mg (kg/ha)",
                "Soil TC (%)",
                "Soil TN (%)",
                "Soil C:N (log)"
                )

coef_plots <- list()
for(i in 1:length(model_list)){
coef_plots[[i]] <- viz_mod(model_list[[i]], resp_names[[i]], path, names(model_list)[[i]])
}

int_plots <- list()
for(i in 1: length(model_list)){
  int_plots[[i]] <- viz_int(model_list[[i]])
}

# plot each response variable as a pdf   
for(i in 1:length(model_list)){
  L = length(int_plots[[i]]$plot.list)

  png(filename = paste0("lme_", names(model_list)[[i]], ".png"), width = 480*(L+1))
  set_theme(theme_pubr())
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 1+length(int_plots[[i]]$plot.list))))
  define_region <- function(row, col){
    viewport(layout.pos.row = row, layout.pos.col = col)
  } 
  
  print(coef_plots[[i]], vp = define_region(row = 1, col = 1))
  
  
  if(L == 1){
    print(int_plots[[i]]$plot.list[[1]], 
               vp = define_region(row = 1, col = 2))
  }
  
  if(L > 1){
    set_theme(theme_pubr(), legend.pos = "")
    for(j in 1:(L-1)){
      print(int_plots[[i]]$plot.list[[j]], 
            vp = define_region(row = 1, col = 1 + j))
      }
    set_theme(theme_pubr())
    print(int_plots[[i]]$plot.list[[length(int_plots[[i]]$plot.list)]], 
          vp = define_region(row = 1, col = L+1))
  }
  dev.off()
}


## SEM -------------------------------------------------------------------------
library(piecewiseSEM)
sem.fit(modelList = model_list[1:9], data = na.omit(all), .progressBar = TRUE)
sem.model.fits(model_list)
sem.plot(model_list[10:18], data=na.omit(all), standardize = "scale")

# in conclusion this doesn't work
# 
# DAG
# 
# ppt -> (cover <-> soil nutrient availability) -> plant nutrient content
# did it on draw.io
# 
# another idea - separate sems for N and C
library(semPlot)
n_list <- model_list[c(1,4,7,10,17)]
test = sem.fit(modelList = n_list, data = na.omit(all), .progressBar = TRUE)
sem.plot(n_list, data = all, show.nonsig = FALSE, scaling = 12)
semPaths(test)

c_list <- model_list[c(2,5,8,11,16)]
sem.plot(c_list, data=all, show.nonsig = FALSE, coef.table = c_coef_t)
semPlot::semPaths()


c_coef_t <- sem.coefs(c_list, all)
n_coef_t <- sem.coefs(n_list, all)

sem.plot()

# try it with lavaan

