# this is for doing an automated model building process using lmms

#### libraries ####
library(nlme) # lmes
library(doBy) # for summaryBy - data mongering
library(dplyr) # piping operators etc, the select function
library(plyr) # join
library(lme4) # duh
library(lmerTest) # for the step function
library(sjPlot) #sjp.lmer is the function to visualize the lmers
library(ggplot2) # duh
library(purrr) # for the map function
library(tidyverse) # this also might be required for some of this code...

#### data prep ####

glmm13 <- read.csv("data/Jones_2013_transect_dec17.csv")
glmm16 <- read.csv("data/jones_all_nov_2017.csv")
glmm13 <- glmm13[-c(10:15,46:54),] # getting rid of unrepeated sites

ppt = read.csv("data/ppt_sums.csv") #previously generated precip data from prism
names(ppt) = c("x", "ppt_11_13", "ppt_14_16", "Site_number") 

glmm13$Year = 2013
glmm13 = plyr::join(glmm13, ppt[,c(2,4)]) # magic numbers
names(glmm13)[33] <- "ppt"


glmm16$Year = 2016
glmm16 <- plyr::join(glmm16, ppt[,c(3,4)])
names(glmm16)[33] <- "ppt"


all = rbind(glmm13, glmm16)
all$Year = as.factor(all$Year)
all$Site_number = as.factor(all$Site_number)


# might have to rescale some variables... or just all of them
# rs <- as.data.frame(apply(resp, 2, scale)) 

#### automated modeling for exploratory analysis ####

# thanks, internet! this starts out with a model of every predictor, then uses the
# step function from lmerTest:: to select a model
# https://stackoverflow.com/questions/46045445/iterate-dependent-variable-using-purr-and-map-tidyverse

lmer.models <- all %>% 
  # getting rid of unused variables
  select(-Transect, -Site.type) %>% 
  
  # responses need to be gathered, but not predictors
  gather(variable, value, 
         -Site_number, -Bare, -Litter, -Crypto, 
         -AIG, -AIF, -PNF, -PNG, -ANF, -Shrubs, -Rock, -Year, -ppt ) %>% 
  
  # I actually don't know what this does but it's imortant
  split(.$variable) %>% 
  
  # then this runs the lmer for every variable
  map(~ lmer(value ~ (PNG + PNF + ANF + AIG +  AIF + Bare + 
                        Litter + Shrubs +Crypto)* ppt + (1|Site_number),
             data = .x)) %>% 
  
  # then this does an automated model building process
  map(lmerTest::step)

#### visualizing the coefficients ####

set_theme(base=theme_blank()) # my preferred minimalist theme

# now we're just looping through visualizing the model outputs
for(i in 1:length(lmer.models)){
  if(class(lmer.models[[i]]$model) == "lmerMod"){
    sjp.lmer(lmer.models[[i]]$model, 
             type = "fe", 
             title = paste(names(lmer.models)[i], class(lmer.models[[i]]$model), lmer.models[[i]]$model@devcomp$cmp[7]),
             geom.colors = "bw",
             sort.est = TRUE)}
  if(class(lmer.models[[i]]$model) == "lm"){
    x= summary(lmer.models[[i]]$model)
    sjp.lm(lmer.models[[i]]$model, 
           type = "lm", 
           title = paste(names(lmer.models)[i], class(lmer.models[[i]]$model), x$adj.r.squared),
           geom.colors = "bw",
           sort.est = TRUE)
  }
}



sjp.lmer(lmm_ca_st$model, type = "fe", title = as.character(paste(
  lmm_ca_st$call$formula[2],"~",lmm_ca_st$call$formula[3])), geom.color = "bw")


#### visualizing interactions ####
library(lattice)
xyplot(SOIL_TN_pct~AIG| cut(ppt,4), panel=function(x,y){
  panel.xyplot(x, y)
  panel.lmline(x, y, span=1)}, data=all,
  main = "Litter TN ~ PNG:ppt")

# model residuals
sjp.setTheme(base="theme_bw")

sjp.lmer(lmm_tn, y.offset = .4, sort.est = "sort.all")

sjp.lmer(lmm_tn, type = "re.qq", geom.colors = "bw")
sjp.lmer(lmm_ca_st$model, type = "fe", title = as.character(paste(
  lmm_tn@call$formula[2],"~",lmm_tn@call$formula[3])), geom.color = "bw")

sjp.lmer(lmm_tn, type = "fe.std")
sjp.lmer(lmm_tn, type = "fe.slope", facet.grid = TRUE)
sjp.lmer(lmm_tn, type = "fe.resid")
sjp.lmer(lmm_tn, type = "fe.cor")
sjp.lmer(lmm_tn, type = "ri.slope")
sjp.lmer(lmm_tn, type = "pred", vars = "Litter")

