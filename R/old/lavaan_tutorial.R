# setup ------------------------------------------------------------------------
library(ggpubr)
library(lavaan)
library(semPlot)

source("R/functions.R")
source("R/a_prep_mjcb.R")

# data mongering -- scaling everything, dividing years -------------------------

# aggregated by plot
all_p_sc <- all_p %>%
  dplyr::rename(sN = soil_n_kg_ha,
                sC = soil_c_kg_ha,
                sOM = SOIL_OM_pct,
                sCN = SOIL_CN,
                sMg = SOIL_Mg_kg_ha,
                sSO = SOIL_SurSo4_kg_ha,
                sCa = SOIL_Ca_kg_ha,
                OCN = Other_CN,
                BCN = Bromus_CN,
                PCN = Poa_CN,
                ltN = Litter_TN_pct,
                ltC = Litter_TC_pct,
                lCN = Litter_CN,
                aet = ja_ju_aet,
                ae2 = twoyr_aet,
                def = ja_ju_def,
                de2 = twoyr_def,
                tmn = ja_ju_tmin_min,
                p2t = ppt_2yrtot,
                p2 = ppt_2yr,
                p1 = ppt_1yr,
                BtC = Bromus_TC_pct,
                PsC = Poa_TC_pct,
                O_C = Other_TC_pct,
                BtN = Bromus_TN_pct,
                PsN = Poa_TN_pct,
                O_N = Other_TN_pct) %>%
  mutate(NF = ANF + PNF)
for(i in 1:ncol(all_p_sc)){
  if(is.numeric(as.data.frame(all_p_sc)[,i])){
    print(names(all_p_sc[,i]))
    all_p_sc[,i] <- as.numeric(scale(all_p_sc[,i]))
  }
}

# transects remain separate
all_sc <- all%>%
  dplyr::rename(sN = soil_n_kg_ha,
                sC = soil_c_kg_ha,
                sCN = SOIL_CN,
                sMg = SOIL_Mg_kg_ha,
                sSO = SOIL_SurSo4_kg_ha,
                sCa = SOIL_Ca_kg_ha,
                OCN = Other_CN,
                BCN = Bromus_CN,
                PCN = Poa_CN,
                ltN = Litter_TN_pct,
                ltC = Litter_TC_pct,
                lCN = Litter_CN,
                p2t = ppt_2yrtot,
                p2 = ppt_2yr,
                p1 = ppt_1yr,
                BtC = Bromus_TC_pct,
                PsC = Poa_TC_pct,
                O_C = Other_TC_pct,
                BtN = Bromus_TN_pct,
                PsN = Poa_TN_pct,
                O_N = Other_TN_pct) %>%
  mutate(NF = ANF + PNF)
for(i in 1:ncol(all_sc)){
  if(is.numeric(all_sc[,i])==TRUE){
    all_sc[,i] <- as.numeric(scale(all_sc[,i], center = F))
  }
}

# competition (i.e. cover vs plant N) ------------------------------------------

competition_path_model1 <- '
BCN ~ AIG + AIF + Litter + tmn + aet
#PCN ~ AIG + AIF + Litter + tmn + aet
#OCN ~ AIG + AIF + Litter + tmn + aet

AIG ~  tmn  + aet 
AIF ~  tmn  + aet + AIG
Litter ~ tmn  + aet 

AIG ~~ Litter
'

competition_path_model2 <- '
BCN ~ AIG + AIF  + tmn + aet + PNG
#PCN ~ AIG + AIF  + tmn + aet + PNG + p2
#OCN ~ AIG + AIF + Litter + tmn + aet + PNG + p2

PNG ~ tmn + aet + Litter + p2
AIG ~  tmn  + aet 
AIF ~  tmn  + aet + AIG
Litter ~ tmn  + aet + p2

#AIG ~~ Litter
'


bcn_fit <- sem(bcn_model, all_p_sc, group = "shrub_b");show(bcn_fit)
pcn_fit <- sem(pcn_model, all_p_sc, group = "shrub_b");show(pcn_fit)
ocn_fit <- sem(ocn_model, all_p_sc, group = "shrub_b");show(ocn_fit)

lcn_model <- '
lCN ~ Litter + AIG + AIF + PNG + aet +NF + tmn+p2
NF ~ tmn + aet +PNG+p2
AIG ~ aet+ tmn +PNG+p2
AIF ~ aet+ tmn +PNG+p2 +AIG
Litter ~ aet+ tmn + AIG+p2
PNG~aet+ tmn+p2
'

lcn_fit <- sem(lcn_model, all_p_sc, group = "shrub_b");show(lcn_fit)

fitmeasures(lcn_fit, c("aic", "ecvi"))
summary(lcn_fit, fit.measures = TRUE, standardized=TRUE, rsquare = TRUE)

summary(cfit, fit.measures = TRUE, standardized=TRUE, rsquare = TRUE)
summary(cfit2, fit.measures = TRUE, standardized=TRUE, rsquare = TRUE)
summary(pcn_fit, fit.measures = TRUE, standardized=TRUE, rsquare = TRUE)
summary(ocn_fit, fit.measures = TRUE, standardized=TRUE, rsquare = TRUE)

modificationindices(lcn_fit, sort = TRUE)[1:10,]

semPaths(cfit, 
         whatLabels = "std",
         what="std", 
         layout="circle", 
         intercepts = F,
         curve=T,
         curvature =5,
         #theme = "gray",
         theme = "colorblind",
         edge.label.cex = 1,
         rotation =1)

semPaths(bcn_fit, 
         whatLabels = "std",
         what="std", 
         layout="spring", 
         intercepts = F,
         curve=T,
         #theme = "gray",
         theme = "colorblind",
         edge.label.cex = 1,
         rotation =1)

semPaths(pcn_fit, 
         whatLabels = "std",
         what="std", 
         layout="spring", 
         intercepts = F,
         curve=T,
         #theme = "gray",
         theme = "colorblind",
         edge.label.cex = 1,
         rotation =1)

semPaths(ocn_fit, 
         whatLabels = "std",
         what="std", 
         layout="spring", 
         intercepts = F,
         curve=T,
         #theme = "gray",
         theme = "colorblind",
         edge.label.cex = 1,
         rotation =1)

# cover vs soil N and C --------------------------------------------------------

soil_model <- '
sN ~ Litter + AIG + AIF + PNG + aet +NF + tmn
#sC ~ Litter + AIG + AIF + PNG +aet + NF + tmn

NF ~ tmn + aet
AIG ~ aet+ tmn
AIF ~ aet+ tmn
Litter ~ aet + tmn + sN +p2
PNG~aet+ tmn
'

soil_n_model <- '
sN ~ Litter + AIG + AIF + PNG + aet +NF + tmn+p2
NF ~ tmn + aet +PNG+p2
AIG ~ aet+ tmn +PNG+p2 
AIF ~ aet+ tmn +PNG+p2 +AIG
Litter ~ aet+ tmn + AIG+p2
PNG~aet+ tmn+p2
'

sn_fit <- sem(soil_n_model, all_p_sc, group = "shrub_b"
); show(sn_fit)
modificationindices(sn_fit, sort = TRUE)[1:10,]
summary(sn_fit, standardized =TRUE, fit.measures =TRUE, rsquare=TRUE)

semPaths(sn_fit, 
         what="std", 
         layout="tree", 
         intercepts = F,
         residuals =F,
         #theme = "gray",
         #panelGroups = T,
         curve = T,
         title =T,
         combineGroups = F,
         edge.label.cex = 1,
         theme = "colorblind",
         rotation =2)

soil_c_model <- '
sC ~ Litter + AIG + aet + tmn + PNG +AIF
AIG ~ aet + AIF  + tmn + p2 + PNG
AIF~ aet + tmn + p2 + AIG + Litter
Litter ~ aet + AIG + PNG + tmn + p2
PNG ~ aet + tmn + p2
'
sc_fit <- sem(soil_c_model, all_p_sc, group = "shrub_b"
); show(sc_fit)
summary(sc_fit, standardized =TRUE, fit.measures =TRUE, rsquare=TRUE)
modificationindices(sc_fit, sort = TRUE)[1:10,]

semPaths(sc_fit, 
         what="std", 
         layout="tree", 
         intercepts = F,
         residuals =F,
         #theme = "gray",
         #panelGroups = T,
         curve = T,
         title =T,
         combineGroups = F,
         edge.label.cex = 1,
         theme = "colorblind",
         rotation =2)

semPaths(sfit2, 
         what="std", 
         layout="circle", 
         intercepts = F,
         residuals =F,
         #theme = "gray",
         #panelGroups = T,
         curve = T,
         title =T,
         combineGroups = F,
         edge.label.cex = 1,
         theme = "colorblind",
         rotation =1)

semPaths(sfitc, 
         what="std", 
         layout="circle", 
         intercepts = F,
         residuals =F,
         #theme = "gray",
         #panelGroups = T,
         curve = T,
         title =T,
         combineGroups = F,
         edge.label.cex = 1,
         theme = "colorblind",
         rotation =1)


# all together -----------------------------------------------------------------

path_model_all_l <- '
BtC ~ AIG + AIF + tmax + PNG + NF + Litter
O_C ~ AIG + AIF + tmax + PNG + NF + Litter
PsC ~ AIG + AIF + tmax + PNG + NF + Litter
BtN ~ AIG + AIF + tmax + PNG + NF + Litter
O_N ~ AIG + AIF + tmax + PNG + NF + Litter
PsN ~ AIG + AIF + tmax + PNG + NF + Litter

sN ~ tmax + AIG + AIF + PNG + NF + Litter
sC ~ tmax + AIG + AIF + PNG + NF + Litter
sCN ~ tmax + AIG + AIF + PNG + NF + Litter

Litter ~ tmax
AIG ~ tmax
AIF ~ tmax
PNG ~ tmax
NF ~ tmax
'

path_model_all <- '
BtC ~ AIG + AIF + tmax + PNG + NF
O_C ~ AIG + AIF + tmax + PNG + NF
PsC ~ AIG + AIF + tmax + PNG + NF
BtN ~ AIG + AIF + tmax + PNG + NF
O_N ~ AIG + AIF + tmax + PNG + NF
PsN ~ AIG + AIF + tmax + PNG + NF

sN ~ tmax + AIG + AIF + PNG  + NF
sC ~ tmax + AIG + AIF + PNG + NF
sCN ~ tmax + AIG + AIF + PNG + NF

AIG ~ tmax
AIF ~ tmax
PNG ~ tmax
NF ~ tmax
'

afits <- sem(path_model_all, all_p_sc[all_p_sc$shrub_b == "Shrub",]);show(afits)
semPaths(afits, 
         what="std", 
         layout="circle", 
         #theme = "gray",
         theme = "colorblind",
         rotation =1); title("Full Model: Unburned (Shrub Sites)", line=2)
afitg <- sem(path_model_all, all_p_sc[all_p_sc$shrub_b == "Grass",]);show(afitg)
semPaths(afitg, 
         what="std", 
         #theme = "gray",
         theme = "colorblind",
         layout="circle", 
         rotation =1); title("Full Model: Burned (No Shrubs)", line=2)

afitgl <- sem(path_model_all_l, all_p_sc[all_p_sc$shrub_b == "Grass",]);show(afitgl)
semPaths(afitgl, 
         what="std", 
         #theme = "gray",
         theme = "colorblind",
         layout="circle", 
         rotation =1); title("Full Model: Burned (No Shrubs)", line=2)



# litter
littermod <- '
lCN ~ BCN + OCN + Litter
Litter ~ tmin + AIG + AIF +p2
AIG ~ tmin 
AIF ~ tmin
sMg ~ tmin
BCN ~ AIF + AIG + tmin + sMg
OCN ~ AIF + AIG + tmin + sMg
'
lit <- sem(littermod, all_p_sc[all_p_sc$shrub_b == "Grass",]);show(lit)
semPaths(lit, what="std",layout="circle")


# piecewiseSEM version ---------------------------------------------------------
library(piecewiseSEM)
library(lme4)

littermod_ <- psem(
lmer(lCN ~ Litter + AIG + AIF + PNG +aet+NF+tmn+p2 + (1|shrub_b), all_p_sc),
lmer(Litter ~ tmn + AIG + aet+p2 + (1|shrub_b), all_p_sc),
lmer(AIG ~ aet+ tmn +PNG+p2 + (1|shrub_b), all_p_sc),
lmer(AIF ~ tmn + aet+p2 + PNG + (1|shrub_b), all_p_sc),
lmer(NF ~ tmn + aet +PNG+p2 + (1|shrub_b), all_p_sc),
lmer(PNG ~ aet + tmn + p2 + (1|shrub_b), all_p_sc)
)
summary(littermod_)

soil_psem <- psem(
lm(sN ~ Litter + tmin + AIG + AIF + PNG + NF + lCN + p1 +vpdmx + Year, all_p_sc[all_p_sc$shrub_b == "Shrub",]),
lm(lCN ~ Litter + tmin + AIG + PNG +  p1 + vpdmx+ Year, all_p_sc[all_p_sc$shrub_b == "Shrub",]),
lm(AIG ~ tmin+ p1+ vpdmx+ Year, all_p_sc[all_p_sc$shrub_b == "Shrub",]),
lm(AIF ~ tmin+ p1+ vpdmx+ Year, all_p_sc[all_p_sc$shrub_b == "Shrub",]),
lm(PNG ~ tmin+ p1+ vpdmx+ Year, all_p_sc[all_p_sc$shrub_b == "Shrub",]),
lm(NF ~ tmin+ p1+ vpdmx+ Year, all_p_sc[all_p_sc$shrub_b == "Shrub",]),
lm(Litter ~ tmin + AIG + AIF + p1+ vpdmx+ Year, all_p_sc[all_p_sc$shrub_b == "Shrub",])
)
summary(soil_psem)

soil_psem <- psem(
  lmer(sC ~ Litter + tmin + AIG + AIF + PNG + NF  + p1  + (1|Site_number), all_sc),
  lmer(AIG ~ tmin+ p1+  (1|Site_number), all_sc),
  lmer(AIF ~ tmin+ p1+ (1|Site_number), all_sc),
  lmer(PNG ~ tmin+ p1+ (1|Site_number), all_sc),
  lmer(NF ~ tmin+ p1+  (1|Site_number), all_sc),
  lmer(Litter ~ tmin + AIG + AIF + p1+ (1|Site_number), all_sc)
)
summary(soil_psem)

soil_psem <- psem(
  lmer(sN ~ Litter + tmin + AIG + AIF + PNG + NF  + p1  + (1|Site_number), all_sc),
  lmer(AIG ~ tmin+ p1+  (1|Site_number), all_sc),
  lmer(AIF ~ tmin+ p1+ (1|Site_number), all_sc),
  lmer(PNG ~ tmin+ p1+ (1|Site_number), all_sc),
  lmer(NF ~ tmin+ p1+  (1|Site_number), all_sc),
  lmer(Litter ~ tmin + AIG + AIF + p1+ (1|Site_number), all_sc)
)
summary(soil_psem)

# make a df for a table