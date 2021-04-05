source("R/a_prep_mjcb.R")


# densityplot(btn.df$Bromus_TN_pct)

f <- formula(pca1_inv ~ ppt_2yrtot+ppt_2yr+ppt_1yr+tmax+tmin+vpdmx)
cor(select(all, ppt_2yrtot, ppt_2yr, ppt_1yr, tmax, tmin, vpdmx))
best_random(df=all) 

clim.lme <- lme(pca1_inv ~ ppt_2yrtot+ppt_2yr+ppt_1yr+tmax+tmin+vpdmx,
                random = ~1 + Year|Site_number,
                control = ctrl,
                method = "ML",
                data = all)
drop1(clim.lme)
droptest(mod = clim.lme, coefs = coefs1)
droptest(mod = clim.lme, coefs = coefs2)
summary(btn.lme1)



## real modelling --------------------------------------------------------------
# check correlation of explanatory variables
# tri_cor(pred) #litter and bare correlated, nothing else problematic, removing 
# bare

# general parameters -----------------------------------------------------------
path <- "Images/model_viz_jan18/" # my path for saving plot images
ctrl <- lmeControl(opt = "optim", maxIter = 1000, msMaxIter = 1000)

coefs1 = c("Rock:ppt","Crypto:ppt", "PNG:ppt", "PNF:ppt", "ANF:ppt", "AIG:ppt",
           "AIF:ppt","Litter:ppt", "Shrubs:ppt","SOIL_SurSo4_kg_ha:ppt", 
           "SOIL_Ca_kg_ha:ppt", "SOIL_Mg_kg_ha:ppt","SOIL_OM_pct:ppt", 
           "SOIL_TN_pct:ppt", "SOIL_CN:ppt")

coefs2 <- c("Rock","Crypto","Shrubs","PNF","PNG",
            "ANF","AIG","AIF","Litter","ppt","SOIL_SurSo4_kg_ha", 
            "SOIL_Ca_kg_ha", "SOIL_Mg_kg_ha", "SOIL_OM_pct", "SOIL_TN_pct", 
            "SOIL_CN")

# all$ppt <- as.numeric(scale(all$ppt, center = FALSE)) # so lmer doesn't get mad
# all$ppt <- round(all$ppt, 2) # easier visualization - doesn't affect models

# Bromus TN NO CHANGE ----------------------------------------------------------
#  
btn.df <- all[!is.na(all$Bromus_TN_pct),] # getting rid of NA rows
# densityplot(btn.df$Bromus_TN_pct)
# 
f <- formula(Bromus_TN_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
                              Litter + Shrubs + SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
                              +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
# 
# 
# # determining best random structure - beyond optimal model
# best_random(df=btn.df) # random s & i is the best
# btn.lme <- lme(f, random = ~1 + Year|Site_number,
#                 control = ctrl,
#                 method = "ML",
#                 data = btn.df)
# #plot_res_vs_all(df=btn.df, mod = btn.lme)
# 
# 
# # finding the optimal fitted structure
# btn.lme1 <- lme(Bromus_TN_pct ~ AIF+ PNG +(
#                                    Shrubs 
#                                  )* ppt,
#                 random = ~1 + Year|Site_number,
#                 control = ctrl,
#                 method = "ML",
#                 data = btn.df)
# drop1(btn.lme1)
# droptest(mod = btn.lme1, coefs = coefs1)
# droptest(mod = btn.lme1, coefs = coefs2)
# summary(btn.lme1)
# 
# 
# 
# # refit and validate
btn.lmef <- lme(Bromus_TN_pct ~ PNG +  AIF + Shrubs * ppt,
                random = ~1 + Year|Site_number,
                control = ctrl,method = "REML",
                data = btn.df)
# # plot_res_vs_all(btn.df, btn.lmef)
# # summary(btn.lmef)
model_list$bromus_tn <- btn.lmef

# Bromus TC ---------------------------------------------------------------------------
btc.df <- all[!is.na(all$Bromus_TC_pct),] # getting rid of NA rows
# densityplot((btc.df$Bromus_TC_pct)^10)
# 
# f <- formula(Bromus_TC_pct^10 ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
#                                    Litter + Shrubs + SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                                  +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)
#              * ppt)
# 
# 
# # determining best random structure - beyond optimal model
# 
# best_random(df = btc.df) #random I
# btc.lme <- lme(f, 
#                random = ~1|Site_number,
#                method = "ML", 
#                control = ctrl,
#                data = btc.df)
# #plot residuals vs predictors
# 
# # optimal fixed structure
# btc.lme1 <- lme(Bromus_TC_pct^10 ~ Crypto + SOIL_SurSo4_kg_ha + AIG * ppt,
#                 random = ~1|Site_number,
#                 method = "ML",
#                 control = ctrl,
#                 data = btc.df)
# drop1(btc.lme1)
# droptest(btc.lme1, coefs1)
# droptest(btc.lme1, coefs2)
# summary(btc.lme1)
# 
# vif(btc.lme1)

#validation
btc.lmef <- lme(Bromus_TC_pct ~ Crypto +SOIL_SurSo4_kg_ha + AIG* ppt,
                random = ~1|Site_number,
                method = "REML",
                control = ctrl,
                data = btc.df)
# summary(btc.lmef)
# MuMIn::r.squaredGLMM(btc.lmef)
# dropping the ^10 doesn't change the model much, but decreases the R2 by 0.1

# plot_res_vs_all(btc.df, btc.lmef)

model_list$bromus_tc <- btc.lmef

# Bromus CN NO CHANGE-----------------------------------------------------------
btcn.df <- all[!is.na(all$Bromus_CN),] # getting rid of NA rows
# densityplot(log(btcn.df$Bromus_CN))
# 
# f <- formula(log(Bromus_CN) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
#                                  Litter + Shrubs + SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                                +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
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
# btcn.lme2 <- lme(log(Bromus_CN) ~ Rock + Litter +PNG + Shrubs* ppt,
#                  random = ~1 + Year|Site_number,
#                  method = "ML",
#                  control = ctrl,
#                  data = btcn.df)
# drop1(btcn.lme2)
# droptest(mod = btcn.lme2, coefs = coefs1)
# droptest(mod = btcn.lme2, coefs = coefs2)
# summary(btcn.lme2)
# 
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
f <- formula(Other_TN_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
                                  Litter + Shrubs+ SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
                             +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)

# #determining random structure
# best_random(on.df)
# on.lme <- lme(f,
#               random = ~1 + Year|Site_number,
#               method = "REML",
#               control = ctrl,
#               data = on.df)
# 
# #plot_res_vs_all(on.df, on.lme)
# 
# # doesn't look to bad
# # finding the optimum fixed structure - round 1
# on.lme1 <- lme(Other_TN_pct ~ SOIL_Ca_kg_ha+
#                (Shrubs+ SOIL_SurSo4_kg_ha+SOIL_OM_pct)* ppt,
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = on.df)
# drop1(on.lme1)
# droptest(mod = on.lme1, coefs = coefs1)
# droptest(mod = on.lme1, coefs = coefs2)
# 
# summary(on.lme1)


# final model?
on.lmef <- lme(Other_TN_pct ~ SOIL_Ca_kg_ha+
                 (Shrubs+ SOIL_SurSo4_kg_ha+SOIL_OM_pct)* ppt, 
               random = ~1 + Year|Site_number,
               control = ctrl,
               method = "REML",
               data = on.df)
# summary(on.lmef)
# plot_res_vs_all(on.df,on.lmef)
# 
model_list$other_tn <- on.lmef


# Other TC NO CHANGE -----------------------------------------------------------
oc.df <- all[!is.na(all$Other_TC_pct),] # getting rid of NA rows
# densityplot(all$Other_TC_pct) # possible outlier
# oc.df <- oc.df[oc.df$Other_TC_pct >30,]
# densityplot(oc.df$Other_TC_pct) #
# 
# 
# f <- formula(Other_TC_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
#                                Litter + Shrubs+ SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                              +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
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
# oc.lme1 <- lme(Other_TC_pct ~ AIG + (PNG + ANF + 
#                                  Shrubs
#                                )* ppt,
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


# Other C:N ---------------------------------------------------------------------------

ocn.df <- all[!is.na(all$Other_CN),] # getting rid of NA rows
# densityplot(log(ocn.df$Other_CN))
# 
# f <- formula(log(Other_CN) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
#                                 Litter + Shrubs+ SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                               +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
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
# finding the optimum fixed structure - round 1
ocn.lme1 <- lme(log(Other_CN) ~ Shrubs + AIF+SOIL_OM_pct+(Crypto +
                                   SOIL_Ca_kg_ha
                                 +SOIL_Mg_kg_ha)* ppt,
                random = ~1|Site_number,
                method = "ML",
                control = ctrl,
                data = ocn.df)
drop1(ocn.lme1)
droptest(mod = ocn.lme1, coefs = coefs1)
droptest(mod = ocn.lme1, coefs = coefs2)
vif(ocn.lme1)
summary(ocn.lme1)

# validation
ocn.lmef <- lme(log(Other_CN) ~ Shrubs + AIF+SOIL_OM_pct+(Crypto +
                                                            SOIL_Ca_kg_ha
                                                          +SOIL_Mg_kg_ha)* ppt, 
                random = ~1|Site_number,
                method = "REML",
                control = ctrl,
                data = ocn.df)
# plot_res_vs_all(ocn.df, ocn.lmef)
# summary(ocn.lmef)
model_list$other_cn <- ocn.lmef


# Poa TN NO CHANGE -------------------------------------------------------------
pn.df <- all[!is.na(all$Poa_TN_pct),] # getting rid of NA rows
# densityplot(log(pn.df$Poa_TN_pct))
# 
# f <- formula(log(Poa_TN_pct) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
#                                   Litter + Shrubs+ SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                                 +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
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
# pn.lme1 <- lme(log(Poa_TN_pct) ~ ANF+ AIG + (
#                                     Shrubs
#                                   )* ppt,
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


# Poa TC ---------------------------------------------------------------------------
pc.df <- all[!is.na(all$Poa_TC_pct),] # getting rid of NA rows
# densityplot(pc.df$Poa_TC_pct) # possible outlier
pc.df <- pc.df[pc.df$Poa_TC_pct > 40,] # got rid of the outlier(s)
# densityplot(pc.df$Poa_TC_pct) # much better
# 
# f<- formula(Poa_TC_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
#                             Litter + Shrubs+ SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                           +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
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
# pc.lme1 <- lme(Poa_TC_pct ~ AIF +Litter + Crypto  +SOIL_TN_pct+ Rock+ PNG + SOIL_SurSo4_kg_ha
#                + (PNF +  AIG)* ppt,
#                random = ~1|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = pc.df)
# drop1(pc.lme1)
# droptest(mod = pc.lme1, coefs = coefs1)
# droptest(mod = pc.lme1, coefs2)
# vif(pc.lme1)
# summary(pc.lme1)

# validation
pc.lmef <- lme(Poa_TC_pct ~ AIF + Litter + Crypto + SOIL_TN_pct +
                 Rock + PNG + SOIL_SurSo4_kg_ha + (PNF +  AIG)* ppt, 
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
#                                Litter + Shrubs+ SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                              +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
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
# pcn.lme1 <- lme(sqrt(Poa_CN) ~ AIG +  Shrubs+ 
#                   (SOIL_OM_pct+SOIL_TN_pct)* ppt,
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
pcn.lmef <- lme(sqrt(Poa_CN) ~ AIG + Shrubs + 
                  (SOIL_OM_pct + SOIL_TN_pct)* ppt, 
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
# f <- formula(sqrt(Litter_TN_pct) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
#                                       Litter + Shrubs+ SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                                     +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
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
# ln.lme1 <- lme(sqrt(Litter_TN_pct) ~ SOIL_OM_pct+SOIL_TN_pct+ ppt,
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                #weights = vf,
#                data = ln.df)
# drop1(ln.lme1)
# droptest(mod = ln.lme1, coefs = coefs1)
# droptest(mod = ln.lme1, coefs = coefs2)
# summary(ln.lme1)

# validation
ln.lmef <- lme(sqrt(Litter_TN_pct) ~  SOIL_OM_pct+SOIL_TN_pct+ ppt,
               random = ~1 + Year|Site_number,
               method = "REML",
               control = ctrl,
               #weights = vf,
               data = ln.df)

# plot_res_vs_all(ln.df, ln.lmef)
# summary(ln.lmef)
model_list$litter_tn <- ln.lmef


# Litter TC ---------------------------------------------------------------------------

lc.df <- all[!is.na(all$Litter_TC_pct),] # getting rid of NA rows
# densityplot((lc.df$Litter_TC_pct^2))
# 
# f <- formula(Litter_TC_pct^2 ~  (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
#                                    Litter + Shrubs+ SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                                  +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
# 
# # determining random structure
# best_random(lc.df)
# lc.lme <- lme(f,
#               random = ~1|Site_number,
#               method = "REML",
#               control = ctrl,
#               data = lc.df) # lme with random intercept
# 
# # plot_res_vs_all(lc.df, lc.lme)
# # v_struct(df = lc.df, r = formula(~1 + Year|Site_number))
# # vf <- varFixed(~ppt)
# 
# # round 1
# lc.lme1 <- lme(Litter_TC_pct^2 ~  PNG + Rock + (AIG + SOIL_CN)* ppt,
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = lc.df)
# drop1(lc.lme1)
# droptest(mod = lc.lme1, coefs = coefs1)
# droptest(mod = lc.lme1, coefs = coefs2)
# summary(lc.lme1)


#validation
lc.lmef <- lme(Litter_TC_pct^2 ~ PNG + Rock + (AIG + SOIL_CN)* ppt,
               random = ~1 + Year|Site_number,
               method = "REML",
               control = ctrl,
               data = lc.df)
# plot_res_vs_all(lc.df, lc.lmef)
# summary(lc.lmef)

model_list$litter_tc <- lc.lmef


# Litter CN ---------------------------------------------------------------------------

lcn.df <- all[!is.na(all$Litter_CN),] # getting rid of NA rows
# densityplot(log(lcn.df$Litter_CN))
# 
# f <- formula(log(Litter_CN) ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF +
#                                   Litter + Shrubs+ SOIL_SurSo4_kg_ha+SOIL_Ca_kg_ha
#                                 +SOIL_Mg_kg_ha+SOIL_OM_pct+SOIL_TN_pct+SOIL_CN)* ppt)
# 
# 
# #determining random structure
# best_random(lcn.df)
# 
# lcn.lme1 <- lme(f,
#                 random = ~1|Site_number,
#                 method = "REML",
#                 data = lcn.df,
#                 control = ctrl) 
# # plot_res_vs_all(lcn.df, lcn.lme1) # no major problems with heterogeneity
# # v_struct(df = lcn.df, r = formula(~1|Site_number)) #none
# 
# # finding the fixed structure
# lcn.lme2 <- lme(log(Litter_CN) ~ SOIL_CN+ PNF + AIF +( AIG +  
#                                      SOIL_Mg_kg_ha)* ppt,
#                 random = ~1|Site_number,
#                 method = "ML",
#                 control = ctrl,
#                 data = lcn.df)
# drop1(lcn.lme2)
# droptest(lcn.lme2, coefs1);
# droptest(lcn.lme2, coefs2)
# summary(lcn.lme2)

#validation
lcn.lmef <- lme(log(Litter_CN) ~ SOIL_CN+ PNF + AIF + 
                  (AIG + SOIL_Mg_kg_ha)* ppt, 
                random = ~1|Site_number,
                method = "REML",
                control = ctrl,
                data = lcn.df)
# summary(lcn.lmef)
# plot_res_vs_all(lcn.df,lcn.lmef)

model_list$litter_cn <- lcn.lmef


# changing coef variables, checking correlation---------------------------------
cor(all[,c(4:15, 23:31)], use = "pairwise.complete.obs")
# > 0.7
# bromusCN/TN #
# other TN/ bromus tn #
# poa tn/bromus tn
# other cn/ other tn #
# poa tn/poa cn
# litter tn/cn #
# removing litter cn, poa tn, bromus cn, other tn

coefs1 = c("Rock:ppt","Crypto:ppt", "PNG:ppt", "PNF:ppt", "ANF:ppt", "AIG:ppt",
           "AIF:ppt","Litter:ppt", "Shrubs:ppt","Bromus_TN_pct:ppt", 
           "Bromus_TC_pct:ppt", 
           "Other_TC_pct:ppt","Other_CN:ppt","Poa_TC_pct:ppt",
           "Poa_CN:ppt","Litter_TN_pct:ppt","Litter_TC_pct:ppt")

coefs2 <- c("Rock","Crypto","Shrubs","PNF","PNG",
            "ANF","AIG","AIF","Litter","ppt","Bromus_TN_pct", "Bromus_TC_pct",
            "Other_TC_pct","Other_CN",
            "Poa_TC_pct","Poa_CN","Litter_TN_pct","Litter_TC_pct")

soil.df <- na.omit(all)


# Soil SO4 ---------------------------------------------------------------------
ss.df <- all[!is.na(all$SOIL_SurSo4_kg_ha),] # getting rid of NA rows
# densityplot((log(1 + ss.df$SOIL_SurSo4_kg_ha)))
# # skewed biomodal with some possible outliers
# # fixed it by relativizing by year - possible lab error?
# 
# f <- formula(log(1 + SOIL_SurSo4_kg_ha) ~ (Rock + Crypto + PNG + PNF + ANF + AIG + AIF +
#                                       Litter + Shrubs+Bromus_TN_pct+Bromus_TC_pct+
#                                         Other_TC_pct+Other_CN+
#                                         Poa_TC_pct+Poa_CN+Litter_TN_pct+Litter_TC_pct)* ppt)
# #determining random structure
# best_random(ss.df)
# ss.lme <- lme(f,
#                random = ~1 + Year|Site_number,
#                method = "REML",
#                data = ss.df,
#                na.action = na.omit,
#                control = ctrl) # lme with random intercept
# 
# plot_res_vs_all(ss.df, ss.lme)
# 
# # v_struct(v=formula(~ppt), ss.df, r=formula(~1 + Year|Site_number)) #none
# 
# # round 1
# ss.lme1 <- lme(log(1+ SOIL_SurSo4_kg_ha) ~  Shrubs + Bromus_TC_pct + ppt,
#                random = ~1 + Year|Site_number,
#                na.action = na.omit,
#                method = "ML",
#                data = ss.df,
#                control = ctrl)
# drop1(ss.lme1) 
# droptest(mod = ss.lme1, coefs = coefs1)
# droptest(mod = ss.lme1, coefs = coefs2)
# summary(ss.lme1)

# validation # used combo of prior model and one built from the above (lots of
# problems with NAs, model complexity made me somewhat untrustful of the above 
# process)
ss.lmef <- lme(log(1+ SOIL_SurSo4_kg_ha) ~ ANF + Bromus_TC_pct + Shrubs * ppt, 
               random = ~1 + Year|Site_number, 
               method = "REML", 
               data = ss.df,
               na.action = na.omit,
               control = ctrl)

# plot_res_vs_all(ss.df, ss.lmef)
# r.squaredGLMM(ss.lmef)
# summary(ss.lmef)

model_list$soil_so4 <- ss.lmef

# Soil CA NO CHANGE-------------------------------------------------------------
sca.df <- all[!is.na(all$SOIL_Ca_kg_ha),] # getting rid of NA rows
# densityplot((log(sca.df$SOIL_Ca_kg_ha)))
# 
# f <- formula(log(SOIL_Ca_kg_ha) ~ (Rock + Crypto + PNG + PNF + ANF + AIG + AIF +
#                                      Litter + Shrubs+Bromus_TN_pct+Bromus_TC_pct+
#                                      Other_TC_pct+Other_CN+
#                                      Poa_TC_pct+Poa_CN+Litter_TN_pct+Litter_TC_pct)* ppt)
# 
# #determining random structure
# best_random(sca.df)
# sca.lme <- lme(f,
#                random = ~1 + Year|Site_number,
#                method = "REML",
#                na.action = na.pass,
#                data = soil.df,
#                control = ctrl) # lme with random slope and intercept

# plot_res_vs_all(sca.df, sca.lme)
# v_struct(formula(~ppt), sca.df, r= formula(~1+Year|Site_number)) # none

# optimum fixed
# sca.lme1 <- lme(log(SOIL_Ca_kg_ha) ~ (Crypto + ANF + AIF + Shrubs) * ppt,
#                 random = ~1 + Year|Site_number,
#                 method = "ML",
#                 data = sca.df,
#                 control = ctrl)
# drop1(sca.lme1) # keep Shrubs, AIF, AIG, ANF
# droptest(mod = sca.lme1, coefs = coefs1)
# droptest(mod = sca.lme1, coefs = coefs2)
# vif(sca.lme1)
# summary(sca.lme1)



# Validation
sca.lmef <- lme(log(SOIL_Ca_kg_ha) ~  (Crypto + ANF + AIF + Shrubs) * ppt, 
                random = ~1 + Year|Site_number, 
                method = "REML", 
                data = sca.df,
                control = ctrl)

# plot_res_vs_all(df= sca.df, mod= sca.lmef)
# summary(sca.lmef)

model_list$soil_sca <- sca.lmef


# Soil Mg ----------------------------------------------------------------------

sm.df <- all[!is.na(all$SOIL_Mg_kg_ha),] # getting rid of NA rows
# densityplot(((sm.df$SOIL_Mg_kg_ha)))
# 
# f <- formula(SOIL_Mg_kg_ha ~ (Rock + Crypto + PNG + PNF + ANF + AIG + AIF +
#                                 Litter + Shrubs+Bromus_TN_pct+Bromus_TC_pct+
#                                 Other_TC_pct+Other_CN+
#                                 Poa_TC_pct+Poa_CN+Litter_TN_pct+Litter_TC_pct)* ppt)
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
# Other TC & CN eventually dropped
sm.df <- sm.df[!is.na(all$Poa_TC_pct),]
sm.df <- sm.df[!is.na(sm.df$Bromus_TN_pct),]
sm.df <- sm.df[!is.na(sm.df$Litter_TC_pct),]
# 
# sm.lme1 <- lme(SOIL_Mg_kg_ha ~ Poa_TC_pct+Poa_CN+(PNG + 
#                                   Bromus_TC_pct+
#                                   Litter_TC_pct)* ppt,
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                #weights = ve,
#                data = sm.df)
# drop1(sm.lme1)
# droptest(sm.lme1, coefs1)
# droptest(sm.lme1, coefs2)
# summary(sm.lme1)
# vif(sm.lme1)

#validation

sm.lmef <- lme(SOIL_Mg_kg_ha ~ Poa_TC_pct + Poa_CN +
                 (PNG + Bromus_TC_pct + Litter_TC_pct) * ppt, 
               random = ~1 + Year|Site_number,
               method = "REML",
               control = ctrl,
               #weights = ve,
               data = sm.df)

# plot_res_vs_all(sm.df, sm.lmef)
# summary(sm.lmef)
# qqnorm(sm.lmef)
# plot(sm.lmef)
model_list$soil_smg <- sm.lmef


# Soil OM ----------------------------------------------------------------------
# sc.df <- all[!is.na(all$SOIL_OM_pct),] # getting rid of NA rows
# densityplot((log(all$SOIL_OM_pct)))
# 
# f <- formula(SOIL_OM_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG + AIF +
#                               Litter + Shrubs+Bromus_TN_pct+Bromus_TC_pct+
#                               Other_TC_pct+Other_CN+
#                               Poa_TC_pct+Poa_CN+Litter_TN_pct+Litter_TC_pct)* ppt)
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
# # poa, bromus, litter nutrients dropped

sc.df <- all[!is.na(all$Other_TC_pct),]
# 
# sc.lme1 <- lme(log(SOIL_OM_pct) ~ PNG + (Crypto + AIF + Other_TC_pct + 
#                                            Other_CN) * ppt,
#                random = ~1 + Year|Site_number,
#                method = "ML",
#                control = ctrl,
#                data = sc.df)
# drop1(sc.lme1)
# droptest(sc.lme1, coefs1)
# droptest(sc.lme1, coefs2)
# vif(sc.lme1)
# summary(sc.lme1)


#validation
sc.lmef <- lme(SOIL_OM_pct ~ PNG + (Crypto + AIF + Other_TC_pct + 
                                      Other_CN) * ppt, 
               random = ~1 + Year|Site_number,
               method = "REML",
               control = ctrl,
               data = sc.df)

# plot_res_vs_all(sc.df, sc.lmef)
# summary(sc.lmef)
# qqnorm(sc.lmef)
# plot(sc.lmef)
model_list$soil_tc <- sc.lmef


# Soil TN ----------------------------------------------------------------------
sn.df <- all[!is.na(all$SOIL_TN_pct),] # getting rid of NA rows
# densityplot(((sn.df$SOIL_TN_pct)))
# 
# f <- formula(SOIL_TN_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG + AIF +
#                               Litter + Shrubs+Bromus_TN_pct+Bromus_TC_pct+
#                               Other_TC_pct+Other_CN+
#                               Poa_TC_pct+Poa_CN+Litter_TN_pct+Litter_TC_pct)* ppt)
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
# sn.lme1 <- lme(SOIL_TN_pct ~  AIG + PNF * ppt,
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
sn.lmef <- lme(SOIL_TN_pct ~AIG + PNF * ppt, 
               
               random = ~1 + Year|Site_number,
               method = "REML",
               #weights = vp,
               control = ctrl,
               data = sn.df)

# plot_res_vs_all(sn.df, sn.lmef)
# summary(sn.lmef)
# plot(sn.lmef)
# qqnorm(sn.lmef)

model_list$soil_stn <- sn.lmef


# Soil CN ----------------------------------------------------------------------
# 
scn.df <- all[!is.na(all$SOIL_CN),] # getting rid of NA rows
# densityplot((log(scn.df$SOIL_CN)))  #possible outlier
scn.df <- scn.df[scn.df$SOIL_CN < 25,]

f <- formula(SOIL_CN ~ (Rock + Crypto + PNG + PNF + ANF + AIG + AIF +
                          Litter + Shrubs+Bromus_TN_pct+Bromus_TC_pct+
                          Other_TC_pct+Other_CN+
                          Poa_TC_pct+Poa_CN+Litter_TN_pct+Litter_TC_pct)* ppt)
# # 
# # scn.lm <- lm(f, data = scn.df)
# # op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
# # plot(scn.lm)
# # par(op)
# # 
# # best_random(scn.df) #ri
# scn.lme <- lme(f, random = ~1+ Year|Site_number,
#               method = "REML",
#               control = ctrl,
#               data = scn.df)
# # plot_res_vs_all(scn.df, scn.lme)
# # v_struct(v = formula(~ppt), df = scn.df, r= formula(~1 + Year|Site_number)) #vp
# # vp <- varPower(form =~ppt)
# # finding the optimum fixed structure

scn.df <- scn.df[!is.na(scn.df$Litter_TN_pct),]
scn.df <- scn.df[!is.na(scn.df$Poa_TN_pct),]

# scn.lme1 <- lme(log(SOIL_CN) ~  Poa_TC_pct+  (PNG + PNF + AIF +
#                                    
#                                    Litter_TC_pct)* ppt,
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
scn.lmef <- lme(log(SOIL_CN) ~ Poa_TC_pct +  
                  (PNG + PNF + AIF +Litter_TC_pct)* ppt, 
                random = ~1 + Year|Site_number,
                method = "REML",
                control = ctrl,
                na.action = na.omit,
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
  coef_plots[[i]] <- viz_mod(model_list[[i]], resp_names[i], path, names(model_list)[[i]])
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

# multiplot from Rmisc
ggsave(file = "all_lmes.png", multiplot(plotlist = coef_plots, cols = 6),
       width = 25, height = 12.5,
       limitsize = FALSE)

ggsave(file = "1_bromus_lmes.png", 
       multiplot(plotlist = coef_plots[1:3], cols = 3),
       width = 12,
       height = 4)
ggsave(file = "2_other_lmes.png", 
       multiplot(plotlist = coef_plots[4:6], cols = 3),
       width = 12,
       height = 4)
ggsave(file = "3_poa_lmes.png", 
       multiplot(plotlist = coef_plots[7:9], cols = 3),
       width = 12,
       height = 4)
ggsave(file = "4_litter_lmes.png", 
       multiplot(plotlist = coef_plots[10:12], cols = 3),
       width = 12,
       height = 4)
ggsave(file = "5_soil_seconday_lmes.png", 
       multiplot(plotlist = coef_plots[13:15], cols = 3),
       width = 12,
       height = 4)
ggsave(file = "6_soil_c_n_lmes.png", 
       multiplot(plotlist = coef_plots[16:18], cols = 3),
       width = 12,
       height = 4)
