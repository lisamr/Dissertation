# doing brms instead of lmer and anova
library(brms)
library(tidyverse)
library(emmeans)
library(tidybayes)
library(multcomp)
library(sjstats)
source("R/a_prep_mjcb.R")
resp <- c("Bromus_TN_pct",    
          "Bromus_TC_pct", "Bromus_CN", "Other_TN_pct", "Other_TC_pct",     
          "Other_CN", "Poa_TN_pct", "Poa_TC_pct", "Poa_CN",    
          "Litter_TN_pct", "Litter_TC_pct", "Litter_CN", "SOIL_SurSo4_kg_ha",
          "SOIL_Ca_kg_ha", "SOIL_Mg_kg_ha" ,
          "SOIL_CN", "soil_n_kg_ha", "soil_c_kg_ha", "total_mineral_n",
          "NO3_kg_ha","NH4_kg_ha")
dat <- all

# beta family needs something between 0 and 1, instead of 100
for(i in 1:ncol(dat)){
  if(names(dat)[i] %>% str_detect("pct")){
    dat[,i] <- dat[,i]/100
  }
}

modfile <- "data/brms/mods.Rda"
redo <- FALSE
if(redo == T){
  for(i in 13:length(resp)){
    # getting rid of NAs in the response variable
    
    gc()
    d <- filter(dat, is.na(dplyr::select(dat, resp[i]))==F)
    f <- formula(paste(resp[i],"~ Year*Site.type + (1|Site_number/Transect)"))
    
    fn<- str_replace(modfile, ".Rda", paste0("_", resp[i], ".Rda"))
    # making the mixed mods
    
    # fsig<-formula(paste("sigma","~ Year*Site.type + (1|Site_number)"))
    
    if(str_detect(resp[i], "pct")){
      mod <- brm(f,data=d, family = Beta())
      print(resp[i])
      save(mod, file=fn)
      
    }else{
      mod <- brm(f,data=d)
      print(resp[i])
      save(mod, file=fn)
      
    }
  }
}

files<- list.files("data/brms", full.names = TRUE)

load(files[15])

summary(mod)
library(broom)
tidy_stan(mod$fit)
plot(mod, "Year")
plot(mod, "Site.type")
launch_shinystan(mod)
emmeans(mod, ~ Year)

emmeans(mod, ~ Year|Site.type) %>% cld(alpha   = 0.05,
                                             Letters = letters,
                                             adjust  = "sidak",
                                             decreasing = T)

emmeans(mod, ~ Site.type|Year) %>% cld(alpha   = .05,
                                        Letters = letters,
                                        adjust  = "sidak",
                                        decreasing = T)
plot(conditional_effects(mod, effects = "Site.type:Year"))
plot(conditional_effects(mod, effects = "Year:Site.type"))

ce<-conditional_effects(mod, effects = "Year:Site.type")
performance::r2(mod)

emmeans(mod, ~Year|Site.type)
emmeans(mod, ~Site.type|Year)
emmeans(mod, ~Site.type:Year)

# Main effect of SITUATION

# Main effect of Month
as.matrix(mod)%>% str#[,which(wch==2)]


# Interaction
mcmcpvalue(as.matrix(starling.brms)[,which(wch==3)])



mod %>%
  emmeans( ~ Year | Site.type) %>%
  gather_emmeans_draws() %>%
  ggplot(aes(x = Site.type, y = .value, fill=Site.type)) +
  geom_boxplot() +
  stat_summary(aes(group = Year), fun = mean, geom = "line") +
  facet_grid(~ Year) +
  theme_light()

names(lut_variables) <- str_replace_all(names(lut_variables), "_","")
lut_inv<- c("Intact Sagebrush" = "I",
            "Invaded Sagebrush" = "II",
            "Cheatgrass-dominated" = "III",
            "Cheatgrass Dieoff" = "IV")

p1<-mod %>%
  emmeans( ~ Site.type | Year) %>%
  gather_emmeans_draws() %>%
  mutate(Site.type = lut_inv[Site.type]) %>%
  ggplot(aes(x = Site.type, y = .value)) +
  ylab(lut_variables[mod$formula$resp])+
  xlab("Invasion Stage") +
  geom_jitter(alpha=0.05, aes(color=Site.type)) +
  stat_summary(fun = "quantile", fun.args = list(probs=(c(.975, .025))), 
               geom="line")+
  stat_summary(fun = "quantile", fun.args = list(probs=(c(.975, .025))),
               geom="crossbar", width=.3)+
  stat_summary(fun = "mean")+
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  scale_color_manual(values = site_colors)+
  # geom_boxplot(fill=NA, color="black", outlier.shape = NA)+
  # stat_summary(aes(group = Year), fun = mean, geom = "line") +
  facet_grid(~ Year) +
  scale_y_continuous(labels=scales::label_number_si())+
  ggthemes::theme_clean()+
  theme(legend.position = "none",
        panel.border = element_rect(fill=NA, size=0.75))

p1d <-mod %>%
  emmeans( ~ Site.type | Year) %>%
  gather_emmeans_draws() %>%
  mutate(Site.type = lut_inv[Site.type]) %>%
  ggplot(aes(x = Site.type, y = .value)) +
  ylab(lut_variables[mod$formula$resp])+
  xlab("Invasion Stage") +
  geom_jitter(data = all %>%
                mutate(Site.type = lut_inv[Site.type]), alpha=1, aes(y = soil_c_kg_ha, color=Site.type)) +
  stat_summary(fun = "quantile", fun.args = list(probs=(c(.975, .025))), 
               geom="line", aes(color = Site.type))+
  stat_summary(fun = "quantile", fun.args = list(probs=(c(.975, .025))),
               geom="crossbar", width=.3, aes(color = Site.type))+
  stat_summary(fun = "mean", aes(color = Site.type))+
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  scale_color_manual(values = site_colors)+
  # geom_boxplot(fill=NA, color="black", outlier.shape = NA)+
  # stat_summary(aes(group = Year), fun = mean, geom = "line") +
  facet_grid(~ Year) +
  scale_y_continuous(labels=scales::label_number_si())+
  ggthemes::theme_clean()+
  theme(legend.position = "none",
        panel.border = element_rect(fill=NA, size=0.75))

p2<-mod %>%
  emmeans( ~ Year | Site.type) %>%
  gather_emmeans_draws() %>%
  mutate(Site.type = str_c(lut_inv[Site.type], ". ", Site.type)) %>%
  ggplot(aes(x = Year, y = .value)) +
  ylab(lut_variables[mod$formula$resp])+
  geom_jitter(alpha=0.05, aes(color=Site.type)) +
  stat_summary(fun = "quantile", fun.args = list(probs=(c(.975, .025))), 
               geom="line")+
  stat_summary(fun = "quantile", fun.args = list(probs=(c(.975, .025))),
               geom="crossbar", width=.3)+
  stat_summary(fun = "mean")+
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  # geom_boxplot(fill=NA, color="black", outlier.shape = NA)+
  # stat_summary(aes(group = Year), fun = mean, geom = "line") +
    facet_grid(~ Site.type) +
  scale_color_manual(values = site_colors)+
  
  scale_y_continuous(labels=scales::label_number_si())+
  ggthemes::theme_clean()+
  theme(legend.position = "none",
        panel.border = element_rect(fill=NA, size=0.75))


c("Site.typeInvadedSagebrush = Site.typeCheatgrassDieoff",
  "Site.typeInvadedSagebrush = Site.typeCheatgrassMdominated",
  "Site.typeInvadedSagebrush = Intercept",
  "Site.typeCheatgrassDieoff = Intercept",
  "Site.typeCheatgrassMdominated = Intercept") %>%
hypothesis(mod, .)
c("Site.typeInvadedSagebrush = Site.typeCheatgrassMdominated")%>%
  hypothesis(fit1, .) 
c("Site.typeInvadedSagebrush = Intercept")%>%
  hypothesis(fit1, .)

bayestestR::hdi(mod)
bayestestR::hdi(mod, effects="random")

eqt<-equivalence_test(mod)
plot(eqt)
parameters::model_parameters(mod)
?rope
bayes_factor(fit1)

bayestestR::area_under_curve(mod)
