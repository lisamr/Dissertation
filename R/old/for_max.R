library(nlme) # lmes
library(plyr)

#### data prep ####
# cleaned and matched data frames
glmm13 <- read.csv("Jones_2013_transect_dec17.csv")
glmm16 <- read.csv("jones_all_nov_2017.csv")
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
all$ppt <- as.numeric(scale(all$ppt, center = FALSE)) # so lmer doesn't get mad

# making Shrubs use the same data from 2013 (that was line
# intercept, I just included artr in my quadrats)

glmm16$Shrubs <- glmm13$Shrubs

all = rbind(glmm13, glmm16)
all$Year = as.factor(all$Year)
all$Site_number = as.factor(all$Site_number)

#### function ####
best_random <- function(f, df){
  # this tests lm vs lme with random slope vs lme with random slope and intercept
  # f is a formula using the formula() function, 
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

#### here we go ####


btn.df <- all[!is.na(all$Bromus_TN_pct),] # getting rid of NA rows
densityplot(btn.df$Bromus_TN_pct)

f0 <- formula(Bromus_TN_pct ~ (Rock + Crypto + PNG + PNF + ANF + AIG +  AIF + 
                                 Litter + Shrubs)* ppt)

best_random(f0, btn.df)