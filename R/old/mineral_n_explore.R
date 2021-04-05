library(tidyverse)

source("R/a_prep_mjcb.R")
allsoil <- allsoil %>% left_join(read_csv("data/growing_degree_days.csv", col_types = "ffd"))
resp <- names(allsoil)[43:46]
clm <- names(allsoil)[c(4:31,33:40,47)]
res_df <- data.frame(resp = NA, pred = NA, year = NA, R2 = NA, p = NA, est = NA)
counter <- 1

for(r in resp){
  for(c in clm){
      f <- formula(paste0(r, "~", c))
      x <- lme(f, data = allsoil, random = ~1|Year, na.action = na.omit)
      res_df[counter, 1] <- r
      res_df[counter, 2] <- c
      res_df[counter, 3] <- NA
      res_df[counter, 4] <- NA
      res_df[counter, 5] <- round(x$coefficients[2,1], 4)
      res_df[counter, 6] <- round(x$coefficients$fixed[2], 4)
      counter = counter +1
      
    
  }
}

arrange(res_df, p)[1:20,]

summary(lm(net_mineralization~AIG, all16 ))
summary(lm)
ggplot(all16 %>% filter(net_mineralization>0), 
       aes(y = net_mineralization, x = AIG)) +
  geom_point(aes(color = Site.type)) + 
  geom_smooth(method = "lm")


ggplot(allsoil, aes(x=gdds, y=total_mineral_n, color=Year, shape = Site.type)) +
  geom_point() + geom_smooth(method = "lm")
