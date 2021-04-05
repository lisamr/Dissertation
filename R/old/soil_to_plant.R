# thanks: https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/
source("R/functions.R")
source("R/a_prep_mjcb.R")
library(purrr)
library(MuMIn)
library(lme4)

all_soil <- names(all_p[,12:20]) %>% set_names()
all_plant <- names(all_p[,3:14]) %>% set_names()
clm <- names(all_p[,31:36]) %>% set_names()
cover <- names(select(ungroup(all_p), AIG, AIF, PNG))
c <- set_names("Site.type")
s <- set_names("Year")

scatter_fun <- function(x, y) {
  #f <- formula(paste(y, "~",x, "+ (",x,"|| Site.type)"))
  #m <- lmer(f, data = all_p)
  ggplot(all_p, aes_string(x = x, y = y) ) +
    geom_point(aes(color=Site.type, shape=Year)) +
    #geom_smooth(method = "loess", se = FALSE, aes(color=Site.type), alpha = 0.75) +
    #geom_line(aes(y=predict(m), color = Site.type))+
    #+
    #geom_text(aes(x=0,y=Inf,label = round(r.squaredGLMM(m)[1],2),
                  #hjust = .1, vjust=.9))
    theme_pubr()
}



all_plots = map(all_plant, 
                ~map(all_soil, scatter_fun, y = .x) )

clm_plots =  map(all_soil,
                 ~map(clm, scatter_fun, y = .x))

cover_plots = map(all_soil,
                  ~map(cover, scatter_fun, y = .x))
cover_plots = map(all_plant,
                  ~map(cover, scatter_fun, y = .x))

response_plots = map(all_plots, ~ggarrange(plotlist = .x, common.legend = TRUE))
response_plots[[1]]

clm_agg <-  map(clm_plots, ~ggarrange(plotlist = .x, common.legend = TRUE))
cov_agg <-  map(cover_plots, ~ggarrange(plotlist = .x, common.legend = TRUE))

