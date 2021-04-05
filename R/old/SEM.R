source("R/lmms_with_s_p_nutrients")
## SEM -------------------------------------------------------------------------
library(piecewiseSEM)
library(semPlot)
sem.fit(modelList = model_list[1:9], data = na.omit(all), .progressBar = TRUE)
sem.model.fits(model_list)
sem.plot(model_list[10:18], data=na.omit(all), standardize = "scale")
sem.plot(model_list, data=all)
# in conclusion this doesn't work
# 
# DAG
# 
# ppt -> (cover <-> soil nutrient availability) -> plant nutrient content
# did it on draw.io
# 
# another idea - separate sems for N and C

n_list <- model_list[c(1,4,7,10,17)]
test = sem.fit(modelList = n_list, data = na.omit(all), .progressBar = TRUE)
sem.plot(n_list, data = all, show.nonsig = FALSE, scaling = 12)
semPaths(test)

c_list <- model_list[c(2,5,8,11,16)]
test = sem.fit(modelList = c_list, data = na.omit(all), .progressBar = TRUE)
sem.plot(c_list, data=all, show.nonsig = FALSE, scaling = NA)
semPlot::semPaths()


c_coef_t <- sem.coefs(c_list, all)
n_coef_t <- sem.coefs(n_list, all)

sem.plot()

semPaths(model_list[1]+model_list[2]+model_list[7])
