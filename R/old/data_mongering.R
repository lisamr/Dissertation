library(doBy)

x <- read.csv("data/data_2013/microsite_life_forms_2013.csv")

y <- summaryBy(. ~ Plot_tran, data = x, keep.names = T)

write.csv(y,"data/data_2013/microsite_life_forms_by_plottran_2013.csv")


shrubs <- read.csv("data/Jones_2013_shrubs_transect.csv")
shrubs <- summaryBy(X..cover.total ~ plot_tran, data=shrubs,keep.names = T)
names(shrubs) <- c("plot_tran", "shrub_cover")
write.csv(shrubs, "data/shrub_cover_2013.csv")
