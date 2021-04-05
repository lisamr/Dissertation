withinRunStats = function(x) c(mean = mean(x), var = var(x), n = length(x))


vars <- all[,c(1, 4:32)]


z =  seq_along(4:30) +1
years = c("2013", "2016")

table = data.frame(name = NA, within_group_var = NA, between_group_var = NA, between_bigger_than_within = NA, year=NA, pct = NA)

counter = 1
for(y in years){
  for(i in 1:length(z)){
    fit = lm(vars[vars$Year == y, z[i]] ~ vars[vars$Year == y,]$Site_number)
    table[counter, 1] <- names(vars)[z[i]]
    table[counter, 2] <- anova(fit)["Residuals", "Mean Sq"]
    table[counter, 3] <- anova(fit)["vars[vars$Year == y, ]$Site_number", "Mean Sq"]
    table[counter, 4] <- table[counter, 2] < table[counter, 3]
    table[counter, 5] <- y
    table[counter, 6] <- round(table[counter, 2] / table[counter, 3], 2)*100
    counter = counter +1
}}
table

sorted_pct<-table[order(table$pct),]
sorted_within<-table[order(table$within_group_var),]


# exploring missing data -------------------------------------------------------
# plotted everything, no systematic trend towards low or high plant tissue N or C
# with decreasing cover of those plants

