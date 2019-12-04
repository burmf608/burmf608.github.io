library(wid)
library(ggplot2)

# Get the data
d <- download_wid(indicators = c("sptinc", "scainc"), areas = c("US", "RU"), perc = c("p90p100"), year = 1926:2018, metadata = TRUE)
d$value<-d$value*100
# Post tax disposable income share
plot(d[d$variable=="scainc992j" && d$country=="RU", "year"], d[d$variable=="scainc992j" && d$country=="RU", "value"], type = "l", xlab = "Years", main = "Top 10% Share Of Total Post-Tax Disposable Income", ylab = "Share in %")
abline(v = c(1971, 1979, 1989, 2007, 2009), lty = 2)
# Pre-tax share of total income
#plot(d[d$variable=="sptinc992j", "year"], d[d$variable=="sptinc992j", "value"], type = "l", main = "Top 10% Share Of Total Pre-Tax Income", ylab = "Share in %", yaxp = c(30, 50, n=4), xlab="")
#grid(ny = 4, col = "lightgray", lty = "solid", lwd = par("lwd"), equilogs = TRUE)
#abline(v = c(1929, 1939, 1945, 1971, 1979, 1989, 2007, 2009), lty = 2)
ggplot(data=d[(d$variable=="sptinc992j") & (d$country=="US"),], aes(x=year, y=value, group=1)) +
  geom_line() + theme_minimal() + theme(axis.title.x=element_blank()) + geom_vline(xintercept = c(1929, 1939, 1945, 1971, 1979, 2007, 2009), linetype="dashed", color="grey") + ylab("Top 10% Share Of Total Pre-Tax Income") + ggtitle("Neoliberal Economic Policy Creates Income Inequality")
#Russia
ggplot(data=d[(d$variable=="sptinc992j") & (d$country=="RU"),], aes(x=year, y=value, group=1)) +
  geom_line() + theme_minimal() + theme(axis.title.x=element_blank(), legend.position="right") + geom_vline(xintercept = c(1991, 2000), linetype="dashed", color="grey") + ylab("Top 10% Share Of Total Pre-Tax Income") + ggtitle("Neoliberal Economic Policy Creates Income Inequality")
#Post-tax disposable income US
ggplot(data=d[(d$variable=="scainc992j") & (d$country=="US"),], aes(x=year, y=value, group=1)) +
  geom_line() + theme_minimal() + theme(axis.title.x=element_blank()) + geom_vline(xintercept = c(1971, 1979, 2007, 2009), linetype="dashed", color="grey") + ylab("Top 10% Share Of Total Pre-Tax Income") + ggtitle("Neoliberal Economic Policy Creates Income Inequality")