library(ggplot2)
library(reshape2)
ucomp<-read.csv("utexts_inferred_modernism100_composition_byyear.csv")
umod<-data.frame(cbind("Year"=ucomp$Year, "modjoyce"=ucomp$X33, "modaes"=ucomp$X38, "modpost"=ucomp$X62, "modcult"=ucomp$X18, "modwoolf"=ucomp$X76))
umodmelt<-melt(umod, id.vars="Year")
colnames(umodmelt)[2]<-"Topic"
colnames(umodmelt)[3]<-"Value"
ggplot(umodmelt, aes(x=Year, y=Value, color=Topic)) + geom_smooth(method=lm)
ggplot(umodmelt, aes(x=Year, y=Value, color=Topic)) + geom_smooth(method=loess)