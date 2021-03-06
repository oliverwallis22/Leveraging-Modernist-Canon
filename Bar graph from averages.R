uwcomp<-read.csv("uwachebecomparisonaverages.csv")
uwcomp<-na.omit(uwcomp)
uwcomp2<-data.frame(cbind("Text"=uwcomp$Year, "Reality"=uwcomp$X20, "Modernity"=uwcomp$X169, "Modernist Aesthetics"=uwcomp$X53, "Modernist Aesthetics"=uwcomp$X93, "Gender"=uwcomp$X31, "Victorian Novels"=uwcomp$X39, "Theory"=uwcomp$X110))
uwcomp2$Text<-c("Woolf", "Achebe", "Joyce")
library(ggplot2)
library(reshape2)
uwcompmelt<-melt(uwcomp2, id.vars="Text")
ggplot(data=uwcompmelt, aes(x=variable, y=value, fill=Text)) + geom_bar(position="dodge", stat="identity")