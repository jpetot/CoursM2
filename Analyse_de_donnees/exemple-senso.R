setwd("~/M2/Analyse de donnees")

library(FactoMineR)
library(Factoshiny)

text = read.csv("litterature.csv", header = TRUE, sep =";", row.names = 1, check.names = FALSE, quote = "\"")
head(text)
summary(text)
class(text)
#View(text)
str(text)

afc = Factoshiny(text)

res.CA<-CA(text,quali.sup=c(2,3),graph=FALSE)
plot.CA(res.CA,cex=0.85,cex.main=0.85,cex.axis=0.85,title="Graphe de l'AFC",col.quali.sup='#FF00FF',invisible=c('col'))

HCPC(res.CA)

