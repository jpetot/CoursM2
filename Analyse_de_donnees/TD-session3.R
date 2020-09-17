library(Factoshiny)
library(FactoMineR)

##################
#### EXO 1 
##################

#Q1
data = data.frame(rnorm(7))
data = matrix(rnorm(7 * 10), ncol = 10)

res.acp = PCA(data) 

# variables colineaire, bien représenté, mais difficilement lisible 
# que 5 individus, donc on recupère bcp d'infos avec deux dim 
# un bonne quantité de variables sont qd même bien projeté
# intéressant de comparer nos données avec des données hasardeuse comme ici
re

#Q2

data(decathlon)
don <- decathlon[,1:10]
res.acp = PCA(don)
res.acp$eig

#Q3

f_simul = function(nind, nvar, nbsimul){
  ei = NULL
  for (k in 1:nbsimul){
    data = matrix(rnorm(nind * nvar), ncol = nvar)
    res.pca = PCA(data, graph = FALSE)
    ei = c(ei, res.pca$eig[2,3])
  }
  return(ei)
}


res = f_simul(6,8,200)
mean(res)
quantile(res, 0.95)
sd(res)

#Q4

permuteLigne <- function(v) {return(v[sample(1:length(v),replace=FALSE)])}
Xnew <- apply(don,2,permuteLigne)
res.pca2 = PCA(Xnew, graph = FALSE)
res.pca2$eig

# cette methode ne casse pas la ditribution de chaque variable mais casse les liaisons entre les variables 
# je dirais oui généralisable, meme généramlisable a un scheam d'ACM ###

##################
#### EXO 2
##################

orange <- read.table("https://husson.github.io/img/orange_chimie_senso.csv",
                     header=TRUE,sep=";",row.names=1)
dim(orange )
summary(orange)


acp_chimie = PCA(orange, quanti.sup = 9:15)
quantile(f_simul(6,8,200),0.95)
# meilleur plan pour voir les variables de chimies

acp_desc = PCA(orange, quanti.sup = 1:8)
quantile(f_simul(6,7,200),0.95)
# meilleur plan pour voir les variables sensorielles

#AFM permet d'analyser conjointement ce genre de données.


################
#### EXO 3 
################

comp <- read.table("http://factominer.free.fr/more/gene.csv",sep=";",header=T,row.names=1)

dim(comp)
summary(comp[,1:15])
