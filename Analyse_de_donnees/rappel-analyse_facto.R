install.packages("ggplot2")
install.packages("FactoMineR")

library(ggplot2)
library(FactoMineR)



data(iris)
x = as.matrix(iris[1:10,1:4])

x = scale(x,scale = FALSE)
cov = t(x) %*% x     # matrice des covariances
ps = x %*% t(x)     # matrice des produits scalaire 

a = eigen(cov)
eigen(ps)
#same valeurs propre

s = svd(x)
# $d = square( a$val)
# $u = a$vectors
# $v = 


xchap = s$u[,1:2] %*% diag(s$d[1:2])%*% t(s$v[,1:2])
j=2
xchap = s$u[,1:j] %*% diag(s$d[1:j])%*% t(s$v[,1:j])
xchap
sum((xchap - x)^2)
j=3
xchap = s$u[,1:j] %*% diag(s$d[1:j])%*% t(s$v[,1:j])
xchap
sum((xchap - x)^2)
j=4
xchap = s$u[,1:j] %*% diag(s$d[1:j])%*% t(s$v[,1:j])
xchap
sum((xchap - x)^2)

x %*% s$v
pca = PCA(x, scale.unit = FALSE)
pca$ind$coord

# donc l'acp c'est juste une SVD 
# les autre methode factoreil c'est juste des SVD avec des coefficients

fichier <- "https://husson.github.io/MOOC_AnaDo/AnaDo_JeuDonnees_Nobel_avecMaths.csv"
Nobel <- read.table(fichier, header=TRUE, sep=";", row.names=1, check.names=FALSE)
Nobel <- Nobel[1:8,]

pca = PCA(Nobel)
acf = CA(Nobel)


library(FactoMineR)
data(tea)
don <- tea[, c(14,18)]
TabCont <- table(don)
TabCont
don
MCA(don)
CA(TabCont)
