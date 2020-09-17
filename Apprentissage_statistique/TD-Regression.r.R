setwd("C:/Users/ocean/Desktop/Apprentissage de données biologiques/Session 1")
require(nnet)

sca = read.table("scanner.txt")
str(sca)  # Overview of data
dim(sca)

mod = glm(LMP~.,data=sca)
summary(mod)

RSS = sum(residuals(mod)^2) 

mod0 = glm(LMP~1,data=sca) # LS fit of the null model
RSS0 = sum(residuals(mod0)^2)  # Residuals sum-of-squares of null model
R2=(RSS0-RSS)/RSS0 # R2
R2

#prediction
out=sample(1:117,17) #selection au hasard de 1è indivs parmi les 117
train=sca[-out,] #jeu d'apprentissage
test=sca[out,] #jeu test
mod2=glm(LMP~.,data=train) #construction du modele a partir du jeu d'apprentissage
pred=predict(mod2,newdata=test)#prediction à partir du jeu test grace au modèle construit
summary(mod2)

plot(test$LMP,pred) #visualisation graphique
cor(test$LMP,pred)^2 #coefficient de correlation

#cf question 7 calcul des correlations
corxy=cor(sca$LMP,sca[,-138])
plot(1:137,corxy)
which.max(corxy)
colnames(corxy)[85]


mod3=glm(LMP~X92,data=train) #construction du modele a partir du jeu d'apprentissage
pred2=predict(mod3,newdata=test)#prediction à partir du jeu test grace au modèle construit
summary(mod3)
plot(test$LMP,pred2) #visualisation graphique
cor(test$LMP,pred2)^2 #coefficient de correlation

# session 2

setwd("~/M2/Apprentissage statistique")

require(nnet)            # Multinomial logistic regression
require(leaps)           # For regsubsets
require(RcmdrMisc)       # For Stepwise
require(pls)             # For segments
require(groupdata2)      # For fold
require(boot)            # For cv.glm

scanner = read.table("scanner.txt")
str(scanner)  # Overview of data
dim(scanner)

mod=glm(LMP~.,data=scanner)
 
select2=stepwise(mod,direction=c("forward/backward"),criterion="BIC", trace = 0) 

cv.mod=cv.glm(select,data=scanner,K=10)$delta[1]
#contient MSEP ET PRESS
#on prend la premiere valeur (la + grande) car c'est la + fausse

aic=rep(0,100)
press=rep(0,100)
for (i in 1:100){
  select=stepwise(mod,direction=c("forward/backward"),criterion="AIC",trace=0)
  cvselect=cv.glm(select,data=scanner,K=10)
  press[i]=117*cvselect$delta[1]
  aic[i]=extractAIC(select)[2]
  print(i)
}

plot( 1:100,aic )


select = regsubsets(scanner[,-138], scanner[,138], nvmax = 100, method="forward")
select = summary(select)
aic=rep(0,100)
press=rep(0,100)
for (i in 1:100){
  mod = glm(LMP~., data = scanner[,c(select$which[i,-1],TRUE)])
  press[i]=117*cv.glm(mod, data = scanner[,c(select$which[i,-1],TRUE)], K = 10)$delta[1]
  aic[i]=extractAIC(mod)[2]
  print(i)
}

plot(1:100, aic)
plot(1:100, press)

# moral : la liste de variables selectionné peu etre instable selon le jeu de donnees. 
# exemple : 

top10 = matrix(TRUE,nrow = 100, ncol=137)
for (i in 1:100){
  out = sample(1:117,10)
  select = regsubsets(scanner[-out,-138], scanner[-out,138],nvmax = 10, method= "forward")
  select = summary(select)
  top10[i,] = select$which[10,-1]
}

freqselect = colMeans(top10)
names(freqselect) = colnames(scanner[,-138])
barplot(sort(freqselect), las=3, cex.names = 0.5)

#la minimisation du critere akaike (aic ou bic) ne conduis pas forcement a la meilleur prédiction 
# la selection des variables n'est pas forcement reproductible.
# donc important d'avoir ce genre de graph pour montrer la stabilité et la non stabilité de nos variables selectionner
# c'est une demarche de critique du résultat


