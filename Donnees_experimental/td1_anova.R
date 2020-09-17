setwd("~/M2/Donnees_experimental")

fich_eau <- read.delim("~/M2/Donnees_experimental/fich_eau.txt")

install.packages("multcomp")
install.packages("emmeans")
install.packages("car")
install.packages("phia")

require(multcomp)
require(emmeans)
require(car)
require(phia)


summary(fich_eau)
dim(fich_eau)

fich_eau$produit = as.factor(fich_eau$produit)
fich_eau$juge = as.factor(fich_eau$juge)

plot(fich_eau$produit, fich_eau$intensite.de.crepitement)
plot(fich_eau$produit, fich_eau$saveur.sucree)
plot(fich_eau$produit, fich_eau$intensite.gustative.globale)

# peu être un effet de intensite de crepitement sinon 
# aucun effet très clair, peu être tous confondus

pairwise.t.test(fich_eau$intensite.gustative.globale,fich_eau$produit,p.adjust="none")
pairwise.t.test(fich_eau$intensite.gustative.globale,fich_eau$produit,p.adjust="bonferroni")

modele.1way <- lm(intensite.gustative.globale ~ produit, data=fich_eau)
anova(modele.1way)

tuk <- glht(modele.1way,linfct=mcp(produit="Tukey"))
summary(tuk)
tuk.cld <- cld(tuk)
plot(tuk.cld)


mod.IGG <- lm(intensite.gustative.globale ~ produit, fich_eau)
emmeans(mod.IGG, pairwise ~ produit)

# q4 
mod.2eff<- lm(intensite.gustative.globale ~ produit + juge, fich_eau)
res.aov = anova(mod.2eff)


# q5
options(contrasts = c("contr.sum", "contr.sum"))
res3 = Anova(mod.2eff,type="III")
res1 = anova(mod.2eff)
 
# Q6
sum(res3$`Sum Sq`)
sum(res1$`Sum Sq`)

#q7
summary(mod.2eff)

#q8
mod.inter <- lm(intensite.gustative.globale ~ produit * juge, fich_eau)
interactionMeans(mod.inter) ## Toutes les moyennes d'interaction
interactionMeans(mod.inter,factors="produit")## Uniquement les moyennes marginales pour le facteur produit

testInteractions(mod.inter,fixed="juge",across="produit") ## On teste l'effet produit sachant le juge
testInteractions(mod.inter)
testInteractions(mod.inter,pairwise="juge",across="produit")



