#ACP

if(!require("PCAmixdata")) install.packages("PCAmixdata")

split = splitmix(station)
station_quali = split$X.quali
station_quanti = split$X.quanti

# acp quantitative station --------------------------------------------------

acp_quanti = PCAmix(X.quanti=station_quanti, ndim=5)
summary(acp_quanti)
acp_quanti$eig

plot(acp_quanti, choice="cor", main="numerical variables")
plot(acp_quanti, choice="ind", label="false", main="numerical variables")


# acp mix station -----------------------------------------------------------

acp_quali = PCAmix(X.quanti = station_quanti, X.quali = station_quali, ndim=5)
summary(acp_quali)
acp_quali$eig

plot(acp_quali, choice="cor")


#------------------------------------------------------------
#Question 1 :
#Expliquer pourquoi le modele a trois variables ne peut etre retenu. 
#Quelle variable faut-il eliminer de ce modele pour obtenir un modele 
#a deux variables qui semble mieux convenir ? Justifier votre choix.

modele3V<-lm(ventes~nbpompes+nbconc+trafic, data = station)
summary(modele3V)
#p-value<<0.05 -> modèle utile
#Adjusted R-squared = 0.9797 -> 97,97% de la variance expliquée
#Coefficients de nbpompes et nbconc ne sont pas significatifs

par(mfrow=c(1,2))
plot(modele3V$fitted,modele3V$residuals)
abline(h=0,col=2)
#Structure dans les résidus -> modèle insatisfaisant

mat<-data.frame(station$ventes,station$nbpompes,station$nbconc,station$trafic)
plot(mat)

#Il semble y avoir une relation linéaire entre :
#ventes et nbpompes
#nbconc et nbpompes
#ventes et nbconc
#Il semble que trafic soit la variable à retirer 
#à vérifier avec drop

drop1(modele3V)
#Retirer nbconc donne l'AIC le moins élevé
#La variable à retirer est nbconc

#------------------------------------------------------------
#Question 2.a : 
#Faire une analyse des indices de qualite du modele a deux variables obtenu a
#la question precedente et en tirer une conclusion quant a sa validite statistique.

modele2V<-lm(ventes~nbpompes+trafic, data = station)
summary(modele2V)
#p-value<<0.05 -> modèle utile
#Adjusted R-squared = 0.9802 -> 98,02% de la variance expliquée
#Coefficients de nbpompes et trafic ont une p-value<<0.05 -> 2 variables utiles pour le modèle
#Ce modèle semble satisfaisant
#à vérifier avec les résidus

par(mfrow=c(1,2))
plot(modele2V$fitted,modele2V$residuals)
abline(h=0,col=2)
#Structure dans les résidus -> modèle insatisfaisant


#------------------------------------------------------------
#SUPPRESSION PREMIER POINT

station2<-station[-1,]
modele2VC<-lm(ventes~nbpompes+trafic, data = station2)
summary(modele2VC)
#p-value<<0.05 -> modèle utile
#Adjusted R-squared = 0.9987 -> 99,87% de la variance expliquée
#Coefficients de nbpompes et trafic ont une p-value<<0.05 -> 2 variables utiles pour le modèle

par(mfrow=c(1,2))
plot(modele2VC$fitted,modele2VC$residuals)
abline(h=0,col=2)
#Pas de structure dans les résidus -> modèle satisfaisant


#------------------------------------------------------------
#Question 2.b : 
#Faire une interpretation des cœfficients de ce modele. Que pensez-vous de la validite
#economique du modele ? Permet-il de mieux comprendre la realite ?


#Interprétons les coefficients du modele2VC (modèle à 2 variables corrigé -suppression
#de la première donnée-).
#On obtient : ventes = 191.98974 + 2.77766*nbpompes + 1.09956*trafic + 0.34



#------------------------------------------------------------
#Question 2.C : 
#Preciser ce que l’on predirait comme nombre de ventes etant donne un nouveau couple 
#de variables explicatives.

nbpompesP<-seq(from=1,to=30,by=1)
traficP<-seq(from=1,to=30,by=1)

Vmoy<-predict(modele2VC,data.frame(nbpompes=nbpompesP, trafic=traficP),interval = "conf", level = 0.95)
round (cbind(nbpompesP,traficP,Vmoy),digit=1)

Vpred<-predict(modele2VC,data.frame(nbpompes=nbpompesP, trafic=traficP),interval = "pred", level = 0.95)
round (cbind(nbpompesP,traficP,Vpred),digit=1)

#----------------------------------------------
#Question 3 :
#Justifier pourquoi le modele a deux variables defini a la premiere question et  
#etudie a la deuxieme est-il preferable aux modeles a une seule variable ?


#On retient les modèles qui ont le R-squared le plus élevé.
#Après comparaison, on observe que ce sont les modèles à 2 variables
#par rapport aux modèles à 1 variable. 

#PREUVE : 

#modele2V étudié précedemment : Adjusted R-squared = 0.9811

modele2V1<-lm(ventes~trafic+nbconc, data = station)
summary(modele2V1)
#Adjusted R-squared = 0.9786

modele2V2<-lm(ventes~nbpompes+nbconc, data = station)
summary(modele2V2)
#Adjusted R-squared = 0.9499

modele1V1<-lm(ventes~nbpompes, data = station)
summary(modele1V1)
#Adjusted R-squared = 0.869

modele1V2<-lm(ventes~nbconc, data = station)
summary(modele1V2)
#Adjusted R-squared = 0.8039

modele1V3<-lm(ventes~trafic, data = station)
summary(modele1V3)
#Adjusted R-squared = 0.2533

