if(!require("PCAmixdata")) {
  install.packages("PCAmixdata")
  library("PCAmixdata")
}

if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}

oz = read.table(file="./data/ozone.txt")

summary(oz)
plot(oz)

split = splitmix(oz)
oz_quali = split$X.quali
oz_quanti = split$X.quanti


# acp quantitative ozone --------------------------------------------------

acp_quanti = PCAmix(X.quanti=oz_quanti, ndim=5)
summary(acp_quanti)
acp_quanti$eig

plot(acp_quanti, choice="cor", main="numerical variables")
plot(acp_quanti, choice="ind", label="false", main="numerical variables")


# impact vent et pluie sur maxO3 ---------------------------------------------------

plot(oz$vent, oz$maxO3)
plot(oz$pluie, oz$maxO3)

# acp mix ozone -----------------------------------------------------------

acp_quali = PCAmix(X.quanti = oz_quanti, X.quali = oz_quali, ndim=5)
summary(acp_quali)
eig.mix = data.frame(acp_quali$eig)

par(mfrow=c(1,1))
plot(acp_quali, choice="cor")
ggplot(eig.mix,aes(x=1:15, y=eig.mix[,2])) + 
  geom_col() + 
  ylab("Proportion of variance") +
  xlab("Dimensions")

# modèle linéaire initial ozone ---------------------------------------------------
model_initial = lm(formula=maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v, data=oz)
summary(model_initial)

residus_initiaux = model_initial$residuals
residus_initiaux_stud = rstudent(model_initial)
hist(residus_initiaux) #on a un résidu extrême en bas
plot(model_initial$fitted, residus_initiaux) #residus en fonction des données fitted
plot(model_initial$fitted, residus_initiaux_stud) #résidus studentisés


# épuration des données à cause de valeurs extrêmes -----------------------

cond = residus_initiaux_stud > -2 & residus_initiaux_stud < 2
new_oz = oz[cond,] #nouveau df sans les valeurs extrêmes

# sélection d'un meilleur modèle ------------------------------------------
model_initial = lm(formula=maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v, data=new_oz)

new_model = lm(maxO3~T15+Ne12, data=new_oz)
plot(new_model$fitted, rstudent(new_model))

step_model = step(new_model, ~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v)

model_final = lm(step_model$call, data = new_oz)
hist(model_final$residuals)
plot(model_final$fitted.values, model_final$residuals)
