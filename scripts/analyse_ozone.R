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

plot(acp_quanti, choice="cor", main="ACP Quanti")
plot(acp_quanti, choice="ind", label="false", main="numerical variables")
plot(acp_quali, choice="levels")

# impact vent et pluie sur maxO3 ---------------------------------------------------

plot(oz$vent, oz$maxO3, xlab="Variable Vent", ylab="maxO3")
plot(oz$pluie, oz$maxO3, xlab="Variable Pluie", ylab="maxO3")

# acp mix ozone -----------------------------------------------------------

acp_quali = PCAmix(X.quanti = oz_quanti, X.quali = oz_quali, ndim=5)
summary(acp_quali)
eig.mix = data.frame(acp_quali$eig)

par(mfrow=c(1,1))
plot(acp_quali, choice="cor")
plot(acp_quali, choice="sqload")
plot(acp_quali, choice="sqload", axes=c(3,4))
plot(acp_quali, choice="levels", axes=c(1,2))
plot(acp_quali, choice="levels", axes=c(3,4))
#plot pour critère du coude
ggplot(eig.mix,aes(x=1:15, y=eig.mix[,2])) + 
  geom_col() + 
  ylab("Proportion of variance") +
  xlab("Dimensions")

# modèle linéaire initial ozone ---------------------------------------------------

model_initial = lm(formula=maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v, data=oz)
summary(model_initial)

residus_initiaux_stud = rstudent(model_initial)
hist(residus_initiaux_stud)
plot(model_initial$fitted, residus_initiaux_stud, ylab="résidus studentisés", xlab="valeurs prédites") #résidus studentisés

# sélection d'un meilleur modèle ------------------------------------------

model_initial = lm(formula=maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v, data=oz)
new_model = lm(maxO3~T15, data=oz)
plot(new_model$fitted, rstudent(new_model))

step_model = step(new_model, ~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v)
model_final = lm(step_model$call, data = oz)
summary(model_final)

res_model_final = rstudent(model_final)
hist(res_model_final)
plot(model_final$fitted.values, res_model_final, ylab="résidus studentisés", xlab="valeurs prédites")


# meilleur modèle sans résidus mauvais ------------------------------------

cond = residus_initiaux_stud > -2 & residus_initiaux_stud < 2
new_oz = oz[cond,] #nouveau df sans les valeurs extrêmes

model_initial = lm(formula=maxO3~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v, data=new_oz)
new_model = lm(maxO3~T15+Vx12+Ne9, data=new_oz)
plot(new_model$fitted, rstudent(new_model))

step_model = step(new_model, ~T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+maxO3v)
model_final = lm(step_model$call, data = new_oz)
summary(model_final)

res_model_final = rstudent(model_final)
hist(res_model_final)
plot(model_final$fitted.values, res_model_final)
   

# tests acp sans valeurs extrêmes -----------------------------------------

test_split = splitmix(new_oz)
test_quanti = test_split$X.quanti
test_quali = test_split$X.quali
res_test = PCAmix(X.quanti = test_quanti, X.quali = test_quali)
plot(res_test, choice="cor")
