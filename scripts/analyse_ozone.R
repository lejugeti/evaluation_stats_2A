if(!require("PCAmixdata")) install.packages("PCAmixdata")

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
acp_quali$eig

plot(acp_quali, choice="cor")


# modèle linéaire ozone ---------------------------------------------------


