## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(arules)
library(arulesViz)
library(pmml)
library(mlbench)

## ----include=FALSE-------------------------------------------------------
Contraceptive = read.csv("./Datos/cmc.data", header = FALSE)
head(Contraceptive)

## ----include=FALSE-------------------------------------------------------
colnames(Contraceptive) = c("Wife's age",
				"Wife's education", "Husband's education",
				"Children", "Wife's religion", "Wife working",
				"Husband's occupation", "Standard-of-living",
				"Media exposure", "Contraceptive method")

# Wife's age
# Se establecen unos rangos de edad que puedan dividir a la población
Contraceptive[,1] = cut(Contraceptive[,1], c(16,22,30,40,50), right = FALSE,
              labels = c("Very young", "Young", "Mid-age", "Adult"))

# Wife's education
Contraceptive[,2] = ordered(Contraceptive[,2], levels = 1:4,
							labels = c("Low", "Mid-low", "Mid-high", "High"))

# Husband's education
Contraceptive[,3] = ordered(Contraceptive[,3], levels = 1:4,
							labels = c("Low", "Mid-low", "Mid-high", "High"))

# Number of born children
Contraceptive[,4] = cut(Contraceptive[,4], c(0,1,3,5,8,20), right = FALSE,
              labels = c("0", "1-2", "3-4", "5-8", "9+"))

# Wife's religion
Contraceptive[,5] = factor(Contraceptive[,5], levels = 0:1,
							labels = c("Non-Islam", "Islam"))

# Wife working
Contraceptive[,6] = factor(Contraceptive[,6], levels = 0:1,
							labels = c("Yes", "No"))
							# ?No es un error! "0" significa "sí" en esta variable...

# Husband's occupation (?no especificado por el dataset!)
Contraceptive[,7] = ordered(Contraceptive[,7], levels = 1:4,
							labels = c("High", "Mid-high", "Mid-low", "Low"))

# Standard-of-living
Contraceptive[,8] = ordered(Contraceptive[,8], levels = 1:4,
							labels = c("Low", "Mid-low", "Mid-high", "High"))

# Media exposure
Contraceptive[,9] = factor(Contraceptive[,9], levels = 0:1,
							labels = c("Good", "Not good"))

# Contraceptive method
Contraceptive[,10] = factor(Contraceptive[,10], levels = 1:3,
							labels = c("No-use", "Long-term", "Short-term"))

## ------------------------------------------------------------------------
summary(Contraceptive)

## ----include=FALSE-------------------------------------------------------
ContraceptiveT = as(Contraceptive, "transactions")
summary(ContraceptiveT)

## ------------------------------------------------------------------------
itemFrequencyPlot(ContraceptiveT, support = 0.2, cex.names = 0.75)

## ------------------------------------------------------------------------
reglas = apriori(ContraceptiveT, parameter = list(support = 0.05,
                                  confidence = 0.8, minlen = 2))

mejoresReglas = subset(reglas,
                subset = support > 0.1 & support < 0.4 & confidence < 1 & lift > 1)

# Eliminar reglas redundantes
esSubconjunto = is.subset(mejoresReglas, mejoresReglas)
esSubconjunto[lower.tri(esSubconjunto, diag = TRUE)] = FALSE
redundantes = colSums(esSubconjunto, na.rm=TRUE) >= 1
subsetMejoresReglas = mejoresReglas[!redundantes]

## ----include=FALSE-------------------------------------------------------
inspect(subsetMejoresReglas)

## ----include=FALSE-------------------------------------------------------
veryYoung = Contraceptive$`Wife's age` == "Very young"
young = Contraceptive$`Wife's age` == "Young"
midAge = Contraceptive$`Wife's age` == "Mid-age"
adult = Contraceptive$`Wife's age` == "Adult"

Contraceptive = Contraceptive[,-1]
Contraceptive = cbind(Contraceptive, veryYoung, young, midAge, adult)
Contraceptive = sapply(Contraceptive, as.factor)
Contraceptive = as.data.frame(Contraceptive)

ContraceptiveT = as(Contraceptive, "transactions")
summary(ContraceptiveT)

reglas = apriori(ContraceptiveT, parameter = list(support = 0.1,
                                  confidence = 0.8, minlen = 2))

mejoresReglas = subset(reglas,
                subset = support < 0.4 & confidence < 1 & lift > 1)

# Eliminar reglas redundantes
esSubconjunto = is.subset(mejoresReglas, mejoresReglas)
esSubconjunto[lower.tri(esSubconjunto, diag = TRUE)] = FALSE
redundantes = colSums(esSubconjunto, na.rm=TRUE) >= 1
subsetMejoresReglas = mejoresReglas[!redundantes]

inspect(subsetMejoresReglas)

## ----include=FALSE-------------------------------------------------------
reglas = apriori(ContraceptiveT, parameter = list(support = 0.05,
                                  confidence = 0.8, minlen = 2))

mejoresReglas = subset(reglas,
                subset = lhs%in%"Children=9+" &
                  support < 0.4 & confidence < 1 & lift > 1)

# Eliminar reglas redundantes
esSubconjunto = is.subset(mejoresReglas, mejoresReglas)
esSubconjunto[lower.tri(esSubconjunto, diag = TRUE)] = FALSE
redundantes = colSums(esSubconjunto, na.rm=TRUE) >= 1
subsetMejoresReglas = mejoresReglas[!redundantes]

inspect(subsetMejoresReglas)

## ----include=FALSE-------------------------------------------------------
reglas = apriori(ContraceptiveT, parameter = list(support = 0.1,
                                  confidence = 0.8, minlen = 2))

mejoresReglas = subset(reglas,
                subset = (lhs%in%"Contraceptive method=Long-term" | 
                  rhs%in%"Contraceptive method=Long-term") &
                  support < 0.4 & confidence < 1 & lift > 1)

# Eliminar reglas redundantes
esSubconjunto = is.subset(mejoresReglas, mejoresReglas)
esSubconjunto[lower.tri(esSubconjunto, diag = TRUE)] = FALSE
redundantes = colSums(esSubconjunto, na.rm=TRUE) >= 1
subsetMejoresReglas = mejoresReglas[!redundantes]

inspect(subsetMejoresReglas)

