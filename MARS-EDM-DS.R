#Install packages
#install.packages("caret")
#install.packages("earth")
#Recall libraries
library(caret)
library(earth)
mt <- read.csv("G:/810017405007/ds/edm.csv")
head(mt)
Y1 <- mt$DFlow
Y2 <- mt$DGap

# fit model

fit1 <- earth(Y1~.
              ,mt
              ,trace = 1 #Trace: shows the details of computation
              ,ncross= 30
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
fit2 <- earth(Y2~.
              ,mt
              ,trace = 1 #Trace: shows the details of computation
              ,ncross= 30
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
# summarize the fit
summary(fit1)
summary(fit2)
# coefficients
# Estimate variable importances in an earth object
evimp(fit1)
evimp(fit2)
# make predictions
prediction1 <- predict(fit1, mt)
#Results
prediction1
prediction2 <- predict(fit2, mt)
prediction2
rmse1 <- mean((Y1 - prediction1)^2)
print(rmse1)
# summarize accuracy
rmse2 <- mean((Y2 - prediction2)^2)
# Print Root Mean Square Error
print(rmse2)
# See Dataset
mt
# Sumary of models
summary(fit1, digits = 2)
summary(fit1, digits = 2, style = "pmax")
summary(fit1, digits = 2, style = "max")
summary(fit1, digits = 2, style = "C")
summary(fit1, digits = 2, style = "h")
summary(fit1, digits = 2, style = "bf")
# Sumary of models
summary(fit2, digits = 2)
summary(fit2, digits = 2, style = "pmax")
summary(fit2, digits = 2, style = "max")
summary(fit2, digits = 2, style = "C")
summary(fit2, digits = 2, style = "h")
summary(fit2, digits = 2, style = "bf")
plot(fit1)
plot(fit2)
plotmo(fit1)
plotmo(fit2)
# Model Comparison
plot.earth.models (fit1, jitter = 0.01)
plot.earth.models (fit2, jitter = 0.01)
# Plot variable importance
vi1 <- evimp(fit1)
vi1
vi2 <- evimp(fit2)
vi2
plot (vi1, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
plot (vi2, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit1$varmod)
plot(fit2$varmod)

