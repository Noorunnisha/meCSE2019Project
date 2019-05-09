#Install packages
#install.packages("caret")
#install.packages("earth")
#Recall libraries
library(caret)
library(earth)
mt <- read.csv("G:/810017405007/CSV DATASET/edm.csv")

#head(mt)
# fit model
fit1 <- earth(Y1~.
              ,mt
              ,trace = 0 #Trace: shows the details of computation
              ,ncross= 3
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
fit1
# summarize the fit
summary(fit1)
evimp(fit1)
# make predictions
prediction1 <- predict(fit1, mt)
#Result
prediction1
rmse1 <- mean((mt$Y1 - prediction1)^2)
print(rmse1)
# Sumary of models
summary(fit1, digits = 2)
summary(fit1, digits = 2, style = "pmax")
summary(fit1, digits = 2, style = "max")
summary(fit1, digits = 2, style = "C")
summary(fit1, digits = 2, style = "h")
summary(fit1, digits = 2, style = "bf")
plot(fit1)
plotmo(fit1)
plot.earth.models (fit1, jitter = 0.01)
# Plot variable importance
vi1 <- evimp(fit1)
vi1
plot (vi1, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit1$varmod)

fit2 <- earth(Y2~.
              ,mt
              ,trace = 0 #Trace: shows the details of computation
              ,ncross= 3
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
fit2
summary(fit2)
evimp(fit2)
# make predictions
prediction2 <- predict(fit2, mt)
#Result
prediction2
rmse2 <- mean((mt$Y2 - prediction2)^2)
print(rmse2)
# Sumary of models
summary(fit2, digits = 2)
summary(fit2, digits = 2, style = "pmax")
summary(fit2, digits = 2, style = "max")
summary(fit2, digits = 2, style = "C")
summary(fit2, digits = 2, style = "h")
summary(fit2, digits = 2, style = "bf")
plot(fit2)
plotmo(fit2)
plot.earth.models (fit2, jitter = 0.01)
# Plot variable importance
vi2 <- evimp(fit2)
vi2
plot (vi2, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit2$varmod)

