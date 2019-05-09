#Install packages
#install.packages("caret")
#install.packages("earth")
#Recall libraries
library(caret)
library(earth)

mt <- read.csv("G:/NEW DS PROJECT/TASK 0/MARS/atp7d.csv")
head(mt)

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

fit3 <- earth(Y3~.
              ,mt
              ,trace = 0 #Trace: shows the details of computation
              ,ncross= 3
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
fit3
# summarize the fit
summary(fit3)
evimp(fit3)
# make predictions
prediction3 <- predict(fit3, mt)
#Result
prediction3
rmse3 <- mean((mt$Y3 - prediction3)^2)
print(rmse3)
# Sumary of models
summary(fit3, digits = 2)
summary(fit3, digits = 2, style = "pmax")
summary(fit3, digits = 2, style = "max")
summary(fit3, digits = 2, style = "C")
summary(fit3, digits = 2, style = "h")
summary(fit3, digits = 2, style = "bf")
plot(fit3)
plotmo(fit3)
plot.earth.models (fit3, jitter = 0.01)
# Plot variable importance
vi3 <- evimp(fit3)
vi3
plot (vi3, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit3$varmod)



fit4 <- earth(Y4~.
              ,mt
              ,trace = 0 #Trace: shows the details of computation
              ,ncross= 3
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
fit4

# summarize the fit
summary(fit4)
evimp(fit4)
# make predictions
prediction4 <- predict(fit4, mt)
#Result
prediction4
rmse4 <- mean((mt$Y4 - prediction4)^2)
print(rmse4)
# Sumary of models
summary(fit4, digits = 2)
summary(fit4, digits = 2, style = "pmax")
summary(fit4, digits = 2, style = "max")
summary(fit4, digits = 2, style = "C")
summary(fit4, digits = 2, style = "h")
summary(fit4, digits = 2, style = "bf")
plot(fit4)
plotmo(fit4)
plot.earth.models (fit4, jitter = 0.01)
# Plot variable importance
vi4 <- evimp(fit4)
vi4
plot (vi4, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit4$varmod)

fit5 <- earth(Y5~.
              ,mt
              ,trace = 0 #Trace: shows the details of computation
              ,ncross= 3
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
fit5

# summarize the fit
summary(fit5)
evimp(fit5)
# make predictions
prediction5 <- predict(fit5, mt)
#Result
prediction5
rmse5 <- mean((mt$Y5 - prediction5)^2)
print(rmse5)
# Sumary of models
summary(fit5, digits = 2)
summary(fit5, digits = 2, style = "pmax")
summary(fit5, digits = 2, style = "max")
summary(fit5, digits = 2, style = "C")
summary(fit5, digits = 2, style = "h")
summary(fit5, digits = 2, style = "bf")
plot(fit5)
plotmo(fit5)
plot.earth.models (fit5, jitter = 0.01)
# Plot variable importance
vi5 <- evimp(fit5)
vi5
plot (vi5, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit5$varmod)

fit6 <- earth(Y6~.
              ,mt
              ,trace = 0 #Trace: shows the details of computation
              ,ncross= 3
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
fit6
#Variance Model: Choose the variance model (linear model in this case)
# summarize the fit
summary(fit6)
evimp(fit6)
# make predictions
prediction6 <- predict(fit6, mt)
#Result
prediction6
rmse6 <- mean((mt$Y6 - prediction6)^2)
print(rmse6)
# Sumary of models
summary(fit6, digits = 2)
summary(fit6, digits = 2, style = "pmax")
summary(fit6, digits = 2, style = "max")
summary(fit6, digits = 2, style = "C")
summary(fit6, digits = 2, style = "h")
summary(fit6, digits = 2, style = "bf")
plot(fit6)
plotmo(fit6)
plot.earth.models (fit6, jitter = 0.01)
# Plot variable importance
vi6 <- evimp(fit6)
vi6
plot (vi6, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit6$varmod)



