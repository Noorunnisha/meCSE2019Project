#Install packages
#install.packages("caret")
#install.packages("earth")
#Recall libraries
library(caret)
library(earth)
mt <- read.csv("G:/NEW DS PROJECT/TASK 0/MARS/oes97.csv")

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
#prediction2 <- predict(fit2, mt)
#Result
#prediction2
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


fit7 <- earth(Y7~.
              ,mt
              ,trace = 0 #Trace: shows the details of computation
              ,ncross= 3
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
fit7
# summarize the fit
summary(fit7)
evimp(fit7)
# make predictions
prediction7 <- predict(fit7, mt)
#Result
prediction7
rmse7 <- mean((mt$Y7 - prediction7)^2)
print(rmse7)
# Sumary of models
summary(fit7, digits = 2)
summary(fit7, digits = 2, style = "pmax")
summary(fit7, digits = 2, style = "max")
summary(fit7, digits = 2, style = "C")
summary(fit7, digits = 2, style = "h")
summary(fit7, digits = 2, style = "bf")
plot(fit7)
plotmo(fit7)
plot.earth.models (fit7, jitter = 0.01)
# Plot variable importance
vi7 <- evimp(fit7)
vi7
plot (vi7, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit7$varmod)

fit8 <- earth(Y8~.
              ,mt
              ,trace = 0 #Trace: shows the details of computation
              ,ncross= 3
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 

fit8
# summarize the fit
summary(fit8)
evimp(fit8)
# make predictions
prediction8 <- predict(fit8, mt)
#Result
prediction8
rmse8 <- mean((mt$Y8 - prediction8)^2)
print(rmse8)
# Sumary of models
summary(fit8, digits = 2)
summary(fit8, digits = 2, style = "pmax")
summary(fit8, digits = 2, style = "max")
summary(fit8, digits = 2, style = "C")
summary(fit8, digits = 2, style = "h")
summary(fit8, digits = 2, style = "bf")
plot(fit8)
plotmo(fit8)
plot.earth.models (fit8, jitter = 0.01)
# Plot variable importance
vi8 <- evimp(fit8)
vi8
plot (vi8, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit8$varmod)

fit9 <- earth(Y9~.
              ,mt
              ,trace = 0 #Trace: shows the details of computation
              ,ncross= 3
              ,nfold = 10
              ,pmethod="backward"
              ,nprune=20
              ,varmod.method="lm") 
fit9
# summarize the fit
summary(fit9)
evimp(fit9)
# make predictions
prediction9 <- predict(fit9, mt)
#Result
prediction9
rmse9 <- mean((mt$Y9 - prediction9)^2)
print(rmse9)
# Sumary of models
summary(fit9, digits = 2)
summary(fit9, digits = 2, style = "pmax")
summary(fit9, digits = 2, style = "max")
summary(fit9, digits = 2, style = "C")
summary(fit9, digits = 2, style = "h")
summary(fit9, digits = 2, style = "bf")
plot(fit9)
plotmo(fit9)
plot.earth.models (fit9, jitter = 0.01)
# Plot variable importance
vi9 <- evimp(fit9)
vi9
plot (vi9, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit9$varmod)

fit10 <- earth(Y10~.
               ,mt
               ,trace = 0 #Trace: shows the details of computation
               ,ncross= 3
               ,nfold = 10
               ,pmethod="backward"
               ,nprune=20
               ,varmod.method="lm") 
fit10
# summarize the fit
summary(fit10)
evimp(fit10)
# make predictions
prediction10 <- predict(fit10, mt)
#Result
prediction10
rmse10 <- mean((mt$Y10 - prediction10)^2)
print(rmse10)
# Sumary of models
summary(fit10, digits = 2)
summary(fit10, digits = 2, style = "pmax")
summary(fit10, digits = 2, style = "max")
summary(fit10, digits = 2, style = "C")
summary(fit10, digits = 2, style = "h")
summary(fit10, digits = 2, style = "bf")
plot(fit10)
plotmo(fit10)
plot.earth.models (fit10, jitter = 0.01)
# Plot variable importance
vi10 <- evimp(fit10)
vi10
plot (vi10, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit10$varmod)

fit11 <- earth(Y11~.
               ,mt
               ,trace = 0 #Trace: shows the details of computation
               ,ncross= 3
               ,nfold = 10
               ,pmethod="backward"
               ,nprune=20
               ,varmod.method="lm") 
fit11
# summarize the fit
summary(fit11)
evimp(fit11)
# make predictions
prediction11 <- predict(fit11, mt)
#Result
prediction11
rmse11 <- mean((mt$Y11 - prediction11)^2)
print(rmse11)
# Sumary of models
summary(fit11, digits = 2)
summary(fit11, digits = 2, style = "pmax")
summary(fit11, digits = 2, style = "max")
summary(fit11, digits = 2, style = "C")
summary(fit11, digits = 2, style = "h")
summary(fit11, digits = 2, style = "bf")
plot(fit11)
plotmo(fit11)
plot.earth.models (fit11, jitter = 0.01)
# Plot variable importance
vi11 <- evimp(fit11)
vi11
plot (vi11, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit11$varmod)

fit12 <- earth(Y12~.
               ,mt
               ,trace = 0 #Trace: shows the details of computation
               ,ncross= 3
               ,nfold = 10
               ,pmethod="backward"
               ,nprune=20
               ,varmod.method="lm") 
fit12
# summarize the fit
summary(fit12)
evimp(fit12)
# make predictions
prediction12 <- predict(fit12, mt)
#Result
prediction12
rmse12 <- mean((mt$Y12 - prediction12)^2)
print(rmse12)
# Sumary of models
summary(fit12, digits = 2)
summary(fit12, digits = 2, style = "pmax")
summary(fit12, digits = 2, style = "max")
summary(fit12, digits = 2, style = "C")
summary(fit12, digits = 2, style = "h")
summary(fit12, digits = 2, style = "bf")
plot(fit12)
plotmo(fit12)
plot.earth.models (fit12, jitter = 0.01)
# Plot variable importance
vi12 <- evimp(fit12)
vi12
plot (vi12, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit12$varmod)


fit13 <- earth(Y13~.
               ,mt
               ,trace = 0 #Trace: shows the details of computation
               ,ncross= 3
               ,nfold = 10
               ,pmethod="backward"
               ,nprune=20
               ,varmod.method="lm") 
fit13
# summarize the fit
summary(fit13)
evimp(fit13)
# make predictions
prediction13 <- predict(fit13, mt)
#Result
prediction13
rmse13 <- mean((mt$Y13 - prediction13)^2)
print(rmse13)
# Sumary of models
summary(fit13, digits = 2)
summary(fit13, digits = 2, style = "pmax")
summary(fit13, digits = 2, style = "max")
summary(fit13, digits = 2, style = "C")
summary(fit13, digits = 2, style = "h")
summary(fit13, digits = 2, style = "bf")
plot(fit13)
plotmo(fit13)
plot.earth.models (fit13, jitter = 0.01)
# Plot variable importance
vi13 <- evimp(fit13)
vi13
plot (vi13, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit13$varmod)

fit14 <- earth(Y14~.
               ,mt
               ,trace = 0 #Trace: shows the details of computation
               ,ncross= 3
               ,nfold = 10
               ,pmethod="backward"
               ,nprune=20
               ,varmod.method="lm") 
fit14
# summarize the fit
summary(fit14)
evimp(fit14)
# make predictions
prediction14 <- predict(fit14, mt)
#Result
prediction14
rmse14 <- mean((mt$Y14 - prediction14)^2)
print(rmse14)
# Sumary of models
summary(fit14, digits = 2)
summary(fit14, digits = 2, style = "pmax")
summary(fit14, digits = 2, style = "max")
summary(fit14, digits = 2, style = "C")
summary(fit14, digits = 2, style = "h")
summary(fit14, digits = 2, style = "bf")
plot(fit14)
plotmo(fit14)
plot.earth.models (fit14, jitter = 0.01)
# Plot variable importance
vi14 <- evimp(fit14)
vi14
plot (vi14, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit14$varmod)

fit15 <- earth(Y15~.
               ,mt
               ,trace = 0 #Trace: shows the details of computation
               ,ncross= 3
               ,nfold = 10
               ,pmethod="backward"
               ,nprune=20
               ,varmod.method="lm") 
fit15
# summarize the fit
summary(fit15)
evimp(fit15)
# make predictions
prediction15 <- predict(fit15, mt)
#Result
prediction15
rmse15 <- mean((mt$Y15 - prediction15)^2)
print(rmse15)
# Sumary of models
summary(fit15, digits = 2)
summary(fit15, digits = 2, style = "pmax")
summary(fit15, digits = 2, style = "max")
summary(fit15, digits = 2, style = "C")
summary(fit15, digits = 2, style = "h")
summary(fit15, digits = 2, style = "bf")
plot(fit15)
plotmo(fit15)
plot.earth.models (fit15, jitter = 0.01)
# Plot variable importance
vi15 <- evimp(fit15)
vi15
plot (vi15, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit15$varmod)

fit16 <- earth(Y16~.
               ,mt
               ,trace = 0 #Trace: shows the details of computation
               ,ncross= 3
               ,nfold = 10
               ,pmethod="backward"
               ,nprune=20
               ,varmod.method="lm") 
fit16
# summarize the fit
summary(fit16)
evimp(fit16)
# make predictions
prediction16 <- predict(fit16, mt)
#Result
prediction16
rmse16 <- mean((mt$Y16 - prediction16)^2)
print(rmse16)
# Sumary of models
summary(fit16, digits = 2)
summary(fit16, digits = 2, style = "pmax")
summary(fit16, digits = 2, style = "max")
summary(fit16, digits = 2, style = "C")
summary(fit16, digits = 2, style = "h")
summary(fit16, digits = 2, style = "bf")
plot(fit16)
plotmo(fit16)
plot.earth.models (fit16, jitter = 0.01)
# Plot variable importance
vi16 <- evimp(fit16)
vi16
plot (vi16, cex.var = 1,
      type.nsubsets = "l", col.nsubsets = "black", lty.nsubsets = 1,
      type.gcv = "l", col.gcv = 2, lty.gcv = 1,
      type.rss = "l", col.rss = "gray60", lty.rss = 1,
      cex.legend = 1, rh.col = 1)
# Plot the embedded variance model
plot(fit16$varmod)

