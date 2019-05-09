#RDS2 -- RFFS LINEAR REGRESSION
# DS I
mt <- read.csv("G:/NEW DS PROJECT/TASK 3/oes10.csv")
head(mt)
model <- lm(cbind(mt$Y1,mt$Y2, mt$Y3, mt$Y4, mt$Y5, mt$Y6, mt$Y7, mt$Y8, mt$Y9, mt$Y10, mt$Y11, mt$Y12, mt$Y13, mt$Y14, mt$Y15, mt$Y16) ~ mt$X1 + mt$X6 + mt$X8 + mt$X17 + mt$X34 + mt$X36 + mt$X102 + mt$X162 + mt$X298)
summary(model)
# DS II
mt <- read.csv("G:/NEW DS PROJECT/TASK 3/oes97.csv")
head(mt)
model <- lm(cbind(mt$Y1,mt$Y2, mt$Y3, mt$Y4, mt$Y5, mt$Y6, mt$Y7, mt$Y8, mt$Y9, mt$Y10, mt$Y11, mt$Y12, mt$Y13, mt$Y14, mt$Y15, mt$Y16) ~ mt$X3 + mt$X14 + mt$X15 + mt$X73 + mt$X107 + mt$X124 + mt$X144 + mt$X154 + mt$X263)
summary(model)
# DS III
mt <- read.csv("G:/NEW DS PROJECT/TASK 3/atp7d.csv")
head(mt)
model <- lm(cbind(mt$Y1,mt$Y2, mt$Y3, mt$Y4, mt$Y5, mt$Y6) ~ mt$X55 + mt$X58 + mt$X130 + mt$X189 + mt$X192 + mt$X193 + mt$X216 + mt$X219 + mt$X220)
summary(model)

#DS ENB
mt <- read.csv("G:/NEW DS PROJECT/TASK 3/ENB.csv")
head(mt)
model <- lm(cbind(mt$Y1, mt$Y2) ~ mt$X0 + mt$X1 + mt$X3)
summary(model)
#DS EDM
mt <- read.csv("G:/NEW DS PROJECT/TASK 3/EDM.csv")
head(mt)
model <- lm(cbind(mt$Y1, mt$Y2) ~ mt$X1 + mt$X3 + mt$X4)
summary(model)

# DS ANDRO
mt <- read.csv("G:/NEW DS PROJECT/TASK 3/ANDRO.csv")
head(mt)
model <- lm(cbind(mt$Y1,mt$Y2, mt$Y3, mt$Y4, mt$Y5, mt$Y6) ~ mt$X1 + mt$X18 + mt$X24 + mt$X26 + mt$X27)
summary(model)

