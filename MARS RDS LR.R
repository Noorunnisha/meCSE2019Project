#RDS4 -- MARS REDUCED DS - LINEAR REGRESSION
# DS I
mt <- read.csv("G:/Viva Coding Preparation/INSHA ALLAH MARS FS/ReducedDS/OES10-MFS-RDS.csv")
head(mt)
model <- lm(cbind(mt$Y1,mt$Y2, mt$Y3, mt$Y4, mt$Y5, mt$Y6, mt$Y7, mt$Y8, mt$Y9, mt$Y10, mt$Y11, mt$Y12, mt$Y13, mt$Y14, mt$Y15, mt$Y16) ~ mt$X30 + mt$X62 + mt$X113 + mt$X143 + mt$X198 + mt$X228 + mt$X268 + mt$X278)
summary(model)
# DS II
mt <- read.csv("G:/Viva Coding Preparation/INSHA ALLAH MARS FS/ReducedDS/OES97-MFS-RDS.csv")
head(mt)
model <- lm(cbind(mt$Y1,mt$Y2, mt$Y3, mt$Y4, mt$Y5, mt$Y6, mt$Y7, mt$Y8, mt$Y9, mt$Y10, mt$Y11, mt$Y12, mt$Y13, mt$Y14, mt$Y15, mt$Y16) ~ mt$X33 + mt$X48 + mt$X77 + mt$X116 + mt$X157 + mt$X201 + mt$X236 + mt$X258)
summary(model)
# DS III
mt <- read.csv("G:/Viva Coding Preparation/INSHA ALLAH MARS FS/ReducedDS/ATP7D-MFS-RDS.csv")
head(mt)
model <- lm(cbind(mt$Y1,mt$Y2, mt$Y3, mt$Y4, mt$Y5, mt$Y6) ~ mt$X1 + mt$X10 + mt$X18 + mt$X155 + mt$X183 + mt$X192 + mt$X227 + mt$X267 + mt$X300)
summary(model)

#DS ENB
mt <- read.csv("G:/Viva Coding Preparation/INSHA ALLAH MARS FS/ReducedDS/ENB-MFS-RDS.csv")
head(mt)
model <- lm(cbind(mt$Y1, mt$Y2) ~ mt$X2 + mt$X4 + mt$X7)
summary(model)
#DS EDM
mt <- read.csv("G:/Viva Coding Preparation/INSHA ALLAH MARS FS/ReducedDS/EDM-MFS-RDS.csv")
head(mt)
model <- lm(cbind(mt$Y1, mt$Y2) ~ mt$X1 + mt$X3 + mt$X5)
summary(model)

# DS ANDRO
mt <- read.csv("G:/Viva Coding Preparation/INSHA ALLAH MARS FS/ReducedDS4/ANDRO-MFS-RDS.csv")
head(mt)
model <- lm(cbind(mt$Y0,mt$Y1, mt$Y2, mt$Y3, mt$Y4, mt$Y5) ~ mt$X2 + mt$X6 + mt$X25 + mt$X27 + mt$X28)
summary(model)
