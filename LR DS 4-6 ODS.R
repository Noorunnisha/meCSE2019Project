mt <- read.csv("G:/810017405007/CSV DATASET/csv_result-andro.csv")
x1 <- mt$id
x2 <- mt$Window0.Att0
x3 <- mt$Window0.Att1
x4 <- mt$Window0.Att2
x5 <- mt$Window0.Att3
x6 <- mt$Window0.Att4
x7 <- mt$Window0.Att5
x8 <- mt$Window1.Att0
x9 <- mt$Window1.Att1
x10 <- mt$Window1.Att2
x11 <- mt$Window1.Att3
x12 <- mt$Window1.Att4
x13 <- mt$Window1.Att5
x14 <- mt$Window2.Att0
x15 <- mt$Window2.Att1
x16 <- mt$Window2.Att2
x17 <- mt$Window2.Att3
x18 <- mt$Window2.Att4
x19 <- mt$Window2.Att5
x20 <- mt$Window3.Att0
x21 <- mt$Window3.Att1
x22 <- mt$Window3.Att2
x23 <- mt$Window3.Att3
x24 <- mt$Window3.Att4
x25 <- mt$Window3.Att5
x26 <- mt$Window4.Att0
x27 <- mt$Window4.Att1
x28 <- mt$Window4.Att2
x29 <- mt$Window4.Att3
x30 <- mt$Window4.Att4
x31 <- mt$Window4.Att5
y1 <- mt$Target
y2 <- mt$Target_2
y3 <- mt$Target_3


# dataset edm
mt <- read.csv("F:/Dataset with Regression/edm.csv")
x1 <- mt$id
x2 <- mt$ASM_A_MeanT
x3 <- mt$ASD_A_SDevT
x4 <- mt$BSM_B_MeanT
x5 <- mt$BSD_B_SDevT
x6 <- mt$CSM_C_MeanT
x7 <- mt$CSD_C_SDevT
x8 <- mt$ISM_I_MeanT
x9 <- mt$ISD_I_SDevT
x10 <- mt$ALM_A_MeanT
x11 <- mt$ALD_A_SDevT
x12 <- mt$BLM_B_MeanT
x13 <- mt$BLD_B_SDevT
x14 <- mt$CLM_C_MeanT
x15 <- mt$CLD_C_SDevT
x16 <- mt$ILM_I_MeanT
x17 <- mt$ILD_I_SDevT
y1 <- mt$DFlow
y2 <- mt$DGap
model <- lm(cbind(y1, y2)~x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17)
summary(model)


#dataset enb
mt <- read.csv("F:/Dataset with Regression/enb.csv")
c <- mt$Relative_compactness
model <- lm(cbind(mt$Y1, mt$Y2)~c + mt$X1 + mt$X3 + mt$X4 + mt$X5 + mt$X6 + mt$X7 +mt$X8)
summary(model)

y4 <- mt$Target_4
y5 <- mt$Target_5
y6 <- mt$Target_6

model <- lm(cbind(y1, y2, y3, y4, y5, y6)~x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 +x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31)
summary(model)
