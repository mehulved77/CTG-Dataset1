library(ISLR)

trainctg1 <- read.csv(file.choose())
str(trainctg1)


# Split Data for from trainctg1_1 file to CVctg1_1 file
trainctg1_1 <- trainctg1[sample(nrow(trainctg1), 1500), ]
trainctg1_1
str(trainctg1_1)
cvctg1_1 <- trainctg1[-trainctg1_1$Sno,]
cvctg1_1
str(cvctg1_1)


library(randomForest)
my_forest <- randomForest(as.factor(NSP) ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV + Width + Min + Max + Nmax + Nzeros + Mode + Mean + Median + Variance + Tendency, data = trainctg1_1, importance = TRUE, ntree= 50)
str(my_forest)
varImpPlot(my_forest)

my_prediction <- predict(my_forest, cvctg1_1, type ="class")
my_prediction

cvctg1_1$PredNSP <- NA
cvctg1_1$PredNSP <- my_prediction
result_rf <- table(cvctg1_1$PredNSP)
result_rf
result_rf_prop <- prop.table(table(cvctg1_1$NSP, cvctg1_1$PredNSP),1)
result_rf_prop
#____________

trainctg_o <- read.csv(file.choose())
str(trainctg_o)

outlier_values_o4 <- boxplot.stats(trainctg_o[,4], coef = 3)$out
boxplot(trainctg_o[,4], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_o4, collapse=", ")), cex=0.6)
outlier_values_o4
o4 <- trainctg_o[trainctg_o[,4] %in% outlier_values_o4,]
str(o4)
trainctg_o4 <- trainctg_o[-o4$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o4)

outlier_values_o6 <- boxplot.stats(trainctg_o[,6], coef = 3)$out
boxplot(trainctg_o[,6], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_o6, collapse=", ")), cex=0.6)
outlier_values_o6
o6 <- trainctg_o[trainctg_o[,6] %in% outlier_values_o6,]
str(o6)
trainctg_o6 <- trainctg_o4[-o6$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o6)

outlier_values_o7 <- boxplot.stats(trainctg_o[,7], coef = 3)$out
boxplot(trainctg_o[,7], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_o7, collapse=", ")), cex=0.6)
outlier_values_o7
o7 <- trainctg_o[trainctg_o[,7] %in% outlier_values_o7,]
str(o7)
trainctg_o7 <- trainctg_o6[-o7$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o7)

outlier_values_o8 <- boxplot.stats(trainctg_o[,8], coef = 3)$out
boxplot(trainctg_o[,8], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_o8, collapse=", ")), cex=0.6)
outlier_values_o8
o8 <- trainctg_o[trainctg_o[,8] %in% outlier_values_o8,]
str(o8)
trainctg_o8 <- trainctg_o7[-o8$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o8)

outlier_values_10 <- boxplot.stats(trainctg_o[,10], coef = 3)$out
boxplot(trainctg_o[,10], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_10, collapse=", ")), cex=0.6)
outlier_values_10
o10 <- trainctg_o[trainctg_o[,10] %in% outlier_values_10,]
str(o10)
trainctg_o10 <- trainctg_o8[-o10$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o10)

outlier_values_11 <- boxplot.stats(trainctg_o[,11], coef = 3)$out
boxplot(trainctg_o[,11], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_11, collapse=", ")), cex=0.6)
outlier_values_11
o11 <- trainctg_o[trainctg_o[,11] %in% outlier_values_11,]
str(o11)
trainctg_o11 <- trainctg_o10[-o11$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o11)

outlier_values_12 <- boxplot.stats(trainctg_o[,12], coef = 3)$out
boxplot(trainctg_o[,12], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_12, collapse=", ")), cex=0.6)
outlier_values_12
o12 <- trainctg_o[trainctg_o[,12] %in% outlier_values_12,]
str(o12)
trainctg_o12 <- trainctg_o11[-o12$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o12)

outlier_values_17 <- boxplot.stats(trainctg_o[,17], coef = 3)$out
boxplot(trainctg_o[,17], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_17, collapse=", ")), cex=0.6)
outlier_values_17
o17 <- trainctg_o[trainctg_o[,17] %in% outlier_values_17,]
str(o17)
trainctg_o17 <- trainctg_o12[-o17$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o17)

outlier_values_18 <- boxplot.stats(trainctg_o[,18], coef = 3)$out
boxplot(trainctg_o[,18], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_18, collapse=", ")), cex=0.6)
outlier_values_18
o18 <- trainctg_o[trainctg_o[,18] %in% outlier_values_18,]
str(o18)
trainctg_o18 <- trainctg_o17[-o18$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o18)

outlier_values_21 <- boxplot.stats(trainctg_o[,21], coef = 3)$out
boxplot(trainctg_o[,21], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values_21, collapse=", ")), cex=0.6)
outlier_values_21
o21 <- trainctg_o[trainctg_o[,21] %in% outlier_values_21,]
str(o21)
trainctg_o21 <- trainctg_o18[-o21$Sno,]
#rownames(trainctg_o[trainctg_o[,4] %in% outlier_values_o4,])
#trainctg_o4 <- trainctg_o[(trainctg[,4] %in% outlier_values_o4)=="FALSE", ]
str(trainctg_o21)

# Split Data for from trainctg1_1 file to CVctg1_1 file
set.seed(1112)

data = trainctg_o21
indexes = sample(1:nrow(data), size=0.7*nrow(data))
trainctg1_ot = data[indexes,]
cvctg1_ot = data[-indexes,]
str(trainctg1_ot)
str(cvctg1_ot)

library(randomForest)
my_forest <- randomForest(as.factor(NSP) ~ LB + AC + FM + UC + DL + DS + DP + ASTV + MSTV + ALTV + MLTV + Width + Min + Max + Nmax + Nzeros + Mode + Mean + Median + Variance + Tendency, data = trainctg1_ot, importance = TRUE, ntree= 50)
str(my_forest)
varImpPlot(my_forest)

my_prediction_1 <- predict(my_forest, cvctg1_ot, type ="class")
my_prediction_1

cvctg1_ot$PredNSP <- NA
cvctg1_ot$PredNSP <- my_prediction_1
result_rf_ot <- table(cvctg1_ot$PredNSP)
result_rf_ot
result_rf_prop_ot <- prop.table(table(cvctg1_ot$NSP, cvctg1_ot$PredNSP),1)
result_rf_prop_ot

cvctg1_1$PredClass <- NA
cvctg1_1$PredClass <- my_prediction
result_rf <- table(cvctg1_1$PredClass)
result_rf
result_rf_prop <- prop.table(table(cvctg1_1$NSP, cvctg1_1$PredNSP),1)
result_rf_prop
