library(ISLR)

trainctg <- read.csv(file.choose())
trainctg

outlier_values <- boxplot.stats(trainctg[,6], coef = 3)$out
boxplot(trainctg[,6], main= "FM", boxwex=0.1)
mtext(paste("Outliers", paste(outlier_values, collapse=", ")), cex=0.6)
outlier_values
rownames(trainctg[trainctg[,6] %in% outlier_values,])
trainctgf <- trainctg[(trainctg[,3] %in% outlier_values)=="FALSE", ]

str(trainctgf)
str(trainctg)
ncol(trainctg)

rownames(trainctg[trainctg[,3] %in% outlier_values,])

colnames(trainctg)
rownames(outlier_values)
outlier_values[1]

grubbs.test(trainctg$AC, type = 10, opposite = FALSE, two.sided = FALSE)


mod <- lm(NSP ~ ., data= trainctg)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
str(trainctg[influential, ])
summary(trainctg)

x<- trainctg$FM
scores(x)

install.packages("outliers")
library(outliers)
x = trainctg
scores(x)
scores(x, type="z", prob=0.95)
scores(x, type="chisq", prob=0.95)
scores(x, type="t", prob=0.95)

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
my_forest <- randomForest(as.factor(Class1) ~ ., data = trainctg1_1, importance = TRUE, ntree=50)
str(my_forest)
varImpPlot(my_forest)

my_prediction <- predict(my_forest, cvctg1_1, type ="class")
my_prediction

cvctg1_1$PredClass <- NA
cvctg1_1$PredClass <- my_prediction
result_rf <- table(cvctg1_1$PredClass)
result_rf
result_rf_prop <- prop.table(table(cvctg1_1$Class1, cvctg1_1$PredClass),1)
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
set.seed(111)
trainctg1_ot <- trainctg_o21[sample(nrow(trainctg_o21), 840), ]
trainctg1_ot
str(trainctg1_ot)
cvctg1_ot <- trainctg_o21[-trainctg1_ot$Sno,]
cvctg1_ot
str(cvctg1_ot)

data = trainctg_o21
indexes = sample(1:nrow(data), size=0.7*nrow(data))
trainctg1_ot = data[indexes,]
cvctg1_ot = data[-indexes,]
str(trainctg1_ot)
str(cvctg1_ot)

library(randomForest)
my_forest <- randomForest(as.factor(Class1) ~ ., data = trainctg1_ot, importance = TRUE, ntree=50)
str(my_forest)
varImpPlot(my_forest)

my_prediction_1A <- predict(my_forest, cvctg1_ot, type ="class")
my_prediction_1A

cvctg1_ot$PredClass <- NA
cvctg1_ot$PredClass <- my_prediction_1A
result_rf_ot <- table(cvctg1_ot$PredClass)
result_rf_ot
result_rf_prop_ot <- prop.table(table(cvctg1_ot$Class1, cvctg1_ot$PredClass),1)
result_rf_prop_ot

cvctg1_1$PredClass <- NA
cvctg1_1$PredClass <- my_prediction
result_rf <- table(cvctg1_1$PredClass)
result_rf
result_rf_prop <- prop.table(table(cvctg1_1$Class1, cvctg1_1$PredClass),1)
result_rf_prop
