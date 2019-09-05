#******************************************************
#
#   	This is the main code of Project
#
#******************************************************

rm(list=ls())

# import self-designed function
setwd(".")
source("PCAplots.R")
source("TestSet.R")
source("SPM_Panel.R")
source("FactorPlots.R")

#****************************
#  Step 1: Data visualization
#****************************

# import data
library(readstata13)              
data <- read.dta13("./data/SCF2016_extract.dta")
data$f_hhsex <- factor(data$hhsex,labels = c("Male","Female"))

data$f_race <- factor(data$race,labels = c("White","Black","Hispanic","Others"))

data$kid <- data$kids>0
summary(data$kid)
data$f_kid <- factor(data$kid,labels = c("No Children","Have children"))

data$aage <- data$age>45
summary(data$aage)
data$f_aage <- factor(data$aage,labels = c("Low Age","High Age"))

data$eeduc <- data$educ>9
summary(data$eeduc)
data$f_eeduc <- factor(data$eeduc,labels = c("Low Eduction","High Education"))

data$man <- rep(0,nrow(data))
data$man[which(data$hhsex==1)] <- 1

data$woman <- rep(0,nrow(data))
data$woman[which(data$hhsex==2)] <- 1

data$white <- rep(0,nrow(data))
data$white[which(data$race==1)] <- 1

data$college <- rep(0,nrow(data))
data$college[which(data$educ>=9)] <- 1

data$work <- rep(0,nrow(data))
data$work[which(data$OCCAT2==1)] <- "M"  # manager/professional
data$work[which(data$OCCAT2==2)] <- "T"  # technical/sales/services
data$work[which(data$OCCAT2==3)] <- "O"  # other (farmer, fisher, production)
data$work[which(data$OCCAT2==4)] <- "N"  # not working
data$work <- as.factor(data$work)

contrasts(data$work) <- matrix(c(1,0,0,0,  0,0,0,1,   0,0,1,0),nrow=4)
colnames(contrasts(data$work)) <- matrix(c("M","T","O"))
contrasts(data$work)

# Boxplot to filter high value assets
data$asset
boxplot(data$asset)
assetbox <- boxplot(data$asset)
# define high value as these above the middle whiske
xdata <- data[data$asset>assetbox$stats[3],]

# Scatter plot matrics
uva.pairs(xdata[,c("educ","age","ntrad","kids","asset")])

# log assets
xdata$logasset <- log(xdata$asset)

# PCA relationship analysis
pred.pca <- princomp(xdata[,c("educ","age","ntrad","kids","asset")],cor=T)
biplot(pred.pca)  # # edu most correlated
screeplot(pred.pca, main = "Variance for PC of Metrics")
barplot(pred.pca$loadings[,1])
barplot(pred.pca$loadings[,2])
cumplot(pred.pca, col = "blue") # first two counts 52% variance

# Heat Map
source("http://www.phaget4.org/R/myImagePlot.R")
myImagePlot(table(xdata$f_hhsex, xdata$f_race), title = "No. of Households with High Asset by Sex and Race")

# xy plot
library(lattice)
xyplot(log(asset)~nown|f_race,data=xdata,type=c("p","r"))

xyplot(log(asset)~nown|f_hhsex,data=xdata,type=c("p","r"))

xyplot(log(asset)~age|f_race*f_hhsex,data=xdata,type=c("p","r"))
xyplot(log(asset)~debt|f_race*f_hhsex,data=xdata,type=c("p","r"))

xyplot(log(asset)~age|f_eeduc*f_hhsex,data=xdata,type=c("p","r"))

xyplot(log(asset)~age|f_eeduc*work,data=xdata,type=c("p","r"))

# Interaction Plot
# age 对 assets 的影响，会受到有没有 children 的干扰 
interaction.plot(xdata$f_aage,xdata$f_kid,log(xdata$asset),xlab="aweqw",ylab="qwe")
interaction.plot(xdata$f_hhsex,xdata$f_race,log(xdata$asset),xlab="aweqw",ylab="qwe")
interaction.plot(xdata$f_eeduc,xdata$f_kid,log(xdata$asset),xlab="aweqw",ylab="qwe")
interaction.plot(xdata$f_hhsex,xdata$f_eeduc,log(xdata$asset),xlab="aweqw",ylab="qwe")

# Bar Plot
library(RColorBrewer)
n <- 4
color <- brewer.pal(n,"Set1")
barplot(table(xdata$f_hhsex),col=color)
barplot(table(xdata$f_race),col=color, beside=F)

uxdata <- data[data$asset<=assetbox$stats[1],]
barplot(table(uxdata$f_hhsex),col=color)
# since the number of white is large, seperate as white or not
barplot(table(uxdata$f_race),col=color, beside=F) 

# PCA for metric  
metric.pca <- princomp(xdata[,c("stocks","saving","checking","asset")],cor=T)
biplot(metric.pca)


#****************************
#  Step 2: Model selection
#****************************
library(MASS)

# model 1
lm1 <- lm(asset~man+white+work+educ+age+kids,data=xdata)
summary(lm1)
summary(lm1)$adj.r.squared

AIC(lm1)

boxcox(lm1)
max(boxcox(lm1, plotit = F)$y)
boxcox(lm1, plotit = F)$x[which.max(boxcox(lm1, plotit = F)$y)] 
L<-boxcox(lm1, plotit = F)$x[which.max(boxcox(lm1, plotit = F)$y)] 
L # 利用boxcox 寻找y的指数, 一种广义幂变换方法, power transformation

# model 2
lm2 <- lm(asset^(L)~man+white+work+educ+age+kids,data=xdata)
summary(lm2)
summary(lm2)$adj.r.squared

AIC(lm2)

lm2step <- step(lm2) # ISL P143, 作用是删除不相关的变量
summary(lm2step)
AIC(lm2step)

# model 3
lm3 <- lm(asset^(L)~(man+white+work+educ+age+kids)^2,data=xdata)
summary(lm3)
summary(lm3)$adj.r.squared
AIC(lm3)

# model 4
lm4 <- step(lm3)
summary(lm4)
summary(lm4)$adj.r.squared

AIC(lm4)

par(mfrow=c(2,2))
plot(lm4,labels.id = NULL)
# 1. 检验线性： 残差图：数据之间是否存在非线性关系
# 2. 检验正态性：Q-Q plot:（Q代表分位数Quantile)是一种通过比较两个概率分布的分位数对这两个概率分布进行比较的概率图方法。
# 3. 检验 同方差性（homogeneity of the variance）: 若满足，水平线周围的点应随机分布，不会漏斗状
# 图4: y: studentized residuals (学生化残差)  x: leverage。 越右 越高杠杆值，越远离虚线 越高残差
#     high residuals->outlier: yi 远离模型预测值的点, yi 异常的点
#     high leverage-> xi 异常的点
par(mfrow=c(1,1))
plot(lm4,labels.id = NULL,which=4)

# outlier
xdata[1797,]
xdata[1797,c("educ","age","ntrad","kids","asset","work")]
xdata[102,c("educ","age","ntrad","kids","asset","work")]



#****************************
#  Step 3: Model Evaluation
#****************************
test.size <- 1/3
xdata.data <- test.set(xdata,test.size)

par(mfrow=c(2,2))  # 看一下 train test 数据分布一不一致
hist(xdata.data$train$asset)
hist(xdata.data$test$asset)
hist(xdata$asset)
par(mfrow=c(1,1))

age.lm1 <- lm(asset^(L)~man+white+work+educ+kids,data=xdata)
anova(age.lm1,lm2step) # 方差分析

age.lm4 <- lm(asset^(L)~(man+white+work+educ+kids)^2,data=xdata)
age.lm4.step <- step(age.lm4)
anova(age.lm4.step,lm4)
summary(lm4)$adj.r.squared
summary(age.lm4)$adj.r.squared

xdata.train.age.lm1 <-lm(asset^(L)~ (man+white+educ+kids)^2,data=xdata.data$train)
xdata.train.lm2 <-lm(asset^(L) ~ (man+white+educ+age+kids)^2,data=xdata.data$train)

pred.age.lm1 <- predict(xdata.train.age.lm1,newdata=xdata.data$test )
pred.lm2 <- predict(xdata.train.lm2,newdata=xdata.data$test )

pmse1.age <- mse(pred.age.lm1,xdata.data$test$asset)
pmse1.age # predicted mean square error
pmse2 <- mse(pred.lm2,xdata.data$test$asset)
pmse2

pmse1.age<=pmse2
pmse2-pmse1.age


race.lm1 <- lm(asset~man+age+educ+kids,data=xdata)
anova(race.lm1,lm1)

race.lm4 <- lm(asset^(L)~(man+age+educ+kids)^2,data=xdata)
anova(age.lm4,lm4)

summary(age.lm4)$adj.r.squared

xdata.train.race.lm1 <-lm(asset^(L)~ (man+white+age+kids)^2,data=xdata.data$train)
xdata.train.lm2 <-lm(asset^(L) ~ (man+white+educ+age+kids)^2,data=xdata.data$train)

pred.race.lm1 <- predict(xdata.train.race.lm1,newdata=xdata.data$test )
pred.lm2 <- predict(xdata.train.lm2,newdata=xdata.data$test )

pmse1.race <- mse(pred.race.lm1,xdata.data$test$asset)
pmse1.race
pmse2 <- mse(pred.lm2,xdata.data$test$asset)
pmse2

pmse1.race<=pmse2
pmse2-pmse1.race



man.lm1 <- lm(asset^(L)~ white+work+educ+age+kids,data=xdata)
anova(man.lm1,lm2step)

man.lm4 <- lm(asset^(L)~(white+educ+work+age+kids)^2,data=xdata)
man.lm4.step <- step(man.lm4)
anova(man.lm4.step,lm4)

summary(man.lm4.step)$adj.r.squared

xdata.train.race.lm1 <-lm(asset^(L)~ (man+educ+work+age+kids)^2,data=xdata.data$train)
xdata.train.lm2 <-lm(asset^(L) ~ (man+white+educ+work+age+kids)^2,data=xdata.data$train)

pred.race.lm1 <- predict(xdata.train.race.lm1,newdata=xdata.data$test )
pred.lm2 <- predict(xdata.train.lm2,newdata=xdata.data$test )

pmse1.race <- mse(pred.race.lm1,xdata.data$test$asset)
pmse1.race
pmse2 <- mse(pred.lm2,xdata.data$test$asset)
pmse2

pmse1.race<=pmse2
pmse2-pmse1.race



xdata.train.lm11 <-lm(asset^(L)~ (educ+white+work+age+kids)^2,data=xdata.data$train)
xdata.train.lm21 <-lm(asset^(L) ~ (man+white+work+educ+age+kids)^2,data=xdata.data$train)

xdata.train.lm1 <-step(xdata.train.lm11)
xdata.train.lm2 <-step(xdata.train.lm21)

pred.lm11 <- predict(xdata.train.lm1,newdata=xdata.data$test )
pred.lm21 <- predict(xdata.train.lm2,newdata=xdata.data$test )

pred.lm1 <- pred.lm11^(1/L)
pred.lm2 <- pred.lm21^(1/L)

pmse1 <- mse(pred.lm1,xdata.data$test$asset)
pmse1
pmse2 <- mse(pred.lm2,xdata.data$test$asset)
pmse2

pmse1<=pmse2
pmse2-pmse1



plot(smooth.spline(xdata.data$test$educ,pred.lm1),type='l',col='blue',xlab = "level of education")
points(smooth.spline(xdata.data$test$educ,pred.lm2),type='l',col='red')
legend('topleft',col=c('blue','red'),lty=1,legend=c('mo man','man'))

xyplot(pred.lm1 ~ xdata.data$test$educ,type=c("p","r"))
xyplot(pred.lm2 ~ xdata.data$test$educ,type=c("p","r"))



boxasset <- boxplot(xdata$asset,xdata.data$train$asset,xdata.data$test$asset,names=c('original','train','test'),main="assets",pch=1)
par(mfrow=(c(3,1)))
boxasset <- boxplot(xdata$educ,xdata.data$train$educ,xdata.data$test$educ,names=c('original','train','test'),main="educ",pch=1)
boxasset <- boxplot(xdata$age,xdata.data$train$age,xdata.data$test$age,names=c('original','train','test'),main="age",pch=1)
boxasset <- boxplot(xdata$kids,xdata.data$train$kids,xdata.data$test$kids,names=c('original','train','test'),main="kids",pch=1)
par(mfrow=(c(1,1)))

x <- c(length(which(xdata$man==1)), (length(xdata$man==1)-length(which(xdata$man==1))))
labels <- c("man", "woman")
pie(x)


###
xdata.train.race.lm1 <-lm(asset^(L)~ (man+educ+work+age+kids)^2,data=xdata.data$train)
xdata.train.lm2 <-lm(asset^(L) ~ (man+white+educ+work+age+kids)^2,data=xdata.data$train)

pred.race.lm1 <- predict(xdata.train.race.lm1,newdata=xdata.data$test )
pred.lm2 <- predict(xdata.train.lm2,newdata=xdata.data$test )

pmse1.race <- mse(pred.race.lm1,xdata.data$test$asset)
pmse1.race
pmse2 <- mse(pred.lm2,xdata.data$test$asset)
pmse2

pmse1.race<=pmse2
pmse2-pmse1.race

###

xdata.train.kidsage.lm1 <-lm(asset^(L)~ (man+white+educ+work+age+kids)^2-kids*age,data=xdata.data$train)
xdata.train.lm2 <-lm(asset^(L) ~ (man+white+educ+work+age+kids)^2,data=xdata.data$train)

summary(xdata.train.kidsage.lm1)

pred.kidsage.lm1 <- predict(xdata.train.kidsage.lm1,newdata=xdata.data$test )
pred.lm2 <- predict(xdata.train.lm2,newdata=xdata.data$test )

pmse1.kidsage <- mse(pred.race.lm1,xdata.data$test$asset)
pmse1.kidsage
pmse2 <- mse(pred.lm2,xdata.data$test$asset)
pmse2

pmse1.kidsage<=pmse2
pmse2-pmse1.kidsage

