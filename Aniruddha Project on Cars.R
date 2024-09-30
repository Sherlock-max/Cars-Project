D=read.csv("dataset.csv")
View(D)
attach(D)
summary(D)
names(D)=c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","Y")
View(D)
attach(D)
summary(D)
fit=lm(price~highway_mpg+city_mpg+peak_rpm+horsepower+compression_ratio+stroke + bore+engine_size+curb_weight+
         height+width+length+wheel_base) 
summary(fit)
fit=lm(price~peak_rpm+horsepower+compression_ratio+stroke+
         engine_size+curb_weight+height+width)
summary(fit)
y=fitted(fit)
e=residuals(fit)
plot(e,main="residual plot",ylab="residual")
plot(e,y,xlab="residual",ylab="fit",main="plot of residual vs fit")
e=residuals(fit)
qqnorm(e)
par(new=T)
qqline(e,col="red")
shapiro.test(e)
##
## Shapiro-Wilk normality test
##
##data: e
##W=0.96143,p-value=5.778e-05
library(car)
vif(fit)
library(car)
durbinWatsonTest(fit,alternative="two.sided")
install.packages("lmtest")
library(lmtest)
bptest(fit)
p=8
n=186
h<-hatvalues(fit)
outlier<-which(h>2*p/n)
COVARIANCE.RATIO<-covratio(fit)
cov.outlier<-which(abs(COVARIANCE.RATIO-1)>3*p/n)
influential.points<-sort(unique(c(outlier,cov.outlier)))
influential.points
D=read.csv("dataset.csv")
D=as.matrix(D)
v=D[-c(1,7,8,9,14,32,39,41,42,43,57,58,59,60,75,86,87,88,96,111,112,113,117,136,154,168,179,180)]
View(v)
v=as.data.frame(v)
attach(v)
fit1=lm(price~peak_rpm+horsepower+compression_ratio+stroke+
          engine_size+curb_weight+height+width)
summary(fit1)
e=residuals(fit)
qqnorm(e)
par(new=T)
qqline(e,col="red")
shapiro.test(e)
