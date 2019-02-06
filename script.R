#use the dataset from VGAMdata to analyse the BMI versus age for Eurpoean-type women in xs.nz
library(VGAMdata)
attach(xs.nz)

women.eth0=subset(xs.nz, sex=="F" & ethnicity=="European")
women.eth0 <- transform(women.eth0, BMI = weight/height^2)
attach(women.eth0)

#plot a hexagonal binning plot of BMI versus age for Eurpoean-type women in xs.nz
ggplot(data=women.eth0, type=lm(BMI~age),aes(x=age, y=BMI), cex=0.3)+
  stat_binhex(color="orange")+
  theme_bw()+
  scale_fill_gradient(low="white", high="orange")+
  labs(x="Age", Y="BMI")



hbin=hexbin(women.eth0$BMI, women.eth0$age)
hvp=hexViewport(hbin)
reg=lm(women.eth0$BMI~women.eth0$age)
hexVP.abline(hvp,reg$coefficients[1],reg$coefficients[2], col="red", lty=1,lwd=2)

library(quantreg)
attach(women.eth0)
plot(age,BMI, cex=0.25, type="n", xlab="Age",ylab="BMI")
points(age, BMI, cex=0.5, col="gray")
#abline(lm(BMI~age+age^2), lty=2, col="red")
reg2=lm(BMI~age+age^2)
pred1=predict(reg2, data=women.eth0)
lines(age, pred1, lwd=.1, col="blue")

#create a new data frame and clear up the data
women.weight=subset(women.eth0, select = c("age", "BMI"))
new.weight=na.omit(women.weight)


attach(new.weight)
reg2=lm(BMI~poly(age,2))
new.weight$pred1=predict(reg2, new.weight)
plot(BMI~age, col="grey")
lines(age, new.weight$pred1, col="red")

sorted.weight=data.frame(age=seq(min(age), max(age)), 0.1)
sorted.weight$pred1=predict(reg2, sorted.weight)
plot(BMI~age, col="orange")
lines(sorted.weight$age, sorted.weight$pred1, col="red")

#plot the polynomial regression line and confidence interval
reg3=lm(BMI~poly(age,3)) #we use poly=3 because it has the highest R-squared
sorted.weight$pred2=predict(reg3, sorted.weight)
pred3=predict(reg3, sorted.weight, interval="confidence")
plot(BMI~age, col="grey", cex=0.5)
lines(sorted.weight$age, sorted.weight$pred2, lty="dashed", col="red")
lines(sorted.weight$age, pred3[,3], lty="dashed", col="red")
lines(sorted.weight$age, pred3[,2], lty="dashed", col="red")



sorted.weight=data.frame(age=seq(min(age), max(age)), 0.1)
sorted.weight$pred1=predict(reg2, sorted.weight)
plot(BMI~age, col="orange")
lines(sorted.weight$age, sorted.weight$pred1, col="red")

#parametric quantile regression (linear)
plot(BMI~age, col="grey", cex=0.5, xlab="Age", ylab="BMI")

reg4=rq(BMI~poly(age,3), tau=0.5)
sorted.weight$pred_para=predict(reg4, sorted.weight)
plot(BMI~age, col="grey", cex=0.5)
lines(sorted.weight$age, sorted.weight$pred_para, col="red")



taus <- c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95)


pred=matrix(ncol=length(taus), nrow=73)
for (i in 1:length(taus)) {
    pred[,i]=predict(rq(BMI~poly(age,3), tau=taus[i]), sorted.weight)
    lines(sorted.weight$age, pred[,i], col="blue")
}



#non-parametric quantile regression
#here we use splines
#we can test different types of splines
#1. fitting B-spline
library(splines)
library(mgcv)
plot(BMI~age, col="grey", cex=0.5, xlab="Age", ylab="BMI")
X=model.matrix(BMI~bs(age, df=15))
fit=rq(BMI~bs(age, df=25), tau=0.5)
pred_non=predict(rq(BMI~bs(age, knots=30), tau=0.5),sorted.weight)
lines(sorted.weight$age, pred_non, col="green")

pred_non=matrix(ncol=length(taus), nrow=73)
for (i in 1:length(taus)) {
  pred_non[,i]=predict(rq(BMI~bs(age,knots = 30), tau=taus[i]), sorted.weight)
  lines(sorted.weight$age, pred_non[,i], col="grey30")
}

#2. fitting natural cubic spline
fit_2=rq(BMI~ns(age, knots=30))
pred_non_2=predict(rq(BMI~ns(age, knots=30), tau=0.5),sorted.weight)
lines(sorted.weight$age, pred_non_2, col="blue")

pred_non_2=matrix(ncol=length(taus), nrow=73)
for (i in 1:length(taus)) {
  pred_non_2[,i]=predict(rq(BMI~ns(age, knots = 50), tau=taus[i]), sorted.weight)
  lines(sorted.weight$age, pred_non_2[,i], col="grey30")
}

#3. lprq method
xx=age-mean(age)

for (i in hs){
  h=hs[i]
  fit=lprq(BMI, age, h=h, tau=0.5)
  lines(fit$xx, fit$fv, lty=i)
}
legend(80,45, c("h=1", "h=2", "h=3", "h=4"), lty=1:length(hs))

#different bandwidth
hs=c(1,2,3,4)
for(i in hs){
  h=hs[i]
  fit=lprq(age, BMI, h=h, tau = 0.5)
  lines(fit$xx, fit$fv, lty=i, col="red")
}

legend(60,40, c("h=1", "h=2", "h=3", "h=4"), lty=1:length(hs), col="red")

#different quantiles
pred_lprq=lprq(age, BMI, h=4, tau=0.5)
lines(pred_lprq$xx, pred_lprq$fv, col="blue")

for (i in 1:length(taus)) {
  taus1=taus[i]
  pred_lprq_1=lprq(age, BMI, h=4, tau=taus[i])
  lines(pred_lprq_1$xx, pred_lprq_1$fv, col="grey30")
}

lines(pred_lprq_1[i]$xx, pred_lprq_1[i]$fv, col="grey30")

