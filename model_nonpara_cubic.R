######for non-parametric quantile regression
##fitting natural cubic spline
#train_set
pred_train=matrix(ncol=length(taus_1), nrow=length(train_set$age))
for (i in 1:length(taus_1)) {
  pred_train[,i]=predict(rq(BMI~ns(age, knots=30), tau=taus_1[i], data =N_train), train_set)
  lines(train_set$age, pred_train[,i], col="grey30")
}

sum_matrix_1=matrix(rep(train_set$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_1-pred_train))/length(train_set$age)

#F1
pred_train_1=matrix(ncol=length(taus_1), nrow=length(F1$age))
for (i in 1:length(taus_1)) {
  pred_train_1[,i]=predict(rq(BMI~ns(age, knots=30), tau=taus_1[i], data =N1), F1)
  lines(F1$age, pred_train_1[,i], col="grey30")
}

sum_matrix_2=matrix(rep(F1$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_2-pred_train_1))/length(F1$age)

#F2
pred_train_2=matrix(ncol=length(taus_1), nrow=length(F2$age))
for (i in 1:length(taus_1)) {
  pred_train_2[,i]=predict(rq(BMI~ns(age, knots=30), tau=taus_1[i], data =N2), F2)
  lines(F2$age, pred_train_2[,i], col="grey30")
}

sum_matrix_3=matrix(rep(F2$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_3-pred_train_2))/length(F2$age)

#F3
pred_train_3=matrix(ncol=length(taus_1), nrow=length(F3$age))
for (i in 1:length(taus_1)) {
  pred_train_3[,i]=predict(rq(BMI~ns(age, knots=30), tau=taus_1[i], data =N3), F3)
  lines(F3$age, pred_train_3[,i], col="grey30")
}

sum_matrix_4=matrix(rep(F3$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_4-pred_train_3))/length(F3$age)

#F4
pred_train_4=matrix(ncol=length(taus_1), nrow=length(F4$age))
for (i in 1:length(taus_1)) {
  pred_train_4[,i]=predict(rq(BMI~ns(age, knots=30), tau=taus_1[i], data =N4), F4)
  lines(F4$age, pred_train_4[,i], col="grey30")
}

sum_matrix_5=matrix(rep(F4$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_5-pred_train_4))/length(F4$age)