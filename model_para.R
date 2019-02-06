#select 80% of the samples
smp_size=floor(0.8*nrow(new.weight))

set.seed(13)
train.ind=sample(seq_len(nrow(new.weight)), size = smp_size)

train=new.weight[train.ind,]
test=new.weight[-train.ind,]

#qtSVM
library(liquidSVM)
library(MASS)
library(caret)
library(caTools)
library(base)
ind=createDataPartition(new.weight$BMI, p=0.8, list = FALSE)
trainDF=new.weight[ind,]
testDF=new.weight[-ind,]

ControlParameters=trainControl(method = "cv",
                               number = 5,
                               savePredictions = TRUE,
                               classProbs = TRUE)
model=train(BMI~poly(age,3), data=new.weight, method=)

require(caret)
flds=createFolds(new.weight$BMI, k=5, list = TRUE, returnTrain = FALSE)
names(flds)[1]="train"
train_set=new.weight[flds$train,]
F1=new.weight[flds[[2]],]
F2=new.weight[flds[[3]],]
F3=new.weight[flds[[4]],]
F4=new.weight[flds[[5]],]


N_train=new.weight[-flds$train,]
N1=new.weight[-flds[[2]],]
N2=new.weight[-flds[[3]],]
N3=new.weight[-flds[[4]],]
N4=new.weight[-flds[[5]],]


sorted.weight.N_train=data.frame(age=seq(min(N_train$age), max(N_train$age)), 0.1, data=N_train)


#for parametric quantile regression
#train_set
pred_train=matrix(ncol=length(taus_1), nrow=length(train_set$age))
for (i in 1:length(taus_1)) {
  pred_train[,i]=predict(rq(BMI~poly(age,3), tau=taus_1[i], data =N_train), train_set)
  lines(train_set$age, pred_train[,i], col="grey30")
}

sum_matrix_1=matrix(rep(train_set$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_1-pred_train))/length(train_set$age)

#F1
pred_train_1=matrix(ncol=length(taus_1), nrow=length(F1$age))
for (i in 1:length(taus_1)) {
  pred_train_1[,i]=predict(rq(BMI~poly(age,3), tau=taus_1[i], data =N1), F1)
  lines(F1$age, pred_train_1[,i], col="grey30")
}

sum_matrix_2=matrix(rep(F1$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_2-pred_train_1))/length(F1$age)

#F2
pred_train_2=matrix(ncol=length(taus_1), nrow=length(F2$age))
for (i in 1:length(taus_1)) {
  pred_train_2[,i]=predict(rq(BMI~poly(age,3), tau=taus_1[i], data =N2), F2)
  lines(F2$age, pred_train_2[,i], col="grey30")
}

sum_matrix_3=matrix(rep(F2$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_3-pred_train_2))/length(F2$age)

#F3
pred_train_3=matrix(ncol=length(taus_1), nrow=length(F3$age))
for (i in 1:length(taus_1)) {
  pred_train_3[,i]=predict(rq(BMI~poly(age,3), tau=taus_1[i], data =N3), F3)
  lines(F3$age, pred_train_3[,i], col="grey30")
}

sum_matrix_4=matrix(rep(F3$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_4-pred_train_3))/length(F3$age)

#F4
pred_train_4=matrix(ncol=length(taus_1), nrow=length(F4$age))
for (i in 1:length(taus_1)) {
  pred_train_4[,i]=predict(rq(BMI~poly(age,3), tau=taus_1[i], data =N4), F4)
  lines(F4$age, pred_train_4[,i], col="grey30")
}

sum_matrix_5=matrix(rep(F4$BMI, each=7), ncol=7, byrow=TRUE)
sum(abs(sum_matrix_5-pred_train_4))/length(F4$age)





