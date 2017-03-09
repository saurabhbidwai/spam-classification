#set the working directory
setwd("E:/Aegis/Machine Learning/Data")

#attaching data.table package
library("data.table")

#read data
spam=fread("spam.csv")

#analysing data 
str(spam)
summary(spam)
head(spam)

#checking NA values
anyNA(spam)

#scaling
minmax=function(x){
  nnew=(x-min(x))/(max(x)-min(x))
}

spam[,55:57]=apply(spam[,55:57],2,minmax)

#checking correlation 
library("corrplot")
corrplot(cor(spam))

#Divide the grade 1 and grade 0 data
class1=subset(spam,V58==1)
class0=subset(spam,V58==0)

#taking sample of 70% grade 1 and grade 0 data
ind0=sample(1:nrow(class0),round(0.70*(nrow(class0))))
ind1=sample(1:nrow(class1),round(0.70*(nrow(class1))))

train1=class1[ind1,]
train0=class0[ind0,]
test1=class1[-ind1,]
test0=class0[-ind0,]

#creating final train and test data
train=rbind(train1,train0)
test=rbind(test1,test0)

#model 1
fitt=glm(formula = V58~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+
         V27+V28+V29+V30+V31+V32+V33+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49+V50+V51+
         V52+V53+V54+V55+V56 , family = binomial("logit"), data = train)

#summary of model 1
summary(fitt)

#applying step() function on model 1
step(fitt)

#model2
fitt1=glm(formula = V58 ~ V1 + V2 + V4 + V5 + V6 + V7 + V8 + V9  + 
            V15 + V16 + V17 + V19 + V20 + V21 + V23 + V24 + V25 + V26 + 
            V27 + V28  + V30 + V33   + V39  + V42 + 
            V44 + V45 + V46 + V48 + V49 + V52 + V53 + V54 , 
          family = binomial("logit"), data = train)

#summary of model 2
summary(fitt1)

#checking multicollinearity 
library("car")
vif(fitt1)

#model 3
fitt2=glm(formula = V58 ~ V1 + V2 + V4 + V5 + V6 + V7 + V8 + V9 + V15 + 
            V16 + V17 + V20 + V21 + V23 + V24 + V25 + V26 + V27 + V28 + 
            V33 + V39 + V42 + V44 + V45 + V46 + V48 + V49 + V52 + V53 + 
            V54, family = binomial("logit"), data = train)

#summary of model 3
summary(fitt2)

#checking multicollinearity
vif(fitt2)

#predict the test data by using model 3
out=predict(fitt2,test,type="response")

#classify the predicted outcome to class 1 or class 0 
out=ifelse(out>0.5,1,0)
out

#checking the accuracy
count=0
accuracy=0
for(i in 1:nrow(test)){
  if(out[i]==test[i,11]){
    count=count+1
  }
  accuracy=c(accuracy,count/nrow(test))
}
accuracy

#checking root mean square error
RMSE=sqrt(mean((out-test[,58])^2))
RMSE

#confusion matrix
library("caret")
confusionMatrix(out,test$V58)

#ROC curve
library(ROCR)
# Compute AUC for predicting Class with the model
pred <- prediction(out, test$V58)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
