spam1=read.csv("E:/Aegis/Machine Learning/Data/spam.csv")
set.seed(10)
str(spam1)
table(spam1$V58)
summary(spam1)

spam=scale(spam1[,-58])

summary(spam)

fit1=prcomp(spam[,-58] ,retx = T, center = TRUE)

str(fit1)
fit1$sdev
fit1$rotation
summary(fit1)
screeplot(fit1,type="line")
#biplot(fit1,scale=0)
dim(fit1$x)

fit1_var=(fit1$sdev)^2
fit1_var[1:20]

prop_varex = fit1_var/sum(fit1_var)
prop_varex[1:20]

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#plot((fit1$sdev/sum(fit1$sdev))*100,type="l")

pca_spam=fit1$x[,1:3]

newspam=data.frame(pca_spam,spam1$V58)

class11=subset(newspam,newspam$spam1.V58==1)
class00=subset(newspam,newspam$spam1.V58==0)

s11=sample(nrow(class11),0.7*(nrow(class11)))
s00=sample(nrow(class00),0.7*(nrow(class00)))

train11=class11[s11,]
test11=class11[-s11,]

train00=class00[s00,]
test00=class00[-s00,]

train10=rbind(train11,train00)
test10=rbind(test11,test00)


log_fit=glm((spam1.V58)~.,family=binomial("logit"),train10)


#checking summary of our logistic model
summary(log_fit)

#predicting the grade of our test data 
out=predict(log_fit,test10,type="response")

#Rounding of the grade vaue
out=ifelse(out>0.5,1,0)

library(caret)
confusionMatrix(test10$spam1.V58,out)
