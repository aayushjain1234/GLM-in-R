getwd()
setwd("C:/Users/hp/Desktop/Stats project/GLM")
data<- na.omit(read.csv("weatherAUS.csv"))
head(data)


x1=data$MinTemp
x2=data$Evaporation
x3=data$Sunshine
x4=data$WindGustSpeed
y=data$RainToday


dataset<-data.frame(x1,x2,x3,x4,y)
trainset.glm<-glm(y ~., data=dataset, family='binomial')
summary(trainset.glm)

set.seed(1390)
n<-nrow(dataset)

index<-sample(n,n*(80/100))
train<-dataset[index,]
test<-dataset[-index,]
#dim(test)

trainset.glm<- glm(train$y ~., data = train, family='binomial')

# prediction
phat_i=predict(trainset.glm , test, type='response')
pred=rep(0, length(phat_i))
pred[phat_i>=0.5]=1

# confusion matrix
actual=test$y
conf_mat=table(pred,actual)
accuracy=mean(pred==actual)

