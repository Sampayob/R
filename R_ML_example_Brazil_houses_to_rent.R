library(ggplot2)
library(dplyr)

df <-  read.csv('E:\\Datasets\\houses_to_rent.csv')

#####EDA##########

print(summary(df))
print(str(df))
nrow(df)
ncol(df)
head(df)
colnames(df)

#####Data Wrangling##########

#Drop 'X' column
df$X <- NULL

#changing factor type

df$floor<- as.numeric(df$floor)

#Replace all '-' value with NA

df[df == '-'] <- NA

#Fixing numeric column:removing 'R$' and transform to numeric

for (i in 9:13){
  df[, i] <- substr(df[, i], 3, 7)
  df[, i] <- gsub(",","",df[, i])
  df[, i] <- as.numeric(df[, i])
}
head(df)

# Missings/NA
any(is.na(df))

#Missings plot
library(Amelia)
missmap(df, y.at=c(1),y.labels=c(''),col=c("yellow", "black"), legend=FALSE)
#removing NA
df  <- na.omit(df)
#or with: df <- df[complete.cases(df), ]
missmap(df, y.at=c(1),y.labels=c(''),col=c("yellow", "black"), legend=FALSE)


#####Visualization##########
library(ggplot2)

#Histogram - Count plot
pl <- ggplot(df, aes(x=total))
pl2 <-  pl + geom_histogram(binwidth = 0.1, color ='red', fill = 'pink', alpha= 0.4)
pl3 <-  pl2 + xlab('Rooms') + ylab('Count') 
print(pl3 + ggtitle('ROOMS COUNT'))

#Histogram - Count plot
pl <- ggplot(df, aes(x=rooms))
pl2 <-  pl + geom_histogram(binwidth = 0.1, color ='red', fill = 'pink', alpha= 0.4)
pl3 <-  pl2 + xlab('Rooms') + ylab('Count') 
print(pl3 + ggtitle('ROOMS COUNT'))


# rent.amount according to number of rooms
pl <- ggplot(data=df, aes(x=rent.amount)) 
pl + geom_bar(aes(fill=factor(rooms)))+ theme_bw()

pl <- ggplot(df, aes(x=rent.amount, y=rooms))
pl2 <- pl + geom_point(size=5, aes(color=parking.spaces))
print(pl2  + theme_bw())

#number of rooms vs number of bathrooms
pl <- ggplot(df,aes(x=rooms,y=bathroom))
print(pl + geom_bin2d(binwidth=c(3,1)) + scale_fill_gradient(high = 'red', low = 'blue'))

library(hexbin)
print(pl + geom_hex()) + scale_fill_gradient(high = 'red', low = 'blue')

print(pl + geom_density2d()) + scale_fill_gradient(high = 'red', low = 'blue')

#####Machine Learning##########

##Data prep####
#Binning classification feature: total price
df_ml <-df
bins <- cut(df_ml$total,breaks=4)
table(bins)
df_ml$total_bins <- cut(df_ml$total,breaks=4, labels = c(0,1,2,3))
as.data.frame(table(df_ml$total_bins))

#droping unneccesary columns

df_ml$total <- NULL

##Correlation matrix####
library(corrplot)
#Numeric columns
num.cols <- sapply(df,is.numeric)
#filter
cor.data <- cor(df[,num.cols])
print(corrplot(cor.data, method='color'))

#Sample equal random rows according to classification labeled column
as.data.frame(table(df_ml$total_bins))
library(dplyr)
df_ml <- df_ml %>% group_by(total_bins) %>% sample_n(407)
table(df_ml$total_bins)
head(df_ml)

##Model####

#Train and test split
library(caTools)
set.seed(101)
split <- sample.split(df_ml$total_bins, SplitRatio = 0.7)
train <- subset(df_ml, split == TRUE)
test <-subset(df_ml, split == FALSE)

##Algorithms
#Logistic regression###
#log.model <- glm(label ~., family=binomial(link='logit'), data = )
log.model <- glm(total_bins ~., family=binomial(link='logit'), data = train)
summary(log.model)

##Predictions
fitted.probabilities <-  predict(log.model, newdata = test, type = 'response')
fitted.results  <- ifelse(fitted.probabilities>0.5,1,0)

##Evaluation
misClassError <- mean(fitted.results != test$total_bins)
print(paste('Accuracy',1-misClassError))

#Confusion matrix
table(test$total_bins, fitted.results>0.5)


#Decision Tree###
library(rpart)
tree<- rpart(total_bins ~.,method = 'class',data=train)
summary(tree)
library(rpart.plot)
prp(tree)
#predictions
tree.preds <- predict(tree,test)
tree.preds <- as.data.frame(tree.preds)
joiner <- function(x){
  if (x >= 0.5){
    return('Yes')
  }else{
    return('No')
  }
}
tree.preds$total_bins <-sapply(tree.preds$Yes,joiner)
print(head(tree.preds))


#Random Forest###
library(randomForest)
rf.model<- randomForest(total_bins ~.,data=train, importante=TRUE)

#Evaluation
rf.model$confusion
rf.model$confusion[, 'class.error']
rf.model$importance

#Predictions
rf.preds <-predict(rf.model,test)
table(rf.preds,test$total_bins)

#SVM###
library(e1071)
help("svm")
model <- svm(total_bins ~., data = train)
summary(model)
#predictions
pred.values <- predict(model, test[1:13])
table(pred.values,test$total_bins)

#tune results
tuned.results <-tune(svm,train.x = total_bins ~., data = train,kernel ='radial',gamma=c(0.1))
print(summary(tuned.results))

#KNN###
#Model
library(class)
set.seed(101)

#Evaluation
predicted <- knn(train[1:4],test[1:4],train$total_bins, k=4)
misclass.error <- mean(test$total_bins != predicted)

print(misclass.error)
predicted<-NULL
error.rate<-NULL

#determine best k value
for (i in 1:20){
  set.seed(101)
  predicted<- knn(train[1:4],test[1:4],train$total_bins, k=i)
  error.rate[i] <- mean(test$total_bins != predicted)
}

library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)

ggplot(error.df, aes(k.values,error.rate)) + geom_point() + geom_line(lty= 'dotted', color='red')

#using new k value

predicted <- knn(train[1:4],test[1:4],train$total_bins, k=5)
misclass.error <- mean(test$total_bins != predicted)
print(misclass.error)
