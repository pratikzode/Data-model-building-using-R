hdata=read.csv(file="/home/sharan/Desktop/heartdisease.csv", header=TRUE,sep=",")
names(hdata)
str(hdata)
dim(hdata)
#dumifying variable num, if num!-0means, 
hdata$num[hdata$num>0]<-1
summary(hdata$num)
#barplot fate(i.e. hear disease 1 or not 0)
barplot(table(hdata$num),
        +         main="Fate", col="black")
#plot sex vs fate using mosaicplot
mosaicplot(hdata$sex ~ hdata$num,
           +            main="Fate by Gender", shade=FALSE,color=TRUE,
           +            xlab="Gender", ylab="Heart disease")
#fate by age using boxplot
boxplot(hdata$age ~ hdata$num,
        main="Fate by Age",
        ylab="Age",xlab="Heart disease")
#replacing ? by NA
levels(h$thal)[levels(h$thal)=="?"]<-NA
table(h$thal)

# 3   6   7 
# 166  18 117 
#replacing NA with max factor
h$thal[is.na(h$thal)]<-3
table(h$thal)

# 3   6   7 
# 168  18 117 

levels(h$ca)[levels(h$ca)=="?"]<-NA

table(h$ca)

# 0   1   2   3 
# 176  65  38  20 
h$ca[is.na(h$ca)]<-0
table(h$ca)

# 0   1   2   3 
# 180  65  38  20 

dim(hdata)
# [1] 303  14
hdata[, c(1)] <- sapply(hdata[, c(1)], as.numeric)
#for more info about sapply visit https://www.r-bloggers.com/apply-lapply-rapply-sapply-functions-in-r/
set.seed(123)
split = sample.split(hdata$num, SplitRatio = 2/3)
train_hdata = subset(hdata, split == TRUE)
test_hdata = subset(hdata, split == FALSE)
# train_hdata=hdata[1:212,]
# test_hdata=hdata[213:303,]
dim(train_hdata)
#[1] 212  14
dim(test_hdata)
#[1] 91 14

library(caTools)

#fitting simple linear Regression to the training set

regressor=lm(formula = num~age, data=train_hdata)

#predicting the test set result


hd_age_predict=predict(regressor, newdata=test_hdata)

round_age=hd_age_predict
rage=round(round_age)
library(e1071)
library(caret)
df=confusionMatrix(rage,test_hdata$num)


# Confusion Matrix and Statistics

# Reference
# Prediction  0  1
# 0 35 20
# 1 20 26
# 
# Accuracy : 0.604           
# 95% CI : (0.5017, 0.6999)
# No Information Rate : 0.5446          
# P-Value [Acc > NIR] : 0.1357          
# 
# Kappa : 0.2016          
# Mcnemar's Test P-Value : 1.0000          
#                                           
#             Sensitivity : 0.6364          
#             Specificity : 0.5652          
#          Pos Pred Value : 0.6364          
#          Neg Pred Value : 0.5652          
#              Prevalence : 0.5446          
#          Detection Rate : 0.3465          
#    Detection Prevalence : 0.5446          
#       Balanced Accuracy : 0.6008          
#                                           
#        'Positive' Class : 0      

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = train_hdata$age, y = train_hdata$num),
             colour = 'red') +
  geom_line(aes(x = train_hdata$age, y = predict(regressor, newdata = train_hdata)),
            colour = 'blue') +
  ggtitle('attack vs age (Training set)') +
  xlab('age') +
  ylab('attack')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_hdata$age, y = test_hdata$num),
             colour = 'red') +
  geom_line(aes(x = train_hdata$age, y = predict(regressor, newdata = train_hdata)),
            colour = 'blue') +
  ggtitle('attack vs age (Test set)') +
  xlab('age') +
  ylab('attack')
#----------------------------------------------------------
#prediction using multiple linear regression

#fitting simple linear Regression to the training set
regressor=lm(formula = num~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope, data=train_hdata)

#predicting the test set result
hd_age_predict=predict(regressor, newdata=test_hdata)

round_age=hd_age_predict
rage=round(round_age)
library(e1071)
library(caret)
df=confusionMatrix(rage,test_hdata$num)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  0  1
# 0 45 13
# 1 10 33
# 
# Accuracy : 0.7723          
# 95% CI : (0.6782, 0.8498)
# No Information Rate : 0.5446          
# P-Value [Acc > NIR] : 1.749e-06       
# 
# Kappa : 0.5384          
# Mcnemar's Test P-Value : 0.6767          
#                                           
#             Sensitivity : 0.8182          
#             Specificity : 0.7174          
#          Pos Pred Value : 0.7759          
#          Neg Pred Value : 0.7674          
#              Prevalence : 0.5446          
#          Detection Rate : 0.4455          
#    Detection Prevalence : 0.5743          
#       Balanced Accuracy : 0.7678          
#                                           
#        'Positive' Class : 0               
#                                           

#---------------------------------------------------------------------------------
#Prediction using KNN
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
h1data<-hdata
prc_n <- as.data.frame(lapply(h1data[1:11], normalize))
#prc_n[12:13]<-h1data[12:13]

traink_hdata=prc_n[1:212,]
testk_hdata=prc_n[213:303,]
library(class)
prc_train_labels <- hdata[1:212, 14]
prc_test_labels <- hdata[213:303, 14] 
prc_test_pred <- knn(train = traink_hdata, test = testk_hdata,cl = prc_train_labels, k=17)


library(gmodels)
CrossTable(x=prc_test_labels,y=prc_test_pred,prop.chisq = FALSE)
table(prc_test_labels,prc_test_pred)
#              prc_test_pred
# prc_test_labels  0  1
#               0 39  9
#               1 17 26

# round_age=hd_age_predict
# rage=round(round_age)
library(e1071)
library(caret)
df=confusionMatrix(prc_test_labels,prc_test_pred)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  0  1
# 0 39  9
# 1 17 26
# 
# Accuracy : 0.7143        
# 95% CI : (0.61, 0.8041)
# No Information Rate : 0.6154        
# P-Value [Acc > NIR] : 0.0317        
# 
# Kappa : 0.4212        
# Mcnemar's Test P-Value : 0.1698        
#                                         
#             Sensitivity : 0.6964        
#             Specificity : 0.7429        
#          Pos Pred Value : 0.8125        
#          Neg Pred Value : 0.6047        
#              Prevalence : 0.6154        
#          Detection Rate : 0.4286        
#    Detection Prevalence : 0.5275        
#       Balanced Accuracy : 0.7196        
#                                         
#        'Positive' Class : 0   
#--------------------------------------------------------------------------
# Naive bayes classifyer needs catagorical data for prediction
h1data<-hdata
h1data$age=factor(h1data$age)
h1data$sex=factor(h1data$sex)
h1data$cp=factor(h1data$cp)
h1data$trestbps=factor(h1data$trestbps)
h1data$chol=factor(h1data$chol)
h1data$fbs=factor(h1data$fbs)
h1data$restecg=factor(h1data$restecg)
h1data$thalach=factor(h1data$thalach)
h1data$exang=factor$exang
h1data$exang=factor(h1data$exang)
h1data$oldpeak=factor(h1data$oldpeak)
h1data$slope=factor(h1data$slope)
h1data$num=factor(h1data$num)

trainnb_hdata=h1data[1:212,]
testnb_hdata=h1data[213:303,-14]
library(e1071)
classifier <- naiveBayes(num ~age+sex+cp+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope,trainnb_hdata)
prediction <- predict(classifier, testnb_hdata ,type="class")
prediction
# [1] 0 0 0 1 0 1 1 0 0 0 0 1 1 0 0 0 1 1 0 1 0 0 0 1 1 1 0 0 0 0 0 0 0 1 0 1 0 0 1 1 1 0 0 0
# [45] 0 0 0 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 1 0 0 0 1 1 0 1 0 0 1 1 0 0 0 1 0 1 1 1 0 1 0 0 1
# [89] 1 0 0
# Levels: 0 1
table(prediction, h1data[213:303,14])

# prediction  0  1
# 0 41 18
# 1  7 25
library(e1071)
library(caret)
df=confusionMatrix(h1data[213:303,14],prediction)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  0  1
# 0 41  7
# 1 18 25
# 
# Accuracy : 0.7253          
# 95% CI : (0.6217, 0.8137)
# No Information Rate : 0.6484          
# P-Value [Acc > NIR] : 0.07486         
# 
# Kappa : 0.4414          
# Mcnemar's Test P-Value : 0.04550         
#                                           
#             Sensitivity : 0.6949          
#             Specificity : 0.7812          
#          Pos Pred Value : 0.8542          
#          Neg Pred Value : 0.5814          
#              Prevalence : 0.6484          
#          Detection Rate : 0.4505          
#    Detection Prevalence : 0.5275          
#       Balanced Accuracy : 0.7381          
#                                           
#        'Positive' Class : 0               
#                                  
