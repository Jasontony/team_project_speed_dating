library(arules)
library(arulesViz)
library(e1071)
library(randomForest)
library(MASS)
library(caret)
library(rpart)
library(pROC)
library(ipred)
library(plyr)
library(DMwR)
library(igraph)
set.seed(2018)
############Association Rule#################
#We first divide the data set by gender, covert the data type from ordinal to nominal
options(warn=0)
patterns = random.patterns(nItems = 1000)
trans = random.transactions(nItems = 1000, nTrans = 1000, method = "agrawal",  patterns = patterns)
#Examining men's/women's decision
data=read.csv("female_male.csv")  #change this to "male_female.csv" when want to examine women's decision
data<-na.omit(data) #omit NA data
data<-data.frame(sapply(data,as.factor))

Adult = data
rules <- apriori(data,parameter = list(support = 0.01, confidence = 0.5),appearance = list(rhs=c("dec_o=0", "dec_o=1"),default="lhs"))
inspect(head(sort(rules, by="confidence"),10)) #set the rules

if(FALSE)
{
  plot(rules);
  head(quality(rules));
  plot(rules, measure=c("support","lift"), shading="confidence")
  plot(rules, shading="order", control=list(main ="Two-key plot"))
  
  sel = plot(rules, measure=c("support","lift"), shading="confidence", interactive=TRUE)
}
subrules = rules[quality(rules)$confidence > 0.8]
inspect(head(sort(rules, by="confidence"),10))

if(FALSE)
{
  plot(subrules, method="matrix", measure="lift")
  plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))
  plot(subrules, method="matrix3D", measure="lift")
  plot(subrules, method="matrix3D", measure="lift", control = list(reorder=TRUE))
  plot(subrules, method="matrix", measure=c("lift", "confidence"))
  plot(subrules, method="matrix", measure=c("lift","confidence"), control = list(reorder=TRUE));
  plot(rules, method="grouped");
  plot(rules, method="grouped", control=list(k=50));
  sel = plot(rules, method="grouped", interactive=TRUE);
}
#3D graphs, matrix, interactive graphs
subrules2 = head(sort(rules, by="lift"), 30)

if(FALSE)
{
  plot(subrules2, method="graph")
  plot(subrules2, method="graph", control=list(type="items"))
  plot(subrules2, method="paracoord")
  plot(subrules2, method="paracoord", control=list(reorder=TRUE))
}

oneRule = sample(rules, 10)
inspect(oneRule)

fsets = eclat(trans, parameter = list(support = 0.05), control = list(verbose=FALSE));
singleItems = fsets[size(items(fsets)) == 1];
singleSupport = quality(singleItems)$support;
names(singleSupport) = unlist(LIST(items(singleItems), decode = FALSE));
head(singleSupport, n = 5);
itemsetList = LIST(items(fsets), decode = FALSE);
allConfidence = quality(fsets)$support / sapply(itemsetList, function(x)
  max(singleSupport[as.character(x)]));

quality(fsets) = cbind(quality(fsets), allConfidence);
summary(fsets);

library("arules");
library("arulesViz");
options(bitmapType="cairo")
for(name in c("female_male.csv")) #change this when want to examine female
{
  print(name)
  data=read.csv(name)
  #data<-na.omit(data)
  
  for (n in colnames(data)[31:36])
  {
    print(head(data[[n]]))
    m = mean(data[[n]], na.rm=TRUE)
    #str=sprintf("c_%s", n)    
    data[[n]] <- cut(data[[n]], c(0, (2.0/3.0)*m, (4.0/3.0)*m, 100), labels=c("L", "M", "H"))
  }
  #changing values to low, median and high 
  for(i in 9:30)
  {
    data[i]<-cut(as.matrix(data[i]), seq(0,10,3), right=FALSE, labels=c("L", "M", "H"))
  }	
  #print(str(data))
  data <- cbind(data[9:30],data[31:36],data[59],data[60])
  data<-data.frame(sapply(data,as.factor))
  if(name!="female_male.csv")
  {
    rules <- apriori(data,parameter = list(support = 0.005, confidence = 0.85),appearance = list(rhs=c("dec_o=1"),default="lhs"))
  }#change dec_o=0 to get when men says no 
  else
  {
    rules <- apriori(data,parameter = list(support = 0.01, confidence = 0.8),appearance = list(rhs=c("dec_o=1"),default="lhs"))
  }
  rules.sorted <- sort(rules, by="lift")
  inspect(head(rules.sorted,10));
  setEPS()
  cairo_ps("f_rules_out.eps")	 #save this to desktop
  plot(rules)
  dev.off()
  cairo_ps("f_graph_out.eps")	
  plot(head(sort(rules, by = "lift"), n=20), method = "graph", control=list(cex=.8))	
  dev.off()
  #subrules <- rules[quality(rules)$confidence > 0.8]
  #plot(subrules, method="matrix", measure="lift")
  setEPS()
  cairo_ps("f_group_out.eps")		
  plot(rules, method="grouped")
  dev.off()
  setEPS()
  subrules2 <- head(sort(rules, by="lift"), 20)
  cairo_ps("f_para_out.eps")	
  plot(subrules2, method="paracoord")
  dev.off()
  if(FALSE)
  {
    subset.matrix <- is.subset(rules.sorted, rules.sorted)
    subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
    redundant <- colSums(subset.matrix, na.rm=T) >= 1
    # remove redundant rules
    rules.pruned <- rules.sorted[!redundant]
    inspect(head(rules.pruned,10))
  }
}
getOption("bitmapType")

############Decision Tree#################
#Looking at decision trees for female and male to see what variables effect their decision
#Match for males
mydata=read.csv("male_female.csv")
mydata[,"train"] <- ifelse(runif(nrow(mydata))<0.80,1,0)
trainColNum <- grep("train",names(mydata))
traindata <- mydata[mydata$train==1,-trainColNum]
testdata <- mydata[mydata$train==0,-trainColNum]

# grow tree
fit_male <- rpart(dec_o ~ attr1_1+sinc1_1+intel1_1+fun1_1+amb1_1+shar1_1+attr3_1+sinc3_1+fun3_1+intel3_1+amb3_1+attr1_1+sinc1_1+intel1_1+fun1_1+amb1_1+shar1_1+attr3_1+sinc3_1+fun3_1+intel3_1+amb3_1+sports+tvsports+exercise+dining+museums+art+hiking+gaming+clubbing+reading+tv+theater+movies+concerts+music+shopping+yoga
                  ,method="class", data=traindata,control=rpart.control(minsplit=2, minbucket=1, cp=0.0025))
print(fit_male)
plotcp(fit_male) #cross-validaiton results
summary(fit_male)
fit1<-predict(fit_male,newdata=testdata,type="class")
source("ROC_AUC.R") #this is a function that allows us to calculate confusion matrix
ConfusionMatrix(fit1, testdata$dec_o) #Calculate our AUC
#Positive prediction 69.34%, AUC 56.02%, Accuracy 63.85%
setEPS()
postscript("male.eps") #save to desktop
plot(fit, uniform=TRUE,main="Match for Males")
text(fit, cex=.8)
dev.off()
labels(fit)

#Match for females
mydata=read.csv("female_male.csv")
mydata[,"train"] <- ifelse(runif(nrow(mydata))<0.80,1,0)
trainColNum <- grep("train",names(mydata))
traindata <- mydata[mydata$train==1,-trainColNum]
testdata <- mydata[mydata$train==0,-trainColNum]

# grow tree
fit_female<- rpart(dec_o ~ attr1_1+sinc1_1+intel1_1+fun1_1+amb1_1+shar1_1+attr3_1+sinc3_1+fun3_1+intel3_1+amb3_1+attr1_1+sinc1_1+intel1_1+fun1_1+amb1_1+shar1_1+attr3_1+sinc3_1+fun3_1+intel3_1+amb3_1+sports+tvsports+exercise+dining+museums+art+hiking+gaming+clubbing+reading+tv+theater+movies+concerts+music+shopping+yoga
                   ,method="class", data=traindata,control=rpart.control(minsplit=2, minbucket=1, cp=0.0025))
print(fit_female)
plotcp(fit_female) #cross-validaiton results
summary(fit_female)
fit2<-predict(fit_female,newdata=testdata,type="class")
#Calculate our AUC
ConfusionMatrix(fit2, testdata$dec_o)
#Positie prediction 64.9% AUC 65.16%, Accuracy 65.17%

setEPS()
postscript("female.eps") #save to desktop
plot(fit, uniform=TRUE,main="Match for Females")
text(fit, cex=.8)
dev.off()
labels(fit)

############Base Line Model#################
base<-read.csv("clean data.csv") #read the data with both gender
mean(base$match) #use the mean as our baseline
#baseline model
data.Testing$pred_null <-  1

TP <- sum((data.Testing$pred_null== 1 )*(data.Testing$match==1))
FP <- sum((data.Testing$pred_null == 1 )*(data.Testing$match==0))
FN <- sum((data.Testing$pred_null == 0 )*(data.Testing$match==1))
TN <- sum((data.Testing$pred_null == 0 )*(data.Testing$match==0))
SVM.FPR <- FP / (FP + TN)
SVM.TPR <- TP / (TP + FN)

accuracy <- (TP+TN)/(TP+TN+FP+FN)
Error_rate <- (FP+FN)/(TP+TN+FP+FN)
sensitivity <- TP/(TP+FN)
false_positive_rate <- FP/(FP+TN)
specificity <- TN/(FP+TN)
precision <- TP/(TP+FP)

accuracy
Error_rate
sensitivity
false_positive_rate
specificity
precision
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c(SVM.FPR ), c(SVM.TPR))
text( c( SVM.FPR ), c(SVM.TPR+.05), labels=c("Null"))

data.Testing$pred_null <-  0

TP <- sum((data.Testing$pred_null== 1 )*(data.Testing$match==1))
FP <- sum((data.Testing$pred_null == 1 )*(data.Testing$match==0))
FN <- sum((data.Testing$pred_null == 0 )*(data.Testing$match==1))
TN <- sum((data.Testing$pred_null == 0 )*(data.Testing$match==0))
SVM.FPR <- FP / (FP + TN)
SVM.TPR <- TP / (TP + FN)

accuracy <- (TP+TN)/(TP+TN+FP+FN)
Error_rate <- (FP+FN)/(TP+TN+FP+FN)
sensitivity <- TP/(TP+FN)
false_positive_rate <- FP/(FP+TN)
specificity <- TN/(FP+TN)
precision <- TP/(TP+FP)

accuracy
Error_rate
sensitivity
false_positive_rate
specificity
precision
plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)
points( c(SVM.FPR ), c(SVM.TPR))
text( c( SVM.FPR ), c(SVM.TPR+.05), labels=c("Null"))

############Random Forest#################
d1<-read.csv("clean-data.csv") #read in the data
variablesToEliminate <- c("income","undergra","income_o","undergra_o","positin1","expnum","expnum_o","gender","gender_o",
                          "X","iid","id") #keeping only variables we want
d1<- d1[ , !(names(d1) %in% variablesToEliminate)]
d1$match[d1$match=="1"] <- "Y"
d1$match[d1$match=="0"] <- "N"
d1$match <- as.factor(d1$match)
#Since we have a very imbalanced data, we first decide to use our data using SMOTE function
d1 <- SMOTE(match ~ ., d1, perc.over = 500,perc.under=100)
#spling the data
inTrain <- createDataPartition(y=d1$match, p=0.8, list=FALSE)
data.Training <- d1[inTrain, ]
data.Testing <- d1[-inTrain, ]
control <- trainControl(method="cv", number=3,classProbs = TRUE, summaryFunction=twoClassSummary)

#running the Random Forest model
cv_rf = caret::train(match~., data = data.Training, model = "rf", trControl = control,na.action=na.exclude)
importance <- varImp(cv_rf, scale=FALSE) #check importance
print(importance)

#Now run only with the variables that are imporatant 
d1<-read.csv("clean-data.csv")
variablesuse <-c("int_corr","order","pid","shar1_1_o","position","idg","partner","age",
                 "date_o","clubbing_o","attr1_1_o","attr2_1_o","fun1_1","clubbing","amb1_1",
                 "fun1_1_o","attr2_1","sinc1_1_o","intel2_1_o","intel2_1","match")
d1<- d1[ , (names(d1) %in% variablesuse)]
#spling the data
d1$match[d1$match=="1"] <- "Y"
d1$match[d1$match=="0"] <- "N"
d1$match<-as.factor(d1$match)
inTrain <- createDataPartition(y=d1$match, p=0.8, list=FALSE)
data.Training <- d1[inTrain, ]
data.Testing <- d1[-inTrain, ]

cv_rf_1 <- train(match~., data = data.Training, model = "rf", trControl = control,allowParallel=TRUE,
                      na.action=na.exclude)
print(cv_rf_1)
data.Testing<-na.omit(data.Testing) #omit the NA values
predict_cf<-predict(cv_rf_1,newdata=data.Testing) 
confusionMatrix(predict_cf,data.Testing$match,positive = 'Y')
#Accuracy 91.4%, Sensitivity 97.6%, Sensitivity 86.4%, Postive predicted value 97.8%

############SVM#################
d1<-read.csv("clean-data.csv") #read in the data
d1 <- d1[,14:116] #keep only the variables we need 
variablesnotneed <-c("income","undergra","income_o","undergra_o","positin1","expnum","expnum_o")
d1<- d1[ , !(names(d1) %in% variablesnotneed)]
d1$match[d1$match=="1"] <- "Y"
d1$match[d1$match=="0"] <- "N"
d1$match <- as.factor(d1$match)
d1 <- SMOTE(match ~ ., d1, perc.over = 500,perc.under=100) #balance the data first
inTrain <- createDataPartition(y=d1$match, p=0.8, list=FALSE)
data.Training <- d1[inTrain, ]
data.Testing <- d1[-inTrain, ]

#running the SVM model
grid <- data.frame(C=c(0.1,0.25,0.5,0.75,1.0))
svm_result1<-train(match~.,data=data.Training,method="svmLinear",tuneGrid=grid,trControl=control,metirc="ROC",na.action=na.exclude)
importance1 <- varImp(svm_results1, scale=FALSE) #check importance
print(importance1)

#Now run only with the variables that are imporatant 
d1<-read.csv("clean-data.csv")
variablesuse <-c("int_corr","order","pid","shar1_1_o","position","idg","partner","age",
                 "date_o","clubbing_o","attr1_1_o","attr2_1_o","fun1_1","clubbing","amb1_1",
                 "fun1_1_o","attr2_1","sinc1_1_o","intel2_1_o","intel2_1","match")
d1<- d1[ , (names(d1) %in% variablesuse)]
#spling the data
d1$match[d1$match=="1"] <- "Y"
d1$match[d1$match=="0"] <- "N"
d1$match<-as.factor(d1$match)
d1 <- SMOTE(match ~ ., d1, perc.over = 500,perc.under=100) #balance the data first
inTrain <- createDataPartition(y=d1$match, p=0.8, list=FALSE)
data.Training <- d1[inTrain, ]
data.Testing <- d1[-inTrain, ]
svm_result2<-train(match~.,data=data.Training,method="svmRadialCost",tuneGrid=grid,trControl=control,metirc="ROC",na.action=na.exclude)
print(svm_result2)
#Predicting on the testing set
data.Testing<-na.omit(data.Testing) #ger rid of NAs
pred_svm1 <- predict(svm_result2, data.Testing)
#checking for ROC curve and how good this model is
confusionMatrix(pred_svm1, data.Testing$match,positive = 'Y') 
#Accuracy 85% Specificity 88.2% Sensitivity 83.2% Positive Prediction 89.5% 

############Neural Network#################
#First we're trying to run the neural network model on the entire data set and picking the best variables 
ANN_selectvariable <- train(match~.,data=data.Training,method="nnet",tuneGrid=expand.grid(size=c(10), decay=c(0.1)),
             trControl=control,metric="ROC",maxit=1000,MaxNWts=10280,na.action=na.exclude)
print(ANN_selectvariable) 
importance <- varImp(ANN, scale=FALSE) #check importance
print(importance)
"nnet variable importance

only 20 most important variables shown (out of 1025)

Overall
clubbing   0.7593642
music      0.7259400
museums_o  0.7171406
imprelig   0.7134054
exercise_o 0.6411545
partner    0.6377540
sinc3_1    0.6139694
gaming_o   0.5928699
go_out_o   0.5780818
age        0.5757329
intel3_1_o 0.5733067
attr3_1    0.5597455
imprelig_o 0.5542109
reading_o  0.5527901
tv         0.5506313
sports_o   0.5371276
reading    0.5307403
fun3_1_o   0.5144376
tvsports   0.5104649
tvsports_o 0.5100769"
#these are the variables we decided to pick for our ANN model
d2<-read.csv("clean-data.csv")
variablesuse <- c("clubbing","music","museums_o","imprelig","exercise_o","partner","sinc3_1","gaming_o","go_out_o",
                  "age","intel3_1_o","attr3_1","imprelig_o","reading_o","tv","sports_o","reading","fun3_1_o","tvsports","tvsports_o","match") #keeping only variables we want
d2<- d2[ , (names(d2) %in% variablesuse)]
d2$match[d2$match=="1"] <- "Y"
d2$match[d2$match=="0"] <- "N"
d2$match <- as.factor(d2$match)
d2 <- SMOTE(match ~ ., d2, perc.over = 500,perc.under=100) #balance the data first
inTrain <- createDataPartition(y=d2$match, p=0.8, list=FALSE)
data.Training <- d2[inTrain, ]
data.Testing <- d2[-inTrain, ]
data.Training <- na.omit(data.Training) #remove NA's
#Running our second model
ANN1<- train(match~.,data=data.Training,method="nnet",tuneGrid=expand.grid(size=c(10), decay=c(0.1)),
             trControl=control,metric="ROC",maxit=1000,na.action=na.exclude)
print(ANN1)
#Try to plot and get a visual of this model
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(ANN1,nid=T)
#Test it on out of sample data
ANN1_prob<-predict(ANN1,data.Testing,na.action=na.pass)
head(ANN1_prob)
confusionMatrix(ANN1_prob, data.Testing$match,positive = 'Y') 
#Accuracy 79.7%, Specificity 83.0%, Sensitivity: 77.0%, Positive prediction value 84.2%

############Putting everything together#################
d3<-read.csv("clean-data.csv") #read in the data again
variablesuse1 <- c("int_corr","order","pid","shar1_1_o","position","idg","partner","age",
                 "date_o","clubbing_o","attr1_1_o","attr2_1_o","fun1_1","clubbing","amb1_1",
"fun1_1_o","attr2_1","sinc1_1_o","intel2_1_o","intel2_1","music","match","museums_o","imprelig","partner",
                 "tv","intel3_1_o","attr3_1","imprelig_o","reading_o","sports_o","reading","fun3_1_o","tvsports",
                 "tvsports_o")
d3<- d3[ , (names(d3) %in% variablesuse1)]
d3$match[d3$match=="1"] <- "Y"
d3$match[d3$match=="0"] <- "N"
d3$match <- as.factor(d3$match)
d3 <- SMOTE(match ~ ., d3, perc.over = 500,perc.under=100) #balance the data first
inTrain <- createDataPartition(y=d3$match, p=0.8, list=FALSE)
data.Training <- d3[inTrain, ]
data.Testing <- d3[-inTrain, ]
##Random Forest##
RF<-train(match~.,data=d3,method="rf",trControl=control,metric="ROC",prox=TRUE,allowParallel=TRUE,na.action=na.omit)
print(RF)
#97% ROC

##SVM##
grid <- data.frame(C=c(0.1,0.25,0.5,0.75,1.0))
svm<-train(match~.,data=d3,method="svmRadialCost",tuneGrid=grid,trControl=control,metirc="ROC",na.action=na.omit)
print(svm)
#92% ROC

##Neural Network##
ann<-train(match~.,data=d3,method="nnet",tuneGrid=expand.grid(size=c(10), decay=c(0.1)),
              trControl=control,metric="ROC",maxit=1000,na.action=na.omit)
print(ann)

#90% ROC

#Confusion Matrix
predict_rf<-predict(RF,newdata=data.Testing) 
confusionMatrix(predict_rf,data.Testing$match,positive = 'Y')
psvm<- predict(svm, data.Testing)
confusionMatrix(psvm, data.Testing$match,positive = 'Y') 
ann_prob<-predict(ann,data.Testing,na.action=na.pass)
confusionMatrix(ann_prob, data.Testing$match,positive = 'Y') 

rALLMETHODS <- resamples(list(SVMRadial=svm,RandomForest=RF,ANN=ann),na.action=na.omit)
bwplot(rALLMETHODS,metric="ROC",main="All Classification Algorithms Compared")
modelCor(rALLMETHODS)
splom(rALLMETHODS)

############Predict Match#################
#Finally we're going to predict the matches for all combination of pids and iids using our 
#best model, which is random forest
d4<-read.csv("finaldata_cluster.csv") #read in the data again
variablesuse1 <- c("iid","int_corr","order","shar1_1_o","position","idg","pid","partner","age",
                   "date_o","clubbing_o","attr1_1_o","attr2_1_o","fun1_1","clubbing","amb1_1",
                   "fun1_1_o","attr2_1","sinc1_1_o","intel2_1_o","intel2_1","music","museums_o","imprelig","partner",
                   "tv","intel3_1_o","attr3_1","imprelig_o","reading_o","sports_o","reading","fun3_1_o","tvsports",
                   "tvsports_o")
d4<- d4[ , (names(d4) %in% variablesuse1)] 
d4$match <- predict(RF, d4[,-1])
finaldata_predict <- d4 %>% filter(match=='Y') #let's see how our data did for predicting
write.csv("finaldata_cluster.csv")

#load the data back in
net <- read.csv("final_cluster.csv")
# Create graph data frame from test match results
net.graph <- graph.data.frame(net, directed = FALSE)

# Apply the fastgreedy clustering algorithm
net.clusters <- fastgreedy.community(net.graph, modularity = TRUE, membership= TRUE, weights = NULL)
#community sizes
sizes((list(membership=cutat(net.clusters, 25))))

# Find the cluster for each participant, export to excel to identify same cluster and match
com<-(list(membership=cutat(net.clusters, 25)))
com <- as.data.frame(com)

#work completed in excel to identify same cluster matches
write.csv2(com, file ="community.csv")

# Plot which cluster each participant is located.
plot(net.clusters, net.graph)
