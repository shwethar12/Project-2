library(car) # advanced scatter plots 
library(corrplot) # plot correlations 
library(dplyr) # data aggregates 
library(Hmisc) # for correlation test of multiple variables 
library(gplots)
library(psych)
library(gmodels) # cross tabulation
library(gplots) # plot means with CI 
library(ggplot2)
set.seed(123)
options(scipen=99)
dev.off()
install.packages("xlsx")
library(xlsx)
#Loading the complete dataset
mnc_complete<-read.xlsx(file.choose(),1, header=TRUE)
View(mnc_complete)
str(mnc_complete)

#Checked for missing values. As the number of rows returned is 0, there are no missing values
mnc_complete[!complete.cases(mnc_complete),]

#Changing the variable c.manipulator from num to factor
mnc_complete$C.MANIPULATOR <- as.factor(mnc_complete$C.MANIPULATOR)

#Class balance: there exists class imbalance in the complete dataset
count<-table(mnc_complete$C.MANIPULATOR)
count
#   0    1 
#1200   39 


#------------------------------------------------------------------------------------------------

#Question (a)

#The M-score calculated using beneish model helps in predicting the manipulations 
#Using beneish model to calculate mscore using formula below
mscore<-(-4.84) + (0.92*mnc_complete$DSRI) + (0.528*mnc_complete$GMI) + (0.404*mnc_complete$AQI) + (0.892*mnc_complete$SGI) + (0.115*mnc_complete$DEPI) - (0.172*mnc_complete$SGAI) + (4.679*mnc_complete$ACCR) - (0.327*mnc_complete$LEVI)

mnc_complete <- data.frame(mnc_complete,mscore)
str(mnc_complete)
View(mnc_complete)

#Setting all the rows for a new column to No
actual_manipulation <- rep("No",nrow(mnc_complete))

#Setting all the rows to Yes for values of mscore less than -2.22
actual_manipulation[mnc_complete$mscore > -2.22] <- "Yes"

#Adding that variable to the dataset
mnc_complete <- data.frame(mnc_complete,actual_manipulation)
str(mnc_complete)
View(mnc_complete)

#Releveling to focus on Yes level for the predictions
mnc_complete$actual_manipulation <- relevel(mnc_complete$actual_manipulation, ref = "Yes")
mnc_complete$Manipulater <- relevel(mnc_complete$Manipulater, ref = "Yes")


library(caret)
confusionMatrix(mnc_complete$actual_manipulation,mnc_complete$Manipulater)
#            Reference
#Prediction   Yes  No
#        Yes  39 371
#        No    0 829

#Sensitivity : 1.00000
#Specificity : 0.69083  
#Accuracy    : 0.7006 

# By comparing the manipulation results from the dataset to the manipulation calculated from mscore having a threshold at -2.22, we get sensitivity at 100% which says that there are no such cases 
# where the companies are manipulators but predicted as non-manipulator. But with 69% specificity, we have 371 cases which are not manipulators but predicted as manipulators. This causes a wastage of resources og regulators
#This makes the beneish model with -2.22 as a threshold as a bad model for Indian data

result<-confusionMatrix(mnc_complete$actual_manipulation,mnc_complete$Manipulater)
precision <- result$byClass['Pos Pred Value'] 
precision
#0.09512195
#Precision with 9% serves as a bad model. Will further look into improvisng the model.

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Question(b)
#As there is imbalance in the instances for the column c.manipulator, if we build a model using this data, we will experience accuracy paradox. 
#This is a situation where the the data is so biased towards one class( here, class 'No') that everytime a a new instance is added, irrespective of the actual result, it will result to the majority class
#as the accuracy is high it is very likely to predict one class regardless of the data it is asked to predict.
#To handle the data imbalance, we can either apply sampling techniques which involves undersamplying or oversampling the instances of the class or use algorithm technique like random forest or naive bayes classifier
#Using undersampling technique, the instances of the majority classes are removed 
#Using oversampling technique, the instances of the minority classes are sampled with replacement.
#SMOTE (Synthetic Minority Over-sampling Technique), is a technique of 'oversampling' the minority class in a classification problem
#Or, we can run ensemble methods over the unbalanced data and predict the performance. Ensemble methods like random forest can be used as it handled data with high bias. XGBoost is another method. 
#for our analysis, We are starting to work on SMOTE and XGBoost method and see how it performs as compared to the other models that we will be building. 
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Question (c)

#Removing the two columns mscore and actual_manipulator as it is not required for further analysis
mnc_complete$mscore<-NULL
mnc_complete$actual_manipulation<-NULL

#creating a subset from the complete data set for manipulators=yes 
mnc_yes<-mnc_complete[mnc_complete$Manipulater=='Yes',]
str(mnc_yes)
mnc_no<-mnc_complete[mnc_complete$Manipulater=='No',]
str(mnc_no)

#As we need 100 samples of 220cases  with 39 manipulators, we take different samples of No class from mnc_no dataframe and combine it with Yes class from mnc_yes dataframe

library(ROSE)
#creating a list to hold all the 100 samples 
mnc_list<- vector(mode="list", length=100)
for(i in 1:100){
  set.seed(i) #So you can reproduce these results
  x1 <- mnc_no[sample(1:nrow(mnc_no), size=181, replace=FALSE),]
  y1 <- mnc_yes[sample(1:nrow(mnc_yes), size=39, replace=FALSE),]
  x2 <- merge(x1,y1,all.x=TRUE, all.y=TRUE) #Merging the sampled data from 2 tables
  
  z<-ovun.sample(Manipulater~.,data=x2,p=0.5, seed=1, method="under")$data #using undersampling technique. for every iteration we have a distribution of 76 cases approximately
  #sampled_dataset[i]<-z
  #Performing stepwise selection for variables
  full<-glm(Manipulater~.,data=z[,-c(1,11)], family="binomial")
  full
  null <- glm(Manipulater~1.,data=z, family="binomial")
  null
  #Storing the values in list for each iteration 
  w<-step(null, scope = list(lower=null, upper=full), direction="both")
  mnc_list[[i]]<-w$coefficients
}


#Taking count of each variable to determine the most important ones to consider for building logistic regression model
DSRI<- 0 
GMI  <- 0 
AQI <- 0 
SGI <- 0 
DEPI <- 0 
SGAI <- 0 
ACCR <- 0 
LEVI <- 0

for(i in 1:100){
  
  
  for(g in 1:length(rownames(data.frame(mnc_list[i])))){
    ifelse(rownames(data.frame(mnc_list[i]))[g]=="AQI",(AQI=AQI+1),0)
    ifelse(rownames(data.frame(mnc_list[i]))[g]=="DSRI",(DSRI=DSRI+1),0)
    ifelse(rownames(data.frame(mnc_list[i]))[g]=="GMI",(GMI=GMI+1),0)
    ifelse(rownames(data.frame(mnc_list[i]))[g]=="SGI",(SGI=SGI+1),0)
    ifelse(rownames(data.frame(mnc_list[i]))[g]=="DEPI",(DEPI=DEPI+1),0)
    ifelse(rownames(data.frame(mnc_list[i]))[g]=="SGAI",(SGAI=SGAI+1),0)
    ifelse(rownames(data.frame(mnc_list[i]))[g]=="ACCR",(ACCR=ACCR+1),0)
    ifelse(rownames(data.frame(mnc_list[i]))[g]=="LEVI",(LEVI=LEVI+1),0)
    
    
  }
}

final_variables<-data.frame("AQI"=AQI,"DSRI"=DSRI,"GMI"=GMI,"SGI"=SGI,"DEPI"=DEPI,"SGAI"=SGAI, "ACCR"=ACCR,"LEVI"=LEVI)
final_variables
#AQI DSRI GMI SGI DEPI SGAI ACCR LEVI
#100  100  90 100   36   16  100   17

#From this we select the top variables and proceeding to build logestic regression
mnc_complete$Manipulater<-relevel(mnc_complete$Manipulater,ref="Yes")

levels(mnc_complete$Manipulater)


#Building a glm model on the under-sampled dataset(76 cases with 11 variables)

z$Manipulater<-relevel(z$Manipulater,ref="Yes")

lm_first_sam <- glm(Manipulater~AQI+DSRI+GMI+SGI+ACCR, data =z, family = "binomial")
summary(lm_first_sam)


#Probability formula sampled
#b(x)=b0+b1x1+b2x2+b3x3+b4x4+b5x5
#b(x)=7.805-0.5626x1-2.5642x2-0.9105x3-2.0202x4-9.6613x5


#reference: Manipulater=YES
#P=1/1+exp(b(x))

#Predictor: Manipulator=NO
#p=exp(b(x))/1+exp(b(x))

#------------------------------------------------------------------------------------------------------------------------------------------

#Question (d)


summary(lm_first_sam)
#model with 76 variables
#Min       1Q   Median       3Q      Max  
#-1.6893  -0.6378   0.0000   0.5200   2.0032 

#All the variables in the model have a significant relationship with response variable
#With 95% confidence, For every one unit increase in AQI, the log of odds for earning manipulation=NO decreases by 0.5626 
#With 95% confidence, For every one unit increase in DSRI, the log of odds for earning manipulation=NO decreases by 2.5642
#With 95% confidence, For every one unit increase in GMI, the log of odds for earning manipulation=NO decreases by 0.9105. 
#With 95% confidence, For every one unit increase in SGI, the log of odds for earning manipulation=NO decreases by 2.02. 
#With 95% confidence, For every one unit increase in ACCR, the log of odds for earning manipulation=NO decreases by9.66. 

#Null deviance: 105.306  on 75  degrees of freedom
#Residual deviance:  59.105  on 70  degrees of freedom
#AIC: 71.105

#to check if the model is good we take the difference of the deviances
dev01<- with(lm_first_sam, null.deviance - deviance) 
dev01 #46.20065 with 5 degrees of freedom

#The number of predictors
dev11<-with(lm_first_sam, df.null, df.residual)
dev11 #75

#Finding the p-value for the model
pvalue1<-with(lm_first_sam, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
pvalue1 # 0.000000008266682
#As the p-value <0.05, we can say that the model is significantly better than a null model with 9 degrees of freedom

#Representation using a plot

earn_pred <- data.frame(earn_prob=lm_first_sam$fitted.values,Manipulater=z$Manipulater,ID=z$Company.ID)
earn_pred <- earn_pred[order(earn_pred$earn_prob, decreasing=FALSE),]
earn_pred$rank <- 1:nrow(earn_pred)
View(earn_pred)

## We can plot the data...
library(ggplot2)
ggplot(data=earn_pred, aes(x=rank, y=earn_prob)) +
  geom_point(aes(color=Manipulater), alpha=1, shape=4, stroke=2) +
  xlab("Index") + ylab("Predicted probability of earning manipulation")

#Interpretation: Probability of earning manipulation=YES has a low probability than the earning manipulation=NO which has a high probability

#Prediction on data using the threshold value = 0.5

#Prediction on complete dataset
lm_pred_sam <- predict(lm_first_sam, newdata = mnc_complete, type = "response")
lm_pred_sam
range(lm_pred_sam)
#0.0000000000000002220446 0.9999999999999997779554
class <- ifelse(lm_pred_sam >=  0.5, "No", "Yes")
class

class <- as.factor(class)
class <- relevel(class, ref = "Yes")
conf<-confusionMatrix(as.factor(class),mnc_complete$Manipulater)
conf$byClass
conf


table1 <- table(class,mnc_complete$Manipulater)
TP1 <- table1[1]
FN1 <- table1[2]
FP1 <- table1[3]
TN1 <- table1[4]

table1
#class
#     Yes   No
#Yes   32  158
#No     7 1042

#Accuracy
Accuracy1<-(table1[1]+table1[4])/nrow(mnc_complete)
Accuracy1#86.6% accuracy

#Recall
TP1/(TP1+FN1)
#82% recall

#Precision
TP1/(TP1+FP1)
#16% Precision

#Trying different cut off points
list_cutoff<-vector(mode="list", length=10000)
table101_sam<-data.frame(cutoff=numeric(), Precision=numeric(), Recall=numeric())
library(caret)
prec123<-rep(NA,10)
rec123<-rep(NA,10)
set.seed(123)
rand_cut_sam <-runif(10)
for(i in 1:10)
{
  
  #rand_cut[i]<-runif(1)
  class <- ifelse(lm_pred_sam >=rand_cut_sam[i], "No", "Yes")
  class <- as.factor(class)
  class <- relevel(class, ref = "Yes")
  class101<-confusionMatrix(class,mnc_complete$Manipulater)
  prec123[i]<-class101$byClass[5] 
  rec123[i]<-class101$byClass[6]
  
  table101_sam[i,]<-c(rand_cut_sam[i],prec123[i],rec123[i])
  
}
table101_sam
# cutoff  Precision    Recall
# 1  0.2875775 0.20720721 0.5897436
# 2  0.7883051 0.08496732 1.0000000
# 3  0.4089769 0.18620690 0.6923077
# 4  0.8830174 0.05313351 1.0000000
# 5  0.9404673 0.03975535 1.0000000
# 6  0.0455565 0.30232558 0.3333333
# 7  0.5281055 0.15841584 0.8205128
# 8  0.8924190 0.05038760 1.0000000
# 9  0.5514350 0.15740741 0.8717949
# 10 0.4566147 0.17751479 0.7692308

#-------------------------------------------------------------------------------------------------------------

#Question (e)
#Youden's index
#Formula = max {sensitivity(p) - specificity(p) -1}

#Cost based method
#P10 = Proportion of manipulators classified as non-manipulators.
#P01 = Proportion of non-manipulators classified as manipulators. 

# as we are more interested in having lesser false positives in our predictions, we will give more penalty to P10 and lesser to p01
# choosing 0.8 for P10 and 0.2 for P01
# formula = min (0.8*P10+0.2*P01)

#Choosing randomly 10 cutoff points between 0 and 1 and calculating accuracy, precision, recall, youdens index, costbased method
table101_y_sam<-data.frame(cutoff=numeric(), Precision=numeric(), Recall=numeric(), Specificity=numeric(),Youden_Index=numeric(),Cost_based=numeric())
library(caret)
prec123<-rep(NA,10)
sens123<-rep(NA,10)
spec123<-rep(NA,10)
youden<-rep(NA,10)
costbased<-rep(NA,10)
m<-rep(NA,10)
n<-rep(NA,10)

for(i in 1:10)
{
  
  #rand_cut[i]<-runif(1)#randomly generates numbers between 0 and 1
  class <- ifelse(lm_pred_sam >=rand_cut_sam[i], "No", "Yes") #Classifying
  class <- as.factor(class)
  class <- relevel(class, ref = "Yes") #Relevling to make predictions on No and have positive class as Yes
  class101<-confusionMatrix(class,mnc_complete$Manipulater) #Confusion matrix
  prec123[i]<-class101$byClass[5] #precision
  sens123[i]<-class101$byClass[1] #sensitivity
  spec123[i]<-class101$byClass[2] #specificity
  youden[i]<-sens123[i]+spec123[i]-1 #Youden's index
  m[i] <- class101$table[3]/nrow(mnc_complete)
  n[i] <- class101$table[2]/nrow(mnc_complete)
  costbased[i]<- (0.8*m[i]+0.2*n[i]) #Cost based method
  table101_y_sam[i,]<-c(rand_cut_sam[i],prec123[i],sens123[i],spec123[i],youden[i],costbased[i]) #A table that holds all the values
  
}
View(table101_y_sam)
table101_y_sam <- table101_y_sam[order(table101_y_sam$Cost_based),]
table101_y_sam

# cutoff  Precision    Recall Specificity Youden_Index Cost_based
# 6  0.0455565 0.30232558 0.3333333   0.9750000    0.3083333 0.02356739
# 1  0.2875775 0.20720721 0.5897436   0.9266667    0.5164103 0.05940274
# 3  0.4089769 0.18620690 0.6923077   0.9016667    0.5939744 0.07812752
# 10 0.4566147 0.17751479 0.7692308   0.8841667    0.6533974 0.09120258
# 7  0.5281055 0.15841584 0.8205128   0.8583333    0.6788462 0.11089588
# 9  0.5514350 0.15740741 0.8717949   0.8483333    0.7201282 0.11832123
# 2  0.7883051 0.08496732 1.0000000   0.6500000    0.6500000 0.27118644
# 4  0.8830174 0.05313351 1.0000000   0.4208333    0.4208333 0.44874899
# 8  0.8924190 0.05038760 1.0000000   0.3875000    0.3875000 0.47457627
# 5  0.9404673 0.03975535 1.0000000   0.2150000    0.2150000 0.60823245

#From this 10 different cut off points we have taken, we can see the point for with the high sensitivity, youden_index and low cost_based: 0.4566147

lm_pred_sam <- predict(lm_first_sam, newdata = mnc_complete, type = "response")
lm_pred_sam
range(lm_pred_sam)
#0.0000000000000002220446 0.9999999999999997779554
class99 <- ifelse(lm_pred_sam >=  0.4566147, "No", "Yes")
class99

class99 <- as.factor(class99)
class99 <- relevel(class99, ref = "Yes")
conf99<-confusionMatrix(as.factor(class99),mnc_complete$Manipulater)
conf99$byClass
conf99


table99 <- table(class99,mnc_complete$Manipulater)
TP1 <- table99[1]
FN1 <- table99[2]
FP1 <- table99[3]
TN1 <- table99[4]

table99
#class
#     Yes   No
#Yes   32  158
#No     7 1042

#Accuracy
Accuracy2<-(table99[1]+table99[4])/nrow(mnc_complete)
#88.6% accuracy

#Recall
TP1/(TP1+FN1)
#79.4% recall

#Precision
TP1/(TP1+FP1)
#18.9% Precision
#-----------------------------------------------------------------------------------------------------------------------------------------------

#Question (f)

#Logistic regression model built on the best youden's index and the least cost based method gives the following results: 
Final_Evaluation[2,]

#Logistic Regression with youden's index and low cost based method to determine cutoff point 
#Accuracy: 0.886198547215496
#Precision 0.189024390243902 
# Recall  0.794871794871795
#Cut off point: 0.45333416

#Which is better than the previous model with cut off 0.5 

#We are going to calculate the M-score using the the following predictors derved from the stepwise varaible selection used in the models previously
#AQI+DSRI+GMI+SGI+ACCR
mscore<-(-4.84) + (0.92*mnc_complete$DSRI) + (0.528*mnc_complete$GMI) + (0.404*mnc_complete$AQI) + (0.892*mnc_complete$SGI)(4.679*mnc_complete$ACCR)

#-----------------------------------------------------------------------------------------------------------------------------------------------

#Question (g)

#AS we saw above, the distribution of the variable is unbalanced, so we will need to balance the data
#by using the concept of OverSampling
library(ROSE)
set.seed(1234)
#Balanced dataset
mnc_balanced_x2<- ovun.sample(Manipulater~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI,
                              data = x2,method = "both", 
                              N=220)$data

#Now checking the distribution of data in this balanced Dataset
mnc_balanced_x2$Manipulater<- relevel(mnc_balanced_x2$Manipulater,ref="Yes")
table(mnc_balanced_x2$Manipulater)
#Yes  No 
#110 110 


library(rpart)
library(rpart.plot)
library(caret)
set.seed(1010)
#Building a classification tree with cp=0 and parameter=gini impurity index
S_Data_x2 <- rpart(Manipulater~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data = mnc_balanced_x2, parms = list(split = "gini"), control = rpart.control(cp = 0))
rpart.plot(S_Data_x2, type=5, cex=0.5)
print(S_Data_x2)
summary(S_Data_x2)


#Consider this:ACCR DSRI  SGI DEPI SGAI  GMI  AQI LEVI 
              #23   19   18   15   10    7    5    3 

#From the variable importance, we see ACCR,DSRI,SGI,DEPI,SGAI are important. 
#Decision rules:
#Decision rule1: 
#If ACCR is less than -0.021 and if DSRI<1.6 then the earning namipulation=NO 
#This rule can be made with 0.96 confidence and 30% confidence

#DEcision rule2
#If ACCR is greater than or equal to -0.021 andSGI less than 1.5 and DEPI<0.93, the manipulater= No
#This rule can be made with 0.96 confidence and 11% confidence

#Decision rule 3
#If ACCR is greater than or equal to 1.2 and SGAI < 1.1 and AQI >=0.51 and ACCR >=0.025,  the manipulater= No
#This rule can be made with 1.00 confidence and 4% confidence

##Decision rule 4
#If ACCR is less than -0.021 and if DSRI>=1.6 then the earning namipulation=YES
#This rule can be made with 0.11 confidence and 4% confidence

#Finding the best complexity parameter to build a tree for best decision rules
printcp(S_Data_x2)
opt <- which.min(S_Data_x2$cptable[,"xerror"])
opt
cp <- S_Data_x2$cptable[opt, "CP"]
cp
#0.00

#With new cp value
S_Data3 <- rpart(Manipulater~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data = mnc_balanced_x2, parms = list(split = "gini"), control = rpart.control(cp =cp))
rpart.plot(S_Data3, type=5, cex=0.5)
print(S_Data3)
summary(S_Data3)
S_Data3$variable.importance

#ACCR  SGI DSRI SGAI  AQI LEVI DEPI  GMI 
#35   20   14    8    6    6    6    5 

#Decision rules
#Decision rule1: 
#If ACCR is less than -0.021 and if DSRI<1.6 then the earning namipulation=NO 
#This rule can be made with 0.95 confidence and 30% confidence

#Decision rule2
#If ACCR is greater than or equal to -0.021 and SGI greater than 1.5 , the manipulater= Yes
#This rule can be made with 0.04 confidence and 20% confidence

#Decision rule 3
#If ACCR is greater than or equal to 1.2 and SGAI < 1.5 and DEPI <0.93 ,  the manipulater= No
#This rule can be made with 0.96 confidence and % confidence

#Prediction

S_Test1 <- predict(S_Data_x2, newdata = mnc_complete, type = "class")

S_Test1
class(S_Test1)
S_Test1<-as.factor(S_Test1)
mean(S_Test1 != mnc_complete$Manipulater)
#0.1452055 #Error Rate

#Releveling to make sure the postive class is Yes
S_Test1 <- relevel(S_Test1, ref = "Yes")
mnc_complete$Manipulater <-relevel(mnc_complete$Manipulater, ref = "Yes")

conf_cart1<-confusionMatrix(S_Test1, mnc_complete$Manipulater)
conf_cart1$overall[1]

conf_cart1$byClass

#Accuracy: 85.48%
#Precision: 19%
#Recall: 85.71%

#Second model

S_Test2 <- predict(S_Data3, newdata = mnc_complete, type = "class")

S_Test2 <- relevel(S_Test3, ref = "Yes")
CTestData$Manipulater <-relevel(mnc_com$Manipulater, ref = "Yes")

conf2<-confusionMatrix(S_Test2, mnc_complete$Manipulater)
conf2

conf2$byClass
#Accuracy: 93.42%
#Precision: 27%
#Recall: 42.85%

#From these two deciion trees we see the second model has a better accuracy and sensitivity which is our mai focus for analysing the earning manipulation
#------------------------------------------------------------------------------------------------------------------------

#Question 1 (h)

#Now let's create a logistic regression model using the complete dataset
mnc_complete$Company.ID<-NULL
mnc_complete$C.MANIPULATOR<-NULL

#Choosing the best variables for the dataset

str(mnc_complete)
full_logit<-glm(Manipulater~.,data=mnc_complete, family="binomial")
full_logit
null_logit <- glm(Manipulater~1.,data=mnc_complete, family="binomial")
null_logit
#Storing the values in list for each iteration 
step(null, scope = list(lower=null, upper=full), direction="both")

#Important variables: SGI + DSRI + AQI + ACCR + GMI + SGAI
#Building a logistic regression model on the complete data set
logitModel <- glm(Manipulater ~ SGI + DSRI + AQI + ACCR + GMI + SGAI, data = mnc_complete, family = "binomial")
summary(logitModel)
#Removing SGAI which holds no significance to the response variable

logitModel <- glm(Manipulater ~ SGI + DSRI + AQI + ACCR + GMI , data = mnc_complete, family = "binomial")
summary(logitModel)
#Null deviance: 346.52  on 1238  degrees of freedom
#Residual deviance: 239.28  on 1233  degrees of freedom

#to check if the model is good we take the difference of the deviances
dev<- with(logitModel, null.deviance - deviance) 
dev #107.2387 with 5 degrees of freedom

#The number of predictors
dev1<-with(logitModel, df.null, df.residual)
dev1 #1238

#Finding the p-value for the model
pvalue<-with(logitModel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
pvalue # 0.000000000000000000001569857
#As the p-value <0.05, we can say that the model is significantly better than a null model with 6 degrees of freedom

#Representation using a plot

earn_pred <- data.frame(earn_prob=logitModel$fitted.values,Manipulater=mnc_complete$Manipulater)
earn_pred <- earn_pred[order(earn_pred$earn_prob, decreasing=FALSE),]
earn_pred$rank <- 1:nrow(earn_pred)
View(earn_pred)

## We can plot the data...
library(ggplot2)
ggplot(data=earn_pred, aes(x=rank, y=earn_prob)) +
  geom_point(aes(color=Manipulater), alpha=1, shape=4, stroke=2) +
  xlab("Index") + ylab("Predicted probability of earning manipulation")

#Interpretation: Probability of earning manipulation=YES has a low probability than the earning manipulation=NO which has a high probability

#Prediction on data using the threshold value = 0.5

lm_pred_com<- predict(logitModel, newdata =mnc_complete , type = "response")
lm_pred_com
range(lm_pred_com)
# 0.000000001684642 0.999999999948369
class_com <- ifelse(lm_pred_com >=  0.5, "No", "Yes")
class_com

class_com <- as.factor(class_com)
class_com <- relevel(class_com, ref = "Yes")
mnc_complete$Manipulater<-relevel(mnc_complete$Manipulater,ref="Yes")
conf_com<-confusionMatrix(class_com,mnc_complete$Manipulater)
conf_com$byClass
conf_com$overall[1]
#Accuracy: 97.42
#Recall: 0.256
#Precision:0.769

#A decent precision but the accuracyof 97% is a bit misleading due to the imbalance in dataset

library(caret)

#Now looking at the ROC curve which will give cut-off point, recall, specificity 
library(ROCR)


#Plotting a ROC Curve
pred101 <- prediction(lm_pred_com,mnc_complete$Manipulater)
pred101
perf101 <- performance(pred101, "tpr", "fpr")
plot(perf101)

auc101 <- performance(pred101, "auc")
auc101
auc101 <- unlist(slot(auc, "y.values"))
auc101
#0.098
opt.cut <- function(perf101){
  # mapply function applies the function FUN to all perf@x.values, perf@y.values,perf@alpha.values
  cut.ind <- mapply(FUN = function(x,y,p){d=(x-0)^2+(y-1)^2 # We compute the distance of all the points from the corner point [1,0]
  ind<- which(d==min(d)) # We find the index of the point that is closest to the corner
  c(recall = y[[ind]], specificity = 1-x[[ind]],cutoff = p[[ind]])},perf101@x.values, perf101@y.values,perf101@alpha.values)
}
cut_offrocr<-print(opt.cut(perf101))
cut_offrocr
#                 [,1]
#recall         0.9285714
#specificity    0.8233618
#cutoff         0.99234484

#So the cutoff point obtained is 0.99234484
#Now that we have cut-off point we can predict properly on test_data
class_imp <- ifelse(lm_pred_com > 0.99234484,"No","Yes")
class_imp
confusion_imp<-table(class_imp,mnc_complete$Manipulater)
confusion_imp
class_imp<-as.factor(class_imp)
levels(class_imp)
levels(mnc_complete$Manipulater)
mnc_complete$Manipulater<-relevel(mnc_complete$Manipulater,ref="Yes")
class_imp<-relevel(class_imp,ref="Yes")

confusion_imp<-confusionMatrix(class_imp,mnc_complete$Manipulater)
confusion_imp
          # Reference
#Prediction Yes  No
       #Yes  39 986
       #No    0 214

#Accuracy = 20%
#Sensitivity = 100%
#Precision = 3%

#This model has a lot of bias which causes the model to have such high sensitivity and accuracy. 
#Let's taken youden's index and cost based method to see the cut off point and the performace on the prediction
table101_y_sam11<-data.frame(cutoff=numeric(), Precision=numeric(), Recall=numeric(), Specificity=numeric(),Youden_Index=numeric(),Cost_based=numeric())
library(caret)
prec123<-rep(NA,10)
sens123<-rep(NA,10)
spec123<-rep(NA,10)
youden<-rep(NA,10)
costbased<-rep(NA,10)
for(i in 1:10)
{
  
  #rand_cut[i]<-runif(1)#randomly generates numbers between 0 and 1
  class <- ifelse(lm_pred >=rand_cut[i], "No", "Yes") #Classifying
  class <- as.factor(class)
  class <- relevel(class, ref = "Yes") #Relevling to make predictions on No and have positive class as Yes
  class111<-confusionMatrix(class,mnc_complete$Manipulater) #Confusion matrix
  prec123[i]<-class111$byClass[5] #precision
  sens123[i]<-class111$byClass[1] #sensitivity
  spec123[i]<-class111$byClass[2] #specificity
  youden[i]<-sens123[i]+spec123[i]-1 #Youden's index
  table101_y_sam11[i,]<-c(rand_cut[i],prec123[i],sens123[i],spec123[i],youden[i],costbased[i]) #A table that holds all the values
  
}
table101_y_sam11<- table101_y_sam11[order(-table101_y_sam11$Youden_Index),]
table101_y_sam11

# cutoff Precision    Recall Specificity Youden_Index 
# 1  0.95683335 0.2000000 0.6410256   0.9166667    0.5576923        
# 10 0.95450365 0.1965812 0.5897436   0.9216667    0.5114103        
# 6  0.89982497 0.3333333 0.3846154   0.9750000    0.3596154        
# 3  0.67757064 0.6315789 0.3076923   0.9941667    0.3018590        
# 4  0.57263340 0.6875000 0.2820513   0.9958333    0.2778846        
# 2  0.45333416 0.8181818 0.2307692   0.9983333    0.2291026        
# 9  0.32792072 0.8181818 0.2307692   0.9983333    0.2291026        
# 7  0.24608773 0.7777778 0.1794872   0.9983333    0.1778205        
# 5  0.10292468 1.0000000 0.1538462   1.0000000    0.1538462        
# 8  0.04205953 1.0000000 0.1538462   1.0000000    0.1538462        


#cutoff     Precision    Recall Specificity Youden_Index
#0.95450365 0.1965812 0.5897436   0.9216667    0.5114103        
#0.89982497 0.3333333 0.3846154   0.9750000    0.3596154        
#0.67757064 0.6315789 0.3076923   0.9941667    0.3018590  

#Checking model with these three cutoff points

#Cut off point : 0.95450365
class_imp12 <- ifelse(lm_pred > 0.95450365,"No","Yes")
class_imp12
confusion_imp12<-table(class_imp12,mnc_complete$Manipulater)
confusion_imp12
class_imp12<-as.factor(class_imp12)
levels(class_imp12)
levels(mnc_complete$Manipulater)
mnc_complete$Manipulater<-relevel(mnc_complete$Manipulater,ref="Yes")
class_imp12<-relevel(class_imp12,ref="Yes")

confusion_imp12<-confusionMatrix(class_imp12,mnc_complete$Manipulater)
confusion_imp12
# Reference
#Prediction Yes   No
      #Yes   23   94
      #No    16 1106

##Accuracy = 91%%
#Sensitivity = 58%
#Precision = 19%

#Cutoff at 0.89982497

class_imp13<- ifelse(lm_pred > 0.89982497,"No","Yes")
class_imp13
confusion_imp13<-table(class_imp13,mnc_complete$Manipulater)
confusion_imp13
class_imp13<-as.factor(class_imp13)
levels(class_imp13)
levels(mnc_complete$Manipulater)
mnc_complete$Manipulater<-relevel(mnc_complete$Manipulater,ref="Yes")
class_imp13<-relevel(class_imp13,ref="Yes")

confusion_imp13<-confusionMatrix(class_imp13,mnc_complete$Manipulater)
confusion_imp13
            # Reference
#Prediction Yes   No
      #Yes  15   30
      #No   24 1170

##Accuracy = 95.6%
#Sensitivity = 38%
#Precision = 33%

#Cutoff at 0.67757064
class_imp14<- ifelse(lm_pred > 0.67757064,"No","Yes")
class_imp14
confusion_imp14<-table(class_imp14,mnc_complete$Manipulater)
confusion_imp14
class_imp14<-as.factor(class_imp14)
levels(class_imp14)
levels(mnc_complete$Manipulater)
mnc_complete$Manipulater<-relevel(mnc_complete$Manipulater,ref="Yes")
class_imp14<-relevel(class_imp14,ref="Yes")

confusion_imp14<-confusionMatrix(class_imp14,mnc_complete$Manipulater)
confusion_imp14$byClass
# Reference
#Prediction Yes   No
    #Yes    12    7
      #No    27 1193

##Accuracy = 97.2%
#Sensitivity = 30%
#Precision = 63%

#--------------------------------------------------------------------------------------

#Question(i)
install.packages("randomForest")
library(randomForest)

#building the random forest model
z$Company.ID<-NULL
z$C.MANIPULATOR<-NULL
rf <- randomForest(Manipulater ~ ., data = z, mtry = sqrt(ncol(z)-1), ntree = 300, proximity = T, importance = T)
rf
plot(rf)
View(mnc_complete)
str(z)

str(mnc_complete)
str(z)
rf_pred<-predict(rf, newdata = mnc_complete)
#cm<-table(mnc_complete$Manipulater,rf_pred)
#knowing the importance of the variables
importance(rf, type = 1)
rf
rf$proximity
rf$predicted
rf$votes

library(caret)

is.factor(mnc_complete$Manipulater)
is.factor(rf_pred)
levels(rf_pred)
levels(mnc_complete$Manipulater)
rf_pred<-relevel(rf_pred,ref = "Yes")
#print the result
conf_random<-confusionMatrix(rf_pred, mnc_complete$Manipulater)
conf_random
#accuracy=79%
#Sensitivity = 100%
#Precision = 13%

#---------------------------------------------------------------------------------------
#ada-boosting
install.packages("adabag")
library(adabag)

mnc.adaboost <- boosting(Manipulater ~ ., data = z, mfinal = 10, control = rpart.control(maxdepth = 1))

mnc.adaboost$trees
mnc.adaboost$trees[[1]]

mnc.adaboost$weights

mnc.adaboost$prob

nrow(mnc.adaboost$class)

# votes indicates the weighted predicted class
mnc.adaboost$votes

#importance returns important variables
mnc.adaboost$importance
levels(mnc.adaboost$class)
mnc.adaboost$class<-as.factor(mnc.adaboost$class)
levels(mnc_complete$Manipulater)
mnc.adaboost$class<-relevel(mnc.adaboost$class,ref = "Yes")
ptab<-confusionMatrix(mnc.adaboost$class, z$Manipulater, dnn = c("Predicted Class", "Observed Class"))
ptab

#Accuracy: 84%
#SEnsitivity: 86%
#Precision: 82%

errorrate <- 1 - sum(mnc.adaboost$class == mnc_balanced_x2$Manipulater) /length(mnc_balanced_x2$Manipulater)
errorrate

pred <- predict(mnc.adaboost,newdata = mnc_complete)

# However if you use predict.boosting, you can change mfinal
mnc.predboosting <- predict.boosting(mnc.adaboost, newdata = mnc_complete)

# errorevol calculates errors at each iteration of adaboost
err.train <- errorevol(mnc.adaboost,CTrainData)
err.test <- errorevol(mnc.adaboost,mnc_complete)

plot(err.test$error, type = "l", ylim = c(0,1), col = "red", lwd = 2)
lines(err.train$error, cex = 0.5, col = "blue", lty = 2, lwd = 2)


#----------------------------------------------------------------------------------------

#Question (i)
#Taking all the metrics into one table
View(Final_Evaluation)
Final_Evaluation<-data.frame(Model=character(), Accuracy = numeric(), Precision=numeric(), Recall=numeric(),stringsAsFactors=FALSE)

Final_Evaluation[1, ] <- c("Stepwise Logistic Regression", Accuracy1, conf$byClass[5],conf$byClass[1] )
Final_Evaluation[2, ]<- c("Logistic Regression with youden's index and low cost based method to determine cutoff point", Accuracy2, conf99$byClass[5],conf99$byClass[1] )
Final_Evaluation[3, ]<- c("CART Model 1 with cp=0", conf_cart1$overall[1], conf_cart1$byClass[5],conf_cart1$byClass[1] )
Final_Evaluation[4, ]<- c("CART Model 2 with cp=0.001162791", conf2$overall[1], conf2$byClass[5],conf2$byClass[1] )
Final_Evaluation[5, ]<- c("Logistic regression on complete dataset and cutoff=0.5", conf_com$overall[1], conf_com$byClass[5],conf_com$byClass[1] )
Final_Evaluation[6, ]<- c("Logistic regression on complete dataset and cutoff based on ROCR", confusion_imp$overall[1], confusion_imp$byClass[5],confusion_imp$byClass[1] )
Final_Evaluation[7, ]<- c("Logistic regression on complete dataset and cutoff based on youden's index=0.95450365", confusion_imp12$overall[1], confusion_imp12$byClass[5],confusion_imp12$byClass[1] )
Final_Evaluation[8, ]<- c("Logistic regression on complete dataset and cutoff based on youden's index=0.89982497", confusion_imp13$overall[1], confusion_imp13$byClass[5],confusion_imp13$byClass[1] )
Final_Evaluation[9, ]<- c("Logistic regression on complete dataset and cutoff based on youden's index=0.67757064", confusion_imp14$overall[1], confusion_imp14$byClass[5],confusion_imp14$byClass[1] )
Final_Evaluation[10, ]<- c("Random Forest Model", conf_random$overall[1], conf_random$byClass[5],conf_random$byClass[1] )
Final_Evaluation[11, ]<- c("ADA-BOOST", ptab$overall[1], ptab$byClass[5],ptab$byClass[1] )




#From the models in the tables we see that model 3 [CART Model 1 with cp=0], model 6[	Logistic regression on complete dataset and cutoff based on ROCR] has very good performance on the data and predcition 
#Model 9[ random forest] and model 11[Ada-Boost] has the best recall 
#Our main focus is lowering the number of false positives(manipulators predicted as non-manipulators) and having a high recall/sensitivity is ideal for best prediction
#Hence these two models ahev the highest recall. 
#although model 6 and model 10 has recall of 100%, we could consider it, but the accuracy for model 6 is very low = 20%. 
#Best would be to consider the CART Model 3 and model 11 to do our predictions as the recall is 85% and 86%
#Considering this to avoid any form of overfitting as well. 

#The predictors would be: AQI DSRI GMI SGI and ACCR which are major predictors!

#-----------------------------------------------------------------------------
