# Load the packages
library(rpart)
library(rpart.plot)
library(tidyverse) 
library(caret)


#load data
data_cleaned <- read.csv("data_cleaned.csv",row.names = 1)
data_subsetted_na <- data_cleaned[,c("Any.arrythmia","Legal.Sex","Age","Total.Amount.Given..mg.","Abdominal.pain","Nausea","Agitation","Vomiting","Unknown","Headache")]
colnames(data_subsetted_na) <- c("Any.arrythmia","Legal.Sex","Age","Total.Dose.mg.","Abdominal.pain","Nausea","Agitation","Vomiting","Unknown","Headache")
data_subsetted<- data_subsetted_na[-which(is.na(data_subsetted_na$Age)==TRUE),] #remove NA
data_subsetted$Any.arrythmia <- as.factor(data_subsetted$Any.arrythmia)

#make legal sex a dummy variable
data_subsetted$Male <- 0
data_subsetted$Male[which(data_subsetted$Legal.Sex == "Male")] <- 1
data_subsetted$Female <- 0
data_subsetted$Female[which(data_subsetted$Legal.Sex == "Female")] <- 1
data_subsetted$X<- 0
data_subsetted$X[which(data_subsetted$Legal.Sex == "X (Legal Non-Binary Designation)")] <- 1
data_subsetted <- data_subsetted[,-2] #remove legal.sex column

arrythmia = data_subsetted[which(data_subsetted$Any.arrythmia == "Y"),] #604
no.arrythmia = data_subsetted[which(data_subsetted$Any.arrythmia == "N"),] #43

#set seed for reproducibility
set.seed(3) 

#make 70% of arrhythmia in the training dataset, 30% test set 
arrythmia.indx_train <- sample(1:nrow(arrythmia), round(0.7 * nrow(arrythmia)))
arrythmia.train = arrythmia %>% filter(row_number() %in% arrythmia.indx_train)
arrhythmia.test = arrythmia %>% filter(!(row_number() %in% arrythmia.indx_train))

#make 70% of no arrhythmia in the training dataset, 30% test set 
no.arrythmia.indx_train <- sample(1:nrow(no.arrythmia), round(0.7 * nrow(no.arrythmia)))
no.arrythmia.train = no.arrythmia %>% filter(row_number() %in% no.arrythmia.indx_train)
no.arrhythmia.test = no.arrythmia %>% filter(!(row_number() %in% no.arrythmia.indx_train))

#unbalanced training set
data_train_unbalanced <- rbind(arrythmia.train,no.arrythmia.train)
#test set 
data_test <- rbind(arrhythmia.test,no.arrhythmia.test)

train_control <- trainControl(method = "cv", number = 5)  # 5-fold CV

# Train decision tree using cross-validation
model_cv <- train(as.factor(Any.arrythmia) ~ ., data = data_train_unbalanced, method = "rpart", trControl = train_control,metric = "Accuracy",parms=list(
  prior=c(.5,.5),loss=matrix(c(0,1,100,0),
                             byrow=TRUE,
                             nrow=2)))

# View cross-validation results
print(model_cv)

# final model
final_model <- model_cv$finalModel
rpart.plot(final_model)

#predictions on test data
predictions <- predict(final_model, newdata = data_test, type = "class")
#contingency table
table(predictions,data_test$Any.arrythmia)
#misclassification error
mean(predictions != data_test$Any.arrythmia)


#ATTEMPTING TO CORRECT FOR UNBALANCED DATA 

set.seed(1)

#upsample unbalanced training set
data_train <- upSample(x = data_train_unbalanced[, -1], y = data_train_unbalanced$Any.arrythmia)

train_control <- trainControl(method = "cv", number = 5)  # 5-fold CV

# Train decision tree using cross-validation
model_cv <- train(Class ~ ., data = data_train, method = "rpart", trControl = train_control,metric = "Accuracy",parms=list(
  prior=c(.5,.5), loss=matrix(c(0,1,3,0),
                              byrow=TRUE,
                              nrow=2)))

# View cross-validation results
print(model_cv)

# final model
final_model <- model_cv$finalModel
rpart.plot(final_model)

#predictions on test data
predictions <- predict(final_model, newdata = data_test, type = "class")
#contingency table
table(predictions,data_test$Any.arrythmia)
#misclassification error
mean(predictions != data_test$Any.arrythmia)


