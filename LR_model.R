
#load data
data_cleaned <- read.csv("data_cleaned.csv",row.names = 1)

#remove the 3 observations for the people who do not have age. 
data_cleaned <- data_cleaned[-which(is.na(data_cleaned$Age)==TRUE),] #remove NA

#warning -> glm.fit: fitted probabilities numerically 0 or 1 occurred 
logistic_model <- glm(as.factor(Any.arrythmia) ~ as.factor(Legal.Sex) + Age + Total.Amount.Given..mg. + as.factor(Abdominal.pain)+ as.factor(Nausea) + as.factor(Agitation)  + as.factor(Vomiting) + as.factor(Unknown)  +as.factor(Headache) , data = data_cleaned, family = "binomial")
summary(logistic_model)


#make correlation matrix
data_subset <- data_cleaned[,c("Legal.Sex","Age","Total.Amount.Given..mg.","QTc..before.","Abdominal.pain","Nausea","Agitation","Vomiting","Unknown","Headache")]
data_subset$Legal.Sex[which(data_subset$Legal.Sex=="Female")] = 0
data_subset$Legal.Sex[which(data_subset$Legal.Sex=="Male")] = 1
data_subset$Legal.Sex[which(data_subset$Legal.Sex=="X (Legal Non-Binary Designation)")] = 2
data_subset$Legal.Sex <- as.integer(data_subset$Legal.Sex)
cor_matrix <- cor(data_subset,use = "pairwise.complete.obs")
round(cor_matrix,digits=2)

#TABLE 2 - logistic regression
logistic_model <- glm(as.factor(Any.arrythmia) ~ as.factor(Legal.Sex) + Age + Total.Amount.Given..mg. + as.factor(Abdominal.pain)+ as.factor(Agitation)  + as.factor(Vomiting), data = data_cleaned, family = "binomial")
summary(logistic_model)$coefficients
confint(logistic_model)


#TABLE 3 - Fisher's exact test

#abdominal pain
abdominalpain.arrythmia <- table(data_cleaned$Abdominal.pain, data_cleaned$Any.arrythmia)
fisher.test(abdominalpain.arrythmia)

#unknown 
unknown.arrythmia <- table(data_cleaned$Unknown, data_cleaned$Any.arrythmia)
fisher.test(unknown.arrythmia)

#headache
headache.arrythmia <- table(data_cleaned$Headache, data_cleaned$Any.arrythmia)
fisher.test(headache.arrythmia)

#X
data_cleaned$X <- NA
data_cleaned$X <- ifelse(data_cleaned$Legal.Sex == "X (Legal Non-Binary Designation)", 1, 0)
x.arrythmia <- table(data_cleaned$X, data_cleaned$Any.arrythmia)
fisher.test(x.arrythmia)