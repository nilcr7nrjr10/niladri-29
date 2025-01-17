#To install and load package called caTools
install.packages("caTools")
library(caTools)

#This command is to read from csv file
data <- read.csv(file="Infosys Nia.csv",header = TRUE)

#This command is to view content of "data" variable which was declared earlier
View(data)

#This command will split the data as per mentioned ratio
set.seed(1)
split <- sample.split(data, SplitRatio = 3/4)
split

#This command will allocate dataset to training and testing dataset
training <- subset(data, split == "TRUE")
testing <- subset(data, split == "FALSE")
View(training)
View(testing)

#This command will develop binomial logistic regression model
model <- glm(Perception.of.long.term.Effectiveness.of.Infosys.Nia.Integration ~ ., family = "binomial", data = training)

#To get summary of created model
summary(model)


#This command will drop the insignificant column
training <- training[,c(-1,-3)]
View(training)
testing<- testing[,c(-1,-3)]
View(testing)

#This command will develop binomial logistic regression model
model <- glm(Perception.of.long.term.Effectiveness.of.Infosys.Nia.Integration ~ ., family = "binomial", data = training)

#To get summary of created model
summary(model)

#To predict using created model
res <- predict(model, testing, type = "response")
View(res)

#To create confusion matrix
table(ActualValue=testing$Perception.of.long.term.Effectiveness.of.Infosys.Nia.Integration, PredictedValue=res>0.5)
