#To install required packages
install.packages("neuralnet")
install.packages("caTools")

#To load required packages in R Project
library(neuralnet)
library(caTools)

#To read data from csv file
dataN <- read.csv("Infosys Nia.csv")
View(dataN)

# Min-Max Scaling of required variables to convert them between 0 to 1
dataN$Technology.Adoption <- ((dataN$Technology.Adoption - min(dataN$Technology.Adoption))/
                   (max(dataN$Technology.Adoption)-min(dataN$Technology.Adoption)))

dataN$Business.Process.Efficiency <- ((dataN$Business.Process.Efficiency- min(dataN$Business.Process.Efficiency))/
                     (max(dataN$Business.Process.Efficiency)- min(dataN$Business.Process.Efficiency)))

dataN$Organizational.Adaptation <- ((dataN$Organizational.Adaptation- min(dataN$Organizational.Adaptation))/
                    (max(dataN$Organizational.Adaptation)-min(dataN$Organizational.Adaptation)))

View(dataN)

#This command will split the data as per mentioned ratio
set.seed(1)
split <- sample.split(dataN, SplitRatio = 3/4)
split

#This command will allocate dataset to training and testing dataset
training <- subset(dataN, split == "TRUE")
testing <- subset(dataN, split == "FALSE")
View(training)
View(testing)

#This command will develop binomial logistic regression model
model <- glm(Perception.of.long.term.Effectiveness.of.Infosys.Nia.Integration ~ ., family = "binomial", data = training)
summary(model)

# From the above Regression Code we will identify the Independent Variable that is not effecting the Dependent Variable
# We can see that "other debt"(othdebt) is not significantly related to Default, therefore we will drop that column.

#drop the column 	othdebt because there is no relationship in p-value
training <- training[,c(-1,-3)]
testing <- testing[,c(-1,-3)]
View(training)
View(testing)

#This command will develop binomial logistic regression model
model <- glm(Perception.of.long.term.Effectiveness.of.Infosys.Nia.Integration ~ ., family = "binomial", data = training)
summary(model)

#Creation of neural network model
n <- neuralnet(Perception.of.long.term.Effectiveness.of.Infosys.Nia.Integration ~ ., data=training, hidden = 1)

#plotting neural network model
plot(n)

#Predicting dependent variable using testing dataset
resN <- predict(n, testing, type = "response")

#To create confusion matrix
table(ActualValue=testing$Perception.of.long.term.Effectiveness.of.Infosys.Nia.Integration, PredictedValue=resN>0.5)
