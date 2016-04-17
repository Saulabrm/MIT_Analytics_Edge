#Unit 4 Assignment
setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week4")

data(state)
statedata = data.frame(state.x77)

  #Problem 1.1 - Linear Regression Models
linreg = lm(Life.Exp ~ Population+ Income+ Illiteracy+ Murder+ HS.Grad+ Frost+ Area, statedata)
summary(linreg)
#Adjusted R - squared  =  0.6922

  #Problem 1.2 - Linear Regression Models
linreg.pred = predict(linreg,statedata)
linreg.sse = sum((linreg.pred - statedata$Life.Exp)^2)
sum(linreg$residuals^2)       #In this case it can be used also this code
linreg.sse  #[1] 23.29714     #SSE

  #Problem 1.3 - Linear Regression Models
linreg2 = lm(Life.Exp ~ Population + Murder+ Frost+ HS.Grad, statedata)
summary(linreg2)
# Adjusted R-squared:  0.7126 

  #Problem 1.4 - Linear Regression Models
  #Calculate the sum of squared errors again, using this reduced model:
linreg2.pred = predict(linreg2, statedata)
linreg.sse = sum((linreg2.pred - statedata$Life.Exp)^2)
sum(linreg$residuals^2)
linreg.sse #[1] 23.30804

  #Problem 1.5 - Linear Regression Models
#[1] Trying different combinations of variables in linear regression is like trying different numbers of splits in a tree - this controls the complexity of the model.


  #Problem 2.1 - CART Models
CartModel  = rpart(Life.Exp ~ Population+ Income+ Illiteracy+ Murder+ HS.Grad+ Frost+ Area, statedata)
prp(CartModel)  #Murder #Default minbuckets

  #Problem 2.2 - CART Models
  #Use the regression tree you just built to predict life expectancies (using the predict function), and calculate the sum-of-squared-errors (SSE) like you did for linear regression. What is the SSE?
cartmod.pred = predict(CartModel, statedata)
cartmod.sse = sum((cartmod.pred-statedata$Life.Exp)^2)
cartmod.sse  #[1] 28.99848

  #Problem 2.3 - CART Models
#Create bigger Tree
CartModel2 = rpart(Life.Exp ~ Population+ Income+ Illiteracy+ Murder+ HS.Grad+ Frost+ Area, statedata, minbucket =5)
prp(CartModel2)  #Murder HSGrad Area

  #Problem 2.4 - CART Models
  #Do you think the default minbucket parameter is smaller or larger than 5 based on the tree that was built?
#[1] Larger

  #Problem 2.5 - CART Models
  #SSE
cartmod2.pred = predict(CartModel2, statedata)
cartmod2.sse = sum((cartmod2.pred - statedata$Life.Exp)^2)
cartmod2.sse #[1] 23.64283

#Problem 2.6 - CART Models
#Can we do even better? Create a tree that predicts Life.Exp using only Area, with the minbucket parameter to 1. What is the SSE of this newest tree?
CartModel3 = rpart(Life.Exp ~ Area, statedata, minbucket =1)
prp(CartModel3)  

cartmod3.pred = predict(CartModel3, statedata)
cartmod3.sse = sum((cartmod3.pred - statedata$Life.Exp)^2)
cartmod3.sse #[1] 9.312442

###Problem 3.1 - Cross-validation
library(caret)
library(e1071)
set.seed(111)


numFolds = trainControl(method = "cv", number = 10) #cv for cross validation, and 10 folds
cp.Grid = expand.grid(.cp = seq(0.01,0.5, 0.01))
tr = train(Life.Exp~ ., data = statedata, method = "rpart", 
           trControl= numFolds, tuneGrid =cp.Grid) 
tr

CartModel4  = rpart(Life.Exp ~ Population+ Income+ Illiteracy+ Murder+ HS.Grad+ Frost+ Area,
                   statedata, cp=.12 )
prp(CartModel4)
#>=6.6 and <=11

  #Problem 3.3 - Cross-Validation
  #Calculate the SSE of this tree:
CartModel4.pred = predict(CartModel4,statedata)
cartmod4.sse = sum((CartModel4.pred-statedata$Life.Exp)^2)
cartmod4.sse #[1] 32.86549 SSE

  #Problem 3.4 - Cross-Validation
#The model we just made with the "best" cp

  #Problem 3.5 - Cross-Validation
numFolds = trainControl(method = "cv", number = 10) #cv for cross validation, and 10 folds
cp.Grid = expand.grid(.cp = seq(0.01,0.5, 0.01))
tr = train(Life.Exp~Area, data = statedata, method = "rpart", 
           trControl= numFolds, tuneGrid =cp.Grid) 
tr

CartModel5  = rpart(Life.Exp ~  Area,
                    statedata, cp=.02 )
prp(CartModel5)

#Problem 3.6 - Cross-Validation
CartModel5.pred = predict(CartModel5,statedata)
cartmod5.sse = sum((CartModel5.pred-statedata$Life.Exp)^2)
cartmod5.sse #[1] 44.26817 SSE
#9579 and 51000

#Problem 3.7 - Cross-Validation
#The Area variable is not as predictive as Murder rate.