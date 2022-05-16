# this is a comment

#----------------------------------------------------------
# Basic regression moddjfhgjsel
#----------------------------------------------------------

load("Missing_imputations.RData")

# set seed for randomizer
rm(list =ls())
set.seed(1234)
options(scipen=15)


#variables in amelia output
wdioutvars <- as.data.frame(grep("\\.", 
                                 names(a.out$imputations[[1]]), 
                                 value = TRUE))
names(wdioutvars) <- "vars" # change name of vector with variable names
wdivars_x <- wdioutvars %>% 
  summarise(vars = paste(vars, collapse = " + "))

form <- as.formula(paste("fgt0_190 ~", wdivars_x))

#-------------------------------------
# Training and test data sets
#-------------------------------------

# Remove missings from 
dataset <- a.out$imputations[[1]] %>% 
  filter(!is.na(fgt0_190))

# create test and training folds
# size of training set
train.size <- floor(0.75 * nrow(dataset))

# indicator for whether in training set
train.ind <- sample(seq_len(nrow(dataset)), size = train.size)

# create separate test and training sets  
dataset.train <- dataset[train.ind,]
dataset.test <- dataset[-train.ind,]

# create formula for estimating graduation rate   
formula <- as.formula(fgt0_190 ~ .)

# estimate ridge model, note alpha = 0!
GR.Ridge.fit <- cv.glmnet(x = build.x(formula,dataset.train),
                          y = build.y(formula,dataset.train),
                          alpha = 0) 

coef(GR.Ridge.fit)
plot(GR.Ridge.fit)



# estimate Lasso model, note alpha = 1!
GR.Lasso.fit <- cv.glmnet(x = build.x(formula,dataset.train),
                          y = build.y(formula,dataset.train),
                          alpha = 1) 
coef(GR.Lasso.fit, s = "lambda.min")
plot(GR.Lasso.fit)


# estimate OLS model
GR.lm.fit <- lm(formula, data = dataset.train) 
coef(GR.lm.fit)
plot(GR.lm.fit)

# make a coefficient matrix comparing 
coef.mat <- matrix(nrow = dim(as.matrix(coef(GR.lm.fit)))[1], ncol = 3)
coef.mat[,1] <- as.matrix(coef(GR.Ridge.fit))[-2,]
coef.mat[,2] <- as.matrix(coef(GR.Lasso.fit))[-2,]
coef.mat[,3] <- as.matrix(coef(GR.lm.fit))
row.names(coef.mat) <- names(coef(GR.lm.fit))
colnames(coef.mat) <- c("Ridge","Lasso","OLS")
coef.mat <- round(coef.mat, digits = 4)
coef.mat

# estimate MSE on test
# create RMSE function
RMSE = function(x1, x2) {
  sqrt( mean( (x1 - x2)^2  ) )
}


# build X and Y matrices using the test data
X.test <- build.x(formula, dataset.test)
Y.test <- build.y(formula, dataset.test)

# get yhats for the respective models
GR.Ridge.yhat <- predict(GR.Ridge.fit, newx = X.test, s = "lambda.min")
GR.Lasso.yhat <- predict(GR.Lasso.fit, newx = X.test, s = "lambda.min")
GR.lm.yhat <- as.matrix(predict(GR.lm.fit, newdata = dataset.test))

# estimate RMSE
RMSE(GR.Ridge.yhat, Y.test)
RMSE(GR.Lasso.yhat, Y.test)
RMSE(GR.lm.yhat, Y.test)

# calculate Rsquared
1 - sum((Y.test-GR.Ridge.yhat)^2)/sum((Y.test-mean(Y.test))^2)
1 - sum((Y.test-GR.Lasso.yhat)^2)/sum((Y.test-mean(Y.test))^2)
1 - sum((Y.test-GR.lm.yhat)^2)/sum((Y.test-mean(Y.test))^2)

# put results into matrix
results.mat <- matrix(nrow = 2, ncol = 3)
results.mat[1,1] <- RMSE(GR.Ridge.yhat, Y.test)
results.mat[1,2] <- RMSE(GR.Lasso.yhat, Y.test)
results.mat[1,3] <- RMSE(GR.lm.yhat, Y.test)
results.mat[2,1] <- 1 - sum((Y.test-GR.Ridge.yhat)^2)/sum((Y.test-mean(Y.test))^2)
results.mat[2,2] <- 1 - sum((Y.test-GR.Lasso.yhat)^2)/sum((Y.test-mean(Y.test))^2)
results.mat[2,3] <- 1 - sum((Y.test-GR.lm.yhat)^2)/sum((Y.test-mean(Y.test))^2)
row.names(results.mat) <- c("RMSE", "R-squared")
colnames(results.mat) <- c("Ridge", "Lasso","OLS")
results.mat


