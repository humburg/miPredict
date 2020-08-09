library(mice)

set.seed(42)

## Impute data
nhanes_mids <- mice(nhanes2, m=5, printFlag=FALSE) 
nhanes_long <- complete(nhanes_mids, action="long")
nhanes_clean <- nhanes_long %>% clean_data()

## Model fit
library(glmnet)
data(BinomialExample)
binomData_complete <- as.data.frame(c(as.data.frame(x), data.frame(y=y)))
for(i in 1:ncol(x)){
  idx <- sample(1:100, size=10)
  x[idx, i] <- NA
}
binomData <- as.data.frame(c(as.data.frame(x), data.frame(y=y)))
binom_mids <- mice(binomData, m=5, printFlag=FALSE) 
suppressWarnings(binom_fit <- fit_model(binom_mids, outcome="y"))
complete_data <- complete(binom_mids, action="long")
pred <- unclass(by(complete_data, complete_data$.imp, function(x) predict(binom_fit$pooled_model, newdata = x, type = "response")))
