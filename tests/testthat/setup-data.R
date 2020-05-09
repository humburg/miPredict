library(mice)
nhanes_mids <- mice(nhanes2, m=5, printFlag=FALSE) 
nhanes_long <- complete(nhanes_mids, action="long")
nhanes_clean <- nhanes_long %>% clean_data()