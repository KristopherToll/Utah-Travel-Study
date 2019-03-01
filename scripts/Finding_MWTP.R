##### Finding Marginal Willingness to Pay ######



# Renter Marginal Willingness to Pay

RenterData  <-read_rds("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoiceData.RDS")

# Check to see if varialbe classes are maintained
sapply(RenterData, class)


# Renter Models

# Income 

RenterData$Income_a <- ifelse()