## Model and Histogram Comparisons ##

library(readr)
options(scipen = 999)

# Import the data

# Demographics with choice dataset #
MasterLogitData <- read.csv2("C:/Users/A01246966/Box/Utah Travel Study/modified_data/MasterLogitData.csv")

# Demographics dataset
library(readxl)
ResDemo <- read_excel("C:/Users/A01246966/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/1. Person Household Dataset/ResidentialChoice_PersonHouseholdData.xlsx", 
                                                    col_types = c("text", "text", "text", 
                                                                  "text", "numeric", "text", "text", 
                                                                  "numeric", "numeric", "text", "numeric", 
                                                                  "numeric", "text", "numeric", "text", 
                                                                  "text", "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "numeric", 
                                                                  "numeric", "numeric", "text", "text"))
# Put owner and renter prices into the same vector

ResDemo$HomeRentPrice <- ifelse(ResDemo$rent_own == "1", ResDemo$rent_price, ResDemo$home_price)

# drop home price and rent price

ResDemo$home_price <- NULL
ResDemo$rent_price <- NULL


ggplot(data = ResDemo, aes(x = curr_res_type, y = HomeRentPrice)) + geom_jitter()  + aes(colour = rent_own) + theme(legend.position = "right") + labs(title = "Exploring Home and Renter Values by Type of home")

ggplot( data = ResDemo, aes(x = HomeRentPrice)) + geom_histogram(binwidth = 400000000) + aes(colour = rent_own) + facet_wrap(~curr_res_type, ncol = 4) + labs(title = "") + theme(legend.position = "right", axis.text.x = element_text(angle = 90, hjust = 1))

##### Replicated Models with only Owners and Renters ######

# Checking number of renters and owners
# We have to divide by 20 so that we only count repondes, 10 for each choice comparision and 2 for each alternative

table(MasterLogitData$rent_own)/20

# Checking how many home and rent values were self-reported

table(MasterLogitData$HomeRentPrice)/20

# Different resident types and Home or Rent Comparision #

table(MasterLogitData$curr_res_type, MasterLogitData$rent_own)

ggplot(data = MasterLogitData, aes(x = curr_res_type, y = HomeRentPrice)) + geom_jitter()  + aes(colour = as.factor(rent_own)) + theme(legend.position = "right") + labs(title = "")



##### Replicated Models with only Owners and Renters ######