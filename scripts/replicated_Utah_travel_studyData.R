    ## Replicated Data Utah Study ##

# Kristopher C. Toll

# Import Unaltered Res Experiment data

library(readxl)
ResidentialChoice_ChoiceExperimentsData <- read_excel("C:/Users/Kristopher/odrive/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/2. Choice Experiments Dataset/ResidentialChoice_ChoiceExperimentsData.xlsx", 
                                                      col_types = c("numeric", "text", "numeric", 
                                                                    "text", "text", "text", "text", 
                                                                    "text", "text", "text", 
                                                                    "text", "text", "text", 
                                                                    "text", "text", "text", 
                                                                    "text", "text"))

# Add and id variable create from password
ResidentialChoice_ChoiceExperimentsData$id <- as.numeric(as.factor(ResidentialChoice_ChoiceExperimentsData$password))


# Convert to Wide data into long data using mlogit package

library(mlogit)
LogitData <- mlogit.data(ResidentialChoice_ChoiceExperimentsData, shape = "wide", choice = "choice", sep = "", varying = c(5:18), alt.levels = c(1,2), id="id")

# rename levels of each variable

LogitData$commute <- ifelse(LogitData$commute == "1", "Walking Distance", ifelse(LogitData$commute == "2", "5 Miles", ifelse(LogitData$commute == "3", "10 Miles", "20 Miles")))
LogitData$destinations <- ifelse(LogitData$destinations == "1", "Walking Distance", ifelse(LogitData$destinations == "2", "Less Than 3 Miles", ifelse(LogitData$destinations == "3", "3 To 10 Miles", "More Than 10 Miles")))
LogitData$homes <- ifelse(LogitData$homes == "1", "Quart Acre Single Homes Apt", ifelse(LogitData$homes == "2", "Half Acre Single Homes Apt", ifelse(LogitData$homes == "3", "Half Acre Homes", "1+ Acre Homes")))
LogitData$parking <- ifelse(LogitData$parking == "1", "Own Driveway Or Garage", ifelse(LogitData$parking == "2", "On Street Or In Free Lot", "Off Street Rental Lot"))
LogitData$price <- ifelse(LogitData$price == "1", "0.8", ifelse(LogitData$price == "2", "0.9", ifelse(LogitData$price == "3", "1", ifelse(LogitData$price == "4", "1.1", "1.2"))))
LogitData$streets <- ifelse(LogitData$streets == "1", "For Cars", "For Cars Ped Bikes")
LogitData$transit <- ifelse(LogitData$transit == "1", "Walking Distance To Rail and Bus", ifelse(LogitData$transit == "2", "Walking To Bus 5 mile To Rail", ifelse(LogitData$transit == "3", "5 Mile to Bus and Rail", "10 Mile to Bus and Rail")))

# factor price so 0 is omited in models

LogitData$price <- factor(LogitData$price, levels = c("1", "0.8", "0.9", "1.1", "1.2"))
LogitData$commute <- relevel(as.factor(LogitData$commute), ref = "Walking Distance")
LogitData$destinations <- relevel(as.factor(LogitData$destinations), ref = "Walking Distance")
LogitData$homes <- relevel(as.factor(LogitData$homes), ref = "Quart Acre Single Homes Apt")
LogitData$streets <- relevel(as.factor(LogitData$streets), ref = "For Cars")
LogitData$transit <- relevel(as.factor(LogitData$transit), ref = "Walking Distance To Rail and Bus")
LogitData$parking <- relevel(as.factor(LogitData$parking), ref = "Own Driveway Or Garage")



## ADDING DOLLAR VALUES TO PRICE ##

# Import Demographic data

ResidentialDemo <- read_excel("C:/Users/Kristopher/odrive/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/1. Person Household Dataset/ResidentialChoice_PersonHouseholdData.xlsx")

# Under rent_own, 3 and 4 do not have a reported renter or owner price and must be dropped

ResidentialDemo <- subset(ResidentialDemo, ResidentialDemo$rent_own != "3" & ResidentialDemo$rent_own != "4")

# Put owner and renter prices into the same vector

ResidentialDemo$HomeRentPrice <- ifelse(ResidentialDemo$rent_own == "1", ResidentialDemo$rent_price, ResidentialDemo$home_price)

# drop home price and rent price

ResidentialDemo$home_price <- NULL
ResidentialDemo$rent_price <- NULL

# Turn price vector in logit data into a numeric vector for calculations

LogitData$price_n <- as.numeric(as.character(LogitData$price))

# Join LogitData with ResidentialChoice_PersonHouseholdData

LogitData2 <- plyr::join(LogitData, ResidentialDemo, type = "inner")

# Create a vector that reflects the relative comparison

LogitData2$DiffNominalPrice <- ifelse(LogitData2$price_n == 0, 0, LogitData2$HomeRentPrice*LogitData2$price_n - LogitData2$HomeRentPrice)
LogitData2$CompNominalPrice  <- ifelse(LogitData2$price_n == 0, LogitData2$HomeRentPrice, LogitData2$HomeRentPrice*LogitData2$price_n)

# Reorder columns 

LogitData2 <- LogitData2[c(1:13, 189, 190, 191, 14:188)]


# scale price variables by 1000

LogitData2$DiffNominalPrice_s <- LogitData2$DiffNominalPrice/1000
LogitData2$CompNominalPrice_s <- LogitData2$CompNominalPrice/1000

# Save LogitData2 for modeling

write.csv2(LogitData2, file = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/MasterLogitData.csv")
saveRDS(LogitData2, file = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/MasterLogitData.rds")
