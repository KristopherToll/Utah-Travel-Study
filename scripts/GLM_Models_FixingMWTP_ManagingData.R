### Thesis Data ####






# We need three data sets
# Load Exsiting Data Sources


# Census Data

library(readr)
options(scipen = 999)
# Importing Five Year Estimate for Home value at the Census Tract Level for 2012
ACS_12_5YR <- read_csv("C:/Users/A01246966/Box/Utah Travel Study/Census_data/Census/ACS_12_5YR_B25077_with_ann.csv", 
                                       na = "NA")[,c(3,4)]
colnames(ACS_12_5YR)[1] <- "census_tract"

# Importing Five Year Estimate for Home value at the Census Block Group Level for 2013
ACS_13_5YR <- read_csv("C:/Users/A01246966/Box/Utah Travel Study/Census_data/Census/ACS_13_5YR_B25077_with_ann.csv", na = "NA")[,c(3,4)]
colnames(ACS_13_5YR)[1] <- "block_group"

# Importing Travel Study Data and Census Data

library(readxl)

# We need this data for census tract and block locations
GeoID <- read_excel("C:/Users/A01246966/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/1. Main Household Diary/1. Household-Level Dataset/HouseholdDiary_HouseholdData.xlsx", 
                                           sheet = "Data")[,c(1,8:9)]

# Household data
library(plyr)
ResidentialChoice_PersonHouseholdData <- read_excel("C:/Users/A01246966/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/1. Person Household Dataset/ResidentialChoice_PersonHouseholdData.xlsx")
a <- subset(ResidentialChoice_PersonHouseholdData, ResidentialChoice_PersonHouseholdData$rent_price > 49)
b  <- subset(ResidentialChoice_PersonHouseholdData, ResidentialChoice_PersonHouseholdData$home_price > 20000 & ResidentialChoice_PersonHouseholdData$home_price < 10000000)

ResidentialChoice_PersonHouseholdData <- rbind(a, b)

#ResidentialChoice_PersonHouseholdData <- subset(ResidentialChoice_PersonHouseholdData, ResidentialChoice_PersonHouseholdData$curr_res_type != "6" & ResidentialChoice_PersonHouseholdData$curr_res_type != "7" & ResidentialChoice_PersonHouseholdData$curr_res_type != "8")


# Convert Variable Types
ResidentialChoice_PersonHouseholdData <- ResidentialChoice_PersonHouseholdData %>% mutate_at(vars(2:4, 5, 6, 7, 10:126, 130, 132:176), funs(as.factor))

sapply(ResidentialChoice_PersonHouseholdData, class)

# Rename and Refactor Variables of Interest

library(forcats)

ResidentialChoice_PersonHouseholdData$Income <- ifelse(ResidentialChoice_PersonHouseholdData$income == "1" | ResidentialChoice_PersonHouseholdData$income == "2" | ResidentialChoice_PersonHouseholdData$income == "3" | ResidentialChoice_PersonHouseholdData$income == "4", "Low", ifelse(ResidentialChoice_PersonHouseholdData$income == "5" | ResidentialChoice_PersonHouseholdData$income == "6" | ResidentialChoice_PersonHouseholdData$income == "7", "Mid", ifelse(ResidentialChoice_PersonHouseholdData$income == "8" | ResidentialChoice_PersonHouseholdData$income == "9" | ResidentialChoice_PersonHouseholdData$income == "10", "High", "NA")))
ResidentialChoice_PersonHouseholdData$Income <- factor(ResidentialChoice_PersonHouseholdData$Income, levels = c("Low", "Mid", "High", "NA"), exclude = "NA")
#ResidentialChoice_PersonHouseholdData$Income_I <- fct_explicit_NA(ResidentialChoice_PersonHouseholdData$Income_I)

ResidentialChoice_PersonHouseholdData$Education <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$education), c("1" = "High School or less", "2" = "High School or less", "3" = "Some College or Voc/tech Training", "4" = "Some College or Voc/tech Training", "5" = "Associates", "6" = "Bachelors", "7" = "Graduate/Post Doc", "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Education <- factor(ResidentialChoice_PersonHouseholdData$Education, levels = c("High School or less", "Some College or Voc/tech Training", "Associates", "Bachelors", "Graduate/Post Doc", "NA"), exclude = "NA")

ResidentialChoice_PersonHouseholdData$Employment <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$employment), c("1" = "Self, part, or full-time employment", "2"= "Self, part, or full-time employment", "3" = "Self, part, or full-time employment", "4" = "student", "5"= "student", "6" = "Homemaker", "7" = "Retired", "8" = "Not Currently Employed", "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Employment <- factor(ResidentialChoice_PersonHouseholdData$Employment, levels = c("Self, part, or full-time employment", "student", "Homemaker", "Retired", "Not Currently Employed", "NA"), exclude = "NA")

ResidentialChoice_PersonHouseholdData$Age <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$age), c("4" = "age 18-34", "5"= "age 18-34", "6" = "age 35-54", "7" = "age 35-54", "8"= "age 55 or older", "9" = "age 55 or older", "10" = "age 55 or older", "11" = "age 55 or older", "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Age <- factor(ResidentialChoice_PersonHouseholdData$Age, levels = c("age 18-34", "age 35-54", "age 55 or older", "NA"), exclude = "NA")

ResidentialChoice_PersonHouseholdData$Gender <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$gender), c("1" = "male", "2"= "female", "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Gender <- factor(ResidentialChoice_PersonHouseholdData$Gender, levels = c("male", "female", "NA"), exclude = "NA")

ResidentialChoice_PersonHouseholdData$Home_Region <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$home_regionid), c("1" = "Cache", "2"= "WFRC_MAG", "3" = "Dixie", "4" = "Utah Other", "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Home_Region <- factor(ResidentialChoice_PersonHouseholdData$Home_Region, levels = c("Cache", "WFRC_MAG", "Dixie", "Utah Other", "NA"), exclude = "NA")

ResidentialChoice_PersonHouseholdData$Plan_to_Move <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$home_regionid), c("1" = "Yes", "2"= "No", "3" = "Unsure", "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Plan_to_Move <- factor(ResidentialChoice_PersonHouseholdData$Plan_to_Move, levels = c("Yes", "No", "Unsure", "NA"), exclude = "NA")

ResidentialChoice_PersonHouseholdData$Curr_Place_Type <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$curr_place_type), c("1" = "City downtown, res and/or comm mix", "2"= "City downtown, res and/or comm mix", "3" = "Suburban res and/or comm mix", "4" = "Suburban res and/or comm mix", "5" = "Small Town", "6" = "rural", "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Curr_Place_Type <- factor(ResidentialChoice_PersonHouseholdData$Curr_Place_Type, levels = c("City downtown, res and/or comm mix", "Suburban res and/or comm mix", "Small Town", "rural", "NA"), exclude = "NA")

ResidentialChoice_PersonHouseholdData$Curr_Res_Type <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$curr_res_type), c("1" = "Single Family Detached Home", "2" = "Townhome/Multi Family 3 or less homes", "3" = "Townhome/Multi Family 3 or less homes", "4" = "Building", "5" = "Building", "6" = "Mobile/Dorm/Other(boat/rv/van)", "7" = "Mobile/Dorm/Other(boat/rv/van)", "8" = "Mobile/Dorm/Other(boat/rv/van)",  "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Curr_Res_Type <- factor(ResidentialChoice_PersonHouseholdData$Curr_Res_Type, levels = c("Single Family Detached Home", "Townhome/Multi Family 3 or less homes", "Building", "Mobile/Dorm/Other(boat/rv/van)", "NA"), exclude = "NA")

ResidentialChoice_PersonHouseholdData$Prefer_Place <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$prefer_place), c("1" = "City downtown, res and/or comm mix", "2"= "City downtown, res and/or comm mix", "3" = "Suburban res and/or comm mix", "4" = "Suburban res and/or comm mix", "5" = "Small Town", "6" = "rural", "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Prefer_Place <- factor(ResidentialChoice_PersonHouseholdData$Prefer_Place, levels = c("City downtown, res and/or comm mix", "Suburban res and/or comm mix", "Small Town", "rural", "NA"), exclude = "NA")

ResidentialChoice_PersonHouseholdData$Prefer_Res_Type <- as.character(revalue(as.factor(ResidentialChoice_PersonHouseholdData$prefer_res_type), c("1" = "Single Family Detached Home", "2" = "Townhome/Multi Family 3 or less homes", "3" = "Townhome/Multi Family 3 or less homes", "4" = "Building", "5" = "Building", "6" = "Mobile/Dorm/Other(boat/rv/van)", "7" = "Mobile/Dorm/Other(boat/rv/van)", "8" = "Mobile/Dorm/Other(boat/rv/van)",  "NA" = "NA")))
ResidentialChoice_PersonHouseholdData$Prefer_Res_Type <- factor(ResidentialChoice_PersonHouseholdData$Prefer_Res_Type, levels = c("Single Family Detached Home", "Townhome/Multi Family 3 or less homes", "Building", "Mobile/Dorm/Other(boat/rv/van)", "NA"), exclude = "NA")

# Remove rents with reported rent less that 49 and homeonwer with homevalues tless than 20,000 and more than 10,000,000

library(psych)
a <- describe(ResidentialChoice_PersonHouseholdData)


# Choice Experiment Data
ResidentialChoice_ChoiceExperimentsData <- read_excel("C:/Users/A01246966/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/2. Choice Experiments Dataset/ResidentialChoice_ChoiceExperimentsData.xlsx")

sapply(ResidentialChoice_ChoiceExperimentsData, class)


ResidentialChoice_ChoiceExperimentsData <- ResidentialChoice_ChoiceExperimentsData %>% mutate_at(vars(1:18), funs(as.factor))



# Use Mlogit to Convert data into long format
library(mlogit)
LogitData <- as.data.frame(mlogit.data(ResidentialChoice_ChoiceExperimentsData, shape = "wide", choice = "choice", sep = "", varying = c(5:18), alt.levels = c(1,2), id="password"))

# Rename Levels of LogitData
library(dplyr)

LogitData$commute <- as.character(revalue(as.factor(LogitData$commute), c("1" = "Less Than 3 Miles", "2"= "5 Miles", "3" = "10 miles", "4" = "20 Miles")))
LogitData$commute <- factor(LogitData$commute, levels = c("Less Than 3 Miles", "5 Miles", "10 miles", "20 Miles"))

LogitData$destinations <- revalue(as.factor(LogitData$destinations), c("1" = "Walking distance", "2" = "Less than 3 Miles", "3" = "Less than 10 Miles", "4" = "10 Miles or more"))
LogitData$destinations <- factor(LogitData$destinations, levels = c("Walking distance", "Less than 3 Miles", "Less than 10 Miles", "10 Miles or more"))

LogitData$homes <- as.character(revalue(as.factor(LogitData$homes), c("1" = "Mix, Single Fam 1/4 acre lots, townhomes, apartments, condos", "2" = "Mix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "3" = "Only single fam 1/2 acre lots", "4" = "Single Fam 1+ acre Lots")))
LogitData$homes <- factor(LogitData$homes, levels = c("Mix, Single Fam 1/4 acre lots, townhomes, apartments, condos", "Mix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "Only single fam 1/2 acre lots", "Single Fam 1+ acre Lots"))

LogitData$parking <- revalue(as.factor(LogitData$parking), c("1" = "In personal drive/garage", "2" = "On-street or free parking", "3" = "Off-street or Paid Parking")) 
LogitData$parking <- factor(LogitData$parking, levels = c("In personal drive/garage", "On-street or free parking", "Off-street or Paid Parking"))
                                                               
LogitData$streets <- revalue(as.factor(LogitData$streets), c("1" = "Primarily for Cars", "2" = "For Cars, Pedestrians, and cyclers"))
LogitData$streets <- factor(LogitData$streets, c("Primarily for Cars", "For Cars, Pedestrians, and cyclers"))

LogitData$transit <- revalue(as.factor(LogitData$transit), c("1" = "Rail station and bus within walking distance", "2" = "Bus stop within walking distance and Rail 5 miles away", "3" = "Rail and bus 5 miles away", "4" = "Rail and Bus 10 miles Away"))
LogitData$transit <- factor(LogitData$transit, levels = c("Rail station and bus within walking distance", "Bus stop within walking distance and Rail 5 miles away", "Rail and bus 5 miles away", "Rail and Bus 10 miles Away"))

LogitData$alt <- factor(LogitData$alt, levels = c("1", "2"))

LogitData$price <- revalue(as.factor(LogitData$price), c("1" = "0.8", "2" = "0.9", "3" = "1", "4" = "1.1", "5" = "1.2"))
sapply(LogitData, class)

# Join Logit data with demographics
ResChoiceData <- plyr::join(LogitData, ResidentialChoice_PersonHouseholdData, type = "inner")


# Number one
# ResChoice Data with Homeowners linked to Census Data

# Joining on Census Tract
CensTract_Estimate <- plyr::join(GeoID, ACS_12_5YR, type = "inner")
CensTract_Estimate <- subset(CensTract_Estimate, CensTract_Estimate$`Estimate; Median value (dollars)` != "-")
CensTract_Estimate$`Estimate; Median value (dollars)` <- as.numeric(CensTract_Estimate$`Estimate; Median value (dollars)`)

ResChoice_w_CensTract <- plyr::join(ResChoiceData, CensTract_Estimate, type = "inner")
ResChoice_w_CensTract <- subset(ResChoice_w_CensTract, ResChoice_w_CensTract$rent_own == "2")

ResChoice_w_CensTract$Change_in_Price <- as.numeric(levels(ResChoice_w_CensTract$price))[ResChoice_w_CensTract$price]*as.numeric(ResChoice_w_CensTract$`Estimate; Median value (dollars)`)

ResChoice_w_CensTract$Relative_Change_Price <- ResChoice_w_CensTract$Change_in_Price - ResChoice_w_CensTract$`Estimate; Median value (dollars)`

ResChoice_w_CensTract$Diff_BTW_HomeV_Estimate <- as.numeric(ResChoice_w_CensTract$home_price) - as.numeric(ResChoice_w_CensTract$`Estimate; Median value (dollars)`)
ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate <- as.numeric(ResChoice_w_CensTract$home_price) / as.numeric(ResChoice_w_CensTract$`Estimate; Median value (dollars)`)

ResChoice_w_CensTract$over_est <- ifelse(ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1, "1", "0")
ResChoice_w_CensTract$under_est <- ifelse(ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1, "0", "1")

saveRDS(ResChoice_w_CensTract, file = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoice_w_CensTract.RDS")


# Joining on Census Block

CensBlock_Estimate <- plyr::join(GeoID, ACS_13_5YR, type = "inner")
CensBlock_Estimate <- subset(CensBlock_Estimate, CensBlock_Estimate$`Estimate; Median value (dollars)` != "1,000,000+"
                             & CensBlock_Estimate$`Estimate; Median value (dollars)` != "10,000-"
                             & CensBlock_Estimate$`Estimate; Median value (dollars)` != "-")

ResChoice_w_CensBlock <- plyr::join(ResChoiceData, CensBlock_Estimate, type = "inner")
ResChoice_w_CensBlock <- subset(ResChoice_w_CensBlock, ResChoice_w_CensBlock$rent_own == "2")

ResChoice_w_CensBlock$Change_in_Price <- as.numeric(levels(ResChoice_w_CensBlock$price))[ResChoice_w_CensBlock$price]*as.numeric(ResChoice_w_CensBlock$`Estimate; Median value (dollars)`)

ResChoice_w_CensBlock$Relative_Change_Price <- ResChoice_w_CensBlock$Change_in_Price - as.numeric(ResChoice_w_CensBlock$`Estimate; Median value (dollars)`)

ResChoice_w_CensBlock$Diff_BTW_HomeV_Estimate <- as.numeric(ResChoice_w_CensBlock$home_price) - as.numeric(ResChoice_w_CensBlock$`Estimate; Median value (dollars)`)
ResChoice_w_CensBlock$Ratio_BTW_HomeV_Estimate <- as.numeric(ResChoice_w_CensBlock$home_price) / as.numeric(ResChoice_w_CensBlock$`Estimate; Median value (dollars)`)

saveRDS(ResChoice_w_CensBlock, file = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoice_w_CensBlock.RDS")


# Number Two and Three
# ResChoice Data with Homeowners and renters linked to Home Values

# Make one Price Estimate vector

ResChoiceData$Stated_Price <- ifelse(ResChoiceData$rent_own == "1", ResChoiceData$rent_price, ResChoiceData$home_price)

ResChoiceData <- subset(ResChoiceData, ResChoiceData$Stated_Price != "NA")

ResChoiceData$Change_in_Price <- as.numeric(ResChoiceData$Stated_Price) * as.numeric(levels(ResChoiceData$price))[ResChoiceData$price]

ResChoiceData$Relative_Change_Price <- ResChoiceData$Change_in_Price - ResChoiceData$Stated_Price

saveRDS(ResChoiceData, file = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoiceData.RDS")
write.csv(ResChoiceData, file = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoiceData.csv")





# Create binary models for logistic regression


              #### Parsimonious Models #######

# Determine Degrees of Freedom, We have 200
df <-unique(ResidentialChoice_ChoiceExperimentsData[5:18])
nrow(df)
library(lmtest)
library(sandwich)
library(ResourceSelection)

Home_Tract_Model <- glm(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price, data = ResChoice_w_CensTract, na.action = na.omit, family = binomial)
Home_Tract_Model_robust <- sqrt(diag(vcovHC(Home_Tract_Model, type = "HC0")))
Home_Tract_Model.pr2 <- round(1 - Home_Tract_Model$deviance/Home_Tract_Model$null.deviance, digits = 3)

Home_Block_Model <- glm(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price, data = ResChoice_w_CensBlock, na.action = na.omit, family = binomial)
Home_Block_Model_robust <- sqrt(diag(vcovHC(Home_Block_Model, type = "HC0")))
Home_Block_Model.pr2 <- round(1 - Home_Block_Model$deviance/Home_Block_Model$null.deviance, digits = 3)


Home_Value_Model <- glm(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price, data = subset(ResChoiceData, ResChoiceData$rent_own == "2"), family = binomial)
Home_Value_Model_robust <- sqrt(diag(vcovHC(Home_Value_Model, type = "HC0")))
Home_Value_Model.pr2 <- round(1 - Home_Value_Model$deviance/Home_Value_Model$null.deviance, digits = 3)


Renter_Value_Model <- glm(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price, data = subset(ResChoiceData, ResChoiceData$rent_own == "1" & ResChoiceData$Stated_Price > 49), na.action = na.omit, family = binomial)
Renter_Value_Model_robust <- sqrt(diag(vcovHC(Renter_Value_Model, type = "HC0")))
Renter_Value_Model.pr2 <- round(1 - Renter_Value_Model$deviance/Renter_Value_Model$null.deviance, digits = 3)


library(stargazer)
stargazer(Home_Tract_Model, Home_Value_Model, Renter_Value_Model,
          se = list(Home_Tract_Model_robust, Home_Value_Model_robust, Renter_Value_Model_robust),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Home_Tract_Model.pr2, Home_Value_Model.pr2, Renter_Value_Model.pr2)),
          column.labels = c("Home with Tract", "Home with Stated", "Renter with Stated"),
          type = "html",
          title = "Parsimonious GLM Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/PGLMmodels.htm")


        
            ##### Scaled Models ###########


# Divide by 100

Home_Tract_Model_div.1 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/100), data = ResChoice_w_CensTract, na.action = na.omit, family = binomial)
Home_Tract_Model_robust_div.1 <- sqrt(diag(vcovHC(Home_Tract_Model_div.1, type = "HC0")))
Home_Tract_Model.pr2_div.1 <- round(1 - Home_Tract_Model_div.1$deviance/Home_Tract_Model_div.1$null.deviance, digits = 3)

Home_Block_Model_div.1 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/100), data = ResChoice_w_CensBlock, na.action = na.omit, family = binomial)
Home_Block_Model_robust_div.1 <- sqrt(diag(vcovHC(Home_Block_Model_div.1, type = "HC0")))
Home_Block_Model.pr2_div.1 <- round(1 - Home_Block_Model_div.1$deviance/Home_Block_Model_div.1$null.deviance, digits = 3)


Home_Value_Model_div.1 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/100), data = subset(ResChoiceData, ResChoiceData$rent_own == "2"), na.action = na.omit, family = binomial)
Home_Value_Model_robust_div.1 <- sqrt(diag(vcovHC(Home_Value_Model_div.1, type = "HC0")))
Home_Value_Model.pr2_div.1 <- round(1 - Home_Value_Model_div.1$deviance/Home_Value_Model_div.1$null.deviance, digits = 3)


Renter_Value_Model_div.1 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/100), data = subset(ResChoiceData, ResChoiceData$rent_own == "1"), na.action = na.omit, family = binomial)
Renter_Value_Model_robust_div.1 <- sqrt(diag(vcovHC(Renter_Value_Model_div.1, type = "HC0")))
Renter_Value_Model.pr2_div.1 <- round(1 - Renter_Value_Model_div.1$deviance/Renter_Value_Model_div.1$null.deviance, digits = 3)




# Divide by 1000

Home_Tract_Model_div.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = ResChoice_w_CensTract, na.action = na.omit, family = binomial)
Home_Tract_Model_robust_div.2 <- sqrt(diag(vcovHC(Home_Tract_Model_div.2, type = "HC0")))
Home_Tract_Model.pr2_div.2 <- round(1 - Home_Tract_Model_div.2$deviance/Home_Tract_Model_div.2$null.deviance, digits = 3)

Home_Block_Model_div.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = ResChoice_w_CensBlock, na.action = na.omit, family = binomial)
Home_Block_Model_robust_div.2 <- sqrt(diag(vcovHC(Home_Block_Model_div.2, type = "HC0")))
Home_Block_Model.pr2_div.2 <- round(1 - Home_Block_Model_div.2$deviance/Home_Block_Model_div.2$null.deviance, digits = 3)


Home_Value_Model_div.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoiceData, ResChoiceData$rent_own == "2"), na.action = na.omit, family = binomial)
Home_Value_Model_robust_div.2 <- sqrt(diag(vcovHC(Home_Value_Model_div.2, type = "HC0")))
Home_Value_Model.pr2_div.2 <- round(1 - Home_Value_Model_div.2$deviance/Home_Value_Model_div.2$null.deviance, digits = 3)


Renter_Value_Model_div.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoiceData, ResChoiceData$rent_own == "1"), na.action = na.omit, family = binomial)
Renter_Value_Model_robust_div.2 <- sqrt(diag(vcovHC(Renter_Value_Model_div.2, type = "HC0")))
Renter_Value_Model.pr2_div.2 <- round(1 - Renter_Value_Model_div.2$deviance/Renter_Value_Model_div.2$null.deviance, digits = 3)




# Divide by 10000

Home_Tract_Model_div.3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/10000), data = ResChoice_w_CensTract, na.action = na.omit, family = binomial)
Home_Tract_Model_robust_div.3 <- sqrt(diag(vcovHC(Home_Tract_Model_div.3, type = "HC0")))
Home_Tract_Model.pr2_div.3 <- round(1 - Home_Tract_Model_div.3$deviance/Home_Tract_Model_div.3$null.deviance, digits = 3)


Home_Block_Model_div.3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/10000), data = ResChoice_w_CensBlock, na.action = na.omit, family = binomial)
Home_Block_Model_robust_div.3 <- sqrt(diag(vcovHC(Home_Block_Model_div.3, type = "HC0")))
Home_Block_Model.pr2_div.3 <- round(1 - Home_Block_Model_div.3$deviance/Home_Block_Model_div.3$null.deviance, digits = 3)


Home_Value_Model_div.3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/10000), data = subset(ResChoiceData, ResChoiceData$rent_own == "2"), na.action = na.omit, family = binomial)
Home_Value_Model_robust_div.3 <- sqrt(diag(vcovHC(Home_Value_Model_div.3, type = "HC0")))
Home_Value_Model.pr2_div.3 <- round(1 - Home_Value_Model_div.3$deviance/Home_Value_Model_div.3$null.deviance, digits = 3)


Renter_Value_Model_div.3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoiceData, ResChoiceData$rent_own == "1"), na.action = na.omit, family = binomial)
Renter_Value_Model_robust_div.3 <- sqrt(diag(vcovHC(Renter_Value_Model_div.3, type = "HC0")))
Renter_Value_Model.pr2_div.3 <- round(1 - Renter_Value_Model_div.3$deviance/Renter_Value_Model_div.3$null.deviance, digits = 3)


stargazer(Home_Tract_Model_div.1, Home_Tract_Model_div.2, Home_Tract_Model_div.3, Home_Value_Model_div.1, Home_Value_Model_div.2, Home_Value_Model_div.3,
          se = list(Home_Tract_Model_robust_div.1, Home_Tract_Model_robust_div.2, Home_Tract_Model_robust_div.3, Home_Value_Model_robust_div.1, Home_Value_Model_robust_div.2, Home_Value_Model_robust_div.3),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Home_Tract_Model.pr2_div.1, Home_Tract_Model.pr2_div.2, Home_Tract_Model.pr2_div.3, Renter_Value_Model.pr2_div.1, Renter_Value_Model.pr2_div.2, Renter_Value_Model.pr2_div.3)),
          column.labels = c("Home with Tract", "Home with Tract", "Home with Tract", "Home with Stated", "Home with Stated", "Home with Stated"),
          type = "html",
          title = "GLM Scaled Price variables",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/PDIVGLMmodels.htm")




          ######## Graphics and Models for Over and Under Valuation #######


# Create Realative Price Change with State Price
ResChoice_w_CensTract$S_Change_in_Price <- as.numeric(levels(ResChoice_w_CensTract$price))[ResChoice_w_CensTract$price]*(ResChoice_w_CensTract$home_price)

ResChoice_w_CensTract$S_Relative_Change_Price <- ResChoice_w_CensTract$Change_in_Price - ResChoice_w_CensTract$home_price


  
  
  
Home_Tract_Model_Over <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1), na.action = na.omit, family = binomial)
Home_Tract_Model_Over_Robust <- sqrt(diag(vcovHC(Home_Tract_Model_Over, type = "HC0")))
Home_Tract_Model_Over.pr2 <- round(1 - Home_Tract_Model_Over$deviance/Home_Tract_Model_Over$null.deviance, digits = 3)



Home_Tract_Model_Under <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1), na.action = na.omit, family = binomial)
Home_Tract_Model_Under_Robust <- sqrt(diag(vcovHC(Home_Tract_Model_Under, type = "HC0")))
Home_Tract_Model_Under.pr2 <- round(1 - Home_Tract_Model_Under$deviance/Home_Tract_Model_Under$null.deviance, digits = 3)



Home_Value_Model_Over.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1), na.action = na.omit, family = binomial)
Home_Tract_Model_Over_Robust.2 <- sqrt(diag(vcovHC(Home_Tract_Model_Over, type = "HC0")))
Home_Tract_Model_Over.pr2.2 <- round(1 - Home_Tract_Model_Over$deviance/Home_Tract_Model_Over$null.deviance, digits = 3)



Home_Value_Model_Under.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1), na.action = na.omit, family = binomial)
Home_Value_Model_Under_Robust.2 <- sqrt(diag(vcovHC(Home_Tract_Model_Under, type = "HC0")))
Home_Value_Model_Under.pr2.2 <- round(1 - Home_Tract_Model_Under$deviance/Home_Tract_Model_Under$null.deviance, digits = 3)

stargazer(Home_Tract_Model_Over, Home_Tract_Model_Under, Home_Value_Model_Over.2, Home_Value_Model_Under.2,
          se = list(Home_Tract_Model_Over_Robust, Home_Tract_Model_Under_Robust, Home_Tract_Model_Over_Robust.2, Home_Value_Model_Under_Robust.2),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Home_Tract_Model_Over.pr2, Home_Tract_Model_Under.pr2, Home_Tract_Model_Over.pr2.2, Home_Value_Model_Under.pr2.2)),
          column.labels = c("Home with Tract(Over)", "Home with Tract(Under)", "Home with Stated(Over)", "Home with Stated(Under)"),
          type = "html",
          title = "GLM Over Under Value Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/OverUnderGLMmodels.htm")







          ######## Remove Outliers ########

ggplot( data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 
                        1), aes(x = Ratio_BTW_HomeV_Estimate)) + geom_histogram(binwidth = 0.04) + labs(title = "") 

ggplot( data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 
                        1), aes(x = Ratio_BTW_HomeV_Estimate)) + geom_histogram(binwidth = 35) + labs(title = "") 


Over_Ratio_mean <- mean(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1)$Ratio_BTW_HomeV_Estimate)
Over_Ratio_sd <- sd(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1)$Ratio_BTW_HomeV_Estimate)
O <- Over_Ratio_mean + 3 * Over_Ratio_sd

x <- describe(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1)$Ratio_BTW_HomeV_Estimate)

ggplot( data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 
                        1 & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), aes(x = Ratio_BTW_HomeV_Estimate)) + geom_histogram(binwidth = 2.3) + labs(title = "") 

Under_Ratio_mean <- mean(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1)$Ratio_BTW_HomeV_Estimate)
Under_Ratio_sd <- sd(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1)$Ratio_BTW_HomeV_Estimate)
U <- Under_Ratio_mean - 3*Under_Ratio_sd

ggplot( data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 
                        1 & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U), aes(x = Ratio_BTW_HomeV_Estimate)) + geom_histogram(binwidth = 0.019) + labs(title = "") 



Home_Value_Model_Over_No.Out <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                                    data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1 & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), na.action = na.omit, family = binomial)
Home_Value_Model_Over_No.Out_Robust <- sqrt(diag(vcovHC(Home_Value_Model_Over_No.Out, type = "HC0")))
Home_Value_Model_Over_No.Out.pr2 <- round(1 - Home_Value_Model_Over_No.Out$deviance/Home_Value_Model_Over_No.Out$null.deviance, digits = 3)



Home_Value_Model_Under_No.Out <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                                     data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1 & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U), na.action = na.omit, family = binomial)
Home_Value_Model_Under_No.Out_Robust <- sqrt(diag(vcovHC(Home_Value_Model_Under_No.Out, type = "HC0")))
Home_Value_Model_Under_No.Out.pr2 <- round(1 - Home_Value_Model_Under_No.Out$deviance/Home_Value_Model_Under_No.Out$null.deviance, digits = 3)


Home_Value_Model_No.Out <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                               data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), na.action = na.omit, family = binomial)
Home_Value_Model_No.Out_Robust <- sqrt(diag(vcovHC(Home_Value_Model_No.Out, type = "HC0")))
Home_Value_Model_No.Out.pr2 <- round(1 - Home_Value_Model_No.Out$deviance/Home_Value_Model_No.Out$null.deviance, digits = 3)



# Number of over valuations two time the median estimate

nrow(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate <1 ))/20
table(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 2)$Ratio_BTW_HomeV_Estimate)/20

Home_Value_Model_No.Out2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                               data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 2), na.action = na.omit, family = binomial)
Home_Value_Model_No.Out2_Robust <- sqrt(diag(vcovHC(Home_Value_Model_No.Out2, type = "HC0")))
Home_Value_Model_No.Ou2t_No.Out.pr2 <- round(1 - Home_Value_Model_No.Out2$deviance/Home_Value_Model_No.Out2$null.deviance, digits = 3)


# Number of over valuations Three time the median estimate


Home_Value_Model_No.Out3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                               data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 3), na.action = na.omit, family = binomial)
Home_Value_Model_No.Out3_Robust <- sqrt(diag(vcovHC(Home_Value_Model_No.Out3, type = "HC0")))
Home_Value_Model_No.Out3.pr2 <- round(1 - Home_Value_Model_No.Out3$deviance/Home_Value_Model_No.Out3$null.deviance, digits = 3)


stargazer(Home_Value_Model_Over_No.Out, Home_Value_Model_Under_No.Out, Home_Value_Model_No.Out, Home_Value_Model_No.Out2, Home_Value_Model_No.Out3,
          se = list(Home_Value_Model_Over_No.Out_Robust, Home_Value_Model_Under_No.Out_Robust, Home_Value_Model_No.Out_Robust, Home_Value_Model_No.Out2_Robust, Home_Value_Model_No.Out3_Robust),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Home_Value_Model_Over_No.Out.pr2, Home_Value_Model_Under_No.Out.pr2, Home_Value_Model_No.Out.pr2, Home_Value_Model_No.Ou2t_No.Out.pr2, Home_Value_Model_No.Out3.pr2, NULL)),
          column.labels = c("Over Valued Estimates, cut off at 3 SD above", "Under Valued Estimates, cutoff at 3 SD below", "Both Over and Under Valued, cutoff +- 3 SD", "Both over and Under, 2 times median cutoff and 3 SD under", "BOth over and under, 3 times median cutoff and 3 SD below"),
          type = "html",
          title = "GLM Outlier Removed Models With Stated Value",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/OverUnderNoOutGLMmodels.htm")


saveRDS(subset(ResChoiceData, ResChoice_w_CensTract$rent_own == "2"), file = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/OwnerData.RDS")
saveRDS(subset(ResChoiceData, ResChoiceData$rent_own == "1"), file = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/RenterData.RDS")


library(nnet)

a <- glm(choice ~ alt + commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O))
b <- glm(choice ~ 0 + alt + commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O))
c <- glm(choice ~ 0 + commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O))
d <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O))

e <- mlogit(choice ~ alt + commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Income:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit) | -1, data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")
f <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit | Income |  I(Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

g <- mlogit(choice ~ alt + commute + destinations + homes + parking + streets + transit | Income:I(Relative_Change_Price/1000) + 0 , data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")





e2 <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Income:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit) | -1, data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")


stargazer(a, b, d, d, e, f,
          no.space = TRUE,
          intercept.bottom = FALSE,
          type = "html",
          title = "Outlier Removed Models With Stated Value",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/mlogitvsglm.htm")




