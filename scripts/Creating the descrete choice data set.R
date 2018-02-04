
  ## Merging Person Data and Choice Exeriment data together ##

# Kristopher C. Toll

# Refactroing Choice Exeriment data

  ## Reading in Residental Choice data ##

ResChoiceExp <- readxl::read_excel("C:/Users/Kristopher/odrive/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/2. Choice Experiments Dataset/ResidentialChoice_ChoiceExperimentsData.xlsx")

# Refactor Choice variables, 1 -> 0 and 2 -> 1

ResChoiceExp$choice_a <- ifelse(ResChoiceExp$choice == "2", "1", "0") # 0 is left side choice, 1 is right side choice

# Refactor Commute,  1 | 2 -> 0, & 3 | 4 -> 1

ResChoiceExp$commute1_a <- ifelse(ResChoiceExp$commute1 == "1" | ResChoiceExp$commute1 == "2", "0", "1") # 0 is less than 5 miles or three miles, 1 is 10 or 20 miles
ResChoiceExp$commute2_a <- ifelse(ResChoiceExp$commute2 == "1" | ResChoiceExp$commute2 == "2", "0", "1") 

# Refactor Destination variables, 1 | 2 -> 0, 3 | 4 -> 1

ResChoiceExp$destinations1_a <- ifelse(ResChoiceExp$destinations1 == "1" | ResChoiceExp$destinations1 == "2", "0", "1") # 0 is less than five miles
ResChoiceExp$destinations2_a <- ifelse(ResChoiceExp$destinations2 == "1" | ResChoiceExp$destinations2 == "2", "0", "1") # 1 is less than 10 or more miles

# Refactor homes variable, 1 | 2 -> 0, 3 | 4 -> 1

ResChoiceExp$homes1_a <- ifelse(ResChoiceExp$homes1 == "1" | ResChoiceExp$homes1 == "2", "0", "1") # 0 is mix of single famliy, townhomes, apartments and condominiums
ResChoiceExp$homes2_a <- ifelse(ResChoiceExp$homes2 == "1" | ResChoiceExp$homes2 == "2", "0", "1") # 1 is only single fmaily houses

# Refactor streets, 1 -> 0, 2 -> 1
ResChoiceExp$streets1_a <- ifelse(ResChoiceExp$streets1 == "1", "0", "1") # 0 is for cars, 1 is for cars, pedestrians and bikes
ResChoiceExp$streets2_a <- ifelse(ResChoiceExp$streets2 == "1", "0", "1")

# Refactor transit, 1 | 2 -> 0, 3 | 4 -> 1

ResChoiceExp$transit1_a <- ifelse(ResChoiceExp$transit1 == "1" | ResChoiceExp$transit1 == "2", "0", "1") # 0 is for public transit is less than 5 miles or walkingdistance
ResChoiceExp$transit2_a <- ifelse(ResChoiceExp$transit2 == "1" | ResChoiceExp$transit2 == "2", "0", "1") # 1 is for les than ten miles

# Refactoring parking, 

ResChoiceExp$parking1_driveway_a <- ifelse(ResChoiceExp$parking1 == "1", "1", "0") # parking is avalible on driveway
ResChoiceExp$parking1_on_street_a <- ifelse(ResChoiceExp$parking1 == "2", "1", "0") # parkding is avalible on street or lot near homes
ResChoiceExp$parking1_off_street_a <- ifelse(ResChoiceExp$parking1 == "3", "1", "0") # parking si avalibe off-street(lot and/or garage) near house(monthly rental)

ResChoiceExp$parking2_driveway_a <- ifelse(ResChoiceExp$parking2 == "1", "1", "0")
ResChoiceExp$parking2_on_street_a <- ifelse(ResChoiceExp$parking2 == "2", "1", "0")
ResChoiceExp$parking2_off_street_a <- ifelse(ResChoiceExp$parking2 == "3", "1", "0")

# Place adjusted variables into its own data object

ResChoiceExp_a <- ResChoiceExp[c(1, 2 , 3, 9, 16, 19:35)]

# Refactor price variable, 1 -> -2, 2 -> -1, 3 -> 0, 4 -> 1, 5 -> 2

ResChoiceExp_a$price1_a <- as.numeric(ifelse(ResChoiceExp_a$price1 == "1", "0.8", ifelse(ResChoiceExp_a$price1 == "2", "0.9", ifelse(ResChoiceExp_a$price1 == "3", "1", ifelse(ResChoiceExp_a$price1 == "4", "1.1", "1.2")))))
ResChoiceExp_a$price2_a <- as.numeric(ifelse(ResChoiceExp_a$price2 == "1", "0.8", ifelse(ResChoiceExp_a$price2 == "2", "0.9", ifelse(ResChoiceExp_a$price2 == "3", "1", ifelse(ResChoiceExp_a$price2 == "4", "1.1", "1.2")))))
ResChoiceExp_a$price1 <- NULL
ResChoiceExp_a$price2 <- NULL
  ## Refactoring PersonHouseHold Data ##

# Reading in data

PersonHouseholdData <- readxl::read_excel("C:/Users/Kristopher/odrive/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/1. Person Household Dataset/ResidentialChoice_PersonHouseholdData.xlsx")

# Rent_Own can be used to keep track of who rent and own
# There is not need for there to be two price vectors

# Combining rent price and owner price into one vector

renters <- subset(PersonHouseholdData, rent_price != "NA")
renters$home_price <- NULL
colnames(renters)[colnames(renters)=="rent_price"] <- "price"

owners <- subset(PersonHouseholdData, home_price != "NA")
owners$rent_price <- NULL
colnames(owners)[colnames(owners)=="home_price"] <- "price"

PersonHouseholdData <- rbind(owners, renters)
remove(owners, renters)

  ## Merging both PersonHouseHoldData with ResChoiceExp ##

HouseResChoice <- plyr::join(ResChoiceExp_a, PersonHouseholdData, type= "inner")

# Creating the Nominal Price Vector

HouseResChoice$nominal_price1 <- (HouseResChoice$price1_a*HouseResChoice$price - HouseResChoice$price)
HouseResChoice$nominal_price2 <- (HouseResChoice$price2_a*HouseResChoice$price - HouseResChoice$price)

# Creating an ID

HouseResChoice$id <- as.numeric(as.factor(HouseResChoice$password))

# Reording variable for convenience

HouseResChoice <- HouseResChoice[c(199, 3:20, 197:198, 23:196)]

# Restructuring the dataset to represent discreate choices

HouseResChoice <- plyr::rename(HouseResChoice, c("thecount" = "thecount", "choice_a" = "choice", "commute1_a" = "commute1", "commute2_a" = "commute2", "destinations1_a" = "destinations1", "destinations2_a" = "destinations2", "homes1_a" = "homes1", "homes2_a" = "homes2", "streets1_a" = "streets1", "streets2_a" = "streets2", "transit1_a" = "transit1" , "transit2_a" = "transit2", "parking1_driveway_a" = "ParkingDriveway1", "parking1_on_street_a" = "ParkingOnStreet1", "parking1_off_street_a" = "ParkingOffStreet1", "parking2_driveway_a" = "ParkingDriveway2", "parking2_on_street_a" = "ParkingOnStreet2", "parking2_off_street_a" = "ParkingOffStreet2", "id" = "id", "nominal_price1" = "NominalPrice1", "nominal_price2" = "NominalPrice2"))

LogitData <- mlogit::mlogit.data(HouseResChoice, shape = "wide", choice = "choice", sep = "", varying = c(4:21), alt.levels = c(1,2), id="id")

# Remove Uneeded Datasets

remove(ResChoiceExp, ResChoiceExp_a, PersonHouseholdData)

# Save the LogitData

write.csv(LogitData, file = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/LogitData.csv")
