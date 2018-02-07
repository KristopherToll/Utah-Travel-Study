    ## Multinominal and Mixed Logit models ##

# Kristopher C. Toll

# Read in data frame

# Data for descriptive stats

Owner_Renter_demographics <- read.csv(file = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/Owner_Renter_demographics.csv")

# Data for the models

LogitData <- read.csv(file = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/LogitData.csv")

# Descriptive Stats

a <- nrow(Owner_Renter_demographics)

region_Owner_rent_count <- table(Owner_Renter_demographics$rent_own, Owner_Renter_demographics$home_regionid)

income <- table(Owner_Renter_demographics$income)

