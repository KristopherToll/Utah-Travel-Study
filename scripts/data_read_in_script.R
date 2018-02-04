# Kristopher Travel Study
# Mergeing data sets

# Load in Residential choice survery

library(readxl)
ResidentialChoice_ChoiceExperimentsData <- read_excel("I:/Utah Travel Study/Utah Travel Study/Kris Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/2. Choice Experiments Dataset/ResidentialChoice_ChoiceExperimentsData.xlsx", 
    col_types = c("numeric", "text", "text", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "text", "text"))

# Load in trip data

library(readxl)
HouseholdDiary_TripData <- read_excel("I:/Utah Travel Study/Utah Travel Study/Kris Travel Study/Utah Travel Study 2012/2. Data and Materials/1. Main Household Diary/3. Trip-Level Dataset/HouseholdDiary_TripData.xlsx", 
    col_types = c("text", "numeric", "text", 
        "text", "text", "text", "text", "text", 
        "text", "text", "text", "text", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "text", "text", "text", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "text", "text", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric"))

library(readxl)
HouseholdDiary_HouseholdData <- read_excel("I:/Utah Travel Study/Utah Travel Study/Kris Travel Study/Utah Travel Study 2012/2. Data and Materials/1. Main Household Diary/1. Household-Level Dataset/HouseholdDiary_HouseholdData.xlsx", 
    col_types = c("text", "numeric", "text", 
        "text", "text", "text", "text", "text", 
        "text", "numeric", "numeric", "numeric", 
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
        "numeric", "numeric", "text", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "text"))

library(readxl)
HouseholdDiary_PersonData <- read_excel("I:/Utah Travel Study/Utah Travel Study/Kris Travel Study/Utah Travel Study 2012/2. Data and Materials/1. Main Household Diary/2. Person-Level Dataset/HouseholdDiary_PersonData.xlsx", 
    col_types = c("text", "numeric", "numeric", 
        "text", "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "text", "text", 
        "numeric", "text", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "text", "text", "text", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric"))

library(readxl)
HouseholdDiary_Vehicles <- read_excel("I:/Utah Travel Study/Utah Travel Study/Kris Travel Study/Utah Travel Study 2012/2. Data and Materials/1. Main Household Diary/4. Vehicle-Level Dataset/HouseholdDiary_Vehicles.xlsx", 
    col_types = c("text", "date", "text", 
        "text", "text", "numeric"))

# Merge Household Data with choice experiment data



saveRDS(HouseResChoice, file = "I:/Utah Travel Study/Utah Travel Study/modified_data/HouseResChoice.rds")

