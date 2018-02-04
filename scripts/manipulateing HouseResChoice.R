
    ## ADJUSTING CHOICE EXPERIMENT DATA AND HOUSEHOLD DIARY AND THEN JOINING THEM TOGETHER ##

# Kristopher C. Toll
# October 2017


install.packages(c("manipulate", "mosaic", "plyr", "read_excel", "dplyr", "readxl"))



    ## Read in Census Data ##

ACS_5Yr_2012 <- readxl::read_excel("I:/Utah Travel Study/Utah Travel Study/modified_data/American_Community_Survery_5Yr_2012.xlsx", 
    sheet = "Sheet2")
names(ACS_5Yr_2012)[names(ACS_5Yr_2012) == 'Subject'] <- "census_tract"

# Remove uneeded rows

ACS_5Yr_2012_a <- subset(ACS_5Yr_2012, Subject2 == "Estimate")

# Create a numeric variable type out of Value_Median

ACS_5Yr_2012_a$`VALUE_Median (dollars)` <- gsub(",", "", ACS_5Yr_2012_a$`VALUE_Median (dollars)`)
ACS_5Yr_2012_a$`VALUE_Median (dollars)` <- as.numeric(ACS_5Yr_2012_a$`VALUE_Median (dollars)`)
ACS_5Yr_2012_a <- subset(ACS_5Yr_2012_a,  `VALUE_Median (dollars)` != "NA")

# Pull out variables of interest

ACS_5Yr_2012_2a <- ACS_5Yr_2012_a[c(1, 3, 11, 12:16, 25, 44, 49, 50)]


    ## REFACTORING VARIABLES IN RES-CHOICE-PERSONAL-DATA ##

ResChoicePes <- readxl::read_excel("I:/Utah Travel Study/Utah Travel Study/Kris Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/1. Person Household Dataset/ResidentialChoice_PersonHouseholdData.xlsx")

    ## REFACTORING VARIABLEs IN CHOICE EXPERIMENT DATA SET ##

# Reading in Choice Experiment data set

ResChoiceExp <- readxl::read_excel("I:/Utah Travel Study/Utah Travel Study/Kris Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/2. Choice Experiments Dataset/ResidentialChoice_ChoiceExperimentsData.xlsx", 
                                                      col_types = c("numeric", "text", "numeric", 
                                                                    "text", "text", "text", "text", "text", 
                                                                    "text", "text", "text", "text", "text", 
                                                                    "text", "text", "text", "text", "text"))



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




    ## REFACTORING VARIABLES IN HOUSEHOLD DIARY DATASET ## 

# This data is from the HouseHold diary, 

# Read in Household diary dataset 

HouseDiary <- readxl::read_excel("I:/Utah Travel Study/Utah Travel Study/Kris Travel Study/Utah Travel Study 2012/2. Data and Materials/1. Main Household Diary/1. Household-Level Dataset/HouseholdDiary_HouseholdData.xlsx", 
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

# Refactoring RegionID

HouseDiary$regionid_cache_a <- ifelse(HouseDiary$regionid == "1", "1", "0")
HouseDiary$regionid_WFRCMAG_a <- ifelse(HouseDiary$regionid == "2", "1", "0")
HouseDiary$regionid_dixie_a <- ifelse(HouseDiary$regionid == "3", "1", "0")
HouseDiary$regionid_other_a <- ifelse(HouseDiary$regionid == "4", "1", "0")

# Refactoring household life cycle

HouseDiary$no_child_or_retirees_a <- ifelse(HouseDiary$life_cycle == "1", "1", "0")
HouseDiary$child_no_retirees_a <- ifelse(HouseDiary$life_cycle == "2", "1", "0")
HouseDiary$retirees_a <- ifelse(HouseDiary$life_cycle == "3", "1", "0")

# Refactoring income variables

HouseDiary$income_missing_a <- ifelse(HouseDiary$hh_income_cat == "-1", "1", "0")
HouseDiary$income_under_35_a <- ifelse(HouseDiary$hh_income_cat == "1", "1", "0")
HouseDiary$income_35_to_49_a <- ifelse(HouseDiary$hh_income_cat == "2", "1", "0")
HouseDiary$income_50_to_99_a <- ifelse(HouseDiary$hh_income_cat == "3", "1", "0")
HouseDiary$income_over_100_a <- ifelse(HouseDiary$hh_income_cat == "4", "1", "0")

# Refactoring house owner type

HouseDiary$OwnerType_rent_a <- ifelse(HouseDiary$rent_own == "1", "1", "0")
HouseDiary$OwnerType_own_a <- ifelse(HouseDiary$rent_own == "2", "1", "0")
HouseDiary$OwnerType_other_a <- ifelse(HouseDiary$rent_own == "3", "1", "0")
HouseDiary$OwnerType_no_ans_a <- ifelse(HouseDiary$rent_own == "4", "1", "0")

# Refactoring years lived at residence

HouseDiary$years_at_res_1_to_5_a <- ifelse(HouseDiary$num_years_res == "1" | HouseDiary$num_years_res == "2", "1", "0")
HouseDiary$years_at_res_6_to_15_a <- ifelse(HouseDiary$num_years_res == "3" | HouseDiary$num_years_res == "4", "1", "0")
HouseDiary$year_at_res_over_16_a <- ifelse(HouseDiary$num_years_res == "5" | HouseDiary$num_years_res == "6", "1", "0")

# Refactoring location type

HouseDiary$place_type_city_a <- ifelse(HouseDiary$place_type == "1" | HouseDiary$place_type == "2", "1", "0")
HouseDiary$place_type_suburban_a <- ifelse(HouseDiary$place_type == "3" | HouseDiary$place_type == "4", "1", "0")
HouseDiary$place_type_SmallTownRural_a <- ifelse(HouseDiary$place_type == "5" | HouseDiary$place_type == "6", "1", "0")

# Refactoring Res_type

HouseDiary$ResType_SingleFam_a <- ifelse(HouseDiary$res_type == "1", "1", "0")
HouseDiary$ResType_TownMultiFam_a <- ifelse(HouseDiary$res_type == "2" | HouseDiary$res_type == "3", "1", "0")
HouseDiary$ResType_Building_a <- ifelse(HouseDiary$res_type == "4" | HouseDiary$res_type == "5", "1", "0")
HouseDiary$ResType_Mobile_a <- ifelse(HouseDiary$res_type == "6", "1", "0")
HouseDiary <- subset(HouseDiary, res_type != "7" | res_type != "8")

# Place Variabes of interests into its own data object

HouseDiary_a <- HouseDiary[c(1, 2, 8, 67:92, 5, 7, 11, 16, 17, 19, 24, 26, 27, 31, 32:51)]




    ## JOIN HOUSEDIARY, RESCHOICE, AND CENSUS DATA SETS, CREATE AN ID AND SAVE AS AN RDS FILE ##


# Joing Census Day with HouseResChoice_a and ResChiceExp_a

HouseResChoice <- plyr::join(ResChoiceExp_a, HouseDiary_a, type = "inner")
HouseResChoice_2 <- plyr::join(HouseResChoice, ACS_5Yr_2012_2a, type = "inner")

# Create a numbered ID

HouseResChoice_2$id <- as.numeric(as.factor(HouseResChoice_2$password))



    ## CREATING A NEW PRICE VARIABLE THAT SHOWS NOMINAL VALUE OF PREFERENCES ##


# Refactor price variable, 1 -> -2, 2 -> -1, 3 -> 0, 4 -> 1, 5 -> 2

HouseResChoice_2$price1_a <- ifelse(HouseResChoice_2$price1 == "1", "0.8", ifelse(HouseResChoice_2$price1 == "2", "0.9", ifelse(HouseResChoice_2$price1 == "3", "1", ifelse(HouseResChoice_2$price1 == "4", "1.1", "1.2"))))
HouseResChoice_2$price2_a <- ifelse(HouseResChoice_2$price2 == "1", "0.8", ifelse(HouseResChoice_2$price2 == "2", "0.9", ifelse(HouseResChoice_2$price2 == "3", "1", ifelse(HouseResChoice_2$price2 == "4", "1.1", "1.2"))))

# Create a Price variable that shows

HouseResChoice_2$price1_a <- as.numeric(HouseResChoice_2$price1_a)
HouseResChoice_2$Nominal_price1 <- (HouseResChoice_2$price1_a * HouseResChoice_2$`VALUE_Median (dollars)` - HouseResChoice_2$`VALUE_Median (dollars)`)

HouseResChoice_2$price2_a <- as.numeric(HouseResChoice_2$price2_a)
HouseResChoice_2$Nominal_price2 <- (HouseResChoice_2$price2_a * HouseResChoice_2$`VALUE_Median (dollars)` - HouseResChoice_2$`VALUE_Median (dollars)`)

# Save Modified HouseResChoice_2 as RDS

write.csv(HouseResChoice_2, file = "I:/Utah Travel Study/Utah Travel Study/modified_data/HouseResChoice_2.csv")
saveRDS(HouseResChoice_2, file = "I:/Utah Travel Study/Utah Travel Study/modified_data/HouseResChoice_2.rds")
