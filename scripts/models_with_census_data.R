
    ## LOGISTIC REGRESSION USING HouseResChoice_2 DATASET ##



library(mlogit, readr, lmtest)

HouseResChoice_2 <- read.csv("I:/Utah Travel Study/Utah Travel Study/modified_data/HouseResChoice_2.csv")
HouseResChoice_2 <- plyr::rename(HouseResChoice_2, c("thecount" = "thecount", "choice_a" = "choice", "commute1_a" = "commute1", "commute2_a" = "commute2", "destinations1_a" = "destinations1", "destinations2_a" = "destinations2", "homes1_a" = "homes1", "homes2_a" = "homes2", "streets1_a" = "streets1", "streets2_a" = "streets2", "transit1_a" = "transit1" , "transit2_a" = "transit2", "parking1_driveway_a" = "ParkingDriveway1", "parking1_on_street_a" = "ParkingOnStreet1", "parking1_off_street_a" = "ParkingOffStreet1", "parking2_driveway_a" = "ParkingDriveway2", "parking2_on_street_a" = "ParkingOnStreet2", "parking2_off_street_a" = "ParkingOffStreet2", "id" = "id", "Nominal_price1" = "NominalPrice1", "Nominal_price2" = "NominalPrice2"))

# A data frame in long format needs to be created so that mlogit will recognize it as a descrete choice dataset

logitdata <- as.data.frame(HouseResChoice_2[c(93, 4, 7:23, 96:97, 24:92)])
logitdata2 <- mlogit::mlogit.data(logitdata, shape = "wide", choice = "choice", sep = "", varying = c(4:21), alt.levels = c(1,2), id="id")
logitdata2 <- logitdata2[c(1:3, 73:83, 4:72)]
logitdata2$NominalPrice <- (logitdata2$NominalPrice * -1)

# Save the data Frame
write.csv(logitdata2, file = "I:/Utah Travel Study/Utah Travel Study/modified_data/logitdata2.csv")

# Contruct the model

# Only Households that own and are single family homes will be considered

logitdata2 <- subset(logitdata2, OwnerType_own_a == "1" & ResType_SingleFam_a == "1")

# Seperate regressions will be done for homes that are located in small towns/Rural, cities, and subarbans
# Alternativly dummy variabes could be added but it was decided that it would be better to not estimate additional parameters to retain more degrees of freedom

# Model with only city observations
res1 <- mlogit::mlogit(choice ~1| commute + destinations + homes + streets + transit + ParkingDriveway  + ParkingOffStreet + NominalPrice + alt + chid,
                       data = subset(logitdata2, place_type_city_a == "1"), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")


# Model with only suburban observations
res2 <- mlogit::mlogit(choice ~1| commute + destinations + homes + streets + transit + ParkingDriveway  + ParkingOffStreet + NominalPrice + alt + chid,
                       data = subset(logitdata2, place_type_suburban_a == "1"), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

# Model with only small town/Rural observations
res3 <- mlogit::mlogit(choice ~1| commute + destinations + homes + streets + transit + ParkingDriveway  + ParkingOffStreet + NominalPrice + alt + chid,
                       data = subset(logitdata2, place_type_SmallTownRural_a == "1"), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")


# The Goodness of fit will be determined by either the Waldtest, lrtest or scoretest as soon as I figure out how to run them
# The same will go for heteroskedasticity