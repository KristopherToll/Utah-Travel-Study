    ## Replicated Utah Study ##

# Kristopher C. Toll

# Import Unaltered Res Experiment data

library(readxl)
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

LogitData$commute <- ifelse(LogitData$commute == "1", "WalkingDistance", ifelse(LogitData$commute == "2", "5Miles", ifelse(LogitData$commute == "3", "10Miles", "20Miles")))
LogitData$destinations <- ifelse(LogitData$destinations == "1", "WalkingDistance", ifelse(LogitData$destinations == "2", "LessThan3Miles", ifelse(LogitData$destinations == "3", "LessThan10Miles", "MoreThan10Miles")))
LogitData$homes <- ifelse(LogitData$homes == "1", "QuarterAcreDetachedHousesTownHomesApartments", ifelse(LogitData$homes == "2", "HalfAcreDetachedHousesTownHomesApartments", ifelse(LogitData$homes == "3", "HalfAcreHomes", "1+AcreHomes")))
LogitData$parking <- ifelse(LogitData$parking == "1", "OwnDrivewayOrGarage", ifelse(LogitData$parking == "2", "OnStreetOrInFreeLot", "OffStreetRentalLot"))
LogitData$price <- ifelse(LogitData$price == "1", "-0.2", ifelse(LogitData$price == "2", "-0.1", ifelse(LogitData$price == "3", "0", ifelse(LogitData$price == "4", "0.1", "0.2"))))
LogitData$streets <- ifelse(LogitData$streets == "1", "ForCars", "ForCarsPedestriansBikes")
LogitData$transit <- ifelse(LogitData$transit == "1", "WalkingDistanceToRailBus", ifelse(LogitData$transit == "2", "WalkingToBus5mileToRail", ifelse(LogitData$transit == "3", "5MiletoBusRail", "10MiletoBusRail")))

# factor price so 0 is omited in models

#LogitData$price <- relevel(factor(LogitData$price), ref = "0")
LogitData$price <- factor(LogitData$price, levels = c("0", "-0.2", "-0.1", "0.1", "0.2"))

# Modle with no intercept and with alt variable included

res1 <- mlogit(choice ~ + commute + destinations + homes + streets + transit + parking + price,
                       data = subset(LogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

res2 <- mlogit(choice ~ + commute + destinations + homes + streets + transit + parking + relevel(factor(price) , ref="3")+ alt,
               data = subset(LogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")



ResidentialChoice_PersonHouseholdData <- read_excel("C:/Users/Kristopher/odrive/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/1. Person Household Dataset/ResidentialChoice_PersonHouseholdData.xlsx")
