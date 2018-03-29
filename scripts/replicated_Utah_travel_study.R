    ## Replicated Utah Study ##

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

LogitData$commute <- ifelse(LogitData$commute == "1", "WalkingDistance", ifelse(LogitData$commute == "2", "5Miles", ifelse(LogitData$commute == "3", "10Miles", "20Miles")))
LogitData$destinations <- ifelse(LogitData$destinations == "1", "WalkingDistance", ifelse(LogitData$destinations == "2", "LessThan3Miles", ifelse(LogitData$destinations == "3", "3To10Miles", "MoreThan10Miles")))
LogitData$homes <- ifelse(LogitData$homes == "1", "QuartAcreSingleHomesApt", ifelse(LogitData$homes == "2", "HalfAcreSingleHomesApt", ifelse(LogitData$homes == "3", "HalfAcreHomes", "1+AcreHomes")))
LogitData$parking <- ifelse(LogitData$parking == "1", "OwnDrivewayOrGarage", ifelse(LogitData$parking == "2", "OnStreetOrInFreeLot", "OffStreetRentalLot"))
LogitData$price <- ifelse(LogitData$price == "1", "0.8", ifelse(LogitData$price == "2", "0.9", ifelse(LogitData$price == "3", "1", ifelse(LogitData$price == "4", "1.1", "1.2"))))
LogitData$streets <- ifelse(LogitData$streets == "1", "ForCars", "ForCarsPedBikes")
LogitData$transit <- ifelse(LogitData$transit == "1", "WalkingDistanceToRailBus", ifelse(LogitData$transit == "2", "WalkingToBus5mileToRail", ifelse(LogitData$transit == "3", "5MiletoBusRail", "10MiletoBusRail")))

# factor price so 0 is omited in models

LogitData$price <- factor(LogitData$price, levels = c("1", "0.8", "0.9", "1.1", "1.2"))
LogitData$commute <- relevel(as.factor(LogitData$commute), ref = "WalkingDistance")
LogitData$destinations <- relevel(as.factor(LogitData$destinations), ref = "WalkingDistance")
LogitData$homes <- relevel(as.factor(LogitData$homes), ref = "QuartAcreSingleHomesApt")
LogitData$streets <- relevel(as.factor(LogitData$streets), ref = "ForCars")
LogitData$transit <- relevel(as.factor(LogitData$transit), ref = "WalkingDistanceToRailBus")
LogitData$parking <- relevel(as.factor(LogitData$parking), ref = "OwnDrivewayOrGarage")

  ## REPILCATED MODELS FROM UTAH TRAVEL STUDY ##

# Model with no intercept and with alt variable included

res1 <- mlogit(choice ~ -1 + commute + destinations + homes + streets + transit + parking + price + alt,
                       data = subset(LogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

# Model With Intercept and alt
res2 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + price,
               data = subset(LogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")


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

 ## Run Models with NominalPrice Variable ##

# scale price variables by 1000

LogitData2$DiffNominalPrice_s <- LogitData2$DiffNominalPrice/1000
LogitData2$CompNominalPrice_s <- LogitData2$CompNominalPrice/1000

# Using Scaled DiffNominalPrice_s
res3 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + DiffNominalPrice_s,
               data = subset(LogitData2), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

# Using Scaled CompeNominalPrice_s
res4 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + CompNominalPrice_s,
               data = subset(LogitData2), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

#Doesn't work
res5 <- mlogit(choice ~ commute + destinations + homes +  DiffNominalPrice,
               data = subset(LogitData2, LogitData2$rent_own == "1"), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

# works with price_n
res6 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + price_n,
               data = subset(LogitData2), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

   ## Lets try binning ##

# Histogram