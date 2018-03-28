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

# Models

# LogitData$NominalPrice <- as.numeric((-1 * LogitData$NominalPrice))


# LogitData$alt <- as.numeric(ifelse(LogitData$alt == "2", "1", "0"))

FullModel <- glm(choice ~ NominalPrice + commute + destinations + homes + streets + transit + ParkingDriveway  + ParkingOffStreet + alt, data=LogitData)

library(ResourceSelection)
hoslem.test(FullModel$y, FullModel$fitted.values)


LogitData$alt1 <- ifelse(LogitData$alt == "2", "0", "1")


res1 <- mlogit::mlogit(choice ~ -1 + commute + destinations + homes + streets + transit + ParkingDriveway  + ParkingOffStreet + NominalPrice + alt1,
                       data = subset(LogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

res2 <- mlogit::mlogit(choice ~ -1 | commute + destinations + homes + streets + transit + ParkingDriveway  + ParkingOffStreet | NominalPrice,
                       data = subset(LogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

res3 <- mlogit::mlogit(choice ~ commute + destinations + transit | NominalPrice,
                       data = subset(LogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

res4 <- mlogit::mlogit(choice ~ commute + destinations + transit + NominalPrice + streets + ParkingDriveway,
                       data = subset(LogitData), shape = "long", alt.var = "alt", id = "id")


