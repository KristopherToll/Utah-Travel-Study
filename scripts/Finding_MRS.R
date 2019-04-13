##### Finding Marginal Willingness to Pay ######

# These models will be used for primary resulsts in my thesis

options(digits = 3)

# Renter Marginal Willingness to Pay

RenterData <- readRDS("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/OwnerData.RDS")
RenterData <- subset(RenterData, RenterData$rent_own == "1")
RenterData$PricePercent <- as.numeric(levels(RenterData$price))[RenterData$price]

# Check to see if variable classes are maintained
sapply(RenterData, class)

library(mlogit)
library(support.CEs)
# Renter Models with Demographics

# Parsomonious 

Renter_Par <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent, data = RenterData, shape = "long", alt.var = "alt", id = "password", chid.var = "chid")
Renter_Par_MWTP <- mwtp(Renter_Par, monetary.variables = c("PricePercent"))

# Income

Renter_Income <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Income:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income,
                             monetary.variables = c("PricePercent", "PricePercent:IncomeMid", "PricePercent:IncomeHigh"),
                             nonmonetary.variables = list(c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
                                                          c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
                                                          c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away")))


# Education

Renter_Education <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Education:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Education_MWTP <- mwtp(Renter_Education, monetary.variables = c("PricePercent"))


# Employment

Renter_Employment <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Employment:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income, monetary.variables = c("PricePercent"))

# Age

Renter_Age <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Age:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income, monetary.variables = c("PricePercent"))

# gender

Renter_Gender <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Gender:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income, monetary.variables = c("PricePercent"))

# Home Region

Renter_Home_Region <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Home_Region:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income, monetary.variables = c("PricePercent"))


library(stargazer)
stargazer(Renter_Par, Renter_Income, Renter_Education, Renter_Employment, Renter_Age, Renter_Gender, Renter_Home_Region,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", round(AIC(Renter_Par), 3), round(AIC(Renter_Income), 3), round(AIC(Renter_Education), 3), round(AIC(Renter_Employment), 3), round(AIC(Renter_Age), 3), round(AIC(Renter_Gender), 3), round(AIC(Renter_Home_Region), 3))),
          column.labels = c("Parsomonious", "Interactions with Income", "Interactions with Education", "Interactions with Employment", "Interactions with Age", "Interactions with Gender", "Interactions with Home Region ID"),
          type = "html",
          title = "Interacted With Demographics Renter Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ThesisResultsInterDemoRenters.htm")


# Renter Models with Preferences

# Plan to Move

Renter_Plan_to_Move <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Plan_to_Move:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Place Type

Renter_Curr_Place_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Curr_Place_Type:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Residency type

Renter_Curr_Res_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Curr_Res_Type:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Place type

Renter_Prefer_Place <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Prefer_Place:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Residency type

Renter_Prefer_Res_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Prefer_Res_Type:(PricePercent + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")


stargazer(Renter_Plan_to_Move, Renter_Curr_Place_Type, Renter_Curr_Res_Type, Renter_Prefer_Place, Renter_Prefer_Res_Type,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", round(AIC(Renter_Plan_to_Move), 3), round(AIC(Renter_Curr_Place_Type), 3), round(AIC(Renter_Curr_Res_Type), 3), round(AIC(Renter_Prefer_Place), 3), round(AIC(Renter_Prefer_Res_Type), 3))),
          column.labels = c("Interactions with Plan to Move", "Interactions with Current Place Type", "Interactions with Current Residency Type", "Interactions with Preffered Place Type", "Interactions with Preffered Residency Type"),
          type = "html",
          title = "Interacted With Preference Renter Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ThesisResultsInterPrefRenters.htm")








# Owner Marginal Willingness to Pay

OwnerData  <-readRDS("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/OutlierRemovedData.RDS")
OwnerData$PricePercent <- as.numeric(levels(OwnerData$price))[OwnerData$price]


# Parsomonious 

Owner_Par <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent, data = OwnerData, shape = "long", alt.var = "alt", id = "password", chid.var = "chid")
Owner_Par_MWTP <- mwtp(Owner_Par, monetary.variables = c("PricePercent"))

stargazer(Owner_Par_MWTP$mwtp.table, type = "html", out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MRSOwnerParPricePercent.htm")


# Income

Owner_Income <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Income:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income,
                            monetary.variables = c("PricePercent", "PricePercent:IncomeMid", "PricePercent:IncomeHigh"),
                            nonmonetary.variables = list(c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
                                                         c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
                                                         c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away")))


# Education

Owner_Education <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Education:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Education_MWTP <- mwtp(Owner_Education, monetary.variables = c("PricePercent"))


# Employment

Owner_Employment <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Employment:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income, monetary.variables = c("PricePercent"))

# Age

Owner_Age <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Age:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income, monetary.variables = c("PricePercent"))

# gender

Owner_Gender <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Gender:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income, monetary.variables = c("PricePercent"))

# Home Region

Owner_Home_Region <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Home_Region:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income, monetary.variables = c("PricePercent"))


library(stargazer)
stargazer(Owner_Par, Owner_Income, Owner_Education, Owner_Employment, Owner_Age, Owner_Gender, Owner_Home_Region,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", round(AIC(Owner_Par), 3), round(AIC(Owner_Income), 3), round(AIC(Owner_Education), 3), round(AIC(Owner_Employment), 3), round(AIC(Owner_Age), 3), round(AIC(Owner_Gender), 3), round(AIC(Owner_Home_Region), 3))),
          column.labels = c("Parsomonious", "Interactions with Income", "Interactions with Education", "Interactions with Employment", "Interactions with Age", "Interactions with Gender", "Interactions with Home Region ID"),
          type = "html",
          title = "Interacted With Demographics Owner Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ThesisResultsInterDemoOwners.htm")


# Owner Models with Preferences

# Plan to Move

Owner_Plan_to_Move <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Plan_to_Move:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Place Type

Owner_Curr_Place_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Curr_Place_Type:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Residency type

Owner_Curr_Res_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Curr_Res_Type:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Place type

Owner_Prefer_Place <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Prefer_Place:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Residency type

Owner_Prefer_Res_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + Prefer_Res_Type:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Under Valued

#Owner_under_est <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + PricePercent + under_est:(PricePercent + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")


stargazer(Owner_Plan_to_Move, Owner_Curr_Place_Type, Owner_Curr_Res_Type, Owner_Prefer_Place, Owner_Prefer_Res_Type,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", round(AIC(Owner_Plan_to_Move), 3), round(AIC(Owner_Curr_Place_Type), 3), round(AIC(Owner_Curr_Res_Type), 3), round(AIC(Owner_Prefer_Place), 3), round(AIC(Owner_Prefer_Res_Type), 3))),
          column.labels = c("Interactions with Plan to Move", "Interactions with Current Place Type", "Interactions with Current Residency Type", "Interactions with Preffered Place Type", "Interactions with Preffered Residency Type"),
          type = "html",
          title = "Interacted With Preference Owner Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ThesisResultsInterPrefOwners.htm")


# Finding Averages

library(mosaic)

# Owner Averages





# GLM VS Mlogit


a <- glm(choice ~ alt + commute + destinations + homes + parking + streets + transit , data = RenterData)
b <- glm(choice ~ 0 + alt + commute + destinations + homes + parking + streets + transit, data = RenterData)
c <- glm(choice ~ 0 + commute + destinations + homes + parking + streets + transit , data = RenterData)
d <- glm(choice ~ commute + destinations + homes + parking + streets + transit , data = RenterData)

e <- mlogit(choice ~ alt + commute + destinations + homes + parking + streets + transit , data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
f <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit , data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

g <- mlogit(choice ~ alt + commute + destinations + homes + parking + streets + transit | Income:I(Relative_Change_Price/1000) + 0 , data = OwnerData, shape = "long", alt.var = "alt", id = "id", chid.var = "chid")


stargazer(a, b, d, d, e, f,
          no.space = TRUE,
          intercept.bottom = FALSE,
          type = "html",
          title = "Outlier Removed Models With Stated Value",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/mlogitvsglm2.htm")

