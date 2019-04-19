##### Finding Marginal Willingness to Pay ######

# Renter Marginal Willingness to Pay

RenterData  <- readRDS("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/RenterData.RDS")
#RenterData <- subset(RenterData, RenterData$rent_own == "1")
RenterData <- subset(RenterData, RenterData$rent_price > 49)
# Check to see if variable classes are maintained
sapply(RenterData, class)

library(mlogit)
library(support.CEs)
# Renter Models with Demographics

# Parsomonious 

RenterData$NumericChange <- as.numeric(levels(RenterData$price))[RenterData$price]


Renter_Par_Price <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange, data = RenterData, shape = "long", alt.var = "alt", id = "password", chid.var = "chid")
Renter_Par_MWTP <- mwtp(Renter_Par_Price, monetary.variables = c("Relative_Change_Price"))


# Create Variable with Price Percent 


Renter_Par_PricePercent <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange, data = RenterData, shape = "long", alt.var = "alt", id = "password", chid.var = "chid")
Renter_Pec_MWTP <- mwtp(Renter_Par_PricePercent, monetary.variables = c("NumericChange"))

# Income

Renter_Income <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Income:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
#Renter_Income_MWTP <- mwtp(Renter_Income,
#                             monetary.variables = c("Relative_Change_Price", "Relative_Change_Price:IncomeMid", "Relative_Change_Price:IncomeHigh"),
#                             nonmonetary.variables = list(c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
#                                                          c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
#                                                          c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away")))


# Education

Renter_Education <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Education:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Education_I_MWTP <- mwtp(Renter_Education, monetary.variables = c("Relative_Change_Price"))


# Employment

Renter_Employment <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Employment:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Age

Renter_Age <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Age:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# gender

Renter_Gender <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Gender:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Home Region

Renter_Home_Region <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Home_Region:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")


library(stargazer)

stargazer(Renter_Par_MWTP$mwtp.table, type = "html", out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MWTPRenterPar.htm")
stargazer(Renter_Pec_MWTP$mwtp.table, type = "html", out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MRSRenterPar.htm")


stargazer(Renter_Par_PricePercent, Renter_Income, Renter_Gender,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", round(AIC(Renter_Par_PricePercent), 0), round(AIC(Renter_Income), 0), round(AIC(Renter_Gender), 0))),
          column.labels = c("Parsimonious Model with Relative Change in Price", "Parsimonious Model with Percant Change", "Interactions with Income", "Interactions with Gender"),
          type = "html",
          title = "Renter Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/thrd_RenterModels.htm")


# Renter Models with Preferences

# Plan to Move

Renter_Plan_to_Move <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Plan_to_Move:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Place Type

Renter_Curr_Place_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Curr_Place_Type:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Residency type

Renter_curr_res_type_I <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Curr_Res_Type:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Place type

Renter_prefer_place_I <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Prefer_Place:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Residency type

Renter_prefer_res_type_I <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + NumericChange + Prefer_Res_Type:(NumericChange + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")


stargazer(Renter_Employment, Renter_Education, Renter_Home_Region, Renter_Plan_to_Move, Renter_curr_res_type_I, Renter_Curr_Place_Type,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", round(AIC(Renter_Employment), 0), round(AIC(Renter_Education), 0), round(AIC(Renter_Home_Region), 0), round(AIC(Renter_Plan_to_Move), 0), round(AIC(Renter_curr_res_type_I), 0),round(AIC(Renter_Curr_Place_Type), 0 ))),
          column.labels = c("Parasimonious Model", "Interactions with Employment", "Interactions with Education", "Interactions with Home Region", "Interactions with Plan to Move", "Interations with Current Residency Type", "Interactions with Current Neighborhood Type"),
          type = "html",
          title = "Interacted With Preference Renter Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/InterPrefRenters.htm")







      # Owner Marginal Willingness to Pay

OwnerData  <-readRDS("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/OwnerData.RDS")

# Parsomonious 

Owner_Par <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = OwnerData, shape = "long", alt.var = "alt", id = "password", chid.var = "chid")
Owner_Par_MWTP <- mwtp(Owner_Par, monetary.variables = c("I(Relative_Change_Price/1000)"))

stargazer(Owner_Par_MWTP$mwtp.table, type = "html", out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MWTPOwnerPar.htm")


# Income

Owner_Income <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Income:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income,
                             monetary.variables = c("I(Relative_Change_Price/1000)", "I(Relative_Change_Price/1000):IncomeMid", "I(Relative_Change_Price/1000):IncomeHigh"),
                             nonmonetary.variables = list(c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
                                                          c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
                                                          c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away")))


# Education

Owner_Education <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Education:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Education_I_MWTP <- mwtp(Owner_Education, monetary.variables = c("I(Relative_Change_Price/1000)"))


# Employment

Owner_Employment <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Employment:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income, monetary.variables = c("I(Relative_Change_Price/1000)"))

# Age

Owner_Age <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Age:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income, monetary.variables = c("I(Relative_Change_Price/1000)"))

# gender

Owner_Gender <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Gender:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income, monetary.variables = c("I(Relative_Change_Price/1000)"))

# Home Region

Owner_Home_Region <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Home_Region:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")
Owner_Income_MWTP <- mwtp(Owner_Income, monetary.variables = c("I(Relative_Change_Price/1000)"))


library(stargazer)
stargazer(Owner_Par, Owner_Income, Owner_Education, Owner_Employment, Owner_Age, Owner_Gender, Owner_Home_Region,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", AIC(Owner_Par), AIC(Owner_Income), AIC(Owner_Education), AIC(Owner_Employment), AIC(Owner_Age), AIC(Owner_Gender), AIC(Owner_Home_Region))),
          type = "html",
          title = "Interacted With Demographics Owner Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MWTPInterDemoOwners.htm")


# Owner Models with Preferences

# Plan to Move

Owner_Plan_to_Move <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Plan_to_Move:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Place Type

Owner_Curr_Place_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Curr_Place_Type:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Residency type

Owner_curr_res_type_I <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Curr_Res_Type:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Place type

Owner_prefer_place_I <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Prefer_Place:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Residency type

Owner_Prefer_Res_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + Prefer_Res_Type:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")

# Under Valued

#Owner_under_est <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + under_est:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")


stargazer(Owner_Plan_to_Move, Owner_Curr_Place_Type, Owner_curr_res_type_I, Owner_prefer_place_I, Owner_Prefer_Res_Type,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", AIC(Owner_Plan_to_Move), AIC(Owner_Curr_Place_Type), AIC(Owner_curr_res_type_I), AIC(Owner_prefer_place_I), AIC(Owner_Prefer_Res_Type))),
          column.labels = c("Interactions with Plan to Move", "Interactions with Current Place Type", "Interactions with Current Residency Type", "Interactions with Preffered Place Type", "Interactions with Preffered Residency Type"),
          type = "html",
          title = "Interacted With Preference Owner Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MWTPInterPrefOwners.htm")
