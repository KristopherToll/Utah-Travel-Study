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

Renter_Par <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price, data = RenterData, shape = "long", alt.var = "alt", id = "password", chid.var = "chid")
Renter_Par_MWTP <- mwtp(Renter_Par, monetary.variables = c("Relative_Change_Price"))

# Income

Renter_Income <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Income:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income,
                             monetary.variables = c("Relative_Change_Price", "Relative_Change_Price:IncomeMid", "Relative_Change_Price:IncomeHigh"),
                             nonmonetary.variables = list(c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
                                                          c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away"),
                                                          c("commute5 Miles", "commute10 miles", "commute20 Miles", "destinationsLess than 3 Miles", "destinationsLess than 10 Miles","destinations10 Miles or more", "homesMix, Single Fam 1/2 acre lots, townhomes, apartments, condos", "homesOnly single fam 1/2 acre lots", "homesSingle Fam 1+ acre Lots", "parkingOn-street or free parking", "parkingOff-street or Paid Parking", "streetsFor Cars, Pedestrians, and cyclers", "transitBus stop within walking distance and Rail 5 miles away", "transitRail and bus 5 miles away", "transitRail and Bus 10 miles Away")))


# Education

Renter_Education <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Education:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Education_I_MWTP <- mwtp(Renter_Education, monetary.variables = c("Relative_Change_Price"))


# Employment

Renter_Employment <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Employment:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income, monetary.variables = c("Relative_Change_Price"))

# Age

Renter_Age <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Age:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income, monetary.variables = c("Relative_Change_Price"))

# gender

Renter_Gender <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Gender:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income, monetary.variables = c("Relative_Change_Price"))

# Home Region

Renter_Home_Region <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Home_Region:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")
Renter_Income_MWTP <- mwtp(Renter_Income, monetary.variables = c("Relative_Change_Price"))


library(stargazer)
stargazer(Renter_Par, Renter_Income, Renter_Education, Renter_Employment, Renter_Age, Renter_Gender, Renter_Home_Region,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", AIC(Renter_Par), AIC(Renter_Income), AIC(Renter_Education), AIC(Renter_Employment), AIC(Renter_Age), AIC(Renter_Gender), AIC(Renter_Home_Region))),
          column.labels = c("Parsimonious Model", "Interactions with Income", "Interactions with Education", "Interactions with Employment", "Interactions with Age", "Interactions with Gender", "Interaction with Region"),
          type = "html",
          title = "Interacted With Demographics Renter Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MWTPInterDemoRenters.htm")

stargazer(Renter_Par_MWTP$mwtp.table, type = "html", out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MWTPRenterPar.htm")

# Renter Models with Preferences

# Plan to Move

Renter_Plan_to_Move <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Plan_to_Move:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Place Type

Renter_Curr_Place_Type <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Curr_Place_Type:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Current Residency type

Renter_curr_res_type_I <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Curr_Res_Type:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Place type

Renter_prefer_place_I <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Prefer_Place:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")

# Prefer Residency type

Renter_prefer_res_type_I <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price + Prefer_Res_Type:(Relative_Change_Price + commute + destinations + homes + parking + streets + transit), data = RenterData, shape = "long", alt.var = "alt", chid.var = "chid")


stargazer(Renter_Plan_to_Move, Renter_Curr_Place_Type, Renter_curr_res_type_I, Renter_prefer_place_I, Renter_prefer_res_type_I,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", AIC(Renter_Plan_to_Move), AIC(Renter_Curr_Place_Type), AIC(Renter_curr_res_type_I), AIC(Renter_prefer_place_I), AIC(Renter_prefer_res_type_I))),
          column.labels = c("Interactions with Plan to Move", "Interactions with Current Place Type", "Interactions with Current Residency Type", "Interactions with Preffered Place Type", "Interactions with Preffered Residency Type"),
          type = "html",
          title = "Interacted With Preference Renter Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MWTPInterPrefRenters.htm")








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

Owner_under_est <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + under_est:(I(Relative_Change_Price/1000) + commute + destinations + homes + parking + streets + transit), data = OwnerData, shape = "long", alt.var = "alt", chid.var = "chid")


stargazer(Owner_Plan_to_Move, Owner_Curr_Place_Type, Owner_curr_res_type_I, Owner_prefer_place_I, Owner_Prefer_Res_Type,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC", AIC(Owner_Plan_to_Move), AIC(Owner_Curr_Place_Type), AIC(Owner_curr_res_type_I), AIC(Owner_prefer_place_I), AIC(Owner_Prefer_Res_Type))),
          column.labels = c("Interactions with Plan to Move", "Interactions with Current Place Type", "Interactions with Current Residency Type", "Interactions with Preffered Place Type", "Interactions with Preffered Residency Type"),
          type = "html",
          title = "Interacted With Preference Owner Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/MWTPInterPrefOwners.htm")
