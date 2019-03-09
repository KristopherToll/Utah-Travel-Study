### Thesis Data ####

ResChoiceData  <-read_rds("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoiceData.RDS")
ResChoice_w_CensBlock  <-read_rds("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoice_w_CensBlock.RDS")
ResChoice_w_CensTract  <-read_rds("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoice_w_CensTract.RDS")


# Create Models using Mlogit


#### Parsimonious Models #######

# Determine Degrees of Freedom, We have 200
df <-unique(ResidentialChoice_ChoiceExperimentsData[5:18])
nrow(df)
library(lmtest)
library(sandwich)
library(ResourceSelection)
library(mlogit)

Home_Tract_Model <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price, data = ResChoice_w_CensTract, na.action = na.omit, family = binomial)
Home_Tract_Model_robust <- sqrt(diag(vcovHC(Home_Tract_Model, type = "HC0")))
Home_Tract_Model.pr2 <- round(1 - Home_Tract_Model$deviance/Home_Tract_Model$null.deviance, digits = 3)

Home_Block_Model <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price, data = ResChoice_w_CensBlock, na.action = na.omit, family = binomial)
Home_Block_Model_robust <- sqrt(diag(vcovHC(Home_Block_Model, type = "HC0")))
Home_Block_Model.pr2 <- round(1 - Home_Block_Model$deviance/Home_Block_Model$null.deviance, digits = 3)


Home_Value_Model <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price, data = subset(ResChoiceData, na.action = na.omit, ResChoiceData$rent_own == "2"), family = binomial)
Home_Value_Model_robust <- sqrt(diag(vcovHC(Home_Value_Model, type = "HC0")))
Home_Value_Model.pr2 <- round(1 - Home_Value_Model$deviance/Home_Value_Model$null.deviance, digits = 3)


Renter_Value_Model <- mlogit(choice ~ commute + destinations + homes + parking + streets + transit + Relative_Change_Price, data = subset(ResChoiceData, ResChoiceData$rent_own == "1" & ResChoiceData$Stated_Price > 49), na.action = na.omit, family = binomial)
Renter_Value_Model_robust <- sqrt(diag(vcovHC(Renter_Value_Model, type = "HC0")))
Renter_Value_Model.pr2 <- round(1 - Renter_Value_Model$deviance/Renter_Value_Model$null.deviance, digits = 3)


library(stargazer)
stargazer(Home_Tract_Model, Home_Value_Model, Renter_Value_Model,
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("AIC Value", AIC(Home_Tract_Model), AIC(Home_Value_Model), AIC(Renter_Value_Model))),
          column.labels = c("Home with Tract", "Home with Stated", "Renter with Stated"),
          type = "html",
          title = "Parsimonious Mlogit Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/P_Mlogit_models.htm")



##### Scaled Models ###########


# Divide by 100

Home_Tract_Model_div.1 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/100), data = ResChoice_w_CensTract, na.action = na.omit, family = binomial)
Home_Tract_Model_robust_div.1 <- sqrt(diag(vcovHC(Home_Tract_Model_div.1, type = "HC0")))
Home_Tract_Model.pr2_div.1 <- round(1 - Home_Tract_Model_div.1$deviance/Home_Tract_Model_div.1$null.deviance, digits = 3)

Home_Block_Model_div.1 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/100), data = ResChoice_w_CensBlock, na.action = na.omit, family = binomial)
Home_Block_Model_robust_div.1 <- sqrt(diag(vcovHC(Home_Block_Model_div.1, type = "HC0")))
Home_Block_Model.pr2_div.1 <- round(1 - Home_Block_Model_div.1$deviance/Home_Block_Model_div.1$null.deviance, digits = 3)


Home_Value_Model_div.1 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/100), data = subset(ResChoiceData, ResChoiceData$rent_own == "2"), na.action = na.omit, family = binomial)
Home_Value_Model_robust_div.1 <- sqrt(diag(vcovHC(Home_Value_Model_div.1, type = "HC0")))
Home_Value_Model.pr2_div.1 <- round(1 - Home_Value_Model_div.1$deviance/Home_Value_Model_div.1$null.deviance, digits = 3)


Renter_Value_Model_div.1 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/100), data = subset(ResChoiceData, ResChoiceData$rent_own == "1"), na.action = na.omit, family = binomial)
Renter_Value_Model_robust_div.1 <- sqrt(diag(vcovHC(Renter_Value_Model_div.1, type = "HC0")))
Renter_Value_Model.pr2_div.1 <- round(1 - Renter_Value_Model_div.1$deviance/Renter_Value_Model_div.1$null.deviance, digits = 3)




# Divide by 1000

Home_Tract_Model_div.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = ResChoice_w_CensTract, na.action = na.omit, family = binomial)
Home_Tract_Model_robust_div.2 <- sqrt(diag(vcovHC(Home_Tract_Model_div.2, type = "HC0")))
Home_Tract_Model.pr2_div.2 <- round(1 - Home_Tract_Model_div.2$deviance/Home_Tract_Model_div.2$null.deviance, digits = 3)

Home_Block_Model_div.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = ResChoice_w_CensBlock, na.action = na.omit, family = binomial)
Home_Block_Model_robust_div.2 <- sqrt(diag(vcovHC(Home_Block_Model_div.2, type = "HC0")))
Home_Block_Model.pr2_div.2 <- round(1 - Home_Block_Model_div.2$deviance/Home_Block_Model_div.2$null.deviance, digits = 3)


Home_Value_Model_div.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoiceData, ResChoiceData$rent_own == "2"), na.action = na.omit, family = binomial)
Home_Value_Model_robust_div.2 <- sqrt(diag(vcovHC(Home_Value_Model_div.2, type = "HC0")))
Home_Value_Model.pr2_div.2 <- round(1 - Home_Value_Model_div.2$deviance/Home_Value_Model_div.2$null.deviance, digits = 3)


Renter_Value_Model_div.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoiceData, ResChoiceData$rent_own == "1"), na.action = na.omit, family = binomial)
Renter_Value_Model_robust_div.2 <- sqrt(diag(vcovHC(Renter_Value_Model_div.2, type = "HC0")))
Renter_Value_Model.pr2_div.2 <- round(1 - Renter_Value_Model_div.2$deviance/Renter_Value_Model_div.2$null.deviance, digits = 3)




# Divide by 10000

Home_Tract_Model_div.3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/10000), data = ResChoice_w_CensTract, na.action = na.omit, family = binomial)
Home_Tract_Model_robust_div.3 <- sqrt(diag(vcovHC(Home_Tract_Model_div.3, type = "HC0")))
Home_Tract_Model.pr2_div.3 <- round(1 - Home_Tract_Model_div.3$deviance/Home_Tract_Model_div.3$null.deviance, digits = 3)


Home_Block_Model_div.3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/10000), data = ResChoice_w_CensBlock, na.action = na.omit, family = binomial)
Home_Block_Model_robust_div.3 <- sqrt(diag(vcovHC(Home_Block_Model_div.3, type = "HC0")))
Home_Block_Model.pr2_div.3 <- round(1 - Home_Block_Model_div.3$deviance/Home_Block_Model_div.3$null.deviance, digits = 3)


Home_Value_Model_div.3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/10000), data = subset(ResChoiceData, ResChoiceData$rent_own == "2"), na.action = na.omit, family = binomial)
Home_Value_Model_robust_div.3 <- sqrt(diag(vcovHC(Home_Value_Model_div.3, type = "HC0")))
Home_Value_Model.pr2_div.3 <- round(1 - Home_Value_Model_div.3$deviance/Home_Value_Model_div.3$null.deviance, digits = 3)


Renter_Value_Model_div.3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoiceData, ResChoiceData$rent_own == "1"), na.action = na.omit, family = binomial)
Renter_Value_Model_robust_div.3 <- sqrt(diag(vcovHC(Renter_Value_Model_div.3, type = "HC0")))
Renter_Value_Model.pr2_div.3 <- round(1 - Renter_Value_Model_div.3$deviance/Renter_Value_Model_div.3$null.deviance, digits = 3)


stargazer(Home_Tract_Model_div.1, Home_Tract_Model_div.2, Home_Tract_Model_div.3, Home_Value_Model_div.1, Home_Value_Model_div.2, Home_Value_Model_div.3,
          se = list(Home_Tract_Model_robust_div.1, Home_Tract_Model_robust_div.2, Home_Tract_Model_robust_div.3, Home_Value_Model_robust_div.1, Home_Value_Model_robust_div.2, Home_Value_Model_robust_div.3),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Home_Tract_Model.pr2_div.1, Home_Tract_Model.pr2_div.2, Home_Tract_Model.pr2_div.3, Renter_Value_Model.pr2_div.1, Renter_Value_Model.pr2_div.2, Renter_Value_Model.pr2_div.3)),
          column.labels = c("Home with Tract", "Home with Tract", "Home with Tract", "Home with Stated", "Home with Stated", "Home with Stated"),
          type = "html",
          title = "Price Divided by 1000",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/PDIVmodels.htm")




######## Graphics and Models for Over and Under Valuation #######


# Create Realative Price Change with State Price
ResChoice_w_CensTract$S_Change_in_Price <- as.numeric(levels(ResChoice_w_CensTract$price))[ResChoice_w_CensTract$price]*(ResChoice_w_CensTract$home_price)

ResChoice_w_CensTract$S_Relative_Change_Price <- ResChoice_w_CensTract$Change_in_Price - ResChoice_w_CensTract$home_price





Home_Tract_Model_Over <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1), na.action = na.omit, family = binomial)
Home_Tract_Model_Over_Robust <- sqrt(diag(vcovHC(Home_Tract_Model_Over, type = "HC0")))
Home_Tract_Model_Over.pr2 <- round(1 - Home_Tract_Model_Over$deviance/Home_Tract_Model_Over$null.deviance, digits = 3)



Home_Tract_Model_Under <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1), na.action = na.omit, family = binomial)
Home_Tract_Model_Under_Robust <- sqrt(diag(vcovHC(Home_Tract_Model_Under, type = "HC0")))
Home_Tract_Model_Under.pr2 <- round(1 - Home_Tract_Model_Under$deviance/Home_Tract_Model_Under$null.deviance, digits = 3)



Home_Value_Model_Over.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1), na.action = na.omit, family = binomial)
Home_Tract_Model_Over_Robust.2 <- sqrt(diag(vcovHC(Home_Tract_Model_Over, type = "HC0")))
Home_Tract_Model_Over.pr2.2 <- round(1 - Home_Tract_Model_Over$deviance/Home_Tract_Model_Over$null.deviance, digits = 3)



Home_Value_Model_Under.2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000), data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1), na.action = na.omit, family = binomial)
Home_Value_Model_Under_Robust.2 <- sqrt(diag(vcovHC(Home_Tract_Model_Under, type = "HC0")))
Home_Value_Model_Under.pr2.2 <- round(1 - Home_Tract_Model_Under$deviance/Home_Tract_Model_Under$null.deviance, digits = 3)

stargazer(Home_Tract_Model_Over, Home_Tract_Model_Under, Home_Value_Model_Over.2, Home_Value_Model_Under.2,
          se = list(Home_Tract_Model_Over_Robust, Home_Tract_Model_Under_Robust, Home_Tract_Model_Over_Robust.2, Home_Value_Model_Under_Robust.2),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Home_Tract_Model_Over.pr2, Home_Tract_Model_Under.pr2, Home_Tract_Model_Over.pr2.2, Home_Value_Model_Under.pr2.2)),
          column.labels = c("Home with Tract(Over)", "Home with Tract(Under)", "Home with Stated(Over)", "Home with Stated(Under)"),
          type = "html",
          title = "Over Under Value Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/OverUndermodels.htm")







######## Remove Outliers ########

ggplot( data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 
                        1), aes(x = Ratio_BTW_HomeV_Estimate)) + geom_histogram(binwidth = 0.04) + labs(title = "") 

ggplot( data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 
                        1), aes(x = Ratio_BTW_HomeV_Estimate)) + geom_histogram(binwidth = 35) + labs(title = "") 


Over_Ratio_mean <- mean(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1)$Ratio_BTW_HomeV_Estimate)
Over_Ratio_sd <- sd(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1)$Ratio_BTW_HomeV_Estimate)
O <- Over_Ratio_mean + 3 * Over_Ratio_sd

ggplot( data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 
                        1 & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), aes(x = Ratio_BTW_HomeV_Estimate)) + geom_histogram(binwidth = 2.3) + labs(title = "") 

Under_Ratio_mean <- mean(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1)$Ratio_BTW_HomeV_Estimate)
Under_Ratio_sd <- sd(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1)$Ratio_BTW_HomeV_Estimate)
U <- Under_Ratio_mean - 3*Under_Ratio_sd

ggplot( data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 
                        1 & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U), aes(x = Ratio_BTW_HomeV_Estimate)) + geom_histogram(binwidth = 0.019) + labs(title = "") 



Home_Value_Model_Over_No.Out <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                                    data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 1 & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), na.action = na.omit, family = binomial)
Home_Value_Model_Over_No.Out_Robust <- sqrt(diag(vcovHC(Home_Value_Model_Over_No.Out, type = "HC0")))
Home_Value_Model_Over_No.Out.pr2 <- round(1 - Home_Value_Model_Over_No.Out$deviance/Home_Value_Model_Over_No.Out$null.deviance, digits = 3)



Home_Value_Model_Under_No.Out <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                                     data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 1 & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U), na.action = na.omit, family = binomial)
Home_Value_Model_Under_No.Out_Robust <- sqrt(diag(vcovHC(Home_Value_Model_Under_No.Out, type = "HC0")))
Home_Value_Model_Under_No.Out.pr2 <- round(1 - Home_Value_Model_Under_No.Out$deviance/Home_Value_Model_Under_No.Out$null.deviance, digits = 3)


Home_Value_Model_No.Out <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                               data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < O), na.action = na.omit, family = binomial)
Home_Value_Model_No.Out_Robust <- sqrt(diag(vcovHC(Home_Value_Model_No.Out, type = "HC0")))
Home_Value_Model_No.Out.pr2 <- round(1 - Home_Value_Model_No.Out$deviance/Home_Value_Model_No.Out$null.deviance, digits = 3)



# Number of over valuations two time the median estimate

nrow(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate <1 ))/20
table(subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > 2)$Ratio_BTW_HomeV_Estimate)/20

Home_Value_Model_No.Out2 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                                data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 2), na.action = na.omit, family = binomial)
Home_Value_Model_No.Out2_Robust <- sqrt(diag(vcovHC(Home_Value_Model_No.Out2, type = "HC0")))
Home_Value_Model_No.Ou2t_No.Out.pr2 <- round(1 - Home_Value_Model_No.Out2$deviance/Home_Value_Model_No.Out2$null.deviance, digits = 3)


# Number of over valuations Three time the median estimate


Home_Value_Model_No.Out3 <- glm(choice ~ commute + destinations + homes + parking + streets + transit + I(S_Relative_Change_Price/1000),
                                data = subset(ResChoice_w_CensTract, ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate > U & ResChoice_w_CensTract$Ratio_BTW_HomeV_Estimate < 3), na.action = na.omit, family = binomial)
Home_Value_Model_No.Out3_Robust <- sqrt(diag(vcovHC(Home_Value_Model_No.Out3, type = "HC0")))
Home_Value_Model_No.Out3.pr2 <- round(1 - Home_Value_Model_No.Out3$deviance/Home_Value_Model_No.Out3$null.deviance, digits = 3)


stargazer(Home_Value_Model_Over_No.Out, Home_Value_Model_Under_No.Out, Home_Value_Model_No.Out, Home_Value_Model_No.Out2, Home_Value_Model_No.Out3,
          se = list(Home_Value_Model_Over_No.Out_Robust, Home_Value_Model_Under_No.Out_Robust, Home_Value_Model_No.Out_Robust, Home_Value_Model_No.Out2_Robust, Home_Value_Model_No.Out3_Robust),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Home_Value_Model_Over_No.Out.pr2, Home_Value_Model_Under_No.Out.pr2, Home_Value_Model_No.Out.pr2, Home_Value_Model_No.Ou2t_No.Out.pr2, Home_Value_Model_No.Out3.pr2, NULL)),
          column.labels = c("Over Valued Estimates", "Under Valued Estimates", "Both Over and Under Valued", "Over 2 is the Cut off", "Over 3 is the Cut off"),
          type = "html",
          title = "Outlier Removed Models With Stated Value",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/OverUnderNoOutmodels.htm")



