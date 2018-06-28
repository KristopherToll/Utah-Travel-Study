  ## REPILCATED MODELS FROM UTAH TRAVEL STUDY ##

# Used Librarys

library(stargazer)
library(lmtest)
library(mlogit)
library(support.CEs)
# Import RDS object

MasterLogitData <- readRDS(file = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/MasterLogitData.rds")

  ## Replicated Results ##

# Replicated Model

res1 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + price,
               data = subset(MasterLogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

bptest(res1)

robust_res1 <- coeftest(res1)

# Save Res2

stargazer(res1, robust_res1, column.labels = c("Logit Model", "Robust Model"),title = "Replicated Results With all Respondents", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/Replicated_res2.htm")

  ## Full Model with Nominal Price ##

# Drop Observations that reported small price values

MasterLogitData.2 <-subset(MasterLogitData, MasterLogitData$HomeRentPrice != "NA")
MasterLogitData.2 <- subset(MasterLogitData.2, MasterLogitData.2$curr_res_type != "6" &
                            MasterLogitData.2$curr_res_type != "7" & 
                              MasterLogitData.2$curr_res_type != "8")

Owners <- subset(MasterLogitData.2, MasterLogitData.2$rent_own != "1")
Owners <- subset(Owners, Owners$HomeRentPrice >= 10000)


Renters <- subset(MasterLogitData.2, MasterLogitData.2$rent_own != "2")
Renters <- subset(Renters, Renters$HomeRentPrice >= 300)

MasterLogitData.2 <- rbind(Owners, Renters)

# Model with scaled Nominal Price
res3 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + DiffNominalPrice_s,
               data = subset(MasterLogitData.2), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

robust_res3 <- coeftest(res3)

# Calculating Willingness to Pay

res3_WTP.2 <- mwtp(res3, monetary.variables = c("DiffNominalPrice_s"))

# Tables With Full Models

stargazer(res3, robust_res3, column.labels = c("Logit Model", "Robust Model"), title = "Results With all Respondents and Nominal Price", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/Full_model_with_NomPrice.htm")

stargazer(res3_WTP.2$mwtp.table, title = "Willingness to Pay: Full Model", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/Full_model_WTP.htm")


# Owner's Model

res4 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + DiffNominalPrice_s,
               data = subset(MasterLogitData.2, MasterLogitData.2$rent_own == "2"), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

robust_res4 <- coeftest(res4)

res4_WTP <- mwtp(res4, monetary.variables = c("DiffNominalPrice_s"))

# Renter's Model

res5 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + DiffNominalPrice_s,
               data = subset(MasterLogitData.2, MasterLogitData.2$rent_own == "1"), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

robust_res5 <- coeftest(res5)

res5_WTP <- mwtp(res5, monetary.variables = c("DiffNominalPrice_s"))

# Table Comparing Owners to Renters

stargazer(res4, robust_res4, res5, robust_res5, column.labels = c("Owner's Logit Model", "Owner's Robust Model", "Renter's Logit Model", "Renter's Robust Model"), title = "Owner and Renter Models", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/OwnRent_model_with_NomPrice.htm")

stargazer(res4_WTP$mwtp.table, title = "Willingness to Pay: Owner's Model", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/Owner_model_WTP.htm")

stargazer(res5_WTP$mwtp.table, title = "Willingness to Pay: Renter's Model", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/Renter_model_WTP.htm")

# Low Income Home Owner Model

res6 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + DiffNominalPrice_s,
               data = subset(MasterLogitData.2, MasterLogitData.2$rent_own == "2" & MasterLogitData.2$income == "1" | MasterLogitData.2$rent_own == "2" & MasterLogitData.2$income == "2" | MasterLogitData.2$rent_own == "2" & MasterLogitData.2$income == "3"), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

robust_res6 <- coeftest(res4)

res6_WTP <- mwtp(res6, monetary.variables = c("DiffNominalPrice_s"))


# Mid Income Home Owner Model

res7 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + DiffNominalPrice_s,
               data = subset(MasterLogitData.2, MasterLogitData.2$rent_own == "2" & MasterLogitData.2$income == "4" | MasterLogitData.2$rent_own == "2" & MasterLogitData.2$income == "5"), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

robust_res7 <- coeftest(res4)

res7_WTP <- mwtp(res7, monetary.variables = c("DiffNominalPrice_s"))

# High Income Home Owner Model

res8 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + DiffNominalPrice_s,
               data = subset(MasterLogitData.2, MasterLogitData.2$rent_own == "2" & MasterLogitData.2$income == "6" | MasterLogitData.2$rent_own == "2" & MasterLogitData.2$income == "7"| MasterLogitData.2$rent_own == "2" & MasterLogitData.2$income == "8"| MasterLogitData.2$rent_own == "2" & MasterLogitData.2$income == "9" ), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

robust_res8 <- coeftest(res4)

res8_WTP <- mwtp(res8, monetary.variables = c("DiffNominalPrice_s"))

# Table Comparing Low, Mid, High income Models

stargazer(res6, robust_res6, res7, robust_res7, res8, robust_res8, column.labels = c("< $35,000 Logit Model", "< $35,000 Robust Model", "$35,000 - $75,000 Logit Model", "$35,000 - $75,000 Robust Model", "> $75,000 Logit Model", "> $75,000 Robust Model"), title = "Income Models", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/Incomes_model_with_NomPrice.htm")

stargazer(res6_WTP$mwtp.table, title = "Willingness to Pay: Low Income Model", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/LowIncome_model_WTP.htm")

stargazer(res7_WTP$mwtp.table, title = "Willingness to Pay: Middle Income Model", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/MidIncome_model_WTP.htm")

stargazer((res8_WTP$mwtp.table*1000), title = "Willingness to Pay: High Income Model", type = "html", out = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/results/HighIncome_model_WTP.htm")

