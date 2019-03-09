##### Finding Marginal Willingness to Pay ######



# Renter Marginal Willingness to Pay

RenterData  <-read_rds("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoiceData.RDS")

# Check to see if variable classes are maintained
sapply(RenterData, class)


# Renter Models with Demographics

# Income 

Renter_Income_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*Income_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_Income_I_robust <- sqrt(diag(vcovHC(Renter_Income_I, type = "HC0")))
Renter_Income_I.pr2 <- round(1 - Renter_Income_I$deviance/Renter_Income_I$null.deviance, digits = 3)

# Education

Renter_Education_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*Education_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_Education_I_robust <- sqrt(diag(vcovHC(Renter_Education_I, type = "HC0")))
Renter_Education_I.pr2 <- round(1 - Renter_Education_I$deviance/Renter_Education_I$null.deviance, digits = 3)

# Employment

Renter_employment_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*employment_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_employment_I_robust <- sqrt(diag(vcovHC(Renter_employment_I, type = "HC0")))
Renter_employment_I.pr2 <- round(1 - Renter_employment_I$deviance/Renter_employment_I$null.deviance, digits = 3)

# Age

Renter_age_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*age_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_age_I_robust <- sqrt(diag(vcovHC(Renter_age_I, type = "HC0")))
Renter_age_I.pr2 <- round(1 - Renter_age_I$deviance/Renter_age_I$null.deviance, digits = 3)

# gender

Renter_gender_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*gender_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_gender_I_robust <- sqrt(diag(vcovHC(Renter_gender_I, type = "HC0")))
Renter_gender_I.pr2 <- round(1 - Renter_gender_I$deviance/Renter_gender_I$null.deviance, digits = 3)

# Home Region

Renter_home_regionid_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*home_regionid_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_home_regionid_I_robust <- sqrt(diag(vcovHC(Renter_home_regionid_I, type = "HC0")))
Renter_home_regionid_I.pr2 <- round(1 - Renter_home_regionid_I$deviance/Renter_home_regionid_I$null.deviance, digits = 3)


library(stargazer)
stargazer(Renter_Income_I, Renter_Education_I, Renter_employment_I, Renter_age_I, Renter_gender_I, Renter_home_regionid_I,
          se = list(Renter_Income_I_robust, Renter_Education_I_robust, Renter_employment_I_robust, Renter_age_I_robust, Renter_gender_I_robust, Renter_home_regionid_I_robust),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Renter_Income_I.pr2, Renter_Education_I.pr2, Renter_employment_I.pr2, Renter_age_I.pr2, Renter_gender_I.pr2, Renter_home_regionid_I.pr2)),
          column.labels = c("Interactions with Income", "Interactions with Education", "Interactions with Employment", "Interactions with Age", "Interactions with Gender", "Interactions with Home Region"),
          type = "html",
          title = "Interacted With Demographic Renter Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/InterDemoRenters.htm")


# Renter Models with Preferences

# Plan to Move

Renter_plan_move_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*plan_move_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_plan_move_I_robust <- sqrt(diag(vcovHC(Renter_plan_move_I, type = "HC0")))
Renter_plan_move_I.pr2 <- round(1 - Renter_plan_move_I$deviance/Renter_plan_move_I$null.deviance, digits = 3)

# Current Place Type

Renter_curr_place_type_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*curr_place_type_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_curr_place_type_I_robust <- sqrt(diag(vcovHC(Renter_curr_place_type_I, type = "HC0")))
Renter_curr_place_type_I.pr2 <- round(1 - Renter_curr_place_type_I$deviance/Renter_curr_place_type_I$null.deviance, digits = 3)

# Current Residency type

Renter_curr_res_type_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*curr_res_type_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_curr_res_type_I_robust <- sqrt(diag(vcovHC(Renter_curr_res_type_I, type = "HC0")))
Renter_curr_res_type_I.pr2 <- round(1 - Renter_curr_res_type_I$deviance/Renter_curr_res_type_I$null.deviance, digits = 3)

# Prefer Place type

Renter_prefer_place_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*prefer_place_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_prefer_place_I_robust <- sqrt(diag(vcovHC(Renter_prefer_place_I, type = "HC0")))
Renter_prefer_place_I.pr2 <- round(1 - Renter_prefer_place_I$deviance/Renter_prefer_place_I$null.deviance, digits = 3)

# Prefer Residency type

Renter_prefer_res_type_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*prefer_res_type_I, data = RenterData, na.action = na.exclude, family = binomial)
Renter_prefer_res_type_I_robust <- sqrt(diag(vcovHC(Renter_prefer_res_type_I, type = "HC0")))
Renter_prefer_res_type_I.pr2 <- round(1 - Renter_prefer_res_type_I$deviance/Renter_prefer_res_type_I$null.deviance, digits = 3)


stargazer(Renter_plan_move_I, Renter_curr_place_type_I, Renter_curr_res_type_I, Renter_prefer_place_I, Renter_prefer_res_type_I,
          se = list(Renter_plan_move_I_robust, Renter_curr_place_type_I_robust, Renter_curr_res_type_I_robust, Renter_prefer_place_I_robust, Renter_prefer_res_type_I_robust),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Renter_plan_move_I.pr2, Renter_curr_place_type_I.pr2, Renter_curr_res_type_I.pr2, Renter_prefer_place_I.pr2, Renter_prefer_res_type_I.pr2)),
          column.labels = c("Interactions with Plan to Move", "Interactions with Current Place Type", "Interactions with Current Residency Type", "Interactions with Preffered Place Type", "Interactions with Preffered Residency Type"),
          type = "html",
          title = "Interacted With Preference Renter Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/InterPrefRenters.htm")








      # Owner Marginal Willingness to Pay

OwnerData  <-read_rds("C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/ResChoice_w_CensTract.RDS")

# Check to see if variable classes are maintained
sapply(OwnerData, class)


# Owner Models with Demographics

# Income 

Owner_Income_I <- glm(choice ~ alt + commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000) + (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000)):Income_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_Income_I_robust <- sqrt(diag(vcovHC(Owner_Income_I, type = "HC0")))
Owner_Income_I.pr2 <- round(1 - Owner_Income_I$deviance/Owner_Income_I$null.deviance, digits = 3)

# Education

Owner_Education_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*Education_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_Education_I_robust <- sqrt(diag(vcovHC(Owner_Education_I, type = "HC0")))
Owner_Education_I.pr2 <- round(1 - Owner_Education_I$deviance/Owner_Education_I$null.deviance, digits = 3)

# Employment

Owner_employment_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*employment_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_employment_I_robust <- sqrt(diag(vcovHC(Owner_employment_I, type = "HC0")))
Owner_employment_I.pr2 <- round(1 - Owner_employment_I$deviance/Owner_employment_I$null.deviance, digits = 3)

# Age

Owner_age_I <- glm(choice ~ (alt + commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000)):age_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_age_I_robust <- sqrt(diag(vcovHC(Owner_age_I, type = "HC0")))
Owner_age_I.pr2 <- round(1 - Owner_age_I$deviance/Owner_age_I$null.deviance, digits = 3)

# gender

Owner_gender_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*gender_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_gender_I_robust <- sqrt(diag(vcovHC(Owner_gender_I, type = "HC0")))
Owner_gender_I.pr2 <- round(1 - Owner_gender_I$deviance/Owner_gender_I$null.deviance, digits = 3)

# Home Region

Owner_home_regionid_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*home_regionid_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_home_regionid_I_robust <- sqrt(diag(vcovHC(Owner_home_regionid_I, type = "HC0")))
Owner_home_regionid_I.pr2 <- round(1 - Owner_home_regionid_I$deviance/Owner_home_regionid_I$null.deviance, digits = 3)


library(stargazer)
stargazer(Owner_Income_I, Owner_Education_I, Owner_employment_I, Owner_age_I, Owner_gender_I, Owner_home_regionid_I,
          se = list(Owner_Income_I_robust, Owner_Education_I_robust, Owner_employment_I_robust, Owner_age_I_robust, Owner_gender_I_robust, Owner_home_regionid_I_robust),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Owner_Income_I.pr2, Owner_Education_I.pr2, Owner_employment_I.pr2, Owner_age_I.pr2, Owner_gender_I.pr2, Owner_home_regionid_I.pr2)),
          column.labels = c("Interactions with Income", "Interactions with Education", "Interactions with Employment", "Interactions with Age", "Interactions with Gender", "Interactions with Home Region"),
          type = "html",
          title = "Interacted With Demographic Owner Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/InterDemoOwners.htm")


# Owner Models with Preferences

# Plan to Move

Owner_plan_move_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*plan_move_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_plan_move_I_robust <- sqrt(diag(vcovHC(Owner_plan_move_I, type = "HC0")))
Owner_plan_move_I.pr2 <- round(1 - Owner_plan_move_I$deviance/Owner_plan_move_I$null.deviance, digits = 3)

# Current Place Type

Owner_curr_place_type_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*curr_place_type_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_curr_place_type_I_robust <- sqrt(diag(vcovHC(Owner_curr_place_type_I, type = "HC0")))
Owner_curr_place_type_I.pr2 <- round(1 - Owner_curr_place_type_I$deviance/Owner_curr_place_type_I$null.deviance, digits = 3)

# Current Residency type

Owner_curr_res_type_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*curr_res_type_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_curr_res_type_I_robust <- sqrt(diag(vcovHC(Owner_curr_res_type_I, type = "HC0")))
Owner_curr_res_type_I.pr2 <- round(1 - Owner_curr_res_type_I$deviance/Owner_curr_res_type_I$null.deviance, digits = 3)

# Prefer Place type

Owner_prefer_place_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*prefer_place_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_prefer_place_I_robust <- sqrt(diag(vcovHC(Owner_prefer_place_I, type = "HC0")))
Owner_prefer_place_I.pr2 <- round(1 - Owner_prefer_place_I$deviance/Owner_prefer_place_I$null.deviance, digits = 3)

# Prefer Residency type

Owner_prefer_res_type_I <- glm(choice ~ (commute + destinations + homes + parking + streets + transit + I(Relative_Change_Price/1000))*prefer_res_type_I, data = OwnerData, na.action = na.exclude, family = binomial)
Owner_prefer_res_type_I_robust <- sqrt(diag(vcovHC(Owner_prefer_res_type_I, type = "HC0")))
Owner_prefer_res_type_I.pr2 <- round(1 - Owner_prefer_res_type_I$deviance/Owner_prefer_res_type_I$null.deviance, digits = 3)

# Home value above census median

Owner

stargazer(Owner_plan_move_I, Owner_curr_place_type_I, Owner_curr_res_type_I, Owner_prefer_place_I, Owner_prefer_res_type_I,
          se = list(Owner_plan_move_I_robust, Owner_curr_place_type_I_robust, Owner_curr_res_type_I_robust, Owner_prefer_place_I_robust, Owner_prefer_res_type_I_robust),
          no.space = TRUE,
          intercept.bottom = FALSE,
          add.lines = list(c("Puesdo R2", Owner_plan_move_I.pr2, Owner_curr_place_type_I.pr2, Owner_curr_res_type_I.pr2, Owner_prefer_place_I.pr2, Owner_prefer_res_type_I.pr2)),
          column.labels = c("Interactions with Plan to Move", "Interactions with Current Place Type", "Interactions with Current Residency Type", "Interactions with Preffered Place Type", "Interactions with Preffered Residency Type"),
          type = "html",
          title = "Interacted With Preference Owner Models",
          out = "C:/Users/A01246966/Box/Utah Travel Study/Thesis_Work/InterPrefOwners.htm")

