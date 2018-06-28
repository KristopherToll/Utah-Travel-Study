## Models and Tables for Draft 2 ##

# Importing data

MasterLogitData <- readRDS(file = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/MasterLogitData.rds")

replicated <- glm(choice ~ commute + destinations + homes + streets + transit + parking + price,
                  data = MasterLogitData, family = binomial)

res1 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + price,
               data = subset(MasterLogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

stargazer(replicated, res1, type = "text")