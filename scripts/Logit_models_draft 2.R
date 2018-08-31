## Models and Tables for Draft 2 ##

# Importing data

MasterLogitData <- readRDS(file = "C:/Users/Kristopher/odrive/Box/Utah Travel Study/modified_data/MasterLogitData.rds")


res1 <- mlogit(choice ~ commute + destinations + homes + streets + transit + parking + price,
               data = subset(MasterLogitData), shape = "long", alt.var = "alt", id = "id", chid.var = "chid")

stargazer(replicated, res1, type = "text")

library(lmtest
        )


coeftest(res1)

# Determining How many Degrees of Freedom we have

library(readxl)
ResidentialChoice_ChoiceExperimentsData <- read_excel("C:/Users/Kristopher/odrive/Box/Utah Travel Study/Utah Travel Study 2012/2. Data and Materials/7. Residential Choice Survey/2. Choice Experiments Dataset/ResidentialChoice_ChoiceExperimentsData.xlsx", 
                                                      col_types = c("numeric", "text", "numeric", 
                                                                    "text", "text", "text", "text", 
                                                                    "text", "text", "text", 
                                                                    "text", "text", "text", 
                                                                    "text", "text", "text", 
                                                                    "text", "text"))

df <- unique(ResidentialChoice_ChoiceExperimentsData[5:18])
nrow(df)

