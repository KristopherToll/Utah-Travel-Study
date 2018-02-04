    
    ## PLOTS FOR HouseResChoice_a ##

# Kristopher C. Toll
# October 2017

# Reading in rds file for HouseResChoice_a

HouseResChoice_a <- readRDS(file = "I:/Utah Travel Study/Utah Travel Study/modified_data/HouseResChoice_a.rds")


# 



ggplot(data = HouseResChoice_a, aes(x = what_months_feb)) + geom_histogram(binwidth = 0.04) + facet_wrap(~choice_a, ncol = 4) + labs(title = "") 

ggplot(data = HouseResChoice_a, aes(x = choice_a, y = hhsize)) + geom_boxplot() + facet_wrap(~as.numeric(thecount), ncol = 5, scales = "free_y") + labs(title = "")

ggplot(data = HouseResChoice_a, aes(x = hh_income_cat, y = hh_children)) +aes(group = hh_income_cat) +geom_boxplot() + facet_wrap(~regionname, ncol = 5, scales = "free_y") + labs(title = "")




#





























# Script for Plots

tab = data.frame(table(df$section, df$weekday))
names(tab) = c("section", "weekday", "count")
Tile plot

p = ggplot(tab, aes(x = section, y = weekday))
p = p + geom_tile(aes(fill = count))

# Create a Scatter Plot matrix of variables of interest


##’smooth’ adds linear fitted line to each scatter plot
GGally::ggpairs(ResChoiceExp[5:16], lower = list(continuous=smooth))

## variable have to be numeric to use this command
car::scatterplotMatrix(ResChoiceExp[4:16])

##

pairs(ResChoiceExp[4:16])

ggcorrplot::ggcorrplot(corr = (ResChoiceExp[4:16]))


# This PDF shows how to weight cases https://cran.r-project.org/web/packages/weights/weights.pdf


mosaicplot(bmi.age.gender, main = "Relationship between BMI, age and gender",
           xlab = "BMI", ylab = "age", cex = 0.75, color = TRUE)