# Install useful packages

install.packages(c("ggplot2", "manipulate", "mosaic", "plyr", "read_excel", "car", "ggcorrplot", "GGally", "readxl", "taRifx", "support.CEs", "mlogit"))


if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggcorrplot")