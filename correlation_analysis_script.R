################# Intro ##################
#This script will load the needed files containing all data 
#then the preparation and multivariate ANOVA perform
#last update: 07.06.2023


# Clear workspace
rm(list=ls())

# Load packages 
library(readxl)
library(readr)
library(magrittr)
library(sf)
library(corrplot)
library(tidyverse)
library(dplyr)
#make sure the working directory is in place
getwd()

#import data:

full_data <-  read_csv("lapalma_output_data/full_data.csv")


str(full_data)

#delete all unnecesary cloumns and filter out wrong row with zero needle length

#full_data <- select(full_data, "tree_id", "needle_type", "t_b","length", "needle_damage_percentage", "damage", "sampl_contr", "dbh",
#                    "ash_depth", "prim_needles", "sec_needles", "trunk_needles", "flowering","cones",
#                    "tip_needles", "burn", "physical_damage", "layer_herb", "other_trees", "layer_tree", "tree_no",
#                    "biomass", "seedlings", "saplings","branch_dead")

#delete all rows with NA in column "length"

full_data <- full_data[!is.na(full_data$length), ]


#select only unique columns

full_data <- select(full_data, "t_b","length", "needle_damage_percentage", "sampl_contr", "dbh",
                    "ash_depth", "prim_needles", "sec_needles", "trunk_needles", "burn", 
                    "physical_damage", 
                    "tree_no", "distance")

#test for correlation with corrplot
coordata <- cor(full_data,use="pairwise.complete.obs")
corrplot(coordata,
         method = "number",      # Display correlation coefficients
         tl.cex = 0.5,    
         number.cex = 0.5,       # Change font size of correlation numbers
         order = "AOE"           # Swap lower left triangle into circles
)



# Calculate the standard deviation for each needle type at each ash depth level
#grouped_sd <- full_data %>%
#  group_by(needle_type, ash_depth) %>%
#  summarize(sd_needle_damage = sd(needle_damage_percentage))

#sd_data <- left_join(grouped_sd, full_data, by= "ash_depth")
#str(sd_data)

#select only unique columns

#sd_data <- select(sd_data, "t_b","length", "needle_damage_percentage", "sampl_contr", "dbh",
#                    "ash_depth", "prim_needles", "sec_needles", "trunk_needles", "flowering","cones", "burn", 
#                    "physical_damage", 
#                    "layer_herb", "layer_tree", "tree_no", "distance")

#coorsd <- cor(sd_data,use="pairwise.complete.obs")
#corrplot(coorsd, method = "number",tl.cex = 0.5,addCoef.col = 1,    # Change font size of correlation coefficients
#                            number.cex = 0.5)


#write result to image
#jpeg(file="lapalma_graphs/correlationplot_numbers.jpeg")
#correlationplot <- corrplot(coordata, method = "number",tl.cex = 0.5,addCoef.col = 1,    # Change font size of correlation coefficients
#                            number.cex = 0.5)
#dev.off()
