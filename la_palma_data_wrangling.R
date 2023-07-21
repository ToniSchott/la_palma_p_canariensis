################# Intro ##################
#This script will load the needed files containing all data 
#it will clean up the data as well as join the measured data point (trees) 
#with the corresponding GPS locations
#last update: 06.06.2023
#######################################

# Clear workspace
rm(list=ls())

# Load packages 
library(readxl)
library(readr)
library(magrittr)
library(tidyverse)
library(sf)
#make sure the working directory is in place
getwd()

#raw data files can be found in the "lapalma_input_data" folder, 
#wrangled data will be stored in the "lapalma_output_data" folder
#graphs and images are stored in "lapmalma_graphs".

######## Import all needed data ###########

#load the sheets "Data_Length" as well as "Data" from the excel file "methods in
#the file methods_altered_ash_depth has only numeric values (no > or < )
needle_length <- read_excel("lapalma_input_data/Methods.xlsx", "Data_Length")

additional_data <- read_excel("lapalma_input_data/Methods.xlsx", "Data")

#check the tables /structure

str(needle_length)
str(additional_data)

#which rows do we need? select those

additional_data <- select(additional_data, !c(Latitude,Longitude,Latitude,Elevation,Additional))

#join tables

full_data <- full_join(needle_length, additional_data, by = "Tree ID")

str(full_data)


#rename columns with complicated names

full_data <- rename(full_data, tree_id = 'Tree ID', needle_type = "Needle TyPe", bundle_number = "Bundle number",
                    needle_id = 'Needle (ID / number)', t_b = 'T/B' , length = 'Length', damage = "Damage", 
                    t_m_l = "T/M/L", dbh = "DBH", ash_depth = "Ash depth", hight_over_ash = "Height over Ash", 
                    prim_needles = "Primary needles", sec_needles = "Secondary needles", 
                    trunk_needles = "Needles on trunk", branch_needles = "Needles on branch", 
                    flowering = "Flowering", cones = "Cones", tip_needles = "Needles on tip",
                    burn = "Burnt", physical_damage = "Physical Damage Branch / Trunk (0-5)", 
                    layer_herb = "Herbaceous layer", layer_shrub = "shrub layer", other_trees = "other trees", 
                    layer_tree = "tree layer", tree_no = "number of trees", biomass = "Biomass",
                    seedlings ="Seedlings", saplings = "Saplings", branch_dead = "dead branches", sampl_contr = "S/C")

#change "N.O."'s in column "burn", "dbh" and "tip_needles" and "hight_over_ash" into NA's

full_data$burn <- ifelse(full_data$burn== "N.O.", NA, full_data$burn)
full_data$tip_needles <- ifelse(full_data$tip_needles== "N.O.", NA, full_data$tip_needles)
full_data$dbh <- ifelse(full_data$dbh== "N.O.", NA, full_data$dbh)
full_data$hight_over_ash <- ifelse(full_data$hight_over_ash== "N.O.", NA, full_data$hight_over_ash)

#change needle type, trunk /branch ash depth and seedlings into numerical values
full_data$needle_type <- ifelse(full_data$needle_type == "P", 0,
                                ifelse(full_data$needle_type =="S",1,NA))
full_data$t_b <- ifelse(full_data$t_b == "T", 0,
                        ifelse(full_data$t_b =="B",1,NA))
full_data$seedlings <- ifelse(full_data$seedlings == ">100", 100,full_data$needle_type)

full_data$layer_tree <- ifelse(full_data$layer_tree == "<0.1", 0.05 , full_data$layer_tree)
full_data$layer_herb <- ifelse(full_data$layer_herb == "<0.1", 0.05 , full_data$layer_herb)
full_data$layer_shrub <- ifelse(full_data$layer_shrub == "<0.1", 0.05 , full_data$layer_shrub)
full_data$tree_no <- ifelse(full_data$tree_no == ">20", 21 , full_data$tree_no)
full_data$sampl_contr <- ifelse(full_data$sampl_contr == "S", 0,
                                ifelse(full_data$sampl_contr =="C",1,NA))

full_data$ash_depth <- ifelse(full_data$ash_depth == "<1.0", 0.9,
                              ifelse(full_data$ash_depth ==">81", 81,
                                     ifelse(full_data$ash_depth == ">150", 151, full_data$ash_depth)))
full_data$dbh <- ifelse(full_data$dbh == "NA", NA,full_data$dbh)
full_data$ash_depth <- ifelse(full_data$ash_depth == "NA", NA,full_data$ash_depth)
full_data$burn <- ifelse(full_data$burn == "NA", NA,full_data$burn)


full_data <- filter(full_data, !length == 0)
full_data$damage <- as.numeric(full_data$damage)
full_data$layer_tree <- as.numeric(full_data$layer_tree)
full_data$layer_herb <- as.numeric(full_data$layer_herb)
full_data$dbh <- as.numeric(full_data$dbh)
full_data$ash_depth <- as.numeric(full_data$ash_depth)
full_data$hight_over_ash <- as.numeric(full_data$hight_over_ash)
full_data$tip_needles <- as.numeric(full_data$tip_needles)
full_data$burn <- as.numeric(full_data$burn)
full_data$layer_shrub <- as.numeric(full_data$layer_shrub)
full_data$tree_no <- as.numeric(full_data$tree_no)
str(full_data)

#add a column with needle damage percentage
full_data <- mutate(full_data, needle_damage_percentage = damage/length)

#filter out all values where damage is wrongfully bigger than needle length
full_data <- filter(full_data, needle_damage_percentage <= 1)

full_data$tree_id <- round(full_data$tree_id, digits = 0)


#save the .txt file into the output folder 

write.csv(full_data, "lapalma_output_data/full_data.csv")

