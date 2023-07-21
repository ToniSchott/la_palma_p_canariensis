
# Clear workspace
rm(list=ls())

#load packages
library(DescTools)
library(readxl)
library(readr)
library(magrittr)
library(tidyverse)
library(dplyr)


full_data <-  read_csv("lapalma_output_data/full_data.csv")
str(full_data)


# glm for different needle types and needle length

secondary_branch <- filter(full_data, needle_type == 1, t_b == 1)

glm_sec_b <- glm(length~ burn+sampl_contr+
                        ash_depth +
                        tree_no + hight_over_ash + prim_needles
                      ,data = secondary_branch, family = "poisson")
summary(glm_sec_b)



secondary_trunk <- filter(full_data, needle_type == 1, t_b == 0)

glm_sec_t  <- glm(length~ burn+sampl_contr+
                        ash_depth +
                        t_b + tree_no + hight_over_ash + prim_needles
                      ,data = secondary_trunk, family = "poisson")
summary(glm_sec_t)


#glm for needle damage percentage
glm_percentage_damage <- glm(needle_damage_percentage ~ burn+sampl_contr+
                               ash_depth+ trunk_needles+ needle_type +
                               t_b + tree_no + hight_over_ash + prim_needles +
                               saplings + flowering, 
                             data = full_data, family = "binomial")
summary(glm_percentage_damage)




#explain variance of the data with our glm model r²
adjusted_r2 <- PseudoR2(glm_percentage_damage, which="Nagelkerke")
adjusted_r2



#do glm for different needle types

secondary_branch <- filter(full_data, needle_type == 1)

#glm for needle damage percentage
glm_percentage_damage_sec_branch <- glm(needle_damage_percentage ~ burn+ ash_depth+sampl_contr+
                                          trunk_needles +
                                          t_b + tree_no + hight_over_ash + prim_needles +
                                          saplings, 
                             data = secondary_branch, family = "binomial")
summary(glm_percentage_damage_sec_branch)

#explain variance of the data with our glm model r²
adjusted_r2_sb <- PseudoR2(glm_percentage_damage_sec_branch, which="Nagelkerke")
adjusted_r2_sb

# Calculate the standard deviation for each needle type at each ash depth level
grouped_sd <- full_data %>%
  group_by(needle_type, ash_depth) %>%
  summarize(sd_needle_damage = sd(needle_damage_percentage), burn_mean = mean(burn), hight_over_ash_mean = mean(hight_over_ash),
            saplings_mean = mean(saplings))

grouped_sd <- left_join( full_data, grouped_sd, by= "")

glm_sd <- glm(sd_needle_damage ~ burn_mean+
                ash_depth + hight_over_ash_mean +
                saplings_mean,
              data = grouped_sd, family = "binomial")
summary(glm_sd)

#explain variance of the data with our glm model r²
adjusted_r2_sd <- PseudoR2(glm_sd, which="Nagelkerke")
adjusted_r2_sd


#glm for primary needle presence and trunk neele presence

glm_trunk <- glm(trunk_needles~ ash_depth
                   ,data = full_data, family = "poisson")
summary(glm_trunk)

glm_prim <- glm(prim_needles~ash_depth, data = full_data, family = "poisson")

summary(glm_prim)


#save glm summary
sink(file = "lapalma_output_data/glm.txt",  append = F)
print("glm_damage_percentage")
summary(glm_percentage_damage)

print("adjusted R² for damage percentage")
adjusted_r2

print("glm needle length")
summary(glm_full_data)

print("glm secondary branch damage")
summary(glm_percentage_damage_sec_branch)

print("adjusted R² for secondary branch damage")

adjusted_r2_sb

print("glm on standard deviation of needle damage")
summary(glm_sd)

print("adjusted R² of standar deviation needle damage")
adjusted_r2_sd

sink()
