
## Hand movements reveal the time course of gender stereotypes during facial categorization  

# Francisco Gutiérrez-Blanco, Ana F. Palenciano and María Ruz
# Department of Experimental Psychology
# Mind, Brain and Behavior Research Center
# University of Granada. Spain

### Pre-processing of mouse_data


#libraries!!!


install.packages("lme4")
install.packages("ordinal")
install.packages("mousetrap")
install.packages("readbulk")
install.packages("tidyverse")



library(readbulk)
library(mousetrap)
library(ordinal)
library(lme4)
library(tidyverse)
library(patchwork)
library(psych)
library(MBESS)
library(viridis)
library(afex)
library(osfr)
library(readr)
library(cowplot) 
library(RColorBrewer) 
library(ggplot2) 
library(tidyr) 
 

library(caret)
library(broom)          
library(leaps)
library(dplyr)
library(car)
library(Hmisc)
library(olsrr)
library(MASS)



# add directory where all the participant data is

setwd("...../subjects.")



#Import all the raw data of the participants into the same data set (data_raw)

data_raw <- read_opensesame(directory = "....", extension = ".csv")


#Check for missing values

colSums(is.na(data_raw))


### Data preprocessing


## 1) First data filtering. 

# We choose only the experimental part, trials with correct answers and trials 
# with reaction times less than 2000 ms


# 1.1) Trials from the experimental part

data_raw <- subset(data_raw, PARTES=="EXP")


# 1.2) Corrects responses

# proportion of correct and errors answers

resp_correct <- sum(data_raw$correct == 1)
resp_error <- sum(data_raw$correct == 0)

p_resp_correct <- resp_correct/ length(data_raw$correct)
p_resp_error <- resp_error / length(data_raw$correct)

cat("proportion of correct answers:", p_resp_correct, "\n")
cat("proportion of incorrect answers:", p_resp_error, "\n")

# We select only correct trials

data_raw <- subset(data_raw, correct == 1)


# 1.3) Trials with reaction times less than 2000 ms

# proportion of responses with RTs less and greater than 200 ms

rt_less_2000 <- sum(data_raw$response_time_get_response < 2000)
rt_more_2000 <- sum(data_raw$response_time_get_response >= 2000)

p_rt_less_2000 <- rt_less_2000 / length(data_raw$response_time_get_response)
p_rt_more_2000 <- rt_more_2000 / length(data_raw$response_time_get_response)

cat("proportion of responses with RTs less than 200 ms:",p_rt_less_2000 , "\n")
cat("proportion of responses with RTs greater than 200 ms:",p_rt_more_2000, "\n")

data_raw <- subset(data_raw, response_time_get_response < 2000)



## 2) Import mouse trajectories and Temporal and longitudinal normalization of the trajectories

mt_data <- mt_import_mousetrap(data_raw)


# 2.1) Assign all trajectories to the same response position "LEFT BY DEFAULT"

mt_data<-mt_remap_symmetric( mt_data)
 

# 2.2) assign the same movement start position to all trajectories

mt_data <- mt_align_start(mt_data, start = c(0,0))


# 2.3) Temporal Normalization

mt_data <- mt_time_normalize(mt_data)


# 2.4) Longitudinal normalization

mt_data <- mt_length_normalize(mt_data)



## 3) Filter Outliers by a distance greater than 2 SD to the grouped prototype


# 3.1) Group the longitudinally normalized trajectories based on their 
# similarity to the five prototypes.

mt_data <- mt_map(mt_data) 


# 3.2) Determine standardized distance to nearest prototype !z_min_dist !

mt_data <- mt_standardize(mt_data,use = "prototyping",use_variables = "min_dist")


# 3.3) Locate and plot Outliers.

mt_data$data$outlier <- ifelse( mt_data$prototyping$z_min_dist > 2, "Distance > 2 SD", "Distance <= 2 SD")



# 3.4) Exclude outliers.

outliers <- sum(mt_data$data$outlier == "Distance > 2 SD")
outliers
no_outliers <- sum(mt_data$data$outlier == "Distance <= 2 SD")

p_outliers <- outliers / length(mt_data$data$outlier)
p_no_outliers <- no_outliers / length(mt_data$data$outlier)

cat("Proportion of outliers :", p_outliers, "\n")
cat("Proportion of no_outliers :", p_no_outliers, "\n")

mt_data <- mt_subset(mt_data,z_min_dist <= 2, check = "prototyping")



## 4) Import Meassures (MAD...)

mt_data <- mt_measures(mt_data)


