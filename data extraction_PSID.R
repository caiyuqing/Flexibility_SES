################## data extraction for "what is ses" project ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    20-02-25         The first version
# 
# 
#
###### input######
# PSID selected data
###### output #####
#
#
#
#
######################## Start of the script ###########################
### clean the memory to avoid unnecessary errors:
rm(list = ls())

### set directory to the folder of analytic data

# Get the directory of the current R script
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the directory where this script is 
setwd(curWD)

#read data
library(foreign)
library(tidyverse)
d <- read.spss("selected for proposal_v1.sav", to.data.frame = TRUE, use.value.labels = TRUE)
#see variable name
d_var <- attr(d, "variable.labels")
#age
d$ER34504[d$ER34504 == 999] <-NA
d_male <-d %>%
  filter(ER32000 == 1)
d_female <-d %>%
  filter(ER32000 == 2)
table(cut(d_male$ER34504, breaks = c(0, 10, 16, 25, 40, 60, 103)))
table(cut(d_female$ER34504, breaks = c(0, 10, 16, 25, 40, 60, 103)))