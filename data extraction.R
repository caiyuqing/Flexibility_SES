################## data extraction for "what is ses" project ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    19-03-09         The first version
# 
# 
#
###### input######
#data 2010
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

# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(dplyr)) {install.packages("dplyr",repos = "http://cran.us.r-project.org"); require(dplyr)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
library("psych")
library("dplyr")
library("tidyverse")
#read data
dfc0 <- read.csv("data/2010child.csv", header=T)
dfa0 <- read.csv("data/2010adult.csv", header=T)
dff0 <- read.csv("data/2010family.csv", header=T)
dffr0 <- read.csv("data/2010familyroster.csv", header = T)

#extract related data according to SES 2010 codebook
##adult questionnaire-individual

individual <- dfa0 %>%
  dplyr::select(pid, fid,
                #education
                edu2010_t1_best,	#Best var of highest level of education attained at 2010
                educ,	#Level of education (detailed)
                eduy2010,	#Best var of years of schooling completed
                #individual income
                income,	#personal income
                qk101,	#Average monthly wage/salary last year (yuan)
                qk102,	#Average monthly floating wage, overtime, bonus, subsidies (yuan)
                qk103,	#Annual bonus last year (yuan)
                qk104,	#Monetary value of the goods received from employer last year (yuan)
                qk105,	#Income from second, part-time or temporary job last year (yuan)
                qk106,	 #Income from other work last year (yuan)
                qk107,	#Pension last year (yuan)
                qk2,	#Net income from non-agricultural business last year(yuan)
                qk301,	#Total financial support from family or relatives last year
                qk401,	#Total support from the community committee last year (yuan)
                qk501,	#Total subsidy from government or work unit last year (yuan)
                qk6_max,	#Income interval (upper limit) last year
                qk6_min,	#Income interval (lower limit) last year
                qk601,	#Total income last year
                 ###occupation
                qg307code,	#Occupation classification (CSCO system)
                qg307isco,	#Occupation classification (ISCO-88)
                qg307egp,	#Occupation classification (EGP)
                qg307isei,	#Occupation SES coding 1 (isei)
                qg307siops,	#Occupation SES coding 2 (occupational prestige）
                ###subjective SES
                qm401,	#Income level in local area
                qm402,	#Social status in local area
                qq601, qq602,qq603,qq604,qq605,qq606,#depression
                wordtest, mathtest) #cognitive ability

###family 
family <- dff0 %>%
  dplyr::select(fid,
                #family income
                fsalary,	#Salary income
                fshift,	#transfer income
                fincome,	#Total family income
                finc_per,	#Per capita family income
                inc_agri,	#Adjusted total agricultural income
                net_agri,	#Adjusted net agricultural income
                finc,	#Wage income
                firm,	#Non-agricultural business income
                fproperty,	#property income
                welfare,	#transfer income
                felse,	#other income
                faminc_old, #Unadjusted total family income
                faminc_net,	#Adjusted total family income
                faminc, #Adjusted net family income
                indinc, #Adjusted total family income per capita
                indinc_net, #Adjusted net family income per capita
                foperate, #Adjusted total operational income (including agricultural and non-agr business)
                foperate_net, #Adjusted total operational income (including agricultural and non-agr business)
                ff6_max, #Upper limit of total family income last year excluding pension/subsidy (yuan)
                ff6_min, #Lower limit of total family income last year excluding pension/subsidy (yuan)
                ff601, #Total family income last year excluding pension/subsidy (yuan)
                ff701, #Non-wage or agricultural production income last year (yuan)
                ff8, #Equivalent value in cash of the gifts received last year (yuan)
                fg1, #All compensable insurances at the end of last year (yuan)
                fg2, #All debts owed to your family at the end of last year (yuan)
                fg3, #Market value of your family's collections at the end of last year (yuan)
                fg4, #Market value of other assets at the end of last year (yuan)
                fh1, #The most expensive piece of family goods last year (yuan)
                fh201_a_1, #Bank loan last year (yuan)
                fh201_a_3, #Money borrowed from relatives/friends (yuan)
                fh201_a_5, #Private loan from the market (yuan)
                fh201_a_6, #Other loan (yuan)
                fh203_a_1, #Amount of loan used for housing (yuan)
                fh203_a_2, #Amount of loan used for education (yuan)
                fh203_a_3, #Amount of loan used for durable goods (yuan)
                fh203_a_4, #Amount of loan used for medical care (yuan)
                fh203_a_5, #Amount of loan used for daily living expenses (yuan)
                fh203_a_6, #Amount of loan used for other purposes (yuan))
                ###family expenditure
                fh301,	#Expenditure on food last month (yuan)
                fh302,	#Expenditure on daily necessities last month (yuan)
                fh303,	#Expenditure on transportation (yuan)
                fh304,	#Expenditure on communication last month (yuan)
                fh305,	#Expenditure on family member support last month (yuan)
                fh306,	#Expenditure on housing mortgage last month (yuan)
                fh307,	#Expenditure on car mortgage last month (yuan)
                fh308,	#Expenditure on other mortgages last month (yuan)
                fh309,	#Expenditure on renting a house last month (yuan)
                fh401,	#Expenditure on electricity last year (yuan)
                fh402,	#Expenditure on medical care last year (yuan)
                fh403,	#Expenditure on clothing last year (yuan)
                fh404,	#Expenditure on education last year (yuan)
                fh405,	#Expenditure on culture/entertainment/leisure activities last year (yuan)
                fh406,	#Expenditure on housing last year (community management, heating, etc.) (yuan)
                fh407,	#Expenditure on other goods and services last year (yuan)
                fh408,	#Expenditure on purchasing or building a house last year (yuan)
                fh409,	#Expenditure on commercial insurance last year (yuan)
                fh410,	#Expenditure on marriage and funerals last year (yuan)
                fh411,	#Expenditure on other items last year (yuan)
                fh502,	#Total monetary value of the donations last year (yuan)
                fh504,	#Total monetary value of the donations for the Wenchuan earthquake (yuan)
                fh6_max,	#Upper limit of total expenditures last year
                fh6_min,	#Lower limit of total expenditures last year
                fh601, #Total expenditures last year(yuan))
                familysize) #familysize
#combine as one dataframe
data2010 <- merge(individual, family, by = "fid", all.x= TRUE)
