########################################################################################
########################################################################################
###                                                                                  ###
###                 R script for Flexibility of SES project                          ###
###                           [data extraction]                                      ###
###               Email = hcp4715@gmail.com       cyq_2016@outlook.com               ###
###                                                                                  ###
########################################################################################
########################################################################################

########################################################################################
########################################################################################
###                                                                                  ###
###  Purpose:                                                                        ###
###  Preprocess the data of CFPS (2010) to extract related variables (SES and mental ###
###  health)                                                                         ###
###  * The data of PSID (2017) was extracted on the official website of PSID,        ###
###    hence, no preprocessing is required                                           ###
###  Code authors: Hu Chuan-Peng Hu, PhD, Leibniz Institute for Resilience Research, ###  
###                55131 Mainz, Germany;                                             ###
###                Yuqing Cai, Tsinghua University, 100086, China                    ###
###                                                                                  ###
###  Input data (v37 of CFPS data)                                                   ###
###      Original data of CFPS 2010: csv file: '2010child_cfpsv37.csv',              ###
###                                             '2010adult_cfpsv37.csv',             ###
###                                   '2010family_cfpsv37.csv',                      ###
###                                   '2010community_cfpsv37.csv'                    ###
###                                                                                  ###
###  Output file and Variables:                                                      ###
###     'CFPS2010.RData' including four dataframes: 'df.children', 'df.family',      ###
###                                   'df.community', 'df.individual'                ###
###     Variables see notes in the code                                              ###
###                                                                                  ###
########################################################################################
########################################################################################

######################## Start of the script ###########################

rm(list = ls())                     # clean the memory to avoid unnecessary errors
Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en")             # set the feedback language to English
options(scipen = 999)               # force R to output in decimal instead of scientific notion
options(digits = 5)                 # limit the number of reporting

### set directory to the folder of analytic data
# Get the directory of the current R script
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the directory where this script is 
setwd(curWD)

# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(dplyr)) {install.packages("dplyr",repos = "http://cran.us.r-project.org"); require(dplyr)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}

# read data
# added fileEncoding to avoid junk text in the 1st colname
dfc0 <- read.csv("data/2010child_cfpsv37.csv", header=T, fileEncoding="UTF-8-BOM")            # CFPS 2010 children's dataset
dfa0 <- read.csv("data/2010adult_cfpsv37.csv", header=T, fileEncoding="UTF-8-BOM")            # CFPS 2010 adults' dataset
dff0 <- read.csv("data/2010family_cfpsv37.csv", header=T, fileEncoding="UTF-8-BOM")           # CFPS 2010 family' dataset
dfcom0 <- read.csv("data/2010community_cfpsv37.csv", header= T, fileEncoding="UTF-8-BOM")     # CFPS 2010 community' dataset
dffr0 <- read.csv("data/2010familyroster_cfpsv37.csv", header =T, fileEncoding = "UTF-8-BOM") # CFPS 2010 family roster dataset

# convert all the column names to lower cases
names(dfc0) <- tolower(names(dfc0))
names(dfa0) <- tolower(names(dfa0))
names(dff0) <- tolower(names(dff0))
names(dfcom0) <- tolower(names(dfcom0))
names(dffr0) <- tolower(names(dffr0))

# extract related data (SES and mental health) according to SES 2010 codebook
## community 
df.community <- dfcom0 %>%
  dplyr::select(### ID	
                cid,    # community id
                # area size and population	
                ca4r,	  # Area size of the community(km2)
                cb1,	  # households last year
                cb2,	  # persons last year
                cb201,	# persons with local household registration last year
                cb202,	# regular residents last year
                cb203,	# floating population last year
                ### basic living allowance
                cc2,	# households eligible for the MLSS system
                cc3,	# households supported by the MLSS system
                cc4,	# Per capita monthly support (yuan) on average from the MLSS system
                ### surrounding environment	
                ce2,	# Tourist destination
                ce3,	# Polluting factory within 5 km
                cg5,	# Dose your village experience frequent natural disasters?
                ### housing price	
                cf1,	# Highest price of commercial housing in history
                cf2,	# Highest price of commercial housing last month
                cf3,	# Average price of commercial housing last month
                ### natural resources	
                cg4,	# Does the village have mineral resource?
                cg601_a_1,	# Per capita arable land (mu/person)
                cg601_a_2,	# Per capita mountainous land (mu/person)
                cg601_a_3,	# Per capita forest (mu/person)
                cg601_a_4,	# Per capita water surface (mu/person)
                cg601_a_5,	# Per capita pasture (mu/person)
                ### Environment observation	
                cz1,	  # Interviewer Observation: Economic condition
                cz2,	  # Interviewer Observation: Cleanlines of the roads
                cz3,	  # Interviewer Observation: Outlook of local people
                cz4,	  # Interviewer Observation: Socioeconomic homogeneity
                cz5,	  # Interviewer Observation: Architectural layout
                cz6,	  # Interviewer Observation: Spaciousness of the buildings
                cz7,	  # Interviewer Observation: Community type
                cz701,	# Interviewer Observation: Detailed city type
                cz702,	# Interviewer Observation: Detailed town type
                cz703)	# Interviewer Observation: Detailed village type)

## adult questionnaire-individual
df.individual <- dfa0 %>%
  dplyr::select(pid, fid, pid_f, pid_m, # pid, fid, pid of father, pid of mother
                gender, # gender
                qa1age, # age
                urban,  # region (rural = 0; urban =1)
                # education
                cfps2010edu_best,  # highest level of education (completed): with 8 levels
                cfps2010eduy_best, # education year: 0-22
                cfps2010sch_best,  # with 8 levels (for those who not finish schooling)
                feduc,             # education level of father 
                meduc,             # education level of mother (same coding as educ)
                # individual income
                income,	  # personal income
                qk101,	  # Average monthly wage/salary last year (yuan)
                qk102,	  # Average monthly floating wage, overtime, bonus, subsidies (yuan)
                qk103,	  # Annual bonus last year (yuan)
                qk104,	  # Monetary value of the goods received from employer last year (yuan)
                qk105,	  # Income from second, part-time or temporary job last year (yuan)
                qk106,	  # Income from other work last year (yuan)
                qk107,	  # Pension last year (yuan)
                qk2,	    # Net income from non-agricultural business last year(yuan)
                qk301,	  # Total financial support from family or relatives last year
                qk401,	  # Total support from the community committee last year (yuan)
                qk501,	  # Total subsidy from government or work unit last year (yuan)
                qk6_max,	# Income interval (upper limit) last year
                qk6_min,	# Income interval (lower limit) last year
                qk601,	  # Total income last year
                 ### occupation
                qg3, # employed or not
                qg7, # part-time
                qg307code,	# Occupation classification (CSCO system)
                qg307isco,	# Occupation classification (ISCO-88)
                qg307egp,	  # Occupation classification (EGP)
                qg307isei,	# Occupation SES coding 1 (isei)
                qg307siops,	# Occupation SES coding 2 (occupational prestige）
                ### subjective SES
                qm401,	    # Income level in local area
                qm402,	    # Social status in local area
                qq601, qq602,qq603,qq604,qq605,qq606,depression, # depression questionnaire items (qq601-qq606); total score of depression
                wordtest, mathtest) # cognitive ability

### family 
df.family <- dff0 %>%
  dplyr::select(# id
                fid, # fid
                # family income
                urban,          # indicator of urban or rural area
                familysize,     # family size
                faminc,         # total family income (adjusted) = finc+welfare+fproperty+firm+felse+inc_agri
                faminc_net,     # total family income (adjusted, net) = finc+welfare+fproperty+firm+felse+net_agri
                faminc_net_old, # total family income (not adjusted net) = finc+welfare+fproperty+firm+felse+max(fk5,fk3-fk4)
                faminc_old,     # total family income (not adjusted) = finc+welfare+fproperty+firm+felse+fk3
                indinc,         # average family income (per family member; adjusted) = faminc/familysize
                indinc_net,     # average family income (per family member; adjusted net) = faminc/familysize
                finc,           # family income (salary)
                inc_agri,	      # Adjusted total agricultural income
                net_agri,	      # Adjusted net agricultural income
                firm,	          # Non-agricultural business income
                fproperty,	    # property income
                welfare,	      # transfer income
                felse,	        # other income
                foperate,       # Adjusted total operational income (including agricultural and non-agr business)
                foperate_net,   # Adjusted total operational income (including agricultural and non-agr business)
                ff6_max,        # Upper limit of total family income last year excluding pension/subsidy (yuan)
                ff6_min,        # Lower limit of total family income last year excluding pension/subsidy (yuan)
                ff601,          # Total family income last year excluding pension/subsidy (yuan)
                ff701,          # Non-wage or agricultural production income last year (yuan)
                ff8,            # Equivalent value in cash of the gifts received last year (yuan)
                fg1,            # All compensable insurances at the end of last year (yuan)
                fg2,            # All debts owed to your family at the end of last year (yuan)
                fg3,            # Market value of your family's collections at the end of last year (yuan)
                fg4,            # Market value of other assets at the end of last year (yuan)
                fh1,            # The most expensive piece of family goods last year (yuan)
                fh201_a_1,      # Bank loan last year (yuan)
                fh201_a_3,      # Money borrowed from relatives/friends (yuan)
                fh201_a_5,      # Private loan from the market (yuan)
                fh201_a_6,      # Other loan (yuan)
                fh203_a_1,      # Amount of loan used for housing (yuan)
                fh203_a_2,      # Amount of loan used for education (yuan)
                fh203_a_3,      # Amount of loan used for durable goods (yuan)
                fh203_a_4,      # Amount of loan used for medical care (yuan)
                fh203_a_5,      # Amount of loan used for daily living expenses (yuan)
                fh203_a_6,      # Amount of loan used for other purposes (yuan))
                ### family expenditure
                fh301,	  # Expenditure on food last month (yuan)
                fh302,	  # Expenditure on daily necessities last month (yuan)
                fh303,	  # Expenditure on transportation (yuan)
                fh304,	  # Expenditure on communication last month (yuan)
                fh305,	  # Expenditure on family member support last month (yuan)
                fh306,	  # Expenditure on housing mortgage last month (yuan)
                fh307,	  # Expenditure on car mortgage last month (yuan)
                fh308,	  # Expenditure on other mortgages last month (yuan)
                fh309,	  # Expenditure on renting a house last month (yuan)
                fh401,	  # Expenditure on electricity last year (yuan)
                fh402,	  # Expenditure on medical care last year (yuan)
                fh403,	  # Expenditure on clothing last year (yuan)
                fh404,	  # Expenditure on education last year (yuan)
                fh405,	  # Expenditure on culture/entertainment/leisure activities last year (yuan)
                fh406,	  # Expenditure on housing last year (community management, heating, etc.) (yuan)
                fh407,	  # Expenditure on other goods and services last year (yuan)
                fh408,	  # Expenditure on purchasing or building a house last year (yuan)
                fh409,	  # Expenditure on commercial insurance last year (yuan)
                fh410,	  # Expenditure on marriage and funerals last year (yuan)
                fh411,	  # Expenditure on other items last year (yuan)
                fh502,	  # Total monetary value of the donations last year (yuan)
                fh504,	  # Total monetary value of the donations for the Wenchuan earthquake (yuan)
                fh6_max,	# Upper limit of total expenditures last year
                fh6_min,	# Lower limit of total expenditures last year
                fh601,    # Total expenditures last year(yuan))
                ### household environment
                fd1,      # House ownership
                fd102,	  # Built the house or bought it
                fd103,	  # When was the house built
                fd2,	    # Building area of the house
                fd4,	    # The current value of your house last month (10,000 yuan)
                fd5,	    # How much rent could you get had you rented the house last month (yuan)
                fd6,	    # Housing type
                fd8_s_1,	# Difficulty with housing: Problem 1
                fd8_s_2,	# Difficulty with housing: Problem 2
                fd8_s_3)	# Difficulty with housing: Problem 3
## children
df.children <- dfc0 %>%
  dplyr::select(### id	
                pid,	             # personal id
                fid,	             # family id
                cid,	             # community id
                pid_f,	           # father id
                pid_m,	           # mother id
                wa1age,            # age
                gender,            # gender
                ### mental health	
                wn401,	           # Feel depressed and cannot cheer up
                wn402,	           # Feel nervous
                wn403,	           # Feel agitated or upset and cannot remain calm 
                wn404,	           # Feel hopeless about the future 
                wn405,	           # Feel that everything is difficult 
                wn406,	           # Think life is meaningless 
                cfps2010edu_best,  # highest level of education: with 8 levels
                cfps2010eduy_best, # educational years
                cfps2010sch_best,  # with 8 levels (for those who not finish schooling)
                feduc,             # education level of father (same coding as educ)
                meduc,             # education level of mother (same coding as educ)
                ### cognitive ability	
                wordtest,	         # score of word test
                mathtest,	         # score of math test
                ### migrated,	
                wa5,               # Child's place of house registration same as birthplace
                ### children living condition	
                wb1,	             # Child's place to live in the latest non-vacation month
                wb2,	             # Child's major caregiver in the latest non-vacation month
                wb201,	           # Child's frequency meeting parent(s) in the latest non-vacation month
                tb6_a_f,	         # Father living in the household (living with family)
                tb6_a_m,	         # Mother living in the household (living with family)
                wz301) # Interviewer Observation: Home environment indicates parents care about child's education	
# save data as RData for futher analysis
save(df.children, df.individual, df.community, df.family,
     file = 'CFPS2010.RData')