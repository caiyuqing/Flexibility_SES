library(tidyverse)

###########################function#############################
# import packages

if (!require("pacman")) install.packages("pacman")             # install the package manager pacman, which is great!
pacman::p_load('tidyverse')

# library(dplyr) # part of tidyverse
# library(tidyr) # part of tidyverse
# library(stringr) # part of tidyverse

# read CPI and PPP data
CPI <- read.csv("CPI.csv",header = TRUE)
PPP <- read.csv("PPP.csv",header = TRUE) %>% dplyr::rename(`Country.Code`=1) # rename the first column, avoid encoding issues

# check if country code are same (indeed the same)
country_code_CPI = CPI$Country.Code
country_code_PPP = PPP$Country.Code

for (i in 1:65) {
  print(country_code_PPP[i] %in% country_code_CPI)
}

# # rename PPP
# names(PPP)[names(PPP) == 'LOCATION'] <- 'Country.Code'

# restructure CPI
CPI <-CPI[,c(2,5:66)]
CPI <- gather(CPI, TIME, Value, X1960:X2021, factor_key=TRUE) %>%
  dplyr::mutate(across('TIME', str_replace, 'X', '')) %>%
  dplyr::mutate(TIME = as.numeric(TIME))

############# conversion functions #############
## CFPS ##
convert_index_cfps_function <- function(paper_year, paper_country){
  # PPP first calculation:
  PPP_CHN_paperyear = PPP$Value[PPP$Country.Code == 'CHN' & PPP$TIME == paper_year]
  PPP_papercountry_paperyear = PPP$Value[PPP$Country.Code == paper_country & PPP$TIME == paper_year]
  CPI_CHN_2010 = CPI$Value[CPI$Country.Code == 'CHN' & CPI$TIME == 2010]
  CPI_CHN_paperyear = CPI$Value[CPI$Country.Code == 'CHN' & CPI$TIME == paper_year]
  index_ppp_first_cfps <<- (PPP_CHN_paperyear / PPP_papercountry_paperyear) * (CPI_CHN_2010 / CPI_CHN_paperyear)
  # print(index_ppp_first_cfps)

  # CPI first calculation:
  CPI_papercountry_2010 = CPI$Value[CPI$Country.Code == paper_country & CPI$TIME == 2010]
  CPI_papercountry_paperyear = CPI$Value[CPI$Country.Code == paper_country & CPI$TIME == paper_year]
  PPP_CHN_2010 = PPP$Value[PPP$Country.Code == 'CHN' & PPP$TIME == 2010]
  PPP_papercountry_2010 = PPP$Value[PPP$Country.Code == paper_country & PPP$TIME == 2010]
  index_cpi_first_cfps <<- (CPI_papercountry_2010 /CPI_papercountry_paperyear)* (PPP_CHN_2010 /PPP_papercountry_2010)
  # print(index_cpi_first_cfps)

  return(c(index_ppp_first_cfps, index_cpi_first_cfps)) # return the indices instead of printing
}

## PSID ##
convert_index_psid_function <- function(paper_year, paper_country){
  # PPP first calculation:
  PPP_USA_paperyear = PPP$Value[PPP$Country.Code == 'USA' & PPP$TIME == paper_year]
  PPP_papercountry_paperyear = PPP$Value[PPP$Country.Code == paper_country & PPP$TIME == paper_year]
  CPI_USA_2017 = CPI$Value[CPI$Country.Code == 'USA' & CPI$TIME == 2017]
  CPI_USA_paperyear = CPI$Value[CPI$Country.Code == 'USA' & CPI$TIME == paper_year]
  index_ppp_first_psid <<- (PPP_USA_paperyear / PPP_papercountry_paperyear) * (CPI_USA_2017 / CPI_USA_paperyear)
  # print(index_ppp_first_psid)

  # CPI first calculation:
  CPI_papercountry_2017 = CPI$Value[CPI$Country.Code == paper_country & CPI$TIME == 2017]
  CPI_papercountry_paperyear = CPI$Value[CPI$Country.Code == paper_country & CPI$TIME == paper_year]
  PPP_USA_2017 = PPP$Value[PPP$Country.Code == 'USA' & PPP$TIME == 2017]
  PPP_papercountry_2017 = PPP$Value[PPP$Country.Code == paper_country & PPP$TIME == 2017]
  index_cpi_first_psid <<- (CPI_papercountry_2017 /CPI_papercountry_paperyear)* (PPP_USA_2017 /PPP_papercountry_2017)
  # print(index_cpi_first_psid)

  return(c(index_ppp_first_psid, index_cpi_first_psid)) # return the indices instead of printing
}
# get two index, names: index_ppp_first_cfps, index_cpi_first_cfps
convert_index_cfps_function(paper_year = 2021,paper_country = 'JPN')
# get two index, names: index_ppp_first_psid, index_cpi_first_psid
convert_index_psid_function(paper_year = 2021,paper_country = 'CAN')




########################trial 2 ##########################

########################Avinun, 2019 (CFPS & PSID) ##########################
## Subjects: Young adults (parent's SES)
## SES: Family SSS: ladder scale (use other SSS variables in CFPS and PSID as alternatives)
##                  CFPS: composite mother and father's subjective social status as family SSS
##                  PSID: composite reference person and spouse subjective subjective social status
## CFPS ##
Avinun_CFPS <- df.CFPS_child %>%
  dplyr::mutate(sss_family = (sss_f + sss_m)) %>%
  dplyr::rename(SES = sss_family)

summary(Avinun_CFPS$SES)

## PSID ##
Avinun_PSID <- df.PSID_child %>%
  dplyr::mutate(sss_rp = na_if(sss_rp, 9),
                sss_sp = na_if(sss_sp, 9)) %>% # set NA for sss (sss = 9 means NA, don't know, refuse to answer)
  dplyr::mutate(sss_family = (sss_rp + sss_sp)) %>%
  dplyr::rename(SES = sss_family)

summary(Avinun_PSID$SES)




###############Banerjee  T.，et al.，2020################
# SES: dichotomous low SES: income below 125% of the federal poverty line
## CFPS ##
Banerjee_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(income_recode = ifelse(income < 1.25*1274, 0, 1)) %>%
  dplyr::mutate(SES = as.numeric(as.character(income_recode)))
table(Banerjee_CFPS$SES)
## PSID ##
Banerjee_PSID <- df.PSID_adult %>%
  dplyr::mutate(income_recode = ifelse(income < 1.25*12060, 0, 1)) %>%
  dplyr::mutate(SES = as.numeric(as.character(income_recode)))
table(Banerjee_PSID$SES)



##############Beatty Moody-2019####################
# Subjects: middle aged
# SES: dichotomous low SES:  below median education and/or income below 125% of the federal poverty line
## CFPS ##
Beatty_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(edu_recode = ifelse(cfps2010eduy_best < 12, 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(income_recode = ifelse(income < 1.25*1274, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES = ifelse(SES_low == 0, 0, 1))
table(Beatty_CFPS$SES)
## PSID ##
Beatty_PSID <- df.PSID_adult %>%
  dplyr::mutate(edu_recode = ifelse(edu < 12, 0, 1)) %>% # Low SES (education) = 0, high SES (education) = 1
  dplyr::mutate(income_recode = ifelse(income < 1.25*12060, 0, 1)) %>%
  dplyr::mutate(income_recode = as.numeric(as.character(income_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES_low = income_recode + edu_recode) %>%
  dplyr::mutate(SES = ifelse(SES_low == 0, 0, 1))
table(Beatty_PSID$SES)






####### Betancourt, L, 2016 (CFPS & PSID)###########
#  Subject: neonates
#  SES = (ITN + mother's edu)/2
#  ITN: income-to-need ratio;5 levels of family income according to poverty line
#       4 cut-point: itn1 = poverty line, itn4 = 400% above poverty line, rest two set between itn1 and itn4)
#      * in China, use the rural poverty line as the poverty line for all population
#      * in America, poverty line depends on family size: poverty line in 2017 = 12060 +  (familysize-1)*4180
#  Mother's education: in categories, convert education year into 7 categories. see 'Education & Occupation recode.xlsx'
#
# Note: in CFPS and PSID, cannot distinguish "Technical/Vocational (3);some college (4)" levels. Only 6 levels
## CFPS ##
betan_CFPS <- df.CFPS_child %>%
  dplyr::mutate(itn = base::cut(faminc/familysize,
                                breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf),
                                labels = c("1", "2", "3", "4", "5"))) %>%
  # set 7 levels for mother's education
  dplyr::mutate(edu_m_recode = base::cut(eduy_m,
                                         breaks = c(-0.01, 9.5, 12.5, 13.5, 14.5, 16.5, 22.5),
                                         labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(itn = as.numeric(as.character(itn)),   # convert factors into numeric variable
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_betan_cfps = (itn + edu_m_recode)/2)    # calculate composite SES score

# check SES score
table(betan_CFPS$SES_betan_cfps)

## PSID ##
betan_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_m_recode = cut(edu_m,
                                   breaks = c(-0.01, 9.5, 12.5, 13.5, 14.5, 16.5, 17.5, 100),
                                   labels = c("1", "2", "3", "4", "5", "6", NA)),  # set 7 levels for mother's education
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  # set the poverty line for every family: poverty line 12060 for one people and increase 4180 for an extra person
  dplyr::mutate(itn1 = 12060 +  (familysize-1)*4180) %>%
  # get the interval of ITN
  dplyr::mutate(itn = ifelse(fincome < itn1, 1,
                             ifelse(itn1 <= fincome & fincome < itn1*2, 2,
                                    ifelse(itn1*2 <= fincome & fincome < itn1*3, 3,
                                           ifelse(itn1*3 <= fincome & fincome < itn1*4, 4,
                                                  ifelse(itn1*4 <= fincome, 5, NA)))))) %>%  # No NA
  dplyr::mutate(SES_betan_psid = (itn + edu_m_recode)/2)   # calculate composite SES

# check SES score
table(betan_PSID$SES_betan_psid)



###############beydoun, 2020################
# SES: dichotomous low SES: income below 125% of the federal poverty line
## CFPS ##
beydoun_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(income_recode = ifelse(income < 1.25*1274, 0, 1)) %>%
  dplyr::mutate(SES = as.numeric(as.character(income_recode)))
table(beydoun_CFPS$SES)
## PSID ##
beydoun_PSID <- df.PSID_adult %>%
  dplyr::mutate(income_recode = ifelse(income < 1.25*12060, 0, 1)) %>%
  dplyr::mutate(SES = as.numeric(as.character(income_recode)))
table(beydoun_PSID$SES)



# Cascio,2017
# Level of education in 7 levels: what level of education their father and mother had completed based on 7-point scale,
# where 1 = less than high school, 2 = high school, 3 = trade school, 4 = associates degree, 5 = bachelor degree, 6 = graduate degree, and 7 = unknown. Unknown levels of education (response = 7) were dropped from the analysis.
# Then a combined continuous parents’ education variable was created using the average score between the father and mother.

## CFPS ##
Cascio_CFPS <- df.CFPS_child %>%
  dplyr::mutate(edu_f_recode = cut(eduy_f,
                                   breaks = c(-0.01, 9.5, 12.5, 13.5, 14.5, 16.5, 22.5),
                                   labels = c("1", "2", "3", "4", "5", "6")),
                edu_m_recode = cut(eduy_m,
                                   breaks = c(-0.01, 9.5, 12.5, 13.5, 14.5, 16.5, 22.5),
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>%
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES = (edu_f_recode + edu_m_recode)/2)
table(Cascio_CFPS$SES)

## PSID ##
Cascio_PSID <- df.PSID_child %>%
  dplyr::mutate(edu_f_recode = cut(edu_f,
                                   breaks = c(-0.01, 8.5,12.5, 13.5, 14.5,16.5, 17.5),
                                   labels = c("1", "2", "3", "4", "5", "6")),
                edu_m_recode = cut(edu_m,
                                   breaks = c(-0.01, 8.5,12.5, 13.5, 14.5,16.5, 17.5),
                                   labels = c("1", "2", "3", "4", "5", "6"))) %>% #similar to the previous ones, no distinguish between master and doctorate (only 6 levels, last level set to 19.5)
  dplyr::mutate(edu_f_recode = as.numeric(as.character(edu_f_recode)),
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES = (edu_f_recode + edu_m_recode)/2)
table(Cascio_PSID$SES)


########################trial 3 ##########################


#################### Conant, 2017 ######################
# SES = maternal education: dichotomous, divided by years


## CFPS ##
Conant_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES = cut(meduc, breaks = c(3.5, 4.5, 5.5, 6.6, 7.7, 8.5), labels = c("12","13.5",  "16", "18", "20"))) %>%  # recode education
  dplyr::mutate(SES = as.numeric(as.character(SES)))   # convert it to numeric one

# check SES score
table(Conant_CFPS$SES)

## PSID ##
Conant_PSID <- df.PSID_child %>%
  dplyr::mutate(SES = ifelse(edu_m <= 11, NA, edu_m)) %>%  # recode education
  dplyr::mutate(SES = ifelse(SES >= 17, NA, SES))

# check SES score
table(Conant_PSID$SES)

#################### Deater-Deckard-2019 ######################
# SES = Annual household ITN
#计算方式是使用哪一个好一些？
#cut(faminc/familysize,
#    breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf),
#    labels = c("1", "2", "3", "4", "5"))

## CFPS ##
ratio=6.8
Deater_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(SES = cut(faminc, breaks = c(-1/ratio, 0.0001/ratio, 1000/ratio, 3000/ratio, 5000/ratio, 7500/ratio, 10000/ratio ,15000/ratio, 20000/ratio ,25000/ratio, 35000/ratio, 50000/ratio ,75000/ratio ,100000/ratio, 200000/ratio, 100000000000), labels = c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))) %>%  # recode education
  dplyr::mutate(SES = as.numeric(as.character(SES)))   # convert it to numeric one



# check SES score
table(Deater_CFPS$SES)

ratio=0.98
## PSID ##
Deater_PSID <- df.PSID_adult %>%
  dplyr::mutate(SES = cut(fincome, breaks = c(-1/ratio, 0.0001/ratio, 1000/ratio, 3000/ratio, 5000/ratio, 7500/ratio, 10000/ratio ,15000/ratio, 20000/ratio ,25000/ratio, 35000/ratio, 50000/ratio ,75000/ratio ,100000/ratio, 200000/ratio, 100000000000), labels = c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))) %>%  # recode education
  dplyr::mutate(SES = as.numeric(as.character(SES)))   # convert it to numeric one

# check SES score
table(Deater_PSID$SES)


####################  Degeilh,2020####################


Degeilh_CFPS<-df.CFPS_child%>%
  select(faminc,eduy_m,eduy_f,familysize)%>%
  dplyr::mutate(itn = cut(faminc/familysize,
                                breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4,1274*5, Inf),
                                labels = c("1", "2", "3", "4", "5",'6')))%>%
  dplyr::mutate(parentaledu=mean(eduy_m+eduy_f,na.rm=T))%>%
  dplyr::mutate(parentaledu = as.numeric(as.character(parentaledu))) %>%
  dplyr::mutate(itn = as.numeric(as.character(itn))) %>%
  dplyr::mutate(SES=parentaledu-itn)

table(Degeilh_CFPS$SES)

Degeilh_PSID<-df.PSID_child%>%
  select(familysize,fincome,edu_m,edu_f)%>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  # calculate poverty line for every family
  dplyr::mutate(FPL = fincome/poverty)%>%
  dplyr::mutate(parentaledu=mean(edu_m+edu_f,na.rm=T))%>%
  dplyr::mutate(parentaledu = as.numeric(as.character(parentaledu))) %>%
  dplyr::mutate(SES=parentaledu-FPL)
table(Degeilh_PSID$SES)



####################  Dejoseph, 2021 ####################

Dejoseph_CFPS<-df.CFPS_child%>%
  select(faminc,eduy_m,eduy_f,familysize)%>%
  dplyr::mutate(itn = base::cut(faminc/familysize,
                                breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4,1274*5, Inf),
                                labels = c("1", "2", "3", "4", "5",'6')))%>%
  dplyr::mutate(SES = itn)

table(Dejoseph_CFPS$SES)


Dejoseph_PSID<-df.PSID_child%>%
  select(familysize,fincome,edu_m,edu_f)%>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%  # calculate poverty line for every family
  dplyr::mutate(FPL = fincome/poverty) %>%
  dplyr::mutate(SES = FPL)

table(Dejoseph_PSID$SES)


#############Dougherty, 2020################
# Subjects: seniors
# SES: total years of formal education and household income
#      education: education: 1 = ≤ 12; 2 = 13–16, 3 = ≥ 17
#      income: income: 1 = ≤ 49,999; 2 = 50,000–99,999; 3 = ≥ 100,001
## CFPS ##
ratio=6.8
Dougherty_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(fincome_recode = cut(faminc,
                                     breaks = c(-0.01, 49999.9/ratio,99999.9/ratio , 100001/ratio, 10000000000000000),
                                     labels = c(NA,"1", "2", "3"))) %>%
  dplyr::mutate(edu_recode = cut(cfps2010eduy_best,
                                 breaks = c(-0.01,12.5, 16.5, 22.5),
                                 labels = c("1", "2", "3"))) %>%
  dplyr::mutate(income_cfps = as.numeric(as.character(fincome_recode)),
                edu_cfps = as.numeric(as.character(edu_recode)))
table(Dougherty_CFPS$income_cfps)
table(Dougherty_CFPS$edu_cfps)


## PSID ##
Dougherty_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome_recode = cut(fincome,
                                     breaks = c(-0.01, 49999.9/ratio,99999.9/ratio , 100001/ratio, 10000000000000000),
                                     labels = c(NA,"1", "2", "3"))) %>% # CPI US dollar 2017/2018 = 245.120/251.107 = 0.976
  dplyr::mutate(edu_recode = cut(edu,
                                 breaks = c(-0.01,12.5, 16.5, 22.5),
                                 labels = c("1", "2", "3"))) %>%
  dplyr::mutate(income_psid = as.numeric(as.character(fincome_recode)),
                edu_psid = as.numeric(as.character(edu_recode)))
table(Dougherty_PSID$income_psid)
table(Dougherty_PSID$edu_psid)


#############Dougherty, 2020################
# Subjects: seniors
# SES: total years of formal education and household income
#      education: education: 1 = ≤ 12; 2 = 13–16, 3 = ≥ 17
#      income: income: 1 = ≤ 49,999; 2 = 50,000–99,999; 3 = ≥ 100,001
## CFPS ##
ratio=6.8
Dougherty_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(fincome_recode = cut(faminc,
                                     breaks = c(-0.01, 49999.9/ratio,99999.9/ratio , 100001/ratio, 10000000000000000),
                                     labels = c(NA,"1", "2", "3"))) %>%
  dplyr::mutate(edu_recode = cut(cfps2010eduy_best,
                                 breaks = c(-0.01,12.5, 16.5, 22.5),
                                 labels = c("1", "2", "3"))) %>%
  dplyr::mutate(income_cfps = as.numeric(as.character(fincome_recode)),
                edu_cfps = as.numeric(as.character(edu_recode)))
table(Dougherty_CFPS$income_cfps)
table(Dougherty_CFPS$edu_cfps)


## PSID ##
Dougherty_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome_recode = cut(fincome,
                                     breaks = c(-0.01, 49999.9/ratio,99999.9/ratio , 100001/ratio, 10000000000000000),
                                     labels = c(NA,"1", "2", "3"))) %>% # CPI US dollar 2017/2018 = 245.120/251.107 = 0.976
  dplyr::mutate(edu_recode = cut(edu,
                                 breaks = c(-0.01,12.5, 16.5, 22.5),
                                 labels = c("1", "2", "3"))) %>%
  dplyr::mutate(income_psid = as.numeric(as.character(fincome_recode)),
                edu_psid = as.numeric(as.character(edu_recode)))
table(Dougherty_PSID$income_psid)
table(Dougherty_PSID$edu_psid)


####################Dufford,2017 (CFPS & PSID)########################
# SES: INR=the total family income/the poverty threshold adjusted for the number of individuals living in the household as specified by the United States Census Bureau

# Note: for PSID, family poverty line varies according to family size

## CFPS ##
Dufford_CFPS <- df.CFPS_child %>%
  dplyr::mutate(INR = (faminc/familysize)/1274)  %>%    # calculate INR
  dplyr::mutate(INR = log10(INR)) %>%                   # calculate INR
  dplyr::mutate(INR = ifelse(!is.finite(INR), NA, INR)) %>% # set infinite value as NA
  dplyr::select(INR,pid)

# check SES score
summary(Dufford_CFPS$INR)

## PSID ##
Dufford_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%     # calculate poverty line for every family
  dplyr::mutate(INR = fincome/poverty) %>%    # log transformation
  dplyr::mutate(INR = log10(INR)) %>%    # log transformation
  dplyr::mutate(INR = ifelse(!is.finite(INR), NA, INR))  # calculate poverty line for every family
#check SES score
summary(Dufford_PSID$INR)


############################## Ellwood-Lowe, 2020 ##############################



n1 = 4 # family size
n2 = 5
k1 = 25000 # poverty line according to the family size
k2 = 35000
Ellwood_CFPS <-df.CFPS_child%>%
  mutate(poverty=case_when(familysize==n1 & faminc <= k1~1,
                       familysize==n2 & faminc <= k2~1,
                       T~0))%>%
  select(poverty)

summary(Ellwood_CFPS$poverty)

Ellwood_PSID <-df.PSID_child%>%
  mutate(poverty=case_when(familysize==n1 & fincome <= k1~1,
                       familysize==n2 & fincome <= k2~1,
                       T~0))%>%
  select(poverty)

summary(Ellwood_PSID$poverty)


####################Trial 5#######################
##############################################

####################Gianaros, 2011 (CFPS & PSID)########################

# Subjects: adults
# SES: the highest level of education attained
## CFPS ##
Gianaros_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(SES_cfps = cut(cfps2010edu_best,
                                     breaks = c(-0.01,  4.5, 8.5),
                                     labels = c("0",  "1"))) %>%
  dplyr::mutate(SES_cfps = as.numeric(as.character(SES_cfps)))
summary(Gianaros_CFPS$SES_cfps)

## PSID ##
Gianaros_PSID <- df.PSID_adult %>%
  dplyr::mutate(SES_psid = cut(edu,
                                     breaks = c(-0.01, 12.5,17.5),
                                     labels = c("0", "1"))) %>%
  dplyr::mutate(SES_psid = as.numeric(as.character(SES_psid)))
summary(Gianaros_PSID$SES_psid)



#################Giulio Pergola,2016 (only CFPS) ##################
# Subject: adult
# SES: Socioeconomic Status Index (Hollingshead, 1975) 是否要纳入？
Giulio_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(occup_recode = recode(qg307egp, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                      "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation
  dplyr::mutate(edu_recode = cut(cfps2010eduy_best,
                                 breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5),
                                 labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education
  dplyr::mutate(occup_recode = as.numeric(as.character(occup_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(SES_cfps = 7*occup_recode +4*edu_recode)
summary(Giulio_CFPS$SES_cfps)

####################Gullick, 2016 (CFPS & PSID)########################

# Subjects: children
# SES: the highest level of education attained
#The average education level of both parents was used as the measure of socioeconomic status;
#SES was used as a continuous variable in all initial analyses; follow-up analyses divided children into lower and higher SES subgroups based on both parents’ education.
#For these analyses, lower SES was defined as 10–14 years of education for both parents (N = 21, mean = 12.5), and higher SES as 16–18 years (N = 21, mean = 16.5)

## CFPS ##
Gullick_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(SES_cfps = cut(cfps2010eduy_best,
                               breaks = c(-0.01,  14.5, 22.5),
                               labels = c("0",  "1"))) %>%
  dplyr::mutate(SES_cfps = as.numeric(as.character(SES_cfps)))
summary(Gullick_CFPS$SES_cfps)

## PSID ##
Gullicks_PSID <- df.PSID_adult %>%
  dplyr::mutate(SES_psid = cut(edu,
                               breaks = c(-0.01, 14.5,17.5),
                               labels = c("0", "1"))) %>%
  dplyr::mutate(SES_psid = as.numeric(as.character(SES_psid)))
summary(Gullicks_PSID$SES_psid)


####################Trial 6#######################
##############################################


####################Hanson, 2011########################

ratio=6.8
## CFPS ##
Hanson_CFPS<- df.CFPS_child %>%
  mutate(income_level = cut(faminc, breaks = c(0, 5000/ratio, 10000/ratio, 15000/ratio , 25000/ratio, 35000/ratio, 50000/ratio, 75000/ratio, 100000/ratio, Inf),
                            labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9))) %>%
  mutate(edu_m = cut(eduy_m,
                            breaks = c(0,9.5, 13.5, 14.5, 15.5,16.5, 22.5),
                            labels = c(1, 2, 3, 4, 5, 6))) %>%
  mutate(edu_f = cut(eduy_f,
                            breaks = c(0,9.5, 13.5, 14.5, 15.5,16.5, 22.5),
                            labels = c(1, 2, 3, 4, 5, 6))) %>%
  select(income_level,edu_m,edu_f)
summary(Hanson_CFPS)

## PSID ##
ratio=0.95
Hanson_PSID <- df.PSID_child %>%
  mutate(edu_m_grade = case_when(
    edu_m <= 9 ~ 1,
    edu_m <= 13 ~ 2,
    edu_m <= 14 ~ 3,
    edu_m <= 15 ~ 4,
    edu_m <= 16 ~ 5,
    edu_m >= 17 ~ 6
  ),
  edu_f_grade = case_when(
    edu_m <= 9 ~ 1,
    edu_m <= 13 ~ 2,
    edu_m <= 14 ~ 3,
    edu_m <= 15 ~ 4,
    edu_m <= 16 ~ 5,
    edu_m >= 17 ~ 6
  )) %>%
  mutate(income_level = cut(fincome, breaks = c(0, 5000*ratio, 10000*ratio, 15000*ratio , 25000*ratio, 35000*ratio, 50000*ratio, 75000*ratio, 100000*ratio, Inf),
                            labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9))) %>%
  select(income_level,edu_m_grade,edu_f_grade)

summary(Hanson_PSID)

####################Hanson, 2019########################
###########Household income measured on 1-10 scale (1=up to $5,000; 2=between $5,000 and $10,000;
########3=between $11,000 and $15,000; 4=between $16,000 and $29,000; 5=between $30,000 and $40,000; 6=between $41,000 and $50,000;
####7=between $51,000 and $60,000;8=between $61,000 and $70,000; 9=between $71,000 and $80,000; 10=beyond $81,000)


## CFPS ##
ratio=6.8
Hanson2019_CFPS <- df.CFPS_child %>%
  mutate(SES = case_when(
    faminc <= 5000/ratio ~ 1,
    faminc <= 10000/ratio ~ 2,
    faminc <= 15000/ratio ~ 3,
    faminc <= 29000/ratio ~ 4,
    faminc <= 40000/ratio ~ 5,
    faminc <= 50000/ratio ~ 6,
    faminc <= 60000/ratio ~ 7,
    faminc <= 70000/ratio ~ 8,
    faminc <= 80000/ratio ~ 9,
    faminc > 81000/ratio ~ 10
  )) %>%
  select(SES)
summary(Hanson2019_CFPS)

## PSID ##
ratio=0.95
Hanson2019_PSID <- df.PSID_child %>%
  mutate(SES = case_when(
    fincome <= 5000*ratio ~ 1,
    fincome <= 10000*ratio ~ 2,
    fincome <= 15000*ratio ~ 3,
    fincome <= 29000*ratio ~ 4,
    fincome <= 40000*ratio ~ 5,
    fincome <= 50000*ratio ~ 6,
    fincome <= 60000*ratio ~ 7,
    fincome <= 70000*ratio ~ 8,
    fincome <= 80000*ratio ~ 9,
    fincome > 81000*ratio ~ 10
  )) %>%
  select(SES)
table(Hanson2019_PSID$SES)


####################Hanson-2012########################
#######"Maternal education was used as an index of socioeconomic
#######status in this study because this measure is strongly associated with child
#######health, household income, and stimulation in the environment; Maternal education varied on a numeric scale from 1 to 8, denoting level of education obtained with possible choices of grade school, high school or general education diploma, 2-year college, trade, or technical school, 4-year college, or
#######graduate school. SES, Socioeconomic status."


## CFPS ##

Hanson2012_CFPS <- df.CFPS_child %>%
  mutate(SES = case_when(
    eduy_m < 9 ~ 1,
    eduy_m < 13 ~ 2,
    eduy_m == 13 ~ 3,
    eduy_m == 14 ~ 4,
    eduy_m == 15 ~ 5,
    eduy_m == 16 ~ 6,
    eduy_m == 17 ~ 7,
    eduy_m >= 18 ~ 8
  )) %>%
  select(SES)
table(Hanson2012_CFPS$SES)

## PSID ##

Hanson2012_PSID <- df.PSID_child %>%
  mutate(SES = case_when(
    edu_m <= 8 ~ 1,
    edu_m <= 12 ~ 2,
    edu_m <= 13 ~ 3,
    edu_m <= 14 ~ 4,
    edu_m <= 16 ~ 5,
    edu_m == 17 ~ 6,
    edu_m == 18 ~ 7,
    edu_m >= 19 ~ 8
  )) %>%
  select(SES)
table(Hanson2012_PSID$SES)

####################Hilary K. Lambert, 2017########################

#income-to-needs ratio was calculated by dividing total house_x0002_hold income by the 2015 U.S. Census-defined poverty line for a family of that size.

## CFPS ##
Hilary_CFPS <- df.CFPS_child %>%
  mutate(poverty_line = familysize*1274) %>%   # calculate itn
  mutate(SES = ifelse(faminc < poverty_line, 0, 1))

table(Hilary_CFPS$SES)


## PSID ##
luby_PSID <- df.PSID_child %>%
  mutate(poverty = 11770 + (4160 * (familysize - 1))) %>%  # 2015 POVERTY GUIDELINES FOR THE 48 CONTIGUOUS STATES AND THE DISTRICT OF COLUMBIA
  mutate(inc = fincome/poverty) %>%
  mutate(SES = ifelse(inc < 1, 0, 1))

table(luby_PSID$SES)


####################Trial 7#######################
##############################################

############## Hutton-2021 (CFPS & PSID)###############
#SES was defined as a binary variable in terms of 2020 US poverty criteria
#by using the midpoint of income category relative to household size.
#Annual household income, $
#  ≤ 25 000 ; 25 001–50 000 ;50 001–100 000 ;
#100 001–150 000; ＞150 000

ratio=convert_index_cfps_function(paper_year = 2021,paper_country = 'USA')[1]

## CFPS ##
Hutton_CFPS <- df.CFPS_child %>%
  dplyr::mutate(faminc_1 = cut(faminc ,
                    breaks = c(-1, 25000 , 75000 ,150000 ,250000),
                    labels = c(12500, 37500, 75000, 125000))) %>%
  dplyr::mutate(faminc_1 = as.numeric(as.character(faminc_1))) %>%
  dplyr::mutate(INR = (faminc_1/familysize)/1274/ratio)  %>%    # calculate INR
  dplyr::mutate(SES = ifelse(INR>1, 1, 0)) %>% # set infinite value as NA
  dplyr::select(SES)

# check SES score
summary(Hutton_CFPS$SES)

## PSID ##

ratio=convert_index_psid_function(paper_year = 2021,paper_country = 'USA')[1]

Hutton_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(faminc_1 = cut(fincome ,
                               breaks = c(-1, 25000 , 75000 ,150000 ,250000),
                               labels = c(12500, 37500, 75000, 125000))) %>%
  dplyr::mutate(faminc_1 = as.numeric(as.character(faminc_1))) %>%
  dplyr::mutate(INR = faminc_1/poverty) %>%
  dplyr::mutate(SES = ifelse(INR>1, 1, 0))  # calculate poverty line for every family
#check SES score
summary(Hutton_PSID$SES)




############## Jednorg, 2012 (CFPS? )###############


Jednorg_CFPS <- df.CFPS_child %>%
  dplyr::mutate(occup_recode = recode(egp_m, "1"= 1, "2"=1, "3"=2,   "4"=3, "5"= 3, "7"= 4,
                                      "8"= 5, "9"=6, "10"=7, "11"=7, "80000"=8, .default = -8)) %>%    # recode occupation
  dplyr::mutate(edu_recode = cut(eduy_m,
                                 breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5),
                                 labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education
  dplyr::mutate(occup_recode = as.numeric(as.character(occup_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(SES = 7*occup_recode +4*edu_recode)
summary(Jednorg_CFPS$SES)



############## Johnson, 2021 (CFPS & PSID)###############


ratio=convert_index_cfps_function(paper_year = 2021,paper_country = 'USA')[1] #年份不确定

## CFPS ##
Johnson_CFPS <- df.CFPS_child %>%
  dplyr::mutate(INR = (faminc/familysize)/1274/ratio)  %>%    # calculate INR
  dplyr::mutate(SES = INR) %>%
  dplyr::select(SES)

# check SES score
summary(Johnson_CFPS$SES)

## PSID ##

ratio=convert_index_psid_function(paper_year = 2021,paper_country = 'USA')[1]#年份不确定

Johnson_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(INR = faminc/poverty) %>%
  dplyr::mutate(SES = INR)  # calculate poverty line for every family
#check SES score
summary(Johnson_PSID$SES)




######################################################
#################### FINAL 30 ########################
######################################################


#################### Stiver-2015 #######################

# Maximum parental education was categorized from 0 through 4 for grade
#school (0), high school (1), postsecondary training (2), university
#(3), and postgraduate training (4). Maternal and paternal education
#were also analyzed separately using these 5 categories and again with
#education further categorized as no postsecondary (a) and postsecondary or above (b).
## CFPS ##
Stiver_CFPS <- df.CFPS_child %>%
  dplyr::mutate(feduc_recode = cut(edu_f,
                                   breaks = c(-0.5,4.5,8.5),
                                   labels = c("0","1"))) %>%
  dplyr::mutate(meduc_recode = cut(edu_m,
                                   breaks = c(-0.5,4.5, 8.5),
                                   labels = c("0","1"))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  # recode the education level as SES (low = 0, high = 1, other = NA)
  dplyr::mutate(SES = ifelse(meduc_recode==0 & feduc_recode ==0,0,
                                           ifelse(meduc_recode ==1|feduc_recode ==1,1, NA)))
table(Stiver_CFPS$SES)
## PSID ##
Stiver_PSID <- df.PSID_child %>%
  dplyr::mutate(feduc_recode = cut(eduy_f,
                                   breaks = c(-0.5,12.5,17.5),
                                   labels = c("0","1"))) %>%
  dplyr::mutate(meduc_recode = cut(eduy_m,
                                   breaks = c(-0.5,12.5, 17.5),
                                   labels = c("0","1"))) %>%
  dplyr::mutate(meduc_recode = as.numeric(as.character(meduc_recode))) %>%
  dplyr::mutate(feduc_recode = as.numeric(as.character(feduc_recode))) %>%
  # recode the education level as SES (low = 0, high = 1, other = NA)
  dplyr::mutate(SES = ifelse(meduc_recode==0 & feduc_recode ==0,0,
                                           ifelse(meduc_recode ==1|feduc_recode ==1,1, NA)))
table(Stiver_PSID$SES)






#################### Swartz-2018 #######################
#parental subjective ses
#If you did not know your biological father, please think of the male that was most like a father
#figure to you (0 = lowest, 10 = highest).
#A similar question asked this information for mothers."

## CFPS ##
Swartz_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(sss_f = (sss_f * 2)) %>%
  dplyr::mutate(sss_m = (sss_m * 2))
summary(Avinun_CFPS$sss_f)

## PSID ##
Swartz_PSID <- df.PSID_adult %>%
  dplyr::mutate(sss_f = na_if(sss_f, 9),
                sss_m = na_if(sss_m, 9)) %>% # set NA for sss (sss = 9 means NA, don't know, refuse to answer)
  dplyr::mutate(sss_f = (sss_f*2)) %>%
  dplyr::mutate(sss_m = (sss_m*2))

summary(Swartz_PSID$sss_f)


##############Takeuchi-2014 ##############

#The measure of socioeconomic status consisted of
#three questions. One was an enquiry relating to family
#annual income as reported in our previous study...The other two questions related to the highest
#educational qualification of both parents.

ratio=convert_index_cfps_function(paper_year = 2014,paper_country = 'USA')[1] #converted from japan

## CFPS ##
Takeuchi_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(faminc = base::cut(faminc,
                                breaks = c(-0.00001, 16000.001, 32000.001, 48000.001, 64000.001,80000.001,96000, Inf)*ratio,
                                labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  # set (1, 6 years; 2, 9 years; 3,  11 years; 4, 12 years; 5, 14 years; 6, 16 years; 7, 18 years;and 8, 21 years)
  dplyr::mutate(edu_m_recode = base::cut(eduy_m,
                                         breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5, 14.5, 16.5, 18.5, 21.5),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>%
  dplyr::mutate(edu_f_recode = base::cut(eduy_f,
                                         breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5, 14.5, 16.5, 18.5, 21.5),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8")))

# check SES score
summary(Takeuchi_CFPS$faminc)

## PSID ##



ratio=convert_index_psid_function(paper_year = 2021,paper_country = 'USA')[1]

Takeuchi_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome = base::cut(fincome,
                                   breaks = c(-0.00001, 16000.001, 32000.001, 48000.001, 64000.001,80000.001,96000, Inf)*ratio,
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(edu_m_recode = base::cut(eduy_m,
                                         breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5, 14.5, 16.5, 18.5, 21.5),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>%
  dplyr::mutate(edu_f_recode = base::cut(eduy_f,
                                         breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5, 14.5, 16.5, 18.5, 21.5),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8")))
#check SES score
summary(Takeuchi_PSID$fincome)


##############Takeuchi-2018 ##############
ratio=convert_index_cfps_function(paper_year = 2018,paper_country = 'JPN')[1]

## CFPS ##
Takeuchi_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(faminc = base::cut(faminc,
                                   breaks = c(-0.00001, 2000000.001, 4000000.001, 6000000.001, 8000000.001,10000000.001,12000000, Inf)*ratio,
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  # set (1, 6 years; 2, 9 years; 3,  11 years; 4, 12 years; 5, 14 years; 6, 16 years; 7, 18 years;and 8, 21 years)
  dplyr::mutate(edu_m_recode = base::cut(eduy_m,
                                         breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5, 14.5, 16.5, 18.5, 21.5),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>%
  dplyr::mutate(edu_f_recode = base::cut(eduy_f,
                                         breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5, 14.5, 16.5, 18.5, 21.5),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8")))

# check SES score
summary(Takeuchi_CFPS$faminc)

## PSID ##



ratio=convert_index_psid_function(paper_year = 2018,paper_country = 'JPN')[1]

Takeuchi_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome = base::cut(fincome,
                                    breaks = c(-0.00001, 2000000.001, 4000000.001, 6000000.001, 8000000.001,10000000.001,12000000, Inf)*ratio,
                                    labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(edu_m_recode = base::cut(eduy_m,
                                         breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5, 14.5, 16.5, 18.5, 21.5),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>%
  dplyr::mutate(edu_f_recode = base::cut(eduy_f,
                                         breaks = c(-0.01, 6.5, 9.5, 11.5, 12.5, 14.5, 16.5, 18.5, 21.5),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8")))



#check SES score
summary(Takeuchi_PSID$fincome)



##############Taki-2010 ##############
ratio=convert_index_cfps_function(paper_year = 2010,paper_country = 'USA')[1] #converted from japan

## CFPS ##
Taki_CFPS <- df.CFPS_child %>%
  dplyr::mutate(faminc = base::cut(faminc,
                                   breaks = c(-0.00001, 20000.001, 40000.001, 60000.001, 80000.001,100000.001,120000, Inf)*ratio,
                                   labels = c("1", "2", "3", "4", "5", "6", "7")))

# check SES score
summary(Taki_CFPS$faminc)

## PSID ##



ratio=convert_index_psid_function(paper_year = 2012,paper_country = 'USA')[1]

Taki_PSID <- df.PSID_child %>%
  dplyr::mutate(fincome = base::cut(fincome,
                                    breaks = c(-0.00001, 20000.001, 40000.001, 60000.001, 80000.001,100000.001,120000, Inf)*ratio,
                                    labels = c("1", "2", "3", "4", "5", "6", "7")))
#check SES score
summary(Taki_PSID$fincome)


##############Taki-2012 ##############
ratio=convert_index_cfps_function(paper_year = 2012,paper_country = 'USA')[1] #converted from japan

## CFPS ##
Taki_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES = base::cut(faminc,
                                   breaks = c(-0.00001, 20000.001, 40000.001, 60000.001, 80000.001,100000.001,120000, Inf)*ratio,
                                   labels = c("1", "2", "3", "4", "5", "6", "7")))

# check SES score
summary(Taki_CFPS$SES)

## PSID ##



ratio=convert_index_psid_function(paper_year = 2010,paper_country = 'USA')[1]

Taki_PSID <- df.PSID_child %>%
  dplyr::mutate(SES = base::cut(fincome,
                                    breaks = c(-0.00001, 20000.001, 40000.001, 60000.001, 80000.001,100000.001,120000, Inf)*ratio,
                                    labels = c("1", "2", "3", "4", "5", "6", "7")))
#check SES score
summary(Taki_PSID$SES)


##############Ted-K. Turesky 2019,Ted Turesky 2020 and 2021(2 paper) ##############

## CFPS ##
TedK_CFPS <- df.CFPS_child %>%
  dplyr::mutate(INR = (faminc/familysize)/1274)  %>%    # calculate INR
  dplyr::mutate(edu_m_recode = base::cut(eduy_m,
                                         breaks = c(-0.01, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 30),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))


# check SES score
summary(TedK_CFPS$INR)

## PSID ##


TedK_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(INR = faminc/poverty) %>%
  dplyr::mutate(edu_m_recode = base::cut(eduy_m,
                                         breaks = c(-0.01, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 30),
                                         labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")))
#check SES score
summary(TedK_PSID$INR)

##############Topiwala, 2019 ##############
## CFPS ##
Topiwala_CFPS <- df.CFPS_elderly %>%
  dplyr::mutate(SES = base::cut(qg307egp,
                                         breaks = c(-0.01, 3.5, 7.5, 8.5, 30),
                                         labels = c("1", "2", "4", "3")))


# check SES score
summary(Topiwala_CFPS$SES)

## PSID  can't not be reproduced##

##########################################

##############Ursache, 2016 ##############

## CFPS ##
Ursache_CFPS <- df.CFPS_child %>%
  dplyr::mutate(faminc = faminc)  %>%    # calculate INR
  dplyr::mutate(avg_edu = (eduy_m+eduy_f)/2)


# check SES score
summary(Ursache_CFPS$avg_edu)

## PSID ##
Ursache_PSID <- df.PSID_child %>%
  dplyr::mutate(faminc = fincome)  %>%    # calculate INR
  dplyr::mutate(avg_edu = (eduy_m+eduy_f)/2)
#check SES score
summary(Ursache_PSID$avg_edu)

##############Vachon-Presseau-2019 ##############
ratio=convert_index_cfps_function(paper_year = 2019,paper_country = 'USA')[1]

## CFPS ##
Vachon_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(SES = base::cut(income,
                                   breaks = c(-0.00001, 10000.001, 25000.001, 50000.001,  Inf)*ratio,
                                   labels = c("1", "2", "3", "4")))

# check SES score
summary(Vachon_CFPS$SES)

## PSID ##



ratio=convert_index_psid_function(paper_year = 2019,paper_country = 'USA')[1]

Vachon_PSID <- df.PSID_adult %>%
  dplyr::mutate(SES = base::cut(income,
                                    breaks = c(-0.00001, 10000.001, 25000.001, 50000.001,  Inf)*ratio,
                                    labels = c("1", "2", "3", "4")))
#check SES score
summary(Vachon_PSID$SES)
##############Vanderauwera-2019 ##############

## CFPS ##
Vanderauwera_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES = base::cut(eduy_f,
                                         breaks = c(-0.01, 6.5, 9.5, 12.5, 22.5),
                                         labels = c("1", "2", "3", "4")))

# check SES score
summary(Vanderauwera_CFPS$SES)

## PSID ##

Vanderauwera_PSID <- df.PSID_child %>%
  dplyr::mutate(SES = base::cut(eduy_f,
                                         breaks = c(-0.01, 8.5, 12.5, 14.5, 17.5),
                                         labels = c("1", "2", "3", "4")))

summary(Vanderauwera_PSID$SES)

##############Wang, 2016 and Wang, 2017 ##############

## CFPS ##
Wang_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(SES =  sss)

# check SES score
summary(Wang_CFPS$SES)

## PSID ##

Wang_PSID <- df.PSID_adult %>%
  dplyr::mutate(SES = sss)
#check SES score
summary(Wang_PSID$sss)

################Weissman, 2018 and Weissman, 2021###################

## CFPS ##
Weissman_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES = (faminc/familysize)/1274)      # calculate INR


# check SES score
summary(Weissman_CFPS$SES)

## PSID ##


Weissman_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(SES = faminc/poverty)
summary(Weissman_PSID$SES)


################White, 2019###################
library(Rita)
## CFPS ##
Weissman_CFPS <- df.CFPS_child %>%
  dplyr::mutate(INR = rankitXform((faminc/familysize)/1274) )  %>%    #  Rankit transformation to reduce kurtosis
  dplyr::mutate(sss_f =  sss_f*2)  %>%
  dplyr::mutate(sss_m =  sss_m*2)

# check SES score
summary(Weissman_CFPS$INR)

## PSID ##


Weissman_PSID <- df.PSID_child %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(INR = rankitXform(faminc/poverty)  ) %>%
  dplyr::mutate(sss_f =  sss_f )  %>%
  dplyr::mutate(sss_m =  sss_m )

summary(Weissman_PSID$INR)




############### Yang, 2016 ##############
# Subject: young adults
# SES: family annual income < RMB 5,000; RMB 5,000–15000; RMB 15,001–30,000; RMB 30,001–50,000; RMB 50,001–100, 000; > RMB 100,000. (in six categories and recoded as 1-6)
#      education: collected in 6 categories and recoded as years (according to chinese and us educational systems respectively)
# composite: average of z-scores of two variables
## CFPS ##
ratio=convert_index_cfps_function(paper_year = 2016,paper_country = 'CHN')[1]

yang_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(fincome_recode = cut(faminc,
                                     breaks = c(-0.01, 4999, 15000, 30000, 50000, 100000, 5000000)*ratio,
                                     labels = c("1", "2", "3", "4", "5", "6"))) %>% #
  dplyr::mutate(edu_f = cut(edu_f,
                                 breaks = c(-0.01, 2.5, 3.5, 4.5, 6.5, 8.5),
                                 labels = c("6", "9", "12", "16", "19"))) %>%
  dplyr::mutate(edu_m = cut(edu_m,
                                 breaks = c(-0.01, 2.5, 3.5, 4.5, 6.5, 8.5),
                                 labels = c("6", "9", "12", "16", "19"))) %>%

  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode)),
                edu_f = as.numeric(as.character(edu_f)),
                edu_m = as.numeric(as.character(edu_m))) %>%
  dplyr::mutate(edu_recode=(edu_f+edu_m)/2) %>%
  dplyr::mutate(fincome_zscore = (fincome_recode - mean(fincome_recode, na.rm = TRUE))/sd(fincome_recode, na.rm = TRUE)) %>%  # calculate z-score of family income and education
  dplyr::mutate(edu_zscore = (edu_recode - mean(edu_recode, na.rm = TRUE))/sd(edu_recode, na.rm = TRUE)) %>%
  dplyr::mutate(SES = (fincome_zscore + edu_zscore)/2)
summary(yang_CFPS$SES)

## PSID ##

ratio=convert_index_psid_function(paper_year = 2016,paper_country = 'CHN')[1]

yang_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome_recode = cut(fincome,
                                     breaks = c(-0.01, 4999, 15000, 30000, 50000, 100000, 500000000)*ratio,
                                     labels = c("1", "2", "3", "4", "5", "6"))) %>% # CPI 2016 = 627.5, CPI 2010 = 536.1, CPI 2016/CPI 2010 = 1.1705, PPP 2010 China/US = 3.329
  dplyr::mutate(eduy_f = cut(eduy_f,
                                 breaks = c(-0.01, 6.5, 8.5, 12.5, 16.5, 18.5, 99),
                                 labels = c("6", "8", "12", "16", "18", NA))) %>%
  dplyr::mutate(eduy_m = cut(eduy_m,
                             breaks = c(-0.01, 6.5, 8.5, 12.5, 16.5, 18.5, 99),
                             labels = c("6", "8", "12", "16", "18", NA))) %>%
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode)),
                  edu_f = as.numeric(as.character(edu_f)),
                  edu_m = as.numeric(as.character(edu_m))) %>%
  dplyr::mutate(edu_recode=(edu_f+edu_m)/2) %>%
  dplyr::mutate(fincome_zscore = (fincome_recode - mean(fincome_recode, na.rm = TRUE))/sd(fincome_recode, na.rm = TRUE)) %>%  # calculate z-score of family income and education
  dplyr::mutate(edu_zscore = (edu_recode - mean(edu_recode, na.rm = TRUE))/sd(edu_recode, na.rm = TRUE)) %>%
  dplyr::mutate(SES = (fincome_zscore + edu_zscore)/2)
summary(yang_PSID$SES)


############ Yasuno, 2020 CFPS only  ##########
## CFPS ##
Yasuno_CFPS <- df.CFPS_elderly %>%
  dplyr::mutate(edu_recode = cut(cfps2010eduy_best,
                                 breaks = c(-0.5, 7.5, 9.5,11.5,12.5,14.5,16.5,22.5),
                                 labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%        # recode education
  dplyr::mutate(occup_recode = recode(qg307egp, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, "7"= 5,
                                      "8"= 4, "9"=3, "10"=2, "11"=1, .default = -8)) %>%    # recode occupation
  dplyr::mutate(occup_recode = as.numeric(as.character(occup_recode)),
                edu_recode = as.numeric(as.character(edu_recode))) %>%
  dplyr::mutate(SES = occup_recode*5 + edu_recode*3)

table(Yasuno_CFPS$SES)
# check SES score
table(mcder_CFPS$SES_mcder_cfps)

##############Younger, 2019 ##############

## CFPS ##
Younger_CFPS <- df.CFPS_child %>%
  dplyr::mutate(SES = base::cut(eduy_m,
                                breaks = c(-0.01, 11.5, 15.5, 17.5, 22.5),
                                labels = c("1", "2", "3", "4")))

# check SES score
summary(Younger_CFPS$SES)

## PSID ##

Younger_PSID <- df.PSID_child %>%
  dplyr::mutate(SES = base::cut(eduy_m,
                                breaks = c(-0.01, 12.5, 14.5, 16.5,17.5),
                                labels = c("1", "2", "3", "4")))

summary(Younger_PSID$SES)


################## Yu,2016   ########
# Subject: children & young adult
# SES: SSS (MAS): self sss
#      ITN: income data were collected in 9 categories: < $5000; $5000 ~ 11, 999; $ 12, 000 ~ 15, 999 ; $ 16, 000 ~ 24, 999; $ 25,000 ~ 34, 999; $ 35, 000 ~ 49, 999;
#                                                       $ 50, 000 ~ 74, 999; $ 75, 000 ~ 99, 999; $ > =100, 000
#      Education father and mother: 7 categories
#      composite: weighted factor composite PCA score for the four variables

# laod related packages

ratio=convert_index_cfps_function(paper_year = 2016,paper_country = 'USA')[1]


library(FactoMineR)
library(factoextra)
yu_CFPS <- df.CFPS_child %>%
  dplyr::mutate(fincome_recode = cut(faminc,
                                     breaks = c(-0.01, 5000, 11999, 15999, 24999,
                                                34999, 49999, 74999,99999, 5000000)*ratio,
                                     labels = c("2500", "8500", "14000", "20500", "30000", "42500",
                                                "62500","875000", "125000"))) %>% #Incomes were  divided into 9 levels, CPI US dollar 2010/2018 = 218.056/251.107 = 0.868; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))*ratio) %>%
  dplyr::mutate(ITN = fincome_recode/(1274*familysize)) %>%
  dplyr::mutate(edu_m_recode = cut(edu_m,
                                   breaks = c(-0.01, 1.5, 3.5, 4.5, 5.5,6.5,7.5,8.5),
                                   labels = c("1", "2", "3", "4", "5", "6", "7")),
                edu_f_recode = cut(edu_f,
                                   breaks = c(-0.01, 1.5, 3.5, 4.5, 5.5,6.5,7.5,8.5),
                                   labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode)),
                edu_f_recode = as.numeric(as.character(edu_f_recode)))%>%
  dplyr::select(ITN, edu_m_recode, edu_f_recode, sss_f, sss_m) %>%
  tidyr::drop_na(.)
# calculate PCA  pending...........

## PSID ##
ratio=convert_index_psid_function(paper_year = 2016,paper_country = 'USA')[1]


yu_CFPS <- df.PSID_child %>%
  dplyr::mutate(fincome_recode = cut(fincome,
                                     breaks = c(-0.01, 5000, 11999, 15999, 24999,
                                                34999, 49999, 74999,99999, 5000000)*ratio,
                                     labels = c("2500", "8500", "14000", "20500", "30000", "42500",
                                                "62500","875000", "125000"))) %>% #Incomes were  divided into 9 levels, CPI US dollar 2010/2018 = 218.056/251.107 = 0.868; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(fincome_recode = as.numeric(as.character(fincome_recode))*ratio) %>%
  dplyr::mutate(poverty = 12060 +  (familysize-1)*4180) %>%
  dplyr::mutate(INR = rankitXform(fincome_recode/poverty)  ) %>%
  dplyr::mutate(edu_m_recode = cut(edu_m,
                                   breaks = c(-0.01, 0.01, 8.5, 12.5, 14.5,16.5,17.5),
                                   labels = c("1", "2", "3", "4", "5", "7")),
                edu_f_recode = cut(edu_m,
                                   breaks = c(-0.01, 0.01, 8.5, 12.5, 14.5,16.5,17.5),
                                   labels = c("1", "2", "3", "4", "5", "7"))) %>%
  dplyr::mutate(edu_m_recode = as.numeric(as.character(edu_m_recode)),
                edu_f_recode = as.numeric(as.character(edu_f_recode)))%>%
  dplyr::select(INR, edu_m_recode, edu_f_recode, sss_f, sss_m) %>%
  tidyr::drop_na(.)
# calculate PCA  pending...........

#############Zhu, 2018  xxxxxxxxxx  can't not be reproduce? ################
# Subjects: young adults
# SES: own education, household income & employment status
#      education: <11 = 11; 12; 13; 14; 15; 16; 17+ = 17
#      income: 1 = <10 000USD; 2 = 10 000 ~ 19 999; 3 = 20 000 ~ 29 999; 4 = 30 000 ~ 39 999; 5 = 40 000 49 999; 6 = 50 000 ~ 74 999; 7 = 75 000 ~ 99 999; 8 = >=100 000 USD
#      employment: not working = 0; part-time = 1; full time =2 (CFPS only distinguish employment/unemployment)
## CFPS ##
ratio=convert_index_cfps_function(paper_year = 2018,paper_country = 'USA')[1]

zhu_CFPS <- df.CFPS_adult %>%
  dplyr::mutate(fincome_recode = cut(faminc,
                                     breaks = c(-0.01, 10000, 19999, 29999, 39999, 49999, 74999, 99999, 10000000)*ratio,
                                     labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>% # CPI US dollar 2010/2018 = 218.056/251.107 = 0.868; PPP: CNY/US dollar in 2010 = 3.329
  dplyr::mutate(edu_recode = cut(eduy,
                                 breaks = c(-0.01, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 22.5),
                                 labels = c("11", "12", "13", "14", "15", "16", "17"))) %>%
  dplyr::mutate(employment_recode = ifelse(employment == 1, 2, 0)) %>%
  dplyr::mutate(SES_zhu_income_cfps = as.numeric(as.character(fincome_recode)),
                SES_zhu_edu_cfps = as.numeric(as.character(edu_recode)),
                SES_zhu_employment_cfps = employment_recode)
table(zhu_CFPS$SES_zhu_income_cfps)
table(zhu_CFPS$SES_zhu_edu_cfps)
table(zhu_CFPS$SES_zhu_employment_cfps)

## PSID ##

ratio=convert_index_psid_function(paper_year = 2018,paper_country = 'USA')[1]

zhu_PSID <- df.PSID_adult %>%
  dplyr::mutate(fincome_recode = cut(fincome,
                                     breaks = c(-0.01, 10000, 19999, 29999, 39999, 49999, 74999, 99999, 100000000000)*ratio,
                                     labels = c("1", "2", "3", "4", "5", "6", "7", "8"))) %>% # CPI US dollar 2017/2018 = 245.120/251.107 = 0.976
  dplyr::mutate(edu_recode = cut(eduy,
                                 breaks = c(-0.01, 11.5, 12.5, 13.5, 14.5, 15.5, 16.5, 22.5),
                                 labels = c("11", "12", "13", "14", "15", "16", "17"))) %>%
  dplyr::mutate(employment_recode = ifelse(occupation_code == 1, 2,
                                           ifelse(occupation_code == 2, 1,
                                                  ifelse(occupation_code==9 | occupation_code == 0, NA, 0)))) %>% # 1= working; 2= Only temporarily laid off; 3-8 = not employed; 9 = DK/NA; 0 = not applicable
  dplyr::mutate(SES_zhu_income_psid = as.numeric(as.character(fincome_recode)),
                SES_zhu_edu_psid = as.numeric(as.character(edu_recode)),
                SES_zhu_employment_psid = employment_recode)

# calculate PCA  pending...........
