# import packages
library(dplyr)
library(tidyr)
library(stringr)
# read CPI and PPP data
CPI <- read.csv("CPI.csv",header = TRUE)
PPP <- read.csv("PPP.csv",header = TRUE)
# check if country code are same (indeed the same)
country_code_CPI = CPI$Country.Code
country_code_PPP = PPP$ï..LOCATION
for (i in 1:65) {
  print(country_code_PPP[i] %in% country_code_CPI)
}
# rename PPP
names(PPP)[names(PPP) == 'ï..LOCATION'] <- 'Country.Code'
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
  print(index_ppp_first_cfps)
  # CPI first calculation:
  CPI_papercountry_2010 = CPI$Value[CPI$Country.Code == paper_country & CPI$TIME == 2010]
  CPI_papercountry_paperyear = CPI$Value[CPI$Country.Code == paper_country & CPI$TIME == paper_year]
  PPP_CHN_2010 = PPP$Value[PPP$Country.Code == 'CHN' & PPP$TIME == 2010]
  PPP_papercountry_2010 = PPP$Value[PPP$Country.Code == paper_country & PPP$TIME == 2010]
  index_cpi_first_cfps <<- (CPI_papercountry_2010 /CPI_papercountry_paperyear)* (PPP_CHN_2010 /PPP_papercountry_2010)
  print(index_cpi_first_cfps)
}

## PSID ## 
convert_index_psid_function <- function(paper_year, paper_country){
  # PPP first calculation:
  PPP_USA_paperyear = PPP$Value[PPP$Country.Code == 'USA' & PPP$TIME == paper_year]
  PPP_papercountry_paperyear = PPP$Value[PPP$Country.Code == paper_country & PPP$TIME == paper_year]
  CPI_USA_2017 = CPI$Value[CPI$Country.Code == 'USA' & CPI$TIME == 2017]
  CPI_USA_paperyear = CPI$Value[CPI$Country.Code == 'USA' & CPI$TIME == paper_year]
  index_ppp_first_psid <<- (PPP_USA_paperyear / PPP_papercountry_paperyear) * (CPI_USA_2017 / CPI_USA_paperyear)
  print(index_ppp_first_psid)
  # CPI first calculation:
  CPI_papercountry_2017 = CPI$Value[CPI$Country.Code == paper_country & CPI$TIME == 2017]
  CPI_papercountry_paperyear = CPI$Value[CPI$Country.Code == paper_country & CPI$TIME == paper_year]
  PPP_USA_2017 = PPP$Value[PPP$Country.Code == 'USA' & PPP$TIME == 2017]
  PPP_papercountry_2017 = PPP$Value[PPP$Country.Code == paper_country & PPP$TIME == 2017]
  index_cpi_first_psid <<- (CPI_papercountry_2017 /CPI_papercountry_paperyear)* (PPP_USA_2017 /PPP_papercountry_2017)
  print(index_cpi_first_psid)
}
# get two index, names: index_ppp_first_cfps, index_cpi_first_cfps
convert_index_cfps_function(paper_year = 2021,paper_country = 'JPN')
# get two index, names: index_ppp_first_psid, index_cpi_first_psid
convert_index_psid_function(paper_year = 2021,paper_country = 'CAN')
