##################################################################################################################
##################################################################################################################
###                                                                                                            ###
###                 R script for Flexibility of SES project                                                    ###
###                           [Correlation analysis]                                                           ###
###               Email = hcp4715@gmail.com       cyq_2016@outlook.com                                         ###
###                                                                                                            ###
##################################################################################################################
##################################################################################################################

##################################################################################################################
##################################################################################################################
###                                                                                                            ###
###  Purpose:                                                                                                  ###
###  Calculate the correlations between the SES scores and mental health measurements                          ###
###                                                                                                            ###
###  Code authors: Chuan-Peng Hu, PhD, Neuroimaging Center (NIC), Johannes Gutenberg                           ###  
###                University Medical Center, 55131 Mainz, Germany;                                            ###
###                Yuqing Cai, Tsinghua University, 100086, China                                              ###
###                                                                                                            ###
###  Input data                                                                                                ###
###      CFPS: "SES_CFPS.RData" (a list of dataframes for SES scores)                                          ###                                   
###      PSID: "SES_PSID.RData" (a list of dataframes for SES scores)                                          ###
###                                                                                                            ###
### Table of SES scores for each paper                                                                         ###
### ------------------------------------------------------------------------------------------------------     ###
### original paper    subject	  SES_cfps (range)	          SES_psid (range)	             type of SES         ###
### ------------------------------------------------------------------------------------------------------     ###
### Betancourt, 2916	children	SES_betan_CFPS (1-6)        SES_betan_PSID (1-5.5)       	 composite           ###
### Moog, 2008	      children	SES_moog_CFPS (1-5)         SES_mpog_PSID (1-5)	           composite           ###
### Jednoróg, 2012	  children	SES_jed_CFPS (11-84)        NA	                           composite           ###
### McDermott, 2019	  children	SES_mcder_CFPS (3-66)       NA	                           composite           ###
### Romeo, 2018a	    children	SES_romeo1_CFPS (8-66)	    NA	             	             composite           ###
### Romeo, 2018b	    children	SES_romeo2_CFPS (-0.39-0.12)SES_romeo2_PSID (-0.39-0.12)   composite           ###
### Qiu, 2017	        children	SES_qiu_CFPS (10200-298600)	SES_qiu_PSID (-73950~229087)	 income              ###
### Kim, 2019	        children	SES_kim_CFPS (-3.88~2.51)	  SES_kim_PSID (-3.65-2.01)	     poverty(income)     ###
### Hanson, 2013	    children	SES_hanson_CFPS (1，2，3)	  SES_hanson_PSID (1，2，3)	     poverty(income)     ###
### Leonard, 2019	    children	SES_leo_CFPS (1，2)	        SES_leo_PSID (1，2)		         education           ###
### Ozernov-Palchik	  children	SES_ozer_CFPS (1，2)	      SES_ozer_PSID (1，2)	         education           ###
### ----------------------------------------------------------------------------------------------------       ###
###                                                                                                            ###
###                                                                                                            ###
###  Output results: Correlation matrix of the SES scores and mental health measurements                       ###                                   
###                  Inter-rater correlation coefficient of all the variables (ICC)                            ###
###                                                                                                            ###
##################################################################################################################
##################################################################################################################
###                                                                                                            ###
###                       ============= Notes about data ==================                                    ###
###   For each family, if there are more than one children, only one children is selected                      ###
###   for the analysis.                                                                                        ###
###   To be consistent across the index reproduced, for all children and adolescents' SES,                     ###
###   We used data from participant with age ranged 10-22 yrs-old.                                             ###
###                                                                                                            ###
##################################################################################################################
##################################################################################################################
###     # --------------------------------------------------------------------------------------------#        ###
###     # ------------------------------Table of Contents --------------------------------------------#        ###
###     # --------------------------------------------------------------------------------------------#        ###
###     # ---------- 1.  Correlation analysis of CFPS ------------------------------------------------#        ###
###     # ----------  # 1.1 Prepare mental health variables of ---------------------------------------#        ###
###     # ----------  # 1.2 Correlation relationship between SES scores and mental health scores ---- #        ###
###     # ----------- # 1.3 Calculate inter-rater correlation coefficient of all the SES variables----#        ###
###     # ---------- 2.  Correlation analysis of PSID ------------------------------------------------#        ###
###     # ----------  # 2.1 Prepare mental health variables ------------------------------------------#        ###
###     # ----------  # 2.2 Correlation relationship between SES scores and mental health scores ---- #        ###
###     # ----------- # 2.3 Calculate inter-rater correlation coefficient of all the SES variables----#        ###
###     # ---------- 3.  Combine two plots into one --------------------------------------------------#        ###
###     # ----------  # 3.1 All variables ----------------------------------------------------------- #        ###
###     # ----------- # 3.2 Only SES variables--------------------------------------------------------#        ###
###     # ---------- 4.  Extract and save data frame -------------------------------------------------#        ###
##################################################################################################################
##################################################################################################################



######################## Start of the script ###########################
### clean the memory to avoid unnecessary errors:
rm(list = ls())
Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English
options(scipen = 999)   # force R to output in decimal instead of scientifc notion
options(digits=5)       # limit the number of reporting

### set directoRy to the folder of analytic data
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curWD)

# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(ggcorrplot)) {install.packages("ggcorrplot",repos = "http://cran.us.r-project.org"); require(ggcorrplot)}
if (!require(corrplot)) {install.packages("corrplot",repos = "http://cran.us.r-project.org"); require(corrplot)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
if (!require(correlation)) {install.packages("correlation",repos = "http://cran.us.r-project.org"); require(correlation)}
if (!require(ltm)) {install.packages("ltm",repos = "http://cran.us.r-project.org"); require(ltm)}
if (!require(magicfor)) {install.packages("magicfor",repos = "http://cran.us.r-project.org"); require(magicfor)}
if (!require(GPArotation)) {install.packages("GPArotation",repos = "http://cran.us.r-project.org"); require(GPArotation)}
if (!require(reshape2)) {install.packages("reshape2",repos = "http://cran.us.r-project.org"); require(reshape2)}
if (!require(lessR)) {install.packages("lessR",repos = "http://cran.us.r-project.org"); require(lessR)}
if (!require(lme4)) {install.packages("lme4",repos = "http://cran.us.r-project.org"); require(lme4)}
if (!require(irr)) {install.packages("irr",repos = "http://cran.us.r-project.org"); require(irr)}

# ---------------------------------------------------------------------------------------
# ---------- 1.  Correlation analysis of CFPS--------------------------------------------
# ---------------------------------------------------------------------------------------

############# 1.1 Prepare mental health variables of CPFS ##########
## children mental health variables
# load data
load("df.CFPS_child.RData")
# extract mental health variables
mental_CFPS <- df.CFPS_child %>%   
  dplyr::select(# from child dataframe
                wn401,	# Feel depressed and cannot cheer up
                wn402,	# Feel nervous
                wn403,	# Feel agitated or upset and cannot remain calm 
                wn404,	# Feel hopeless about the future 
                wn405,	# Feel that everything is difficult 
                wn406,	# Think life is meaningless 
                # from adult dataframe (*some children defined here are in adult dataframe in the original dataset (age above 16))
                qq601,  # Feel depressed and cannot cheer up
                qq602,  # Feel nervous
                qq603,  # Feel agitated or upset and cannot remain calm
                qq604,  # Feel hopeless about the future 
                qq605,  # Feel that everything is difficult 
                qq606,  # Think life is meaningless 
                ### cognitive ability	
                wordtest,	# word test
                mathtest, # math test
                pid) %>% # pid
  dplyr::na_if(., -8) %>%   # set NA
  # calculate the total score of depression and cognition
  dplyr::mutate(depression1 = wn401+wn402+wn403+wn404+wn405+wn406, # from children's data frame
                depression2 = qq601+qq602+qq603+qq604+qq605+qq606, # from adults' data frame
                cognition = wordtest + mathtest) %>% # cognition (same for children and adult's questionnarie)
  dplyr::mutate(depression1 = as.character(depression1),  # convert to numeric variables
                depression2 = as.character(depression2)) %>%
  tidyr::unite("depression",depression1:depression2, na.rm = TRUE, remove = TRUE) %>%  # combine two columns of depression into one column
  dplyr::select(pid, depression, cognition) %>%  #select only necessary variables
  dplyr::mutate(depression = as.numeric(depression))   # convert to numeric variables


# check score
table(mental_CFPS$depression) # hcp: is it normal to have negative scores here?? CYQ: I dont say any negative score here (?)

############ 1.2 Correlation relationship between SES scores and mental health scores CFPS ##############
# import SES data
load("SES_CFPS.RData")
# merge all ordinal and continuous SES cfps and mental health 
SES_mental_CFPS <- Reduce(function(x, y) merge(x, y, by = "pid", all = TRUE), dataframes_cfps) %>% 
  dplyr::left_join(., mental_CFPS, by = "pid", all.x = TRUE) %>%  # merge SES data with mental health data
  dplyr::select(-pid) %>%  # delete the column of pid 
  dplyr::rename(dep = depression,  #rename columns
                cog = cognition,
                # composite SES 1-6
                c1 = SES_betan_cfps, 
                c2 = SES_moog_cfps, 
                c3 = SES_jed_cfps, 
                c4 = SES_mcder_cfps,
                c5 = SES_romeo1_cfps,
                c6 = SES_romeo2_cfps,
                # income 1-3
                i1 = SES_qiu_cfps,
                i2 = SES_kim_cfps,
                i3 = SES_hanson_cfps,
                # education 1-2
                e1 = SES_leo_cfps,
                e2 = SES_ozer_cfps)


# Extract colnames of SES_mental_CFPS
dimname <- colnames(SES_mental_CFPS)
dimname #see the names

#extract number of SES and mental health variables
N_variable_cfps <- ncol(SES_mental_CFPS) # all variables
N_SES_CFPS <- N_variable_cfps - 2 # only SES variables
N_correlation <- N_variable_cfps*N_variable_cfps # number of correlations 


# determine the type of each of variable (dichotomous or ordinal)
# create a data frame to store the result
variable_type <- data_frame(variable = dimname,
                            type = as.character(NA)) 
# determine the type of variable
for (i in 1:N_variable_cfps) {
  var <- as.character(variable_type[i, "variable"])  #extract name of each column
  if(length(unique(na.omit(SES_mental_CFPS[,var]))) ==2){ # if the variable only have two values, then identify it as dichotomous 
    variable_type[i, "type"] <- "bin"} else if(length(unique(na.omit(SES_mental_CFPS[,var]))) > 2){
      variable_type[i, "type"] <- "ordi"} else {variable_type[i, "type"] <- NA} # if the variable have more than two values, then identify it as ordinal
}
# see data frame
variable_type

#-------------cor matrix & p-value matrix CFPS-------------
## create correlation table for all the correlation relationship between variables
# build an empty correlation table to store the results for the correlation relationship
Correlations_cfps <- data.frame(variable1 = as.character(rep(dimname, each = N_variable_cfps)),# first variable in the correlation relationship
                                variable2 = as.character(rep(dimname, N_variable_cfps)),# second variable in the correlation relationship
                                correlation = rep(NA, N_correlation), # correlation coefficient
                                p = rep(NA, N_correlation), # p-value
                                ci1 = rep(NA, N_correlation),# confidence interval (lower)
                                ci2 = rep(NA, N_correlation)) # confidence interval (upper)

# calculate correlation between SESs and mental health variables with different correlational analysis methods depend on the type of variable
for (i in 1:N_correlation) {
   v1 <- SES_mental_CFPS %>% dplyr::select(as.character(Correlations_cfps[i, "variable1"])) %>% dplyr::pull() # extract first variable in the correlation analysis
   v2 <- SES_mental_CFPS %>% dplyr::select(as.character(Correlations_cfps[i, "variable2"])) %>% dplyr::pull() # extract second variable in the correlation analysis
   # if both variables are ordinal, use Spearsman
   if(dplyr::n_distinct(v1, na.rm = T) > 2 && dplyr::n_distinct(v2, na.rm = T) > 2){
     a <- cor.test(v1, v2, method = "spearman", exact = FALSE)
     Correlations_cfps[i, "correlation"]<- a$estimate
     Correlations_cfps[i, "p"] <- round(a$p.value, digits = 5)
     Correlations_cfps[i, "ci1"] <- NA
     Correlations_cfps[i, "ci2"] <- NA
     # if both variables are dichotomous, use phi analysis
     } else if (dplyr::n_distinct(v1, na.rm = T) == 2 && dplyr::n_distinct(v2, na.rm = T) == 2){
       Correlations_cfps[i, "correlation"] <- phi(table(v1, v2))
       b <- cor.test(v1, v2, use = "complete.obs")                                       
       Correlations_cfps[i, "p"]<- round(b$p.value, digits = 5)
       Correlations_cfps[i, "ci1"] <- round(b$conf.int[1],  digits = 5)
       Correlations_cfps[i, "ci2"] <- round(b$conf.int[2],  digits = 5)
       # if one variable is dichotomous and another is ordinal, use biserial correlation (first variable is dichotomous)
       } else if (dplyr::n_distinct(v1, na.rm = T) == 2  &&  dplyr::n_distinct(v2, na.rm = T) > 2){
         Correlations_cfps[i, "correlation"] <- biserial.cor(v2, v1, use = "complete.obs", level = 2)
         c<-cor.test(v1, v2, use = "complete.obs", level = 2)
         Correlations_cfps[i, "p"] <- round(c$p.value, digits = 5)
         Correlations_cfps[i, "ci1"] <- round(c$conf.int[1],  digits = 5)
         Correlations_cfps[i, "ci2"] <- round(c$conf.int[2],  digits = 5)
         # same here, when second variable is dichotomous
         } else if (dplyr::n_distinct(v1, na.rm = T) > 2 && dplyr::n_distinct(v2, na.rm = T) == 2 ){
           Correlations_cfps[i, "correlation"]<- biserial.cor(v1, v2, use = "complete.obs", level = 2)
           d<-cor.test(v1, v2, use = "complete.obs", level = 2)
           Correlations_cfps[i, "p"] <- round(d$p.value, digits = 5)
           Correlations_cfps[i, "ci1"] <- round(d$conf.int[1],  digits = 5)
           Correlations_cfps[i, "ci2"] <- round(d$conf.int[2],  digits = 5)
           # check if there is any variables left 
           } else {
           Correlations_cfps[i, "correlation"] <- NA
           Correlations_cfps[i, "p"] <- NA
           Correlations_cfps[i, "ci1"] <- NA
           Correlations_cfps[i, "ci2"] <- NA
         }
     # print(Correlations_cfps)
}
# Correlations_cfps

# Create correlation matrix from the correlation table
cormatrix_cfps <- reshape2::dcast(Correlations_cfps[, c("variable1", "variable2", "correlation")], variable1~variable2, value.var="correlation") %>%
  dplyr::select(-variable1) %>%
  as.matrix(.) %>%
  `rownames<-`(colnames(.)) # naming the rows

# Rearrange the matrix (put mental health variables first)
cormatrix_cfps <- lessR::corReorder(R=cormatrix_cfps, order = "manual", vars = c(dep, cog, c1, c2, c3, c4, c5, c6, i1, i2, i3, e1, e2))
cormatrix_cfps

# Create p matrix
pmatrix_cfps <- reshape2::dcast(Correlations_cfps[, c("variable1", "variable2", "p")], variable1~variable2, value.var="p") %>%
  dplyr::select(-variable1) %>%
  as.matrix(.) %>%
  # naming the rows
  `rownames<-`(colnames(.))
# rearrange the matrix (put mental health variables first)
pmatrix_cfps<- lessR::corReorder(R= pmatrix_cfps, order = "manual", vars = c(dep, cog, c1, c2, c3, c4, c5, c6, i1, i2, i3, e1, e2))
pmatrix_cfps

#-------------CI calculating-------------
## cor.test with spearman cannot calculate ci, calculate them separately 
# calculate ci of spearman correlation for ordinal variables
corrtest_spearman <-corr.test(SES_mental_CFPS[, variable_type[variable_type$type == "ordi",]$variable], y = NULL, use = "pairwise",method="spearman",adjust="holm", 
                     alpha=.05,ci=TRUE,minlength=5)
# select rownames (correlation analysis pair of variables) for further pairing
ci_spearman_name <- rownames(corrtest_spearman$ci)
ci_spearman_name
# create a column for data frame merge
corrtest_spearman$ci$name_combine <- ci_spearman_name
# because all correlations have two pairs, repeat the process
corrtest_spearman$ci$name_combine2 <- ci_spearman_name
# check the table
corrtest_spearman$ci
# create another ci table for the second merge
corrtest_spearman_ci2<-corrtest_spearman$ci
# rename the varibales 
names(corrtest_spearman_ci2) <- c("lower2", "r2", "upper2", "p2", "name_combine","name_combine2")
# check the table
corrtest_spearman_ci2
# combine all the confidence intervals into two columns (ci_upper and ci_lower)
Correlations_cfps_with_ci <- Correlations_cfps %>%
  dplyr::mutate(name_combine = paste0(variable1, "-", variable2)) %>%  # create a column of name for merge in the original correlation table
  dplyr::mutate(name_combine2 = paste0(variable2, "-", variable1)) %>%  # create a second column (reverse variable names) of name for merge in the original correlation table
  dplyr::left_join(., corrtest_spearman$ci[,c("upper", "lower", "name_combine")], by = "name_combine") %>%  # merge the original correlation table with ci table (first one)
  dplyr::left_join(., corrtest_spearman_ci2[,c("upper2", "lower2", "name_combine2")], by = "name_combine2")%>%  # merge the original correlation table with ci table (second one)
  dplyr::select(variable1, variable2, correlation, p, ci1, lower, lower2, ci2, upper, upper2) %>%   # merge the original correlation table with ci table (second one)
  dplyr::mutate(ci_upper = ifelse(!is.na(ci2), ci2,                # combine two columns of upper ci into one column
                                  ifelse(!is.na(upper), upper, upper2)),
                ci_lower = ifelse(!is.na(ci1), ci1,                # combine two columns of upper ci into one column
                                  ifelse(!is.na(lower), lower, lower2))) %>%
  dplyr::select(variable1, variable2, correlation, p, ci_lower, ci_upper) %>%  # select necessary columns
  dplyr::mutate(ci = paste0("[", round(ci_lower, digits = 4), ",", " ", round(ci_upper, digits= 4), "]")) # combine two columns of upper ci into one column
# check ci table
Correlations_cfps_with_ci

# -------------plot CFPS-------------
## draw plot
# plot with mental health variables
corrplot_CFPS <- corrplot.mixed(cormatrix_cfps, p.mat = pmatrix_cfps, insig = "blank",sig.level = 0.05,
                              cl.lim = c(-0.12, 1), tl.cex = 0.8, number.cex = 0.8)
# extract only SES variables
cormatrix_cfps_ses <- cormatrix_cfps[3:13, 3:13]
pmatrix_cfps_ses <- pmatrix_cfps[3:13, 3:13]
# plot only SES variables
corrplot_CFPS_SES <-corrplot.mixed(cormatrix_cfps_ses, p.mat =pmatrix_cfps_ses, insig = "blank", sig.level = 0.05,
                              cl.lim = c(-0.12, 1), tl.cex = 0.8, number.cex = 0.8)
############### 1.3 Calculate inter-rater correlation coefficient of all the SES variables###########
# ICC
# calculate z-score for all SES scores
# build a table for z-scores
z_score_CFPS <- SES_mental_CFPS[NA,]
# transfer the scores for each SES into z-score 
for(i in 1:N_SES_CFPS){
  var <- colnames(SES_mental_CFPS[i])
  z_score_CFPS[,i] <- (SES_mental_CFPS[,var] - mean(SES_mental_CFPS[,var], na.rm = TRUE))/sd(SES_mental_CFPS[,var], na.rm = TRUE)}

# Two-way mixed effect model, absolute agreement, single measurement
ICC_CFPS_1 <- z_score_CFPS %>%
  dplyr::select(1:(ncol(.)-2)) %>%
  tidyr::drop_na() %>%
  irr::icc(., model = "twoway", type = "agreement", unit = "single")

# Two-way mixed effect model, absolute agreement, average measurement
ICC_CFPS_2 <- z_score_CFPS %>%
  dplyr::select(1:(ncol(.)-2)) %>%
  tidyr::drop_na() %>%
  irr::icc(., model = "twoway", type = "agreement", unit = "average")
# check ICCs
ICC_CFPS_1
ICC_CFPS_2

# ---------------------------------------------------------------------------------------
# ---------- 2.  Correlation analysis of PSID--------------------------------------------
# ---------------------------------------------------------------------------------------
############# 2.1 Prepare mental health variables of PSID ##########
# load data
load("df.PSID_child.RData")
# prepare mental health data of PSID
mental_PSID <- df.PSID_child %>%
  dplyr::select(depression, # sum for depression: very healthy 0----24 very depressed
                life_satisfaction, # life satisfaction: completed satisfied 1----5 not at all satisfied
                pid) %>%
  dplyr::mutate(depression = ifelse(depression == 99, NA, depression),  # set NA
                life_satisfaction = ifelse(life_satisfaction<1 | life_satisfaction>5, NA, life_satisfaction)) %>%
  dplyr::mutate(depression = -depression + 24,  # reverse score for depresion and life_satisfaction (higher-better mental health, same as CFPS)
                life_satisfaction = -life_satisfaction + 6) 

############ 2.2 Correlation relationship between SES scores and mental health PSID ##############
# load SES data
load("SES_PSID.RData")

## merge all ordinal and continuous SES cfps and mental health
SES_mental_PSID <- Reduce(function(x, y) merge(x, y, by = "pid", all = TRUE), dataframes_psid) %>%
  dplyr::left_join(., mental_PSID, by = "pid", all.x = TRUE) %>% # merge two dataset together
  dplyr::select(-pid) %>%  # delete unnecessary column
  dplyr::rename(dep = depression,  # rename columns
                satis = life_satisfaction,
                # composite SES 1-6
                c1 = SES_betan_psid,
                c2 = SES_moog_psid, 
                c6 = SES_romeo2_psid,
                # income 1-3
                i1 = SES_qiu_psid,
                i2 = SES_kim_psid,
                i3 = SES_hanson_psid,
                # education 1-2
                e1 = SES_leo_psid,
                e2 = SES_ozer_psid)   


# extract colnames of SES_mental_PSID
dimname <- colnames(SES_mental_PSID)
dimname # see the names

# extract number of variable
N_variable_psid <- ncol(SES_mental_PSID) # number of all variables
N_SES_PSID <- N_variable_psid- 2 # number of ses variables
N_correlation <- N_variable_psid*N_variable_psid # number of correlations


## determine the type of each of variable (dichotomous or ordinal)
# build an empty table to save the results
variable_type <- data_frame(variable = dimname,
                            type = as.character(NA)) 
for (i in 1:N_variable_psid) {
  var <- as.character(variable_type[i, "variable"])  
  if(length(unique(na.omit(SES_mental_PSID[,var]))) ==2){ # if the variable has only two values, it is a dichotomous variable ("bin")
    variable_type[i, "type"] <- "bin"} else if(length(unique(na.omit(SES_mental_PSID[,var]))) > 2){ # if the variable has more than two values, it is ordinal ('ordi")
      variable_type[i, "type"] <- "ordi"} else {variable_type[i, "type"] <- NA}
}
variable_type

# -----------------cor matrix & p-value matrix PSID--------------
# build an empty data frame for correlation results
Correlations_psid <- data.frame(variable1 = rep(dimname, each = N_variable_psid),
                                variable2 = rep(dimname, N_variable_psid),
                                correlation = rep(NA, N_correlation),
                                p = rep(NA, N_correlation),
                                ci1 = rep(NA, N_correlation),
                                ci2 = rep(NA, N_correlation)) 


# calculating the correlations between varibles with different methods
for (i in 1:N_correlation) {
  v1 <- SES_mental_PSID %>% dplyr::select(as.character(Correlations_psid[i, "variable1"])) %>% dplyr::pull()
  v2 <- SES_mental_PSID %>% dplyr::select(as.character(Correlations_psid[i, "variable2"])) %>% dplyr::pull()
  # if both variables are ordinal, use spearman correlation
  if(dplyr::n_distinct(v1, na.rm = T) > 2 && dplyr::n_distinct(v2, na.rm = T) > 2){
    a <- cor.test(v1, v2, method = "spearman", exact = FALSE)
    Correlations_psid[i, "correlation"]<- a$estimate
    Correlations_psid[i, "p"] <- round(a$p.value, digits = 5)
    Correlations_psid[i, "ci1"] <- NA
    Correlations_psid[i, "ci2"] <- NA
    ## if both variables are dichonomous, use phi analysis
    } else if(dplyr::n_distinct(v1, na.rm = T) == 2 && dplyr::n_distinct(v2, na.rm = T) == 2){
      Correlations_psid[i, "correlation"] <- phi(table(v1, v2))
      b <- cor.test(v1, v2, use = "complete.obs")
    Correlations_psid[i, "p"]<- round(b$p.value, digits = 5)
    Correlations_psid[i, "ci1"] <- round(b$conf.int[1],  digits = 5)
    Correlations_psid[i, "ci2"] <- round(b$conf.int[2],  digits = 5)
      # if one variable is ordinal another is dichonomous, use biserial analysis (here because I use polyserial function, I calculate the p-value and ci manually)
      } else if (dplyr::n_distinct(v1, na.rm = T) == 2  &&  dplyr::n_distinct(v2, na.rm = T) > 2){
      c<- polycor::polyserial(v2, v1, std.err = TRUE)
      Correlations_psid[i, "correlation"]<- c$rho
      Correlations_psid[i, "p"]<- round(2 * pnorm(-abs(c$rho / sqrt(c$var[1,1]))), digits = 5) #std.erro = sqrt(X$var[1,1]), p-value = 2 * pnorm(-abs(rho / std.error)))
      v1_v2 <- cbind(v1, v2)
      n<- drop_na(as.data.frame(v1_v2))
      Correlations_psid[i, "ci1"] <- round(fisherz(2*c$rho / sqrt(5)) - 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
      Correlations_psid[i, "ci2"] <- round(fisherz(2*c$rho / sqrt(5)) + 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
        # same here, just alternating the sequence of two variables
        } else if (dplyr::n_distinct(v2, na.rm = T) == 2  &&  dplyr::n_distinct(v1, na.rm = T) > 2){
        c <- polycor::polyserial(v1, v2, std.err = TRUE)
        Correlations_psid[i, "correlation"]<- c$rho
        Correlations_psid[i, "p"]<- round(2 * pnorm(-abs(c$rho / sqrt(c$var[1,1]))), digits = 5) #std.erro = sqrt(X$var[1,1]), p-value = 2 * pnorm(-abs(rho / std.error)))
        v1_v2 <- cbind(v1,v2)
        n<- drop_na(as.data.frame(v1_v2))
        Correlations_psid[i, "ci1"] <- round(fisherz(2*c$rho / sqrt(5)) - 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
        Correlations_psid[i, "ci2"] <- round(fisherz(2*c$rho / sqrt(5)) + 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
          # see whether there are any variables left
          } else {
           Correlations_psid[i, "correlation"] <- NA
           Correlations_psid[i, "p"]<- NA
           Correlations_psid[i, "ci1"] <- NA
           Correlations_psid[i, "ci2"] <- NA
  }
  # print(Correlations_psid)
}
Correlations_psid

## transform correlation result into matrix
cormatrix_psid <- reshape2::dcast(Correlations_psid[, c("variable1", "variable2", "correlation")], variable1~variable2, value.var="correlation") %>%
  dplyr::select(-variable1) %>%
  as.matrix(.)%>%
  `rownames<-`(colnames(.))  # rename columns

# rearrange the matrix (put mental health variables first)
cormatrix_psid<- lessR::corReorder(R= cormatrix_psid, order = "manual", vars = c(dep, satis, c1, c2, c6, i1, i2, i3, e1, e2))
cormatrix_psid

## transform p-value table into matrix
pmatrix_psid <- reshape2::dcast(Correlations_psid[, c("variable1", "variable2", "p")], variable1~variable2, value.var="p") %>%
  dplyr::select(-variable1) %>%
  as.matrix(.) %>%
  `rownames<-`(colnames(.))  # naming the rows
# rearrange the matrix (put mental health variables first)
pmatrix_psid<- lessR::corReorder(R= pmatrix_psid, order = "manual", vars = c(dep, satis, c1, c2, c6, i1, i2, i3, e1, e2))
pmatrix_psid

# -------------CI calculating PSID-------------
## cor.test with spearman cannot calculate ci, calculate them separately (same as CFPS)
corrtest_spearman <-corr.test(SES_mental_PSID[,variable_type[variable_type$type == "ordi",]$variable], y = NULL, use = "pairwise",method="spearman",adjust="holm", 
                     alpha=.05,ci=TRUE,minlength=5)
corrtest_spearman$ci
ci_spearman_name <- rownames(corrtest_spearman$ci)
ci_spearman_name
corrtest_spearman$ci$name_combine <- ci_spearman_name
corrtest_spearman$ci$name_combine2 <- ci_spearman_name
corrtest_spearman_ci2<-corrtest_spearman$ci 
corrtest_spearman_ci2
names(corrtest_spearman_ci2) <- c("lower2", "r2", "upper2", "p2", "name_combine","name_combine2")
Correlations_psid_with_ci <- Correlations_psid %>%
  dplyr::mutate(name_combine = paste0(variable1, "-", variable2)) %>%
  dplyr::mutate(name_combine2 = paste0(variable2, "-", variable1)) %>%
  dplyr::left_join(., corrtest_spearman$ci[,c("upper", "lower", "name_combine")], by = "name_combine") %>%
  dplyr::left_join(., corrtest_spearman_ci2[,c("upper2", "lower2", "name_combine2")], by = "name_combine2")%>%
  dplyr::select(variable1, variable2, correlation, p, ci1, lower, lower2, ci2, upper, upper2) %>% 
  dplyr::mutate(ci_upper = ifelse(!is.na(ci2), ci2, 
                                  ifelse(!is.na(upper), upper, upper2)),
                ci_lower = ifelse(!is.na(ci1), ci1, 
                                  ifelse(!is.na(lower), lower, lower2))) %>%
  dplyr::select(variable1, variable2, correlation, p, ci_lower, ci_upper) %>%
  dplyr::mutate(ci = paste0("[", round(ci_lower, digits = 4), ",", " ", round(ci_upper, digits= 4), "]"))
Correlations_psid_with_ci
Correlations_psid_with_ci[, c("variable1","variable2","correlation", "ci")]

# -------------plot PSID-------------
## plot correlation of all the variables
corrplot_PSID <-corrplot.mixed(cormatrix_psid, p.mat = pmatrix_psid, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)
# extract only SES variables
cormatrix_psid_ses <-cormatrix_psid[3:10, 3:10]
pmatrix_psid_ses <- pmatrix_psid[3:10, 3:10]
# plot correlation of only SES variables
corrplot_PSID_SES <-corrplot.mixed(cormatrix_psid_ses, p.mat = pmatrix_psid_ses, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)

###############2.3 Calculate inter-rater correlation coefficient of all the SES variables (ICC)###########
# calculate z-score for all SES scores
# build a table for z-scores
z_score_PSID <- SES_mental_PSID
# transfer the scores for each SES into z-score 
for(i in 1:N_SES_PSID){
  var <- colnames(SES_mental_PSID[i])
  z_score_PSID[,i] <- (SES_mental_PSID[,var] - mean(SES_mental_PSID[,var], na.rm = TRUE))/sd(SES_mental_PSID[,var], na.rm = TRUE)}
# Two-way mixed effect model, absolute agreement, single measurement
ICC_PSID_1 <- z_score_PSID %>%
  dplyr::select(1:(ncol(.)-2)) %>%
  drop_na() %>%
  icc(., model = "twoway", type = "agreement", unit = "single")
# Two-way mixed effect model, absolute agreement, average measurement
ICC_PSID_2 <- z_score_PSID %>%
  dplyr::select(1:(ncol(.)-2)) %>%
  drop_na() %>%
  icc(., model = "twoway", type = "agreement", unit = "average")
# Check ICC scores
ICC_PSID_1
ICC_PSID_2

# ---------------------------------------------------------------------------------------
# ---------- 3.  Combine two plots into one--------------------------------------------
# ---------------------------------------------------------------------------------------

#################3.1 All variables###############
# create figure of two correlation matrix in one pdf (all variables)
# CFPS & PSID
pdf("Correlational matrix.pdf",width=15,height=9)
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
# CFPS
corrplot.mixed(cormatrix_cfps, p.mat = pmatrix_cfps, insig = "blank",sig.level = 0.05,
               cl.lim = c(-0.11, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1) # text
# PSID
corrplot.mixed(cormatrix_psid, p.mat = pmatrix_psid, insig = "blank", sig.level = 0.05,
               cl.lim = c(-0.11, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix PSID", side = 1, line = -1) # text
par(opar)
dev.off()

#################3.2  Only SES variables###############
# create figure of two correlation matrix in one pdf (only SES variables)
# CFPS & PSID
pdf("Correlational matrix SES.pdf",width=15,height=9)
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
# CFPS
corrplot.mixed(cormatrix_cfps_ses, p.mat = pmatrix_cfps_ses, insig = "blank",sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1) # text
# PSID
corrplot.mixed(cormatrix_psid_ses, p.mat = pmatrix_psid_ses, insig = "blank", sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix PSID", side = 1, line = -1) # text
par(opar)
dev.off()

# ---------------------------------------------------------------------------------------
# ---------- 4.  Extract and save data frame--------------------------------------------
# ---------------------------------------------------------------------------------------

# extract correlation between SES and mental health variables
table_ses_mental_cfps <- cormatrix_cfps[3:13,1:2]
table_ses_mental_cfps_p <-pmatrix_cfps[3:13,1:2]
table_ses_mental_cfps
table_ses_mental_cfps_p
table_ses_mental_psid <- cormatrix_psid[3:10,1:2]
table_ses_mental_psid_p<- pmatrix_psid[3:10, 1:2]
table_ses_mental_psid
table_ses_mental_psid_p

write.csv(round(table_ses_mental_cfps, digits = 3), file = "table_ses_cfps.csv")
write.csv(round(table_ses_mental_psid, digits = 3), file = "table_ses_psid.csv")