##################################################################################################################
##################################################################################################################
###                                                                                                            ###
###                 The 2nd R script for Flexibility of SES project                                            ###
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
###  Code authors: HU Chuan-Peng, PhD, Leibniz Institute for Resilience Research                               ###
###                                    55131 Mainz, Germany;                                                   ###
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
###     # ----------    1.1 Prepare mental health variables of ---------------------------------------#        ###
###     # ----------    1.2 Correlation relationship between SES scores and mental health scores ---- #        ###
###     # ----------    1.3 Calculate inter-rater correlation coefficient of all the SES variables----#        ###
###     # ---------- 2.  Correlation analysis of PSID ------------------------------------------------#        ###
###     # ----------    2.1 Prepare mental health variables ------------------------------------------#        ###
###     # ----------    2.2 Correlation relationship between SES scores and mental health scores ---- #        ###
###     # ----------    2.3 Calculate inter-rater correlation coefficient of all the SES variables----#        ###
###     # ---------- 3.  Combine two plots into one --------------------------------------------------#        ###
###     # ----------    3.1 All variables ----------------------------------------------------------- #        ###
###     # ----------    3.2 Only SES variables--------------------------------------------------------#        ###
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
if (!require(lessR)) {install.packages("lessR",repos = "http://cran.us.r-project.org"); require(lessR)}
if (!require(lme4)) {install.packages("lme4",repos = "http://cran.us.r-project.org"); require(lme4)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
# if (!require(jtools)) {install.packages("jtools",repos = "http://cran.us.r-project.org"); require(jtools)}

if (!require(ggcorrplot)) {install.packages("ggcorrplot",repos = "http://cran.us.r-project.org"); require(ggcorrplot)}
if (!require(corrplot)) {install.packages("corrplot",repos = "http://cran.us.r-project.org"); require(corrplot)}
if (!require(correlation)) {install.packages("correlation",repos = "http://cran.us.r-project.org"); require(correlation)}
if (!require(ltm)) {install.packages("ltm",repos = "http://cran.us.r-project.org"); require(ltm)}
if (!require(magicfor)) {install.packages("magicfor",repos = "http://cran.us.r-project.org"); require(magicfor)}
if (!require(GPArotation)) {install.packages("GPArotation",repos = "http://cran.us.r-project.org"); require(GPArotation)}
if (!require(reshape2)) {install.packages("reshape2",repos = "http://cran.us.r-project.org"); require(reshape2)}
if (!require(irr)) {install.packages("irr",repos = "http://cran.us.r-project.org"); require(irr)}

# ---------------------------------------------------------------------------------------
# ---------- 1.  Correlation analysis of CFPS--------------------------------------------
# ---------------------------------------------------------------------------------------

############# 1.1 Prepare mental health variables of CPFS ##########
## children mental health variables
load("df.CFPS_child.RData") # load data

# extract mental health variables
mental_CFPS <- df.CFPS_child %>%   
  dplyr::select(wn401,	    # Feel depressed and cannot cheer up
                wn402,	    # Feel nervous
                wn403,	    # Feel agitated or upset and cannot remain calm 
                wn404,	    # Feel hopeless about the future 
                wn405,	    # Feel that everything is difficult 
                wn406,	    # Think life is meaningless 
                # from adult dataframe (*some children defined here are in adult dataframe in the original dataset (age above 16))
                qq601,      # Feel depressed and cannot cheer up
                qq602,      # Feel nervous
                qq603,      # Feel agitated or upset and cannot remain calm
                qq604,      # Feel hopeless about the future 
                qq605,      # Feel that everything is difficult 
                qq606,      # Think life is meaningless 
                ### cognitive ability	
                wordtest,	  # word test
                mathtest,   # math test
                pid) %>%    # pid
  dplyr::na_if(., -8) %>%   # set NA
  # calculate the total score of depression and cognition
  dplyr::mutate(depression1 = wn401+wn402+wn403+wn404+wn405+wn406,   # from children data frame
                depression2 = qq601+qq602+qq603+qq604+qq605+qq606,   # from adults' data frame
                cognition = wordtest + mathtest) %>%                 # cognition (same for children and adult's questionnaire)
  dplyr::mutate(depression1 = as.character(depression1),             # convert to numeric variables
                depression2 = as.character(depression2)) %>%
  tidyr::unite("depression", depression1:depression2, 
               na.rm = TRUE, remove = TRUE) %>%                      # combine two columns of depression into one column
  dplyr::select(pid, depression, cognition) %>%                      #select only necessary variables
  dplyr::mutate(depression = as.numeric(depression))                 # convert to numeric variables

# check score
summary(mental_CFPS)

############ 1.2 Correlation relationship between SES scores and mental health scores CFPS ##############
# Load SES data
load("SES_CFPS.RData") # why dataframes_cfps is not a dataframe?
# merge all ordinal and continuous SES cfps and mental health 
SES_mental_CFPS <- dplyr::left_join(dataframes_cfps, mental_CFPS, by = "pid", all.x = TRUE) %>%  # merge SES data with mental health data
  dplyr::select(-pid) %>%          # delete the column of pid 
  dplyr::rename(dep = depression,  # rename columns
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
                e2 = SES_ozer_cfps) %>%
  dplyr::select(c(dep, cog, c1, c2, c3, c4, c5, c6, i1, i2, i3, e1, e2))
# summary(SES_mental_CFPS)

#-------------cor matrix & p-value matrix CFPS-------------
# Create a correlation table for all the correlation relationship between variables
# used Spearman for all variables

Corr_CFPS <-  psych::corr.test(SES_mental_CFPS, 
                              use = "pairwise", method="spearman", adjust="holm", 
                              alpha=.05, ci=TRUE, minlength=5)
Corr_CFPS$r
Corr_CFPS_sort <- data.frame(Corr_CFPS$ci) %>%
  dplyr::arrange(r)

psych::alpha(SES_mental_CFPS[, 3:ncol(SES_mental_CFPS)])$total$average_r # 0.52162

# -------------plot CFPS-------------
## draw plot
# plot with mental health variables
corrplot_CFPS <- corrplot::corrplot.mixed(Corr_CFPS$r, p.mat = Corr_CFPS$p, insig = "blank",sig.level = 0.05,
                         cl.lim = c(-0.12, 1), tl.cex = 0.8, number.cex = 0.8)
# extract only SES variables
cormatrix_cfps_ses <- Corr_CFPS$r[3:ncol(SES_mental_CFPS), 3:ncol(SES_mental_CFPS)]
pmatrix_cfps_ses <- Corr_CFPS$p[3:ncol(SES_mental_CFPS), 3:ncol(SES_mental_CFPS)]

# plot only SES variables
corrplot_CFPS_SES <- corrplot::corrplot.mixed(cormatrix_cfps_ses, p.mat =pmatrix_cfps_ses, insig = "blank", sig.level = 0.05,
                              cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)

############### 1.3 Calculate Intra-class correlation coefficient of all the SES variables ###########
# Here we tried 4 different approaches to transform the SES scores for ICC:
#   (1) Original data
#   (2) Z-score
#   (3) D_ij/Max_i
#   (4) (D_ij-Min_i)/(Max_i - Min_i); D_ij is data of j for the i-th SES method; Max_i/Min_i is the max/min value for the i-th SES method;
#   (5) Rank-based
# But both z-score and rank-based transformation centered all scores, therefore, make the ICC calculation impossible.
# See our previous commit for details

SES_mental_CFPS_trans <- SES_mental_CFPS %>% tidyr::drop_na()
for(i in 1: ncol(SES_mental_CFPS_trans)){
  var <- colnames(SES_mental_CFPS_trans[i])
  SES_mental_CFPS_trans[,i] <- (SES_mental_CFPS_trans[,var] - min(SES_mental_CFPS_trans[,var]))/(max(SES_mental_CFPS_trans[,var]-min(SES_mental_CFPS_trans[,var])))}

data_long_cfps <- SES_mental_CFPS_trans %>%
  dplyr::select(-dep, -cog) %>%
  dplyr::mutate(ID = 1:nrow(.)) %>%
  tidyr::gather(., SES, score,  c1:e2, factor_key=TRUE)

m_cfps <- lme4::lmer(score ~ 1 + (1|ID) + (1|SES) , data=data_long_cfps)

# jtools::summ(m1)  # ICC of ID: 0.00; ICC of SES: 0.48

# variance explained by different ways of calculating SES:
CFPS_vcv <- data.frame(VarCorr(m_cfps))
CFPS_vcv[CFPS_vcv$grp == 'SES', "vcov"]/(CFPS_vcv[CFPS_vcv$grp == 'ID', "vcov"] + CFPS_vcv[CFPS_vcv$grp == 'SES', "vcov"] 
                                         + CFPS_vcv[CFPS_vcv$grp == 'Residual', "vcov"]) # 0.44169

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
SES_mental_PSID <- dplyr::left_join(dataframes_psid, mental_PSID, by = "pid", all.x = TRUE) %>% # merge two dataset together
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
                e2 = SES_ozer_psid) %>%
  dplyr::select(c(dep, satis, c1, c2, c6, i1, i2, i3, e1, e2))

Corr_PSID <-  psych::corr.test(SES_mental_PSID, 
                               use = "pairwise", method="spearman", adjust="holm", 
                               alpha=.05, ci=TRUE, minlength=5)
Corr_PSID_sort <- data.frame(Corr_PSID$ci) %>%
  dplyr::arrange(r)

psych::alpha(SES_mental_PSID[, 3:ncol(SES_mental_PSID)])$total$average_r # 0.6225

# -------------plot PSID-------------
## plot correlation of all the variables
corrplot_PSID <- corrplot::corrplot.mixed(Corr_PSID$r, p.mat = Corr_PSID$p, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)

# extract only SES variables
cormatrix_psid_ses <-Corr_PSID$r[3:ncol(Corr_PSID$r), 3:ncol(Corr_PSID$r)]
pmatrix_psid_ses <- Corr_PSID$p[3:ncol(Corr_PSID$p), 3:ncol(Corr_PSID$p)]

# plot correlation of only SES variables
corrplot_PSID_SES <- corrplot::corrplot.mixed(cormatrix_psid_ses, p.mat = pmatrix_psid_ses, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)

############### 2.3 Calculate inter-rater correlation coefficient of all the SES variables (ICC)###########
## calculate z-score for all SES scores
## build a table for z-scores
#z_score_PSID <- drop_na(SES_mental_PSID)[NA,]
#
## transfer the scores for each SES into z-score 

SES_mental_PSID_trans <- SES_mental_PSID %>% tidyr::drop_na()
for(i in 1: ncol(SES_mental_PSID_trans)){
  var <- colnames(SES_mental_PSID_trans[i])
  SES_mental_PSID_trans[,i] <- (SES_mental_PSID_trans[,var] - min(SES_mental_PSID_trans[,var]))/(max(SES_mental_PSID_trans[,var]-min(SES_mental_PSID_trans[,var])))}

data_long_psid <- SES_mental_PSID_trans %>%
  dplyr::select(-dep, -satis) %>%
  dplyr::mutate(ID = 1:nrow(.)) %>%
  tidyr::gather(., SES, score,  c1:e2, factor_key=TRUE)

m_psid <- lme4::lmer(score ~ 1 + (1|ID) + (1|SES) , data=data_long_psid)

# jtools::summ(m1)  # ICC of ID: 0.00; ICC of SES: 0.48

# variance explained by different ways of calculating SES:
PSID_vcv <- data.frame(VarCorr(m_psid))
PSID_vcv[PSID_vcv$grp == 'SES', "vcov"]/(PSID_vcv[PSID_vcv$grp == 'ID', "vcov"] + PSID_vcv[PSID_vcv$grp == 'SES', "vcov"] 
                                         + PSID_vcv[PSID_vcv$grp == 'Residual', "vcov"]) # 0.21156

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
corrplot::corrplot.mixed(Corr_CFPS$r, p.mat = Corr_CFPS$p, insig = "blank",
               sig.level = 0.05,
               cl.lim = c(-0.12, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1) # text

# PSID
corrplot::corrplot.mixed(Corr_PSID$r, p.mat = Corr_PSID$p, insig = "blank", 
               sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)

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
corrplot::corrplot.mixed(cormatrix_cfps_ses, p.mat = pmatrix_cfps_ses, insig = "blank",sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1) # text
# PSID
corrplot::corrplot.mixed(cormatrix_psid_ses, p.mat = pmatrix_psid_ses, insig = "blank", sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix PSID", side = 1, line = -1) # text
par(opar)
dev.off()

# ---------------------------------------------------------------------------------------
# ---------- 4.  Extract and save data frame--------------------------------------------
# ---------------------------------------------------------------------------------------

# extract correlation between SES and mental health variables
table_ses_mental_cfps <- Corr_CFPS$r[3:ncol(Corr_CFPS$r),1:2]
table_ses_mental_cfps_p <- Corr_CFPS$p[3:ncol(Corr_CFPS$r),1:2]

table_ses_mental_psid <- Corr_PSID$r[3:ncol(Corr_PSID$r),1:2]
table_ses_mental_psid_p<- Corr_PSID$p[3:ncol(Corr_PSID$r), 1:2]

save(Corr_CFPS, Corr_PSID, file = "correlation_matrix.RData")
write.csv(round(table_ses_mental_cfps, digits = 3), file = "table_ses_cfps.csv")
write.csv(round(table_ses_mental_psid, digits = 3), file = "table_ses_psid.csv")

