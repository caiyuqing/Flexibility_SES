dfc0 <- read.csv("data/2010child_cfpsv37.csv", header=T, fileEncoding="UTF-8-BOM")   # CFPS 2010 children's dataset
dfa0 <- read.csv("data/2010adult_cfpsv37.csv", header=T, fileEncoding="UTF-8-BOM")   # CFPS 2010 adults' dataset
dff0 <- read.csv("data/2010family_cfpsv37.csv", header=T, fileEncoding="UTF-8-BOM")  # CFPS 2010 family' dataset
dfc0_old <- read.csv("data/2010child_v1.csv", header=T, fileEncoding="UTF-8-BOM")   # CFPS 2010 children's dataset
dfa0_old <- read.csv("data/2010adult_v1.csv", header=T, fileEncoding="UTF-8-BOM")   # CFPS 2010 adults' dataset
dff0_old <- read.csv("data/2010family_v1.csv", header=T, fileEncoding="UTF-8-BOM")  # CFPS 2010 family' dataset
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
if (!require(foreign)) {install.packages("foreign",repos = "http://cran.us.r-project.org"); require(foreign)}

# calculate c1, i2, i3 using new data
names(dfc0)<- tolower(names(dfc0))
names(dfa0)<- tolower(names(dfa0))
names(dff0)<- tolower(names(dff0))



df.individual <- dfa0 %>%
  dplyr::select(pid, fid, pid_f, pid_m, # pid, fid, pid of father, pid of mother
                gender, # gender
                qa1age, # age
                # education
                cfps2010edu_best,# highest level of education (completed): with 8 levels
                cfps2010eduy_best, # education year: 0-22
                cfps2010sch_best, # with 8 levels (for those who not finish schooling)
                feduc, # education level of father 
                meduc, # education level of mother (same coding as educ)
                qg307egp)	# Occupation classification (EGP)
                

df.family <- dff0 %>% 
  dplyr::select(# id
    fid, # fid
    # family income
    urban, # indicator of urban or rural area
    familysize, #family size
    faminc,# total family income (adjusted) = finc+welfare+fproperty+firm+felse+inc_agri
    faminc_net, # total family income (adjusted, net) = finc+welfare+fproperty+firm+felse+net_agri
    faminc_net_old, # total family income (not adjusted, net) = finc+welfare+fproperty+firm+felse+max(fk5,fk3-fk4)
    faminc_old, # total family income (not adjusted) = finc+welfare+fproperty+firm+felse+fk3
    indinc)
df.children <- dfc0 %>%
  dplyr::select(### id	
    pid,	# personal id
    fid,	# family id
    cid,	# community id
    pid_f,	# father id
    pid_m,	# mother id
    wa1age, # age
    gender) # gender
df.CFPS <- df.children %>%
  dplyr::full_join(., df.individual) %>%    # merge children and individual data
  dplyr::arrange(fid, pid) %>%              # arrange the sequence of data according to fid and pid
  dplyr::mutate(age = ifelse(is.na(qa1age), wa1age, qa1age)) %>%  # combine age in child datafrome and adult dataframe
  dplyr::mutate(role_c = ifelse(age <= 22 & age >= 10, "child", NA))%>% # select 10-22 years old as child
  dplyr::left_join(., df.family, by = "fid") %>%   # merge with family data
  dplyr::mutate(role_f = ifelse(pid %in% subset(., role_c =="child")$pid_f, 
                                'father', NA),               
                # add a column to indicate mother  or NA
                role_m = ifelse(pid %in% subset(., role_c =="child")$pid_m, 
                                'mother', NA)) %>%      
  dplyr::select(pid, fid, cid, pid_f, pid_m, role_f, role_m, role_c, everything()) %>%  # get some columns as the first few columns
  tidyr::unite("role", role_f:role_c, na.rm = TRUE, remove= TRUE) %>% # combine the columns indicating role of the individuals into one role
  dplyr::mutate(pid_mother = ifelse(role == 'child', pid_m,         # pid_mother: if the person is mother, it is her pid; if the person is a child, it is her mother's pid = pid_m
                                    ifelse(role == 'mother', pid, NA)), 
                pid_father = ifelse(role == 'child', pid_f,         # pid_father: same as pid_mother
                                    ifelse(role == 'father', pid, NA)), 
                pid_child= ifelse(role == 'child', pid, NA)) %>%    # pid_chilld: if the person is child, it is his/her pid; if not, NA
  dplyr::na_if(., -8) %>% # set all the '-8' as NA (in CFPS -8 means not applicable)
  dplyr::na_if(., -1) %>% # set all the '-1' as NA (in CFPS -1 means "I don't know")
  dplyr::na_if(., -2) %>% # set all the '-2' as NA (in CFPS -2 means "I don't want to answer"
  dplyr::na_if(., -7) %>% # set all the '-7' as NA (in CFPS -7 means "did not answer clearly. cannot categorize")
  dplyr::na_if(., -9) # set all the '-9' as NA (in CFPS -9 means "missing value"
# check the data
summary(df.CFPS)

# select children data (and parents SES) for the further analysis
df.CFPS_child <- df.CFPS %>%
  dplyr::filter(role == "child") %>% # Select data of children
  # select one child for every family
  dplyr::group_by(fid) %>% 
  arrange(pid) %>%
  dplyr::filter(row_number()==1) %>%  # hcp: what is the function of this line?
  dplyr::ungroup() %>%
  # rename variables of children to distinguish from parents
  dplyr::rename(egp_c = qg307egp, 
                eduy_c = cfps2010eduy_best,
                edu_c = cfps2010edu_best) %>% # occupation coding
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (mother)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "mother", c('pid_mother', "qg307egp", "cfps2010eduy_best")], by = 'pid_mother') %>%
  # rename variables of parents to distinguish from children (mother)
  dplyr::rename(egp_m= qg307egp,
                eduy_m = cfps2010eduy_best) %>% # occupation coding
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (father)
  dplyr::left_join(., df.CFPS[df.CFPS$role == "father", c('pid_father',"qg307egp","cfps2010eduy_best")], by = 'pid_father') %>%
  # rename variables of parents to distinguish from children (father)
  dplyr::rename(egp_f = qg307egp,
                eduy_f = cfps2010eduy_best)

# check data
summary(df.CFPS_child)

#c1
c1 <- df.CFPS_child %>%
  dplyr::mutate(itn = ifelse(urban == 0, 
                             base::cut(faminc/familysize,
                                       breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf), 
                                       labels = c("1", "2", "3", "4", "5")),
                             base::cut(faminc/familysize, 
                                       breaks = c(-0.00001, 5483.1, 5483.1*2, 5483.1*3, 5483.1*4, Inf), 
                                       labels = c("1", "2", "3", "4", "5")))) %>%   # set the ITN for every individual
  # set 7 levels for mother's education
  dplyr::mutate(edu_m_recode = base::cut(eduy_m, 
                                         breaks = c(-0.01, 9.5, 12.5, 13.5, 14.5, 16.5, 22.5), 
                                         labels = c("1", "2", "3", "4", "5", "6"))) %>% 
  dplyr::mutate(itn = as.numeric(as.character(itn)),   # convert factors into numeric variable
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>% 
  dplyr::mutate(SES_betan_cfps = (itn + edu_m_recode)/2)    # calculate composite SES score
table(c1$SES_betan_cfps)

# i2 -kim
i2 <- df.CFPS_child %>%
  dplyr::mutate(INR = ifelse(urban ==0, (faminc/familysize)/1274, (faminc/familysize)/5483.1))  %>%    # calculate INR 
  dplyr::mutate(INR = log10(INR)) %>%   # calculate INR 
  dplyr::mutate(SES_kim_cfps = ifelse(!is.finite(INR), NA, INR)) # set infinite value as NA
summary(i2$SES_kim_cfps)

# i3
i3<- df.CFPS_child %>%
  dplyr::mutate(FPL = ifelse(urban ==0, (indinc)/1274, (indinc)/5483.1)) %>%   # calculate FPL
  dplyr::mutate(SES_hanson_cfps = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>%   #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_cfps = as.numeric(as.character(SES_hanson_cfps)))  #convert SES score into numeric
# check SES score
table(i3$SES_hanson_cfps)

# c5 romeo1
c5 <- df.CFPS_child %>%
  dplyr::mutate(edu_m_recode = cut(eduy_m, 
                                     breaks = c(-0.5, 6.5, 9.5,11.5,12.5,14.5,16.5,22.5), 
                                     labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%  # recode education
  dplyr::mutate(occu_m_recode = recode(egp_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, 
                                       "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1,  .default = -8)) %>%  # recode occupation
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(occu_m_recode = as.numeric(as.character(occu_m_recode)),  # convert variables into numeric ones
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_romeo1_cfps = occu_m_recode*5 + edu_m_recode*3)   # calculate composite SES score
table(c5$SES_romeo1_cfps)

#############################old version#############
df.individual_old <- dfa0_old %>%
  dplyr::select(pid, fid, pid_f, pid_m, # pid, fid, pid of father, pid of mother
                gender, # gender
                qa1age, # age
                # education
                edu2010_t1_best,	# Best var of highest level of education attained at 2010
                educ,	# Level of education (detailed)
                eduy2010,
                qg307egp)	# Occupation classification (EGP)

df.family_old <- dff0_old %>% 
  dplyr::select(# id
    fid, # fid
    # family income
    fincome,
    finc_per,
    urban, # indicator of urban or rural area
    familysize, #family size
    faminc,# total family income (adjusted) = finc+welfare+fproperty+firm+felse+inc_agri
    faminc_net, # total family income (adjusted, net) = finc+welfare+fproperty+firm+felse+net_agri
    faminc_net_old, # total family income (not adjusted, net) = finc+welfare+fproperty+firm+felse+max(fk5,fk3-fk4)
    faminc_old, # total family income (not adjusted) = finc+welfare+fproperty+firm+felse+fk3
    indinc)
df.children_old <- dfc0_old %>%
  dplyr::select(### id	
    pid,	# personal id
    fid,	# family id
    cid,	# community id
    pid_f,	# father id
    pid_m,	# mother id
    wa1age, # age
    gender) # gender
df.CFPS_old <- df.children_old %>%
  dplyr::full_join(., df.individual_old) %>%    # merge children and individual data
  dplyr::arrange(fid, pid) %>%              # arrange the sequence of data according to fid and pid
  dplyr::mutate(age = ifelse(is.na(qa1age), wa1age, qa1age)) %>%  # combine age in child datafrome and adult dataframe
  dplyr::mutate(role_c = ifelse(age <= 22 & age >= 10, "child", NA))%>% # select 10-22 years old as child
  dplyr::left_join(., df.family_old, by = "fid") %>%   # merge with family data
  dplyr::mutate(role_f = ifelse(pid %in% subset(., role_c =="child")$pid_f, 
                                'father', NA),               
                # add a column to indicate mother  or NA
                role_m = ifelse(pid %in% subset(., role_c =="child")$pid_m, 
                                'mother', NA)) %>%      
  dplyr::select(pid, fid, cid, pid_f, pid_m, role_f, role_m, role_c, everything()) %>%  # get some columns as the first few columns
  tidyr::unite("role", role_f:role_c, na.rm = TRUE, remove= TRUE) %>% # combine the columns indicating role of the individuals into one role
  dplyr::mutate(pid_mother = ifelse(role == 'child', pid_m,         # pid_mother: if the person is mother, it is her pid; if the person is a child, it is her mother's pid = pid_m
                                    ifelse(role == 'mother', pid, NA)), 
                pid_father = ifelse(role == 'child', pid_f,         # pid_father: same as pid_mother
                                    ifelse(role == 'father', pid, NA)), 
                pid_child= ifelse(role == 'child', pid, NA)) %>%    # pid_chilld: if the person is child, it is his/her pid; if not, NA
  dplyr::na_if(., -8) %>% # set all the '-8' as NA (in CFPS -8 means not applicable)
  dplyr::na_if(., -1) %>% # set all the '-1' as NA (in CFPS -1 means "I don't know")
  dplyr::na_if(., -2) %>% # set all the '-2' as NA (in CFPS -2 means "I don't want to answer"
  dplyr::na_if(., -7) %>% # set all the '-7' as NA (in CFPS -7 means "did not answer clearly. cannot categorize")
  dplyr::na_if(., -9) # set all the '-9' as NA (in CFPS -9 means "missing value"
# check the data
summary(df.CFPS_old)

df.CFPS_old$edu
# select children data (and parents SES) for the further analysis
df.CFPS_child_old <- df.CFPS_old %>%
  dplyr::filter(role == "child") %>% # Select data of children
  # select one child for every family
  dplyr::group_by(fid) %>% 
  arrange(pid) %>%
  dplyr::filter(row_number()==1) %>%  # hcp: what is the function of this line?
  dplyr::ungroup() %>%
  # rename variables of children to distinguish from parents
  dplyr::rename(educ_c = educ, # education level
                edu2010_t1_best_c = edu2010_t1_best, # education year
                egp_c = qg307egp) %>% # occupation coding
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (mother)
  dplyr::left_join(., df.CFPS_old[df.CFPS_old$role == "mother", c('pid_mother', "qg307egp", "educ", "edu2010_t1_best")], by = 'pid_mother') %>%
  # rename variables of parents to distinguish from children (mother)
  dplyr::rename(egp_m= qg307egp,
                educ_m = educ, # education level
                edu2010_t1_best_m = edu2010_t1_best) %>%# education year 
  # select their parents, and parents SES variables (only select those will be used in the following analysis) (father)
  dplyr::left_join(., df.CFPS_old[df.CFPS_old$role == "father", c('pid_father',"qg307egp","educ", "edu2010_t1_best")], by = 'pid_father') %>%
  # rename variables of parents to distinguish from children (father)
  dplyr::rename(egp_f = qg307egp,
                educ_f = educ, # education level
                edu2010_t1_best_f = edu2010_t1_best)# education year

# check data
summary(df.CFPS_child_old$educ_m)
## old data, new method
#c1
c1_old <- df.CFPS_child_old %>%
  dplyr::mutate(itn = ifelse(urban == 0, 
                             base::cut(finc_per,
                                       breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf), 
                                       labels = c("1", "2", "3", "4", "5")),
                             base::cut(finc_per,
                                       breaks = c(-0.00001, 5483.1, 5483.1*2, 5483.1*3, 5483.1*4, Inf), 
                                       labels = c("1", "2", "3", "4", "5")))) %>%   # set the ITN for every individual
  # set 7 levels for mother's education
  dplyr::mutate(edu_m_recode = dplyr::recode(educ_m, "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1, "6" = 1, 
                                             "7" = 2, "8" = 2, "9" = 2, "10" = 2, 
                                             "11" = 3,"12" = 4,"13" = 5, "14" = 6, "15" = 7,"16" = 7)) %>% 
  dplyr::mutate(itn = as.numeric(as.character(itn)),   # convert factors into numeric variable
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>% 
  dplyr::mutate(SES_betan_cfps_old = (itn + edu_m_recode)/2)    # calculate composite SES score
table(c1_old$SES_betan_cfps_old)

# i2 -kim
i2_old <- df.CFPS_child_old %>%
  dplyr::mutate(INR = ifelse(urban ==0, finc_per/1274, finc_per/5483.1))  %>%    # calculate INR 
  dplyr::mutate(INR = log10(INR)) %>%   # calculate INR 
  dplyr::mutate(SES_kim_cfps_old = ifelse(!is.finite(INR), NA, INR)) # set infinite value as NA
summary(i2_old$SES_kim_cfps_old)

# i3
i3_old<- df.CFPS_child_old %>%
  dplyr::mutate(FPL = ifelse(urban ==0, finc_per/1274, finc_per/5483.1)) %>%   # calculate FPL
  dplyr::mutate(SES_hanson_cfps = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>%   #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_cfps_old = as.numeric(as.character(SES_hanson_cfps)))  #convert SES score into numeric
# check SES score
table(i3$SES_hanson_cfps)

# c5 
c5_old <- df.CFPS_child_old %>%
  dplyr::mutate(edu_m_recode = cut(educ_m, breaks = c(-0.5, 3.5, 5.5,6.5,10.5,13.5,14.5,16.5), labels = c("1", "2", "3", "4", "5", "6", "7"))) %>%  # recode education
  dplyr::mutate(occu_m_recode = recode(egp_m, "1"= 9, "2"=8, "3"=7,   "4"=6, "5"= 5, 
                                       "7"= 5, "8"= 4, "9"=3, "10"=2, "11"=1,  .default = -8)) %>%  # recode occupation
  dplyr::na_if(., -8) %>%  # set NA
  dplyr::mutate(occu_m_recode = as.numeric(as.character(occu_m_recode)),  # convert variables into numeric ones
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>%
  dplyr::mutate(SES_romeo1_cfps_old = occu_m_recode*5 + edu_m_recode*3)   # calculate composite SES score
table(c5_old$SES_romeo1_cfps_old)

# check correlation between c1 & c1_old; i2 & i2_old; i3 & i3_old
c1_both <- merge(c1[, c("fid", "pid", "SES_betan_cfps")], c1_old[, c("fid", "pid", "SES_betan_cfps_old")], all = TRUE)
cor.test(c1_both$SES_betan_cfps, c1_both$SES_betan_cfps_old)
i2_both <- merge(i2[, c("fid", "pid", "SES_kim_cfps")], i2_old[, c("fid", "pid", "SES_kim_cfps_old")], all = TRUE)
cor.test(i2_both$SES_kim_cfps, i2_both$SES_kim_cfps_old)
i3_both <- merge(i3[, c("fid", "pid", "SES_hanson_cfps")], i3_old[, c("fid", "pid", "SES_hanson_cfps_old")], all = TRUE)
cor.test(i3_both$SES_hanson_cfps, i3_both$SES_hanson_cfps_old)
c5_both <- merge(c5[, c("fid", "pid", "SES_romeo1_cfps")], c5_old[, c("fid", "pid", "SES_romeo1_cfps_old")], all = TRUE)
cor.test(c5_both$SES_romeo1_cfps, c5_both$SES_romeo1_cfps_old)


# old data, old method

# c1 old
c1_old2 <- df.CFPS_child_old %>%
  dplyr::mutate(itn = base::cut(finc_per, breaks = c(-0.00001, 1274, 1274*2, 1274*3, 1274*4, Inf), labels = c("1", "2", "3", "4", "5"))) %>%   # set the ITN for every individual
  # set 7 levels for mother's education
  dplyr::mutate(edu_m_recode = dplyr::recode(educ_m, "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1, "6" = 1, 
                                             "7" = 2, "8" = 2, "9" = 2, "10" = 2, 
                                             "11" = 3,"12" = 4,"13" = 5, "14" = 6, "15" = 7,"16" = 7)) %>% 
  dplyr::mutate(itn = as.numeric(as.character(itn)),   # convert factors into numeric variable
                edu_m_recode = as.numeric(as.character(edu_m_recode))) %>% 
  dplyr::mutate(SES_betan_cfps_old2 = (itn + edu_m_recode)/2)    # calculate composite SES score

# check SES score
table(c1_old2$SES_betan_cfps_old2)

# i2 old
i2_old2 <- df.CFPS_child_old %>%
  dplyr::mutate(INR = finc_per/1274)  %>%    # calculate INR 
  dplyr::mutate(INR = log10(INR)) %>%   # calculate INR 
  dplyr::mutate(SES_kim_cfps_old2 = ifelse(!is.finite(INR), NA, INR))  # set infinite value as NA
summary(i2_old2$SES_kim_cfps_old2)

# i3 old 
i3_old2 <- df.CFPS_child_old %>%
  dplyr::mutate(FPL = finc_per/1274) %>%   #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_cfps = cut(FPL, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3")))%>%   #200%, 400% of poverty line as cut point
  dplyr::mutate(SES_hanson_cfps_old2 = as.numeric(as.character(SES_hanson_cfps)))  #convert SES score into numeric
# check SES score
summary(i3_old2$SES_hanson_cfps_old2)

# c5 old
c5_old2<-c5_old # c5 does not involve income. method doe not differ

# check correlation between c1 & c1_old; i2 & i2_old; i3 & i3_old
c1_both2 <- merge(c1[, c("fid", "pid", "SES_betan_cfps")], c1_old2[, c("fid", "pid", "SES_betan_cfps_old2")], all = TRUE)
cor.test(c1_both2$SES_betan_cfps, c1_both2$SES_betan_cfps_old2)
i2_both2 <- merge(i2[, c("fid", "pid", "SES_kim_cfps")], i2_old2[, c("fid", "pid", "SES_kim_cfps_old2")], all = TRUE)
cor.test(i2_both2$SES_kim_cfps, i2_both2$SES_kim_cfps_old2)
i3_both2 <- merge(i3[, c("fid", "pid", "SES_hanson_cfps")], i3_old2[, c("fid", "pid", "SES_hanson_cfps_old2")], all = TRUE)
cor.test(i3_both2$SES_hanson_cfps, i3_both2$SES_hanson_cfps_old2)


### correlation between i2/i3 & c5
# new data, new method
i2_c5 <- merge(i2[, c("fid", "pid", "SES_kim_cfps", "urban")], c5[, c("fid", "pid", "SES_romeo1_cfps", "urban")], all = TRUE)
cor.test(i2_c5$SES_kim_cfps, i2_c5$SES_romeo1_cfps, method = "spearman")

i2_c5_urban <- i2_c5 %>%
  dplyr::filter(urban ==1)
summary(i2_c5_urban$SES_kim_cfps)
summary(i2_c5_urban$SES_romeo1_cfps)
i2_c5_rural <- i2_c5 %>%
  dplyr::filter(urban ==0)
summary(i2_c5_rural$SES_kim_cfps)
summary(i2_c5_rural$SES_romeo1_cfps)

cor.test(i2_c5_urban$SES_kim_cfps, i2_c5_urban$SES_romeo1_cfps, method = "spearman")
cor.test(i2_c5_rural$SES_kim_cfps, i2_c5_rural$SES_romeo1_cfps, method = "spearman")

# old data, new method
i2_c5_old <- merge(i2_old[, c("fid", "pid", "SES_kim_cfps_old", "urban")], c5_old[, c("fid", "pid", "SES_romeo1_cfps_old")], all = TRUE)
cor.test(i2_c5_old$SES_kim_cfps_old, i2_c5_old$SES_romeo1_cfps_old,method = "spearman")
i2_c5_old_urban <- i2_c5_old %>%
  dplyr::filter(urban ==1)
i2_c5_old_rural <- i2_c5_old %>%
  dplyr::filter(urban ==0)
cor.test(i2_c5_old_urban$SES_kim_cfps_old, i2_c5_old_urban$SES_romeo1_cfps_old, method = "spearman")
cor.test(i2_c5_old_rural$SES_kim_cfps_old, i2_c5_old_rural$SES_romeo1_cfps_old, method = "spearman")


# old data, old method
i2_c5_old2 <- merge(i2_old2[, c("fid", "pid", "SES_kim_cfps_old2", "urban")], c5_old2[, c("fid", "pid", "SES_romeo1_cfps_old")], all = TRUE)
cor.test(i2_c5_old2$SES_kim_cfps_old2, i2_c5_old2$SES_romeo1_cfps_old,method = "spearman")

i2_c5_old2_urban <- i2_c5_old2 %>%
  dplyr::filter(urban ==1)
i2_c5_old2_rural <- i2_c5_old2 %>%
  dplyr::filter(urban ==0)
cor.test(i2_c5_old2_urban$SES_kim_cfps_old2, i2_c5_old2_urban$SES_romeo1_cfps_old, method = "spearman")
cor.test(i2_c5_old2_rural$SES_kim_cfps_old2, i2_c5_old2_rural$SES_romeo1_cfps_old, method = "spearman")

#check correlation between SES and income (indinc = faminc/familysize)
cor.test(i2$SES_kim_cfps, i2$indinc,method = "spearman") #0.64
cor.test(i3$SES_hanson_cfps, i3$indinc,method = "spearman") #0.56
cor.test(c1$SES_betan_cfps, c1$indinc,method = "spearman") #0.65
cor.test(c5$SES_romeo1_cfps, c5$indinc, method = "spearman") #0.46

# c5 new data, old method: I think the problem is the new method.
i2_newdata_oldmethod <- df.CFPS_child %>%
  dplyr::mutate(INR = faminc/1274)  %>%    # calculate INR 
  dplyr::mutate(INR = log10(INR)) %>%   # calculate INR 
  dplyr::mutate(SES_kim_cfps_newdata_oldmethod = ifelse(!is.finite(INR), NA, INR))  # set infinite value as NA
summary(i2_newdata_oldmethod$SES_kim_cfps_newdata_oldmethod)
i2_newdata_bothmethod <- merge(i2[, c("fid", "SES_kim_cfps", "urban")], i2_newdata_oldmethod[,c("fid", "SES_kim_cfps_newdata_oldmethod")], all = TRUE)
cor.test(i2_newdata_bothmethod$SES_kim_cfps, i2_newdata_bothmethod$SES_kim_cfps_newdata_oldmethod, method = "spearman")
i2_c5_newdata_oldmethod <-merge(i2_newdata_oldmethod[, c("pid", "SES_kim_cfps_newdata_oldmethod", "urban")], c5[, c("pid", "SES_romeo1_cfps")], all = TRUE)
cor.test(i2_c5_newdata_oldmethod$SES_kim_cfps_newdata_oldmethod, i2_c5_newdata_oldmethod$SES_romeo1_cfps, method = "spearman")
i2_c5_newdata_oldmetod_urban <- i2_c5_newdata_oldmethod %>%
  dplyr::filter(urban ==1)
i2_c5_newdata_oldmetod_rural <- i2_c5_newdata_oldmethod %>%
  dplyr::filter(urban ==0)
cor.test(i2_c5_newdata_oldmetod_urban$SES_kim_cfps_newdata_oldmethod, i2_c5_newdata_oldmetod_urban$SES_romeo1_cfps, method = "spearman")
cor.test(i2_c5_newdata_oldmetod_rural$SES_kim_cfps_newdata_oldmethod, i2_c5_newdata_oldmetod_rural$SES_romeo1_cfps, method = "spearman")


#check whether total family income = average family income * familysize
# v1: fincome, finc_per
# cfpsv37: faminc, indinc
income_check_old <-df.family_old %>%
  dplyr::mutate(fincome_per = fincome/familysize) %>%
  dplyr::mutate(diff = fincome_per-finc_per) %>%
  dplyr::select(diff, finc_per,fincome, fincome_per, familysize) 
income_check_old <- round(income_check, digits = 3) %>%
  dplyr::filter(diff !=0) #39
summary(tmp$diff)

income_check <-df.family %>%
  dplyr::mutate(faminc_per = faminc/familysize) %>%
  dplyr::mutate(diff = faminc_per -indinc)%>%
  dplyr::select(diff, indinc,faminc, faminc_per, familysize)
income_check <- round(income_check, digits = 1) %>%
  dplyr::filter(diff !=0) #0


# check correlation between different family income
income_all <- merge(df.CFPS_child[, c("fid", "faminc", "indinc")], df.CFPS_child_old[, c("fid", "finc_per", "fincome")], all = TRUE)
cor.test(income_all$faminc, income_all$fincome)
cor.test(income_all$indinc, income_all$finc_per)
cor.test(income_all$faminc, income_all$finc_per)
cor.test(income_all$faminc, income_all$indinc)


####rural vs urban
rural_adult <- df.individual %>%
  dplyr::filter(urban == 0) %>%
  dplyr::select(cfps2010edu_best, cfps2010eduy_best, cfps2010sch_best,income)
summary(rural_adult)
urban_adult <- df.individual %>%
  dplyr::filter(urban == 1) %>%
  dplyr::select(cfps2010edu_best, cfps2010eduy_best, cfps2010sch_best,income)
summary(urban_adult)

rural_family <- df.family %>%
  dplyr::filter(urban ==0) %>%
  dplyr::mutate(indinc_std = indinc/1274)%>%
  dplyr::select(faminc, indinc, indinc_std)
summary(rural_family)
urban_family <- df.family %>%
  dplyr::filter(urban ==1) %>%
  dplyr::mutate(indinc_std = indinc/5483.1) %>%
  dplyr::select(faminc, indinc,indinc_std) 
summary(urban_family)
