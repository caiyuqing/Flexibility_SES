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
if (!require(lessR)) {install.packages("spearmanCI",repos = "http://cran.us.r-project.org"); require(lessR)}

##################CFPS######################
#mental health
# Mental health and cognition CPFS
# children
mental_CFPS <- df.CFPS_child %>%   
  dplyr::select(#from child dataframe
                wn401,	# Feel depressed and cannot cheer up
                wn402,	# Feel nervous
                wn403,	# Feel agitated or upset and cannot remain calm 
                wn404,	# Feel hopeless about the future 
                wn405,	# Feel that everything is difficult 
                wn406,	# Think life is meaningless 
                #from adult dataframe
                qq601,  # Feel depressed and cannot cheer up
                qq602,  # Feel nervous
                qq603,  # Feel agitated or upset and cannot remain calm
                qq604,  # Feel hopeless about the future 
                qq605,  # Feel that everything is difficult 
                qq606,  # Think life is meaningless 
                ### cognitive ability	
                wordtest,	
                mathtest, pid) %>%
  dplyr::na_if(., -8) %>%
  dplyr::mutate(depression1 = wn401+wn402+wn403+wn404+wn405+wn406,
                depression2 = qq601+qq602+qq603+qq604+qq605+qq606,
                cognition = wordtest + mathtest) %>%
  dplyr::mutate(depression1 = as.character(depression1),
                depression2 = as.character(depression2)) %>%
  tidyr::unite("depression",depression1:depression2, na.rm = TRUE, remove = TRUE) %>%
  dplyr::select(pid, depression, cognition) %>%
  dplyr::mutate(depression = as.numeric(depression)) %>%
  dplyr::mutate(depression = ifelse(depression <0, NA, depression))%>%
  tidyr::drop_na()

# extract data from each dataframe  
names_dataframe_cfps <- list(betan_CFPS, moog_CFPS,
                          jed_CFPS, mcder_CFPS,
                          romeo1_CFPS, romeo2_CFPS, qiu_CFPS, kim_CFPS, 
                          hanson_CFPS, leo_CFPS, ozer_CFPS)
names_paper_cfps <- c("betan", "moog","jed", "mcder",
                 "romeo1", "romeo2", "qiu", "kim", 
                 "hanson", "leo", "ozer")

# extract columns of pid and SES from all the dataframes of SES
N_SES_CFPS <- 11
dataframes_cfps <- replicate(N_SES_CFPS, data.frame())

for (i in 1:N_SES_CFPS) {
  dataframes_cfps[[i]] <- names_dataframe_cfps[[i]][, c("pid", paste0("SES_", names_paper_cfps[i], "_cfps"))]
  print(dataframes_cfps)
}
dataframes_cfps

## correlation CFPS
# merge all ordinal and continuous SES cfps and mental health 
SES_mental_CFPS <- Reduce(function(x, y) merge(x, y, by = "pid", all = TRUE), dataframes_cfps) %>%
  dplyr::left_join(., mental_CFPS, by = "pid") %>%
  dplyr::select(-pid) %>%
  dplyr::rename(dep = depression,
                cog = cognition,
                #composite SES 1-6
                c1 = SES_betan_cfps,
                c2 = SES_moog_cfps, 
                c3 = SES_jed_cfps, 
                c4 = SES_mcder_cfps,
                c5 = SES_romeo1_cfps,
                c6 = SES_romeo2_cfps,
                #income 1-3
                i1 = SES_qiu_cfps,
                i2 = SES_kim_cfps,
                i3 = SES_hanson_cfps,
                #education 1-2
                e1 = SES_leo_cfps,
                e2 = SES_ozer_cfps)        

#select ordinal variables                             
SES_mental_CFPS_ordinal <- SES_mental_CFPS[, c("dep", "cog","c1", "c2", "c3", "c4","c5", "c6", "i1", "i2","i3")]
#select dichotomous varibles
SES_mental_CFPS_dicho <- SES_mental_CFPS[,c("e1","e2")]

#McDonald’s omega
CFPS_omega <- psych::omega(SES_mental_CFPS[, 3:(N_SES_CFPS+2)])
print(c(CFPS_omega$omega_h, CFPS_omega$omega.tot))
#ICC
install.packages("irr")
library(irr)
CFPS_ICC <- irr::icc(SES_mental_CFPS[, 3:(N_SES_CFPS+2)], model = "oneway", 
                     type = "agreement")
CFPS_ICC

# extract colnames of SES_mental_CFPS
dimname <- colnames(SES_mental_CFPS)
dimname #see the names
# create an empty dataframe to store the result of correlation
N_variable_cfps <- N_SES_CFPS +2
N_correlation <- N_variable_cfps*N_variable_cfps
Correlations_cfps <- data.frame(variable1 = rep(dimname, each = N_variable_cfps),
                           variable2 = rep(dimname, N_variable_cfps),
                           correlation = rep(NA, N_correlation),
                           p = rep(NA, N_correlation),
                           ci1 = rep(NA, N_correlation),
                           ci2 = rep(NA, N_correlation)) 
# calculate correlation between SESs and mental health variables with different correlational analysis methods depend on the type of variable
for (i in 1:N_correlation) {
   v1 <- Correlations_cfps[i, "variable1"]
   v2 <- Correlations_cfps[i, "variable2"]
   # if both variables are ordinal, use spearsman
   if(v1 %in% colnames(SES_mental_CFPS_ordinal) && v2 %in% colnames(SES_mental_CFPS_ordinal)){
     a <- cor.test(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], method = "spearman", exact = FALSE)
     Correlations_cfps[i, "correlation"]<- a$estimate
     Correlations_cfps[i, "p"] <- round(a$p.value, digits = 5)
     Correlations_cfps[i, "ci1"] <- NA
     Correlations_cfps[i, "ci2"] <- NA
     # if both variables are dichonomous, use phi analysis
     } else if (!(v1 %in% colnames(SES_mental_CFPS_ordinal)) && !(v2 %in% colnames(SES_mental_CFPS_ordinal))){
       Correlations_cfps[i, "correlation"] <- phi(table(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2]))
       b <- cor.test(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], use = "complete.obs")                                       
       Correlations_cfps[i, "p"]<- round(b$p.value, digits = 5)
       Correlations_cfps[i, "ci1"] <- round(b$conf.int[1],  digits = 5)
       Correlations_cfps[i, "ci2"] <- round(b$conf.int[2],  digits = 5)
       # if one variable is dichonomous and another is ordinal, use biserial correlation (first variable is dichotomous)
       } else if (v1 %in% colnames(SES_mental_CFPS_dicho) && !(v2 %in% colnames(SES_mental_CFPS_dicho))){
         Correlations_cfps[i, "correlation"]<- biserial.cor(SES_mental_CFPS[,v2], SES_mental_CFPS[,v1], use = "complete.obs", level = 2)
         c<-cor.test(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], use = "complete.obs", level = 2)
         Correlations_cfps[i, "p"]<- c$p.value
         Correlations_cfps[i, "ci1"] <- round(c$conf.int[1],  digits = 5)
         Correlations_cfps[i, "ci2"] <- round(c$conf.int[2],  digits = 5)
         # same here, when second variable is dichonomous
         } else if (!(v1 %in% colnames(SES_mental_CFPS_dicho)) && v2 %in% colnames(SES_mental_CFPS_dicho)){
           Correlations_cfps[i, "correlation"]<- biserial.cor(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], use = "complete.obs", level = 2)
           d<-cor.test(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], use = "complete.obs", level = 2)
           Correlations_cfps[i, "p"]<- d$p.value
           Correlations_cfps[i, "ci1"] <- round(d$conf.int[1],  digits = 5)
           Correlations_cfps[i, "ci2"] <- round(d$conf.int[2],  digits = 5)
           #chek if there is any variables left 
           } else {
           Correlations_cfps[i, "correlation"] <- NA
           Correlations_cfps[i, "p"]<- NA
           Correlations_cfps[i, "ci1"] <- NA
           Correlations_cfps[i, "ci2"] <- NA
         }
     print(Correlations_cfps)
}
Correlations_cfps

#create correlation matrix
cor_cfps_new <- reshape2::dcast(Correlations_cfps[, c("variable1", "variable2", "correlation")], variable1~variable2, value.var="correlation") %>%
  dplyr::select(-variable1)
#convert dataframe into matrix
cormatrix_cfps_new<-as.matrix(cor_cfps_new)
#naming the rows
rownames(cormatrix_cfps_new) <- colnames(cormatrix_cfps_new)
#rearrange the matrix (put mental health variables first)
cormatrix_cfps_new<- lessR::corReorder(R= cormatrix_cfps_new, order = "manual", vars = c(dep, cog, c1, c2, c3, c4, c5, c6, i1, i2, i3, e1, e2))
cormatrix_cfps_new
#create p matrix
p_cfps_new <- reshape2::dcast(Correlations_cfps[, c("variable1", "variable2", "p")], variable1~variable2, value.var="p") %>%
  dplyr::select(-variable1) 
pmatrix_cfps_new<-as.matrix(p_cfps_new)
rownames(pmatrix_cfps_new) <- colnames(pmatrix_cfps_new)
#rearrange the matrix (put mental health variables first)
pmatrix_cfps_new<- lessR::corReorder(R= pmatrix_cfps_new, order = "manual", vars = c(dep, cog, c1, c2, c3, c4, c5, c6, i1, i2, i3, e1, e2))
pmatrix_cfps_new

# cor.test with spearman cannot calculate ci, calculate them separately 
corrtest <-corr.test(SES_mental_CFPS_ordinal, y = NULL, use = "pairwise",method="spearman",adjust="holm", 
                     alpha=.05,ci=TRUE,minlength=5)
corrtest$ci
#create a column for dataframe merge
corrtest$ci$name_combine <- c("dep-cog" ,"dep-c1"  ,"dep-c2"  ,"dep-c3"  ,"dep-c4"  ,"dep-c5"  ,"dep-c6"  ,"dep-i1"  ,"dep-i2"  ,"dep-i3"  ,"cog-c1"  ,"cog-c2"  ,"cog-c3"  ,"cog-c4"  ,"cog-c5"  ,"cog-c6"  ,"cog-i1"  ,"cog-i2"  ,"cog-i3"  ,"c1-c2"   ,"c1-c3"   ,"c1-c4"   ,"c1-c5"   ,"c1-c6"   ,"c1-i1"   ,"c1-i2"   ,"c1-i3"   ,"c2-c3"   ,"c2-c4"   ,"c2-c5"   ,"c2-c6"   ,"c2-i1"   ,"c2-i2"   ,"c2-i3"   ,"c3-c4"   ,"c3-c5"   ,"c3-c6"   ,"c3-i1"   ,"c3-i2"   ,"c3-i3"   ,"c4-c5"   ,"c4-c6"   ,"c4-i1"   ,"c4-i2"   ,"c4-i3"   ,"c5-c6"   ,"c5-i1"   ,"c5-i2"   ,"c5-i3"   ,"c6-i1"   ,"c6-i2"    ,"c6-i3"    ,"i1-i2"    ,"i1-i3"    ,"i2-i3")    
#because all correlations have two pairs, repeat the process
corrtest$ci$name_combine2 <- c("dep-cog" ,"dep-c1"  ,"dep-c2"  ,"dep-c3"  ,"dep-c4"  ,"dep-c5"  ,"dep-c6"  ,"dep-i1"  ,"dep-i2"  ,"dep-i3"  ,"cog-c1"  ,"cog-c2"  ,"cog-c3"  ,"cog-c4"  ,"cog-c5"  ,"cog-c6"  ,"cog-i1"  ,"cog-i2"  ,"cog-i3"  ,"c1-c2"   ,"c1-c3"   ,"c1-c4"   ,"c1-c5"   ,"c1-c6"   ,"c1-i1"   ,"c1-i2"   ,"c1-i3"   ,"c2-c3"   ,"c2-c4"   ,"c2-c5"   ,"c2-c6"   ,"c2-i1"   ,"c2-i2"   ,"c2-i3"   ,"c3-c4"   ,"c3-c5"   ,"c3-c6"   ,"c3-i1"   ,"c3-i2"   ,"c3-i3"   ,"c4-c5"   ,"c4-c6"   ,"c4-i1"   ,"c4-i2"   ,"c4-i3"   ,"c5-c6"   ,"c5-i1"   ,"c5-i2"   ,"c5-i3"   ,"c6-i1"   ,"c6-i2"    ,"c6-i3"    ,"i1-i2"    ,"i1-i3"    ,"i2-i3")    
corrtest_ci2<-corrtest$ci 
names(corrtest_ci2) <- c("lower2", "r2", "upper2", "p2", "name_combine","name_combine2")
#combine all the confidence intervals into two columns (ci_upper and ci_lower)
Correlations_cfps_with_ci <- Correlations_cfps %>%
  dplyr::mutate(name_combine = paste0(variable1, "-", variable2)) %>%
  dplyr::mutate(name_combine2 = paste0(variable2, "-", variable1)) %>%
  dplyr::left_join(., corrtest$ci[,c("upper", "lower", "name_combine")], by = "name_combine") %>%
  dplyr::left_join(., corrtest_ci2[,c("upper2", "lower2", "name_combine2")], by = "name_combine2")%>%
  dplyr::select(variable1, variable2, correlation, p, ci1, lower, lower2, ci2, upper, upper2) %>% 
  dplyr::mutate(ci_upper = ifelse(!is.na(ci2), ci2, 
                                  ifelse(!is.na(upper), upper, upper2)),
                ci_lower = ifelse(!is.na(ci1), ci1, 
                                  ifelse(!is.na(lower), lower, lower2))) %>%
  dplyr::select(variable1, variable2, correlation, p, ci_lower, ci_upper) 

##draw plot
#plot with mental health variables
corrplot_CFPS<-corrplot.mixed(cormatrix_cfps_new, p.mat = pmatrix_cfps_new, insig = "blank",sig.level = 0.05,
                              cl.lim = c(-0.111, 1), tl.cex = 0.8, number.cex = 0.8)
#extract only SES variables
cormatrix_cfps_ses_new <- cormatrix_cfps_new[3:13, 3:13]
pmatrix_cfps_ses_new <- pmatrix_cfps_new[3:13, 3:13]
#plot only SES variables
corrplot_CFPS_SES <-corrplot.mixed(cormatrix_cfps_ses_new, p.mat =pmatrix_cfps_ses_new, insig = "blank", sig.level = 0.05,
                              cl.lim = c(-0.1, 1), tl.cex = 0.8, number.cex = 0.8)

###################old version##############
n_o<- 11
n_d<- 2
#r
dimname <- c( "dep", "cog","c1",  "c2",  "c3",  "c4",  "c5",  "c6" , "i1" , "i2" , "i3",  "e1" , "e2" )
cormatrix_CFPS <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d))
colnames(cormatrix_CFPS) <- dimname #rename the matrix
rownames(cormatrix_CFPS) <- dimname
cormatrix_CFPS
#p
pmatrix_CFPS <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d), dimnames = list(colnames(SES_mental_CFPS)))
colnames(pmatrix_CFPS) <- dimname #rename the matrix
rownames(pmatrix_CFPS) <- dimname
pmatrix_CFPS

######### biserial correlation ##########
# Yuqing: I try to use polycor::polyserial but SES_leo_cfps seem not fit it 
#         polycor::polyserial(SES_mental_CFPS$c1, SES_mental_CFPS$e1, std.err = TRUE)
#         so I use ltm::biserial.cor instead
# use magicfor to store result of for loop
# library(magicfor)               
# calculate correlation between leo and other ordinal variable
# r
library(ltm)
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(biserial.cor(SES_mental_CFPS_ordinal[,i], SES_mental_CFPS$e1, use = "complete.obs", level = 2))
}
e1_biserial <- magic_result_as_dataframe()  
e1_biserial <- e1_biserial[,2] # select the vector of result
e1_biserial

# p
# extract cor.test result as the results for the p-value of point-biserial test
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(cor.test(SES_mental_CFPS_ordinal[,i], SES_mental_CFPS_dicho$e1, use = "complete.obs", level = 2))
}
e1_biserial_cortest <- magic_result_as_dataframe()  
e1_biserial_cortest <- e1_biserial_cortest[,2]
e1_biserial_cortest #see result

# extract p-value from cor.test result
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(e1_biserial_cortest[[i]]$p.value)
}
e1_biserial_p <- magic_result_as_dataframe() 
e1_biserial_p <- e1_biserial_p[,2]
e1_biserial_p<-round(e1_biserial_p, digits = 5) #see result
e1_biserial_p

# calculate correlation between ozer and other ordinal variable
# r
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(ltm::biserial.cor(SES_mental_CFPS_ordinal[,i], SES_mental_CFPS_dicho$e2, use = "complete.obs", level= 2))
}
e2_biserial <-magic_result_as_dataframe()  
e2_biserial<- e2_biserial[,2] #select the vector of result
e2_biserial

# p
# extract cor.test result as the results for the p-value of point-biserial test
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(cor.test(SES_mental_CFPS_ordinal[,i], SES_mental_CFPS_dicho$e2, use = "complete.obs", level = 2))
}
e2_biserial_cortest <-magic_result_as_dataframe()  
e2_biserial_cortest<-e2_biserial_cortest[,2]
e2_biserial_cortest[[2]] #see result

magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(e2_biserial_cortest[[i]]$p.value)
}
e2_biserial_p <- magic_result_as_dataframe() 
e2_biserial_p <- e2_biserial_p[,2]
e2_biserial_p<-round(e2_biserial_p, digits = 5) #see result
e2_biserial_p

# insert biserial correlation into the big matrix
# r
# insert columns and rows and dignol
# columns
cormatrix_CFPS[1:n_o,(n_o+1)] <- e1_biserial
cormatrix_CFPS[1:n_o,(n_o+2)] <- e2_biserial
# rows
cormatrix_CFPS[(n_o+1), 1:(n_o)]<- e1_biserial
cormatrix_CFPS[(n_o+2), 1:(n_o)]<-e2_biserial
# diagnol
diag(cormatrix_CFPS)<-1 # insert 1 in diagnal 
cormatrix_CFPS

# p
# insert columns and rows and dignol
# columns
e2_biserial_p
e1_biserial_p
pmatrix_CFPS[1:n_o,(n_o +1)] <- e1_biserial_p
pmatrix_CFPS[1:n_o,(n_o +2)] <- e2_biserial_p
pmatrix_CFPS

# rows
pmatrix_CFPS[(n_o +1), 1:(n_o)]<- e1_biserial_p
pmatrix_CFPS[(n_o +2), 1:(n_o)]<- e2_biserial_p
# diagnol
diag(pmatrix_CFPS)<-NA # insert NA in diagnal 
pmatrix_CFPS

####### phi coefficient
# calculate phi coefficient between dichotomous variables
phi_e1_e2 <-phi(table(SES_mental_CFPS_dicho))
# p-value for phi coefficient
phi_e1_e2_cortest <- cor.test(SES_mental_CFPS_dicho$e1,SES_mental_CFPS_dicho$e2, use = "complete.obs", method = "pearson")
phi_e1_e2_p <- round(phi_e1_e2_cortest$p.value, digits = 5)

# insert phi coeefficient into the big matrix
# r
cormatrix_CFPS[(n_o +1),(n_o +2)] <- phi_e1_e2
cormatrix_CFPS[(n_o +2),(n_o +1)] <- phi_e1_e2
cormatrix_CFPS #see result

# p
pmatrix_CFPS[(n_o +1),(n_o +2)] <- phi_e1_e2_p
pmatrix_CFPS[(n_o +2),(n_o +1)] <- phi_e1_e2_p
pmatrix_CFPS #see result

cormatrix_CFPS

corrplot_CFPS<-corrplot.mixed(cormatrix_CFPS, p.mat = pmatrix_CFPS, insig = "blank",sig.level = 0.05,
               cl.lim = c(-0.111, 1), tl.cex = 0.8, number.cex = 0.8)
cormatrix_CFPS_SES <- cormatrix_CFPS[3:13, 3:13]
pmatrix_CFPS_SES <- pmatrix_CFPS[3:13, 3:13]
corrplot_CFPS<-corrplot.mixed(cormatrix_CFPS_SES, p.mat = pmatrix_CFPS_SES, insig = "blank", sig.level = 0.05,
                              cl.lim = c(-0.04, 1), tl.cex = 0.8, number.cex = 0.8)

##################################### psid matrix##########################################
# Mental health and cognition PSID
mental_PSID <- df.PSID_child %>%
  dplyr::select(depression, #sum for depression: very healthy 0----24 very depressed
                life_satisfaction, #life satisfaction: completed satisfied 1----5 not at all satisfied
                pid) %>%
  dplyr::mutate(depression = ifelse(depression == 99, NA, depression),
                life_satisfaction = ifelse(life_satisfaction<1 | life_satisfaction>5, NA, life_satisfaction)) %>%
  # reverse score for depresion and life_satisfaction (higher-better mental health)
  dplyr::mutate(depression = -depression + 24,
                life_satisfaction = -life_satisfaction + 6) 
# extract data 
names_dataframe_psid <- list(betan_PSID, moog_PSID,
                             romeo2_PSID, qiu_PSID, kim_PSID, 
                             hanson_PSID, leo_PSID, ozer_PSID)
names_paper_psid <- c("betan", "moog", "romeo2", "qiu", "kim", 
                      "hanson", "leo", "ozer")
# extract columns of pid and SES from all the dataframes of SES
N_SES_PSID <- 8
dataframes_psid <- replicate(N_SES_PSID, data.frame())

for (i in 1:N_SES_PSID) {
  dataframes_psid[[i]] <- names_dataframe_psid[[i]][, c("pid", paste0("SES_", names_paper_psid[i], "_psid"))]
  print(dataframes_psid)
}
dataframes_psid

## correlation psid
# merge all ordinal and continuous SES cfps and mental health 
SES_mental_PSID <- Reduce(function(x, y) merge(x, y, by = "pid", all = TRUE), dataframes_psid) %>%
  dplyr::left_join(., mental_PSID, by = "pid") %>%
  dplyr::select(-pid) %>%
  dplyr::rename(dep = depression,
                satis = life_satisfaction,
                #composite SES 1-6
                c1 = SES_betan_psid,
                c2 = SES_moog_psid, 
                c6 = SES_romeo2_psid,
                #income 1-3
                i1 = SES_qiu_psid,
                i2 = SES_kim_psid,
                i3 = SES_hanson_psid,
                #education 1-2
                e1 = SES_leo_psid,
                e2 = SES_ozer_psid)        
SES_mental_PSID
#select ordinal variables                             
SES_mental_PSID_ordinal <- SES_mental_PSID[, c("c1", "c2", "c6", "i1", "i2","i3", "dep", "satis")]
#select dichotomous varibles
SES_mental_PSID_dicho <- SES_mental_PSID[,c("e1","e2")]

#McDonald’s omega
PSID_omega <- psych::omega(SES_mental_PSID[,3:(N_SES_PSID+2)])
print(c(PSID_omega$omega_h, PSID_omega$omega.tot))

#extract colnames of SES_mental_PSID
dimname <- colnames(SES_mental_PSID)
dimname #see the names
#build an empty dataframe for correlation result
N_variable_psid <- N_SES_PSID+2
N_correlation <- N_variable_psid*N_variable_psid
Correlations_psid <- data.frame(variable1 = rep(dimname, each = N_variable_psid),
                                variable2 = rep(dimname, N_variable_psid),
                                correlation = rep(NA, N_correlation),
                                p = rep(NA, N_correlation),
                                ci1 = rep(NA, N_correlation),
                                ci2 = rep(NA, N_correlation)) 
#calculating the correlations between varibles with different methods
for (i in 1:N_correlation) {
  v1 <- Correlations_psid[i, "variable1"]
  v2 <- Correlations_psid[i, "variable2"]
  # if both variables are ordinal, use spearman correlation
  if(v1 %in% colnames(SES_mental_PSID_ordinal) && v2 %in% colnames(SES_mental_PSID_ordinal)){
    a <- cor.test(SES_mental_PSID[,v1], SES_mental_PSID[,v2], method = "spearman", exact = FALSE)
    Correlations_psid[i, "correlation"]<- a$estimate
    Correlations_psid[i, "p"] <- round(a$p.value, digits = 5)
    Correlations_psid[i, "ci1"] <- NA
    Correlations_psid[i, "ci2"] <- NA
    #if both variables are dichonomous, use phi analysis
    } else if (!(v1 %in% colnames(SES_mental_PSID_ordinal)) && !(v2 %in% colnames(SES_mental_PSID_ordinal))){
    Correlations_psid[i, "correlation"] <- phi(table(SES_mental_PSID[,v1], SES_mental_PSID[,v2]))
    b <- cor.test(SES_mental_PSID[,v1], SES_mental_PSID[,v2], use = "complete.obs")                                       
    Correlations_psid[i, "p"]<- round(b$p.value, digits = 5)
    Correlations_psid[i, "ci1"] <- round(b$conf.int[1],  digits = 5)
    Correlations_psid[i, "ci2"] <- round(b$conf.int[2],  digits = 5)
      #if one variable is ordinal another is dichonomous, use biserial analysis (here because I use polyserial function, I calculate the p-value and ci manually)
      } else if (v1 %in% colnames(SES_mental_PSID_dicho) && !(v2 %in% colnames(SES_mental_PSID_dicho))){
      c<- polycor::polyserial(SES_mental_PSID[,v2], SES_mental_PSID[,v1], std.err = TRUE)
      Correlations_psid[i, "correlation"]<- c$rho
      Correlations_psid[i, "p"]<- 2 * pnorm(-abs(c$rho / sqrt(c$var[1,1]))) #std.erro = sqrt(X$var[1,1]), p-value = 2 * pnorm(-abs(rho / std.error)))
      v1_v2 <- SES_mental_PSID[,c(v1,v2)]
      n<- drop_na(v1_v2)
      Correlations_psid[i, "ci1"] <- round(fisherz(2*c$rho / sqrt(5)) - 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
      Correlations_psid[i, "ci2"] <- round(fisherz(2*c$rho / sqrt(5)) + 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
        #same here, just alternating the sequence of two variables
        } else if (!(v1 %in% colnames(SES_mental_PSID_dicho)) && v2 %in% colnames(SES_mental_PSID_dicho)){
        c <- polycor::polyserial(SES_mental_PSID[,v1], SES_mental_PSID[,v2], std.err = TRUE)
        Correlations_psid[i, "correlation"]<- c$rho
        Correlations_psid[i, "p"]<- 2 * pnorm(-abs(c$rho / sqrt(c$var[1,1]))) #std.erro = sqrt(X$var[1,1]), p-value = 2 * pnorm(-abs(rho / std.error)))
        v1_v2 <- SES_mental_PSID[,c(v1,v2)]
        n<- drop_na(v1_v2)
        Correlations_psid[i, "ci1"] <- round(fisherz(2*c$rho / sqrt(5)) - 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
        Correlations_psid[i, "ci2"] <- round(fisherz(2*c$rho / sqrt(5)) + 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
          #see whether there are any variables left
          } else {
           Correlations_psid[i, "correlation"] <- NA
           Correlations_psid[i, "p"]<- NA
           Correlations_psid[i, "ci1"] <- NA
           Correlations_psid[i, "ci2"] <- NA
  }
  print(Correlations_psid)
}
Correlations_psid
#transform correlation result into matrix
cor_psid_new <- reshape2::dcast(Correlations_psid[, c("variable1", "variable2", "correlation")], variable1~variable2, value.var="correlation") %>%
  dplyr::select(-variable1) 
#transform dataframs to matrix
cormatrix_psid_new <- as.matrix(cor_psid_new)
#name rows with column names
rownames(cormatrix_psid_new) <- colnames(cormatrix_psid_new)
#rearrange the matrix (put mental health variables first)
cormatrix_psid_new<- lessR::corReorder(R= cormatrix_psid_new, order = "manual", vars = c(dep, satis, c1, c2, c6, i1, i2, i3, e1, e2))
cormatrix_psid_new

#transform p-value table into matrix
p_psid_new <- reshape2::dcast(Correlations_psid[, c("variable1", "variable2", "p")], variable1~variable2, value.var="p") %>%
  dplyr::select(-variable1) 
#transform dataframs to matrix
pmatrix_psid_new<-as.matrix(p_psid_new)
#name rows with column names
rownames(pmatrix_psid_new) <- colnames(pmatrix_psid_new)
#rearrange the matrix (put mental health variables first)
pmatrix_psid_new<- lessR::corReorder(R= pmatrix_psid_new, order = "manual", vars = c(dep, satis, c1, c2, c6, i1, i2, i3, e1, e2))
pmatrix_psid_new
# cor.test with spearman cannot calculate ci, calculate them separately (same as CFPS)
corrtest <-corr.test(SES_mental_PSID_ordinal, y = NULL, use = "pairwise",method="spearman",adjust="holm", 
                     alpha=.05,ci=TRUE,minlength=5)
corrtest$ci
corrtest$ci$name_combine <- c("c1-c2",    "c1-c6",    "c1-i1",    "c1-i2",    "c1-i3",    "c1-dep",  "c1-satis","c2-c6",    "c2-i1",    "c2-i2",    "c2-i3",   "c2-dep",  "c2-satis", "c6-i1",    "c6-i2",    "c6-i3",    "c6-dep",   "c6-satis", "i1-i2",    "i1-i3",    "i1-dep",   "i1-satis", "i2-i3",   "i2-dep",  "i2-satis","i3-dep",   "i3-satis", "dep-satis")    
corrtest$ci$name_combine2 <- c("c1-c2",    "c1-c6",    "c1-i1",    "c1-i2",    "c1-i3",    "c1-dep",  "c1-satis","c2-c6",    "c2-i1",    "c2-i2",    "c2-i3",   "c2-dep",  "c2-satis", "c6-i1",    "c6-i2",    "c6-i3",    "c6-dep",   "c6-satis", "i1-i2",    "i1-i3",    "i1-dep",   "i1-satis", "i2-i3",   "i2-dep",  "i2-satis","i3-dep",   "i3-satis", "dep-satis")    
corrtest_ci2<-corrtest$ci 
names(corrtest_ci2) <- c("lower2", "r2", "upper2", "p2", "name_combine","name_combine2")
Correlations_psid_with_ci <- Correlations_psid %>%
  dplyr::mutate(name_combine = paste0(variable1, "-", variable2)) %>%
  dplyr::mutate(name_combine2 = paste0(variable2, "-", variable1)) %>%
  dplyr::left_join(., corrtest$ci[,c("upper", "lower", "name_combine")], by = "name_combine") %>%
  dplyr::left_join(., corrtest_ci2[,c("upper2", "lower2", "name_combine2")], by = "name_combine2")%>%
  dplyr::select(variable1, variable2, correlation, p, ci1, lower, lower2, ci2, upper, upper2) %>% 
  dplyr::mutate(ci_upper = ifelse(!is.na(ci2), ci2, 
                                  ifelse(!is.na(upper), upper, upper2)),
                ci_lower = ifelse(!is.na(ci1), ci1, 
                                  ifelse(!is.na(lower), lower, lower2))) %>%
  dplyr::select(variable1, variable2, correlation, p, ci_lower, ci_upper) 

#plot correlation of all the variables
corrplot_PSID <-corrplot.mixed(cormatrix_psid_new, p.mat = pmatrix_psid_new, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)
#extract only SES variables
cormatrix_psid_ses_new <-cormatrix_psid_new[3:10, 3:10]
pmatrix_psid_ses_new <- pmatrix_psid_new[3:10, 3:10]
#plot correlation of only SES variables
corrplot_PSID_SES <-corrplot.mixed(cormatrix_psid_ses_new, p.mat = pmatrix_psid_ses_new, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)

#######Combine two plots into one#########
##install.packages("PerformanceAnalytics")
#library(PerformanceAnalytics)
#chart.Correlation(SES_mental_PSID, histogram=TRUE, density = TRUE)

#create figure of two correlational matrix in one pdf (all variables)
pdf("Correlational matrix.pdf",width=15,height=9)
cormatrix_CFPS
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
corrplot.mixed(cormatrix_cfps_new, p.mat = pmatrix_cfps_new, insig = "blank",sig.level = 0.05,
               cl.lim = c(-0.11, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1)
corrplot.mixed(cormatrix_psid_new, p.mat = pmatrix_psid_new, insig = "blank", sig.level = 0.05,
               cl.lim = c(-0.11, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix PSID", side = 1, line = -1)
par(opar)
dev.off()

#create figure of two correlational matrix in one pdf (only SES variables)
pdf("Correlational matrix SES.pdf",width=15,height=9)
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
corrplot.mixed(cormatrix_cfps_ses_new, p.mat = pmatrix_cfps_ses_new, insig = "blank",sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1)
corrplot.mixed(cormatrix_psid_ses_new, p.mat = pmatrix_psid_ses_new, insig = "blank", sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix PSID", side = 1, line = -1)
par(opar)
dev.off()

#extract correlation between SES and mental health variables
table_ses_mental_cfps <- cormatrix_cfps_new[3:13,1:2]
table_ses_mental_cfps_p <-pmatrix_cfps_new[3:13,1:2]
table_ses_mental_cfps
table_ses_mental_cfps_p
table_ses_mental_psid <- cormatrix_psid_new[3:10,1:2]
table_ses_mental_psid_p<- pmatrix_psid_new[3:10, 1:2]
table_ses_mental_psid
table_ses_mental_psid_p

write.csv(round(table_ses_mental_cfps, digits = 3), file = "table_ses_cfps.csv")
write.csv(round(table_ses_mental_psid, digits = 3), file = "table_ses_psid.csv")

##################old version###############
#extract colnames of SES_mental_PSID
n_o<- 8
n_d<- 2
#r
dimname <- c( "dep", "satis","c1",  "c2",    "c6" , "i1" , "i2" , "i3",  "e1" , "e2" )
cormatrix_PSID <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d))
colnames(cormatrix_PSID) <- dimname #rename the matrix
rownames(cormatrix_PSID) <- dimname
cormatrix_PSID
#p
pmatrix_PSID <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d), dimnames = list(colnames(SES_mental_PSID)))
colnames(pmatrix_PSID) <- dimname #rename the matrix
rownames(pmatrix_PSID) <- dimname
pmatrix_PSID

#calculate the spearman correlation matrix of all ordinal variables
#library("correlation")
summary(SES_mental_PSID_ordinal)
#cor_ses_mental_psid_ordinal<- Hmisc::rcorr(as.matrix(SES_mental_PSID_ordinal), type = "spearman")
cor_ses_mental_psid_ordinal<- corr.test(as.matrix(SES_mental_PSID_ordinal), use = "pairwise",method="spearman",adjust="holm", 
          alpha=.05,ci=TRUE,minlength=5)

#insert correlation matrix into the big matrix
#r
cormatrix_PSID[1:n_o, 1:n_o] <- cor_ses_mental_psid_ordinal$r
cormatrix_PSID
#p
pmatrix_PSID[1:n_o, 1:n_o] <- cor_ses_mental_psid_ordinal$p
pmatrix_PSID<-round(pmatrix_PSID, digits = 5)
pmatrix_PSID
#ci
ci_psid<-round(cor_ses_mental_psid_ordinal$ci, digits = 3)
write.csv(ci_psid[2:13, 1:3], file ="ci_psid1.csv")
#########biserial correlation##########
#?polycor::polyserial work here
#install.packages("magicfor")
#use magicfor to store result of for loop
library(magicfor)            
#calculate correlation between leo and other ordinal variable
#polyserial result
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(polycor::polyserial(SES_mental_PSID_ordinal[,i], SES_mental_PSID$e1, std.err = TRUE))
}
e1_biserial_poly <- magic_result_as_dataframe()  #polyserial result
e1_biserial_poly <- e1_biserial_poly[,2] #select the vector of result
e1_biserial_poly

#extract r
magic_for(print, silent = TRUE)
for (i in 1:n_o) {
  print(e1_biserial_poly[[i]]$rho)
}
e1_biserial <- magic_result_as_dataframe()  
e1_biserial<-e1_biserial[,2]
e1_biserial

#extract p
magic_for(print, silent = TRUE)
for (i in 1:n_o) {
  print(2 * pnorm(-abs(e1_biserial_poly[[i]]$rho / sqrt(e1_biserial_poly[[i]]$var[1,1])))) #std.erro = sqrt(X$var[1,1]), p-value = 2 * pnorm(-abs(rho / std.error)))
}
e1_biserial_p <- magic_result_as_dataframe()  
e1_biserial_p <-e1_biserial_p[,2]
e1_biserial_p<-round(e1_biserial_p, digits = 5)

#calculate correlation between ozer and other ordinal variable
#polyserial result
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(polycor::polyserial(SES_mental_PSID_ordinal[,i], SES_mental_PSID$e2,std.err = TRUE))
}
e2_biserial_poly <-magic_result_as_dataframe()  
e2_biserial_poly<- e2_biserial_poly[,2] #select the vector of result
e2_biserial_poly

#extract r
magic_for(print, silent = TRUE)
for (i in 1:n_o) {
  print(e2_biserial_poly[[i]]$rho)
}
e2_biserial <- magic_result_as_dataframe()  
e2_biserial<- e2_biserial[,2]
e2_biserial

#extract p
magic_for(print, silent = TRUE)
for (i in 1:n_o) {
  print(2 * pnorm(-abs(e2_biserial_poly[[i]]$rho / sqrt(e2_biserial_poly[[i]]$var[1,1])))) #std.erro = sqrt(X$var[1,1]), p-value = 2 * pnorm(-abs(rho / std.error)))
}
e2_biserial_p <- magic_result_as_dataframe()  
e2_biserial_p <-e2_biserial_p[,2]
e2_biserial_p <- round(e2_biserial_p, digits = 5)
e2_biserial_p

#insert biserial correlation into the big matrix
#r
#insert columns and rows and dignol
#columns
cormatrix_PSID[1:n_o,(n_o+1)] <- e1_biserial
cormatrix_PSID[1:n_o,(n_o+2)] <- e2_biserial
#rows
cormatrix_PSID[(n_o+1), 1:(n_o)]<- e1_biserial
cormatrix_PSID[(n_o+2), 1:(n_o)]<-e2_biserial
#diagnol
diag(cormatrix_PSID)<-1 # insert 1 in diagnal 
cormatrix_PSID

#p
#insert columns and rows and dignol
#columns
pmatrix_PSID[1:n_o,(n_o +1)] <- e1_biserial_p
pmatrix_PSID[1:n_o,(n_o +2)] <- e2_biserial_p
pmatrix_PSID
#rows
pmatrix_PSID[(n_o +1), 1:n_o]<- e1_biserial_p
pmatrix_PSID[(n_o +2), 1:n_o]<-e2_biserial_p
#diagnol
diag(pmatrix_PSID)<-NA # insert NA in diagnal 
pmatrix_PSID

#######phi coefficient
#calculate phi coefficient between dichotomous variables
SES_mental_PSID_dicho <- SES_mental_PSID[, c("e1","e2")]
phi_e1_e2 <-phi(table(SES_mental_PSID_dicho))
#p-value for phi coefficient
phi_e1_e2_cortest <- cor.test(SES_mental_PSID_dicho$e1,SES_mental_PSID_dicho$e2, use = "complete.obs", method = "pearson")
phi_e1_e2_p <- round(phi_e1_e2_cortest$p.value, digits = 5)
#insert phi coeefficient into the big matrix
#r
cormatrix_PSID[(n_o +1),(n_o +2)] <- phi_e1_e2
cormatrix_PSID[(n_o +2),(n_o +1)] <- phi_e1_e2
cormatrix_PSID #see result
#p
pmatrix_PSID[(n_o +1),(n_o +2)] <- phi_e1_e2_p
pmatrix_PSID[(n_o +2),(n_o +1)] <- phi_e1_e2_p
pmatrix_PSID #see result
cor_matrix_psid_new

#correlational plot 
corrplot_PSID <-corrplot.mixed(cormatrix_PSID, p.mat = pmatrix_PSID, insig = "blank", sig.level = 0.05,
         cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)
cormatrix_PSID_SES <-cormatrix_PSID[3:10, 3:10]
pmatrix_PSID_SES<- pmatrix_PSID[3:10, 3:10]
corrplot_PSID <-corrplot.mixed(cormatrix_PSID_SES, p.mat = pmatrix_PSID_SES, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)

###########calculate ci between e1 and dep, e1 and LS, e1 and i2 by hand#########
#formula from http://www.real-statistics.com/correlation/biserial-correlation/
#e1 and dep
library(psych)
r<-polycor::polyserial(SES_mental_PSID_ordinal$dep, SES_mental_PSID$e1)
dep_e1<-SES_mental_PSID[,c("dep", "e1")]
dep_e1<-drop_na(dep_e1)
n<- count.pairwise(SES_mental_PSID_ordinal$dep, SES_mental_PSID$e1)
ub <-fisherz(2*r / sqrt(5)) + 0.5*1.96*sqrt(5 / n)
ub
lb <-fisherz(2*r / sqrt(5)) - 0.5*1.96*sqrt(5 / n)
lb
#e1 and LS
r<-polycor::polyserial(SES_mental_PSID_ordinal$LS, SES_mental_PSID$e1)
r
LS_e1<-SES_mental_PSID[,c("LS", "e1")]
LS_e1<-drop_na(LS_e1)
n<- 
ub <-fisherz(2*r / sqrt(5)) + 0.5*1.96*sqrt(5 / n)
ub <-pairwiseCount(SES_mental_PSID_ordinal$LS, SES_mental_PSID$e1)
lb <-fisherz(2*r / sqrt(5)) - 0.5*1.96*sqrt(5 / n)
lb
#e2 and dep
library(psych)
r<-polycor::polyserial(SES_mental_PSID_ordinal$dep, SES_mental_PSID$e2)
dep_e2<-SES_mental_PSID[,c("dep", "e2")]
dep_e2<-drop_na(dep_e2)
n<- pairwiseCount(SES_mental_PSID_ordinal$dep, SES_mental_PSID$e2)
ub <-fisherz(2*r / sqrt(5)) + 0.5*1.96*sqrt(5 / n)
ub
lb <-fisherz(2*r / sqrt(5)) - 0.5*1.96*sqrt(5 / n)
lb
#e2 and LS
r<-polycor::polyserial(SES_mental_PSID_ordinal$LS, SES_mental_PSID$e2)
r
LS_e2<-SES_mental_PSID[,c("LS", "e2")]
LS_e2<-drop_na(LS_e2)
n<- pairwiseCount(SES_mental_PSID_ordinal$LS, SES_mental_PSID$e2)
ub <-fisherz(2*r / sqrt(5)) + 0.5*1.96*sqrt(5 / n)
ub
lb <-fisherz(2*r / sqrt(5)) - 0.5*1.96*sqrt(5 / n)
lb
#e1 and i2
r<-polycor::polyserial(SES_mental_PSID_ordinal$i2, SES_mental_PSID$e1)
i2_e1<-SES_mental_PSID[,c("i2", "e1")]
i2_e1<-drop_na(i2_e1)
n<- 2823
ub <-fisherz(2*r / sqrt(5)) + 0.5*1.96*sqrt(5 / n)
ub
lb <-fisherz(2*r / sqrt(5)) - 0.5*1.96*sqrt(5 / n)
lb
