################## Correlation analysis for "Flexibility of SES" project ##############################
# 
# Author      Date(yy-mm-dd)   Change History
# =================================================
# Cai, Y-Q    20-03-15         The first version
# Hu, C-P     20-04-27         Validate the script
# 
# 
# ============= Notes about data ==================
#   CFPS: there are some families in which two or more children has different mother (i.e., pid_m)
#   PSID: there are one family with two female adults; there are families without child.
#   To be consistent across the index reproduced, for all children and adolescents' SES, 
#   We used data from participant with age ranged 10-22 yrs-old.
#
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
if (!require(lessR)) {install.packages("spearmanCI",repos = "http://cran.us.r-project.org"); require(lessR)}

################## CFPS ######################
# Mental health and cognition CPFS
# children
load("df.CFPS_child.RData")
mental_CFPS <- df.CFPS_child %>%   
  dplyr::select(# from child dataframe
                wn401,	# Feel depressed and cannot cheer up
                wn402,	# Feel nervous
                wn403,	# Feel agitated or upset and cannot remain calm 
                wn404,	# Feel hopeless about the future 
                wn405,	# Feel that everything is difficult 
                wn406,	# Think life is meaningless 
                # from adult dataframe
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

# import SES data
load("SES_CFPS.RData")
## correlation CFPS
# merge all ordinal and continuous SES cfps and mental health 
SES_mental_CFPS <- Reduce(function(x, y) merge(x, y, by = "pid", all = TRUE), dataframes_cfps) %>%
  dplyr::left_join(., mental_CFPS, by = "pid", all.x = TRUE) %>%
  dplyr::select(-pid) %>%
  dplyr::rename(dep = depression,
                cog = cognition,
                # composite SES 1-6
                c1 = SES_betan_cfps, # hcp: we need to think about this part, make it scalable.
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

# Select ordinal variables                             
SES_mental_CFPS_ordinal <- SES_mental_CFPS[, c("dep", "cog","c1", "c2", "c3", "c4","c5", "c6", "i1", "i2","i3")]

# Select dichotomous varibles
SES_mental_CFPS_dicho <- SES_mental_CFPS[,c("e1","e2")]

# McDonald’s omega
CFPS_omega <- SES_mental_CFPS %>%
  dplyr::select(2:ncol(.)) %>%
  psych::omega()

print(c(CFPS_omega$omega_h, CFPS_omega$omega.tot))

# Extract colnames of SES_mental_CFPS
dimname <- colnames(SES_mental_CFPS)
# dimname #see the names
# create an empty dataframe to store the result of correlation
N_variable_cfps <- ncol(SES_mental_CFPS)
N_SES_CFPS <- N_variable_cfps - 2
N_correlation <- N_variable_cfps*N_variable_cfps
# determine the type of each of varible (dichomonomous or ordinal)
variable_type <- data_frame(variable = dimname,
                            type = as.character(NA)) 
for (i in 1:N_variable_cfps) {
  var <- as.character(variable_type[i, "variable"])  
  if(length(unique(na.omit(SES_mental_CFPS[,var]))) ==2){
    variable_type[i, "type"] <- "bin"} else if(length(unique(na.omit(SES_mental_CFPS[,var]))) > 2){
      variable_type[i, "type"] <- "ordi"} else {variable_type[i, "type"] <- NA}
}

#build a correlation table to store the results
Correlations_cfps <- data.frame(variable1 = as.character(rep(dimname, each = N_variable_cfps)),
                           variable2 = as.character(rep(dimname, N_variable_cfps)),
                           correlation = rep(NA, N_correlation),
                           p = rep(NA, N_correlation),
                           ci1 = rep(NA, N_correlation),
                           ci2 = rep(NA, N_correlation)) 
# calculate correlation between SESs and mental health variables with different correlational analysis methods depend on the type of variable
for (i in 1:N_correlation) {
   v1 <- SES_mental_CFPS %>% dplyr::select(as.character(Correlations_cfps[i, "variable1"])) %>% dplyr::pull()
   v2 <- SES_mental_CFPS %>% dplyr::select(as.character(Correlations_cfps[i, "variable2"])) %>% dplyr::pull()
   # if both variables are ordinal, use spearsman
   if(dplyr::n_distinct(v1, na.rm = T) > 2 && dplyr::n_distinct(v2, na.rm = T) > 2){
   # if(variable_type[variable_type$variable == v1, "type"] == "ordi" && variable_type[variable_type$variable == v2, "type"] == "ordi"){
     
     #a <- cor.test(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], method = "spearman", exact = FALSE)
     a <- cor.test(v1, v2, method = "spearman", exact = FALSE)
     Correlations_cfps[i, "correlation"]<- a$estimate
     Correlations_cfps[i, "p"] <- round(a$p.value, digits = 5)
     Correlations_cfps[i, "ci1"] <- NA
     Correlations_cfps[i, "ci2"] <- NA
     # if both variables are dichonomous, use phi analysis
     } else if (dplyr::n_distinct(v1, na.rm = T) == 2 && dplyr::n_distinct(v2, na.rm = T) == 2){
       Correlations_cfps[i, "correlation"] <- phi(table(v1, v2))
       b <- cor.test(v1, v2, use = "complete.obs")                                       
#     } else if(variable_type[variable_type$variable == v1, "type"] == "bin" && variable_type[variable_type$variable == v2, "type"] == "bin"){
#       Correlations_cfps[i, "correlation"] <- phi(table(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2]))
#       b <- cor.test(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], use = "complete.obs")                                       
       Correlations_cfps[i, "p"]<- round(b$p.value, digits = 5)
       Correlations_cfps[i, "ci1"] <- round(b$conf.int[1],  digits = 5)
       Correlations_cfps[i, "ci2"] <- round(b$conf.int[2],  digits = 5)
       
       # if one variable is dichonomous and another is ordinal, use biserial correlation (first variable is dichotomous)
       } else if (dplyr::n_distinct(v1, na.rm = T) == 2  &&  dplyr::n_distinct(v2, na.rm = T) > 2){
         
         Correlations_cfps[i, "correlation"] <- biserial.cor(v2, v1, use = "complete.obs", level = 2)
         c<-cor.test(v1, v2, use = "complete.obs", level = 2)
         Correlations_cfps[i, "p"] <- c$p.value
#       } else if (variable_type[variable_type$variable == v1, "type"] == "bin" && variable_type[variable_type$variable == v2, "type"] == "ordi"){
#         Correlations_cfps[i, "correlation"]<- biserial.cor(SES_mental_CFPS[,v2], SES_mental_CFPS[,v1], use = "complete.obs", level = 2)
#         c<-cor.test(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], use = "complete.obs", level = 2)
#         Correlations_cfps[i, "p"]<- round(c$p.value, digits = 5)
         Correlations_cfps[i, "ci1"] <- round(c$conf.int[1],  digits = 5)
         Correlations_cfps[i, "ci2"] <- round(c$conf.int[2],  digits = 5)
         
         # same here, when second variable is dichonomous
         } else if (dplyr::n_distinct(v1, na.rm = T) > 2 && dplyr::n_distinct(v2, na.rm = T) == 2 ){
           
           Correlations_cfps[i, "correlation"]<- biserial.cor(v1, v2, use = "complete.obs", level = 2)
           d<-cor.test(v1, v2, use = "complete.obs", level = 2)
           Correlations_cfps[i, "p"] <- d$p.value
#         } else if (variable_type[variable_type$variable == v1, "type"] == "ordi" && variable_type[variable_type$variable == v2, "type"] == "bin"){
#           Correlations_cfps[i, "correlation"]<- biserial.cor(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], use = "complete.obs", level = 2)
#           d<-cor.test(SES_mental_CFPS[,v1], SES_mental_CFPS[,v2], use = "complete.obs", level = 2)
#           Correlations_cfps[i, "p"]<- round(d$p.value, digits = 5)
           Correlations_cfps[i, "ci1"] <- round(d$conf.int[1],  digits = 5)
           Correlations_cfps[i, "ci2"] <- round(d$conf.int[2],  digits = 5)
           #chek if there is any variables left 
           } else {
           Correlations_cfps[i, "correlation"] <- NA
           Correlations_cfps[i, "p"] <- NA
           Correlations_cfps[i, "ci1"] <- NA
           Correlations_cfps[i, "ci2"] <- NA
         }
     #print(Correlations_cfps)
}
Correlations_cfps

#create correlation matrix
cormatrix_cfps <- reshape2::dcast(Correlations_cfps[, c("variable1", "variable2", "correlation")], variable1~variable2, value.var="correlation") %>%
  dplyr::select(-variable1) %>%
  as.matrix(.)
#naming the rows
rownames(cormatrix_cfps) <- colnames(cormatrix_cfps)
#rearrange the matrix (put mental health variables first)
cormatrix_cfps<- lessR::corReorder(R= cormatrix_cfps, order = "manual", vars = c(dep, cog, c1, c2, c3, c4, c5, c6, i1, i2, i3, e1, e2))
cormatrix_cfps
#create p matrix
pmatrix_cfps <- reshape2::dcast(Correlations_cfps[, c("variable1", "variable2", "p")], variable1~variable2, value.var="p") %>%
  dplyr::select(-variable1) %>%
  as.matrix(.)
rownames(pmatrix_cfps) <- colnames(pmatrix_cfps)
#rearrange the matrix (put mental health variables first)
pmatrix_cfps<- lessR::corReorder(R= pmatrix_cfps, order = "manual", vars = c(dep, cog, c1, c2, c3, c4, c5, c6, i1, i2, i3, e1, e2))
pmatrix_cfps

# cor.test with spearman cannot calculate ci, calculate them separately 
corrtest_spearman <-corr.test(SES_mental_CFPS[, variable_type[variable_type$type == "ordi",]$variable], y = NULL, use = "pairwise",method="spearman",adjust="holm", 
                     alpha=.05,ci=TRUE,minlength=5)
ci_spearman_name <- rownames(corrtest_spearman$ci)
ci_spearman_name
#create a column for dataframe merge
corrtest_spearman$ci$name_combine <- ci_spearman_name
#because all correlations have two pairs, repeat the process
corrtest_spearman$ci$name_combine2 <- ci_spearman_name
corrtest_spearman$ci
corrtest_spearman_ci2<-corrtest_spearman$ci
names(corrtest_spearman_ci2) <- c("lower2", "r2", "upper2", "p2", "name_combine","name_combine2")
corrtest_spearman_ci2
#combine all the confidence intervals into two columns (ci_upper and ci_lower)
Correlations_cfps_with_ci <- Correlations_cfps %>%
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
Correlations_cfps_with_ci
Correlations_cfps_with_ci[, c("variable1","variable2","correlation", "ci")]

##draw plot
#plot with mental health variables
corrplot_CFPS<-corrplot.mixed(cormatrix_cfps, p.mat = pmatrix_cfps, insig = "blank",sig.level = 0.05,
                              cl.lim = c(-0.111, 1), tl.cex = 0.8, number.cex = 0.8)
#extract only SES variables
cormatrix_cfps_ses <- cormatrix_cfps[3:13, 3:13]
pmatrix_cfps_ses <- pmatrix_cfps[3:13, 3:13]
#plot only SES variables
corrplot_CFPS_SES <-corrplot.mixed(cormatrix_cfps_ses, p.mat =pmatrix_cfps_ses, insig = "blank", sig.level = 0.05,
                              cl.lim = c(-0.1, 1), tl.cex = 0.8, number.cex = 0.8)

#McDonald’s omega
CFPS_omega <- psych::omega(SES_mental_CFPS[, 3:(N_SES_CFPS+2)])
print(c(CFPS_omega$omega_h, CFPS_omega$omega.tot))

##################################### PSID matrix##########################################
# Mental health and cognition PSID
load("df.PSID_child.RData")
mental_PSID <- df.PSID_child %>%
  dplyr::select(depression, #sum for depression: very healthy 0----24 very depressed
                life_satisfaction, #life satisfaction: completed satisfied 1----5 not at all satisfied
                pid) %>%
  dplyr::mutate(depression = ifelse(depression == 99, NA, depression),
                life_satisfaction = ifelse(life_satisfaction<1 | life_satisfaction>5, NA, life_satisfaction)) %>%
  # reverse score for depresion and life_satisfaction (higher-better mental health)
  dplyr::mutate(depression = -depression + 24,
                life_satisfaction = -life_satisfaction + 6) 

# load SES data
load("SES_PSID.RData")

## correlation psid

# merge all ordinal and continuous SES cfps and mental health
SES_mental_PSID <- Reduce(function(x, y) merge(x, y, by = "pid", all = TRUE), dataframes_psid) %>%
  dplyr::left_join(., mental_PSID, by = "pid", all.x = TRUE) %>%
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

#extract colnames of SES_mental_PSID
dimname <- colnames(SES_mental_PSID)
dimname #see the names

# extract number of variable
N_variable_psid <- ncol(SES_mental_PSID) #number of all variables
N_SES_PSID <- N_variable_psid- 2 #number of ses variables
N_correlation <- N_variable_psid*N_variable_psid
# determine the type of each of varible (dichomonomous or ordinal)
variable_type <- data_frame(variable = dimname,
                            type = as.character(NA)) 
for (i in 1:N_variable_psid) {
  var <- as.character(variable_type[i, "variable"])  
  if(length(unique(na.omit(SES_mental_PSID[,var]))) ==2){
    variable_type[i, "type"] <- "bin"} else if(length(unique(na.omit(SES_mental_PSID[,var]))) > 2){
      variable_type[i, "type"] <- "ordi"} else {variable_type[i, "type"] <- NA}
}

#build an empty dataframe for correlation result
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
  if(variable_type[variable_type$variable == v1, "type"] == "ordi" && variable_type[variable_type$variable == v2, "type"] == "ordi"){
    a <- cor.test(SES_mental_PSID[,v1], SES_mental_PSID[,v2], method = "spearman", exact = FALSE)
    Correlations_psid[i, "correlation"]<- a$estimate
    Correlations_psid[i, "p"] <- round(a$p.value, digits = 5)
    Correlations_psid[i, "ci1"] <- NA
    Correlations_psid[i, "ci2"] <- NA
    #if both variables are dichonomous, use phi analysis
    } else if (variable_type[variable_type$variable == v1, "type"] == "bin" && variable_type[variable_type$variable == v2, "type"] == "bin"){
    Correlations_psid[i, "correlation"] <- phi(table(SES_mental_PSID[,v1], SES_mental_PSID[,v2]))
    b <- cor.test(SES_mental_PSID[,v1], SES_mental_PSID[,v2], use = "complete.obs")                                       
    Correlations_psid[i, "p"]<- round(b$p.value, digits = 5)
    Correlations_psid[i, "ci1"] <- round(b$conf.int[1],  digits = 5)
    Correlations_psid[i, "ci2"] <- round(b$conf.int[2],  digits = 5)
      #if one variable is ordinal another is dichonomous, use biserial analysis (here because I use polyserial function, I calculate the p-value and ci manually)
      } else if (variable_type[variable_type$variable == v1, "type"] == "bin" && variable_type[variable_type$variable == v2, "type"] == "ordi"){
      c<- polycor::polyserial(SES_mental_PSID[,v2], SES_mental_PSID[,v1], std.err = TRUE)
      Correlations_psid[i, "correlation"]<- c$rho
      Correlations_psid[i, "p"]<- round(2 * pnorm(-abs(c$rho / sqrt(c$var[1,1]))), digits = 5) #std.erro = sqrt(X$var[1,1]), p-value = 2 * pnorm(-abs(rho / std.error)))
      v1_v2 <- SES_mental_PSID[,c(v1,v2)]
      n<- drop_na(v1_v2)
      Correlations_psid[i, "ci1"] <- round(fisherz(2*c$rho / sqrt(5)) - 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
      Correlations_psid[i, "ci2"] <- round(fisherz(2*c$rho / sqrt(5)) + 0.5*1.96*sqrt(5 / nrow(n)),  digits = 5)
        #same here, just alternating the sequence of two variables
        } else if (variable_type[variable_type$variable == v1, "type"] == "ordi" && variable_type[variable_type$variable == v2, "type"] == "bin"){
        c <- polycor::polyserial(SES_mental_PSID[,v1], SES_mental_PSID[,v2], std.err = TRUE)
        Correlations_psid[i, "correlation"]<- c$rho
        Correlations_psid[i, "p"]<- round(2 * pnorm(-abs(c$rho / sqrt(c$var[1,1]))), digits = 5) #std.erro = sqrt(X$var[1,1]), p-value = 2 * pnorm(-abs(rho / std.error)))
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
  #print(Correlations_psid)
}
Correlations_psid
#transform correlation result into matrix
cormatrix_psid <- reshape2::dcast(Correlations_psid[, c("variable1", "variable2", "correlation")], variable1~variable2, value.var="correlation") %>%
  dplyr::select(-variable1) %>%
  as.matrix(.)
#name rows with column names
rownames(cormatrix_psid) <- colnames(cormatrix_psid)
#rearrange the matrix (put mental health variables first)
cormatrix_psid<- lessR::corReorder(R= cormatrix_psid, order = "manual", vars = c(dep, satis, c1, c2, c6, i1, i2, i3, e1, e2))
cormatrix_psid

#transform p-value table into matrix
pmatrix_psid <- reshape2::dcast(Correlations_psid[, c("variable1", "variable2", "p")], variable1~variable2, value.var="p") %>%
  dplyr::select(-variable1) %>%
  as.matrix(.)
#name rows with column names
rownames(pmatrix_psid) <- colnames(pmatrix_psid)
#rearrange the matrix (put mental health variables first)
pmatrix_psid<- lessR::corReorder(R= pmatrix_psid, order = "manual", vars = c(dep, satis, c1, c2, c6, i1, i2, i3, e1, e2))
pmatrix_psid
# cor.test with spearman cannot calculate ci, calculate them separately (same as CFPS)
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

#plot correlation of all the variables
corrplot_PSID <-corrplot.mixed(cormatrix_psid, p.mat = pmatrix_psid, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)
#extract only SES variables
cormatrix_psid_ses <-cormatrix_psid[3:10, 3:10]
pmatrix_psid_ses <- pmatrix_psid[3:10, 3:10]
#plot correlation of only SES variables
corrplot_PSID_SES <-corrplot.mixed(cormatrix_psid_ses, p.mat = pmatrix_psid_ses, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)
#McDonald’s omega
PSID_omega <- psych::omega(SES_mental_PSID[,3:(N_SES_PSID+2)])
print(c(PSID_omega$omega_h, PSID_omega$omega.tot))

#######Combine two plots into one#########

# create figure of two correlational matrix in one pdf (all variables)
pdf("Correlational matrix.pdf",width=15,height=9)
cormatrix_CFPS
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
corrplot.mixed(cormatrix_cfps, p.mat = pmatrix_cfps, insig = "blank",sig.level = 0.05,
               cl.lim = c(-0.11, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1)
corrplot.mixed(cormatrix_psid, p.mat = pmatrix_psid, insig = "blank", sig.level = 0.05,
               cl.lim = c(-0.11, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix PSID", side = 1, line = -1)
par(opar)
dev.off()

# create figure of two correlational matrix in one pdf (only SES variables)
pdf("Correlational matrix SES.pdf",width=15,height=9)
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
corrplot.mixed(cormatrix_cfps_ses, p.mat = pmatrix_cfps_ses, insig = "blank",sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1)
corrplot.mixed(cormatrix_psid_ses, p.mat = pmatrix_psid_ses, insig = "blank", sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix PSID", side = 1, line = -1)
par(opar)
dev.off()

#extract correlation between SES and mental health variables
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