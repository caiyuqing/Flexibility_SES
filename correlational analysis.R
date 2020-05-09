library(tidyverse)

#################################mental health###################
# Mental health and cognition CPFS
# children
mental_children_cfps <- df.children %>%     
  dplyr::select(wn401,	# Feel depressed and cannot cheer up
                wn402,	# Feel nervous
                wn403,	# Feel agitated or upset and cannot remain calm 
                wn404,	# Feel hopeless about the future 
                wn405,	# Feel that everything is difficult 
                wn406,	# Think life is meaningless 
                ### cognitive ability	
                wordtest,	
                mathtest, pid) 
mental_children_cfps[mental_children_cfps == -8] <- NA
mental_children_cfps <- mental_children_cfps %>%
  dplyr::mutate(depression = wn401+wn402+wn403+wn404+wn405+wn406,
                cognition = wordtest + mathtest) %>%
  dplyr::select(pid, depression, cognition)
mental_children_cfps[mental_children_cfps < 0] <-NA
mental_children_cfps <- drop_na(mental_children_cfps) #3355 left

# adult
mental_adult_cfps <- df.individual %>%
  dplyr::select(depression, wordtest, mathtest, pid)%>%  #depression sum score, word/math test
  dplyr::mutate(cognition = wordtest + mathtest) %>%
  dplyr::select(pid, depression, cognition)
mental_adult_cfps <- drop_na(mental_adult_cfps)

# Mental health and cognition psid
mental_psid <- df.psid %>%
  dplyr::select(ER70680, #sum for depression: very healthy 0----24 very depressed
                ER66025, #life satisfaction: completed satisfied 1----5 not at all satisfied
                ER34501,ER30002) #fid, pid
names(mental_psid) <- c("depression", "life_satisfaction", "fid", "pid")
mental_psid$depression[mental_psid$depression == 99] <-NA
mental_psid$life_satisfaction[mental_psid$life_satisfaction >5] <-NA
mental_psid$life_satisfaction[mental_psid$life_satisfaction <1] <-NA

# reverse score for depresion and life_satisfaction (higher-better mental health)
mental_psid <- mental_psid %>%
  dplyr::mutate(depression = -depression + 24,
                life_satisfaction = -life_satisfaction + 6) 

####################################### extract data ############################################
# extract personal coding and SES indicator for each study 
# betan
SES_betan_child_cfps <- betan_child_cfps[,c("pid", "SES_betan_cfps")]
SES_betan_child_psid <- betan_child_psid[,c("pid", "fid", "SES_betan_psid")]
# moog
SES_moog_child_cfps <- moog_child_cfps[,c("pid", "SES_moog_cfps")]
SES_moog_child_psid <- moog_child_psid[,c("pid", "fid", "SES_moog_psid")]
# Jed
SES_jed_child_cfps <- jed_cfps[,c("pid", "SES_jed_cfps")]
# mcder
SES_mcder_child_cfps <- mcder_child_cfps[,c("pid", "SES_mcder_cfps")]
# romeo1
SES_romeo1_child_cfps <- romeo1_child_cfps[,c("pid", "SES_romeo1_cfps")]
# romeo2
SES_romeo2_child_cfps <- romeo2_child_cfps[,c("pid", "SES_romeo2_cfps")]
SES_romeo2_child_psid <- romeo2_child_psid[,c("pid", "SES_romeo2_psid", "fid")]
head(SES_romeo2_child_psid)
# qiu
SES_qiu_child_cfps <- qiu_child_cfps[,c("pid","SES_qiu_cfps")]
SES_qiu_child_psid <- qiu_child_psid[,c("pid", "fid", "SES_qiu_psid")]
# kim
SES_kim_child_cfps <- kim_child_cfps[,c("pid","SES_kim_cfps")]
SES_kim_child_psid <- kim_child_psid[,c("pid", "fid", "SES_kim_psid")]
# hanson
SES_hanson_child_cfps <- hanson_child_cfps[, c("pid", "SES_hanson_cfps")]
SES_hanson_child_psid <- hanson_child_psid[,c("pid", "fid", "SES_hanson_psid")]
# leonard
SES_leo_child_cfps <- leo_child_cfps[, c("pid", "SES_leo_cfps")]
SES_leo_child_psid <- leo_child_psid[, c("pid", "fid", "SES_leo_psid")]
# ozernov
SES_ozer_child_cfps <- ozer_child_cfps[, c("pid", "SES_ozer_cfps")]
SES_ozer_child_psid <- ozer_child_psid[, c("pid", "fid", "SES_ozer_psid")]

##################################### cfps matrix ##########################################
# write a merge function for cfps
merge_SES_CFPS <- function(x, y){
  df <- merge(x, y, by= "pid", all.x= TRUE, all.y= TRUE)
  return(df)
}
##### correlation CFPS######
# merge all ordinal and continuous SES cfps and mental health 
SES_mental_CFPS <- Reduce(merge_SES_CFPS, list(mental_children_cfps,SES_betan_child_cfps, SES_moog_child_cfps, SES_jed_child_cfps,
                                          SES_mcder_child_cfps, SES_romeo1_child_cfps,SES_romeo2_child_cfps,
                                          SES_qiu_child_cfps,SES_kim_child_cfps,SES_hanson_child_cfps, 
                                          SES_leo_child_cfps, SES_ozer_child_cfps))
SES_mental_CFPS <- SES_mental_CFPS[, -1] #delete pid column
head(SES_mental_CFPS)
colnames(SES_mental_CFPS) <- c("dep", "cog","c1", "c2", "c3", "c4","c5", "c6", #composite 1-6
                              "i1", "i2","i3", #income 1-3
                              #mental health
                              "e1", "e2") #education 1-2
SES_mental_CFPS_ordinal <- SES_mental_CFPS[, c("dep", "cog","c1", "c2", "c3", "c4","c5", "c6", #composite 1-6
                              "i1", "i2","i3")]
                                              #"depression", "cognition", "SES_betan_cfps","SES_moog_cfps", "SES_jed_cfps",
                                              #"SES_mcder_cfps","SES_romeo1_cfps","SES_romeo2_cfps",
                                              #"SES_qiu_cfps","SES_kim_cfps","SES_hanson_cfps")]
SES_mental_CFPS_dicho <- SES_mental_CFPS[,c("e1","e2")]

#extract colnames of SES_mental_CFPS

dimname <- list(colnames(SES_mental_CFPS))
dimname #see the names
###############
#build a matrix for corrrelation result
n_o <-11 #set number of ordinal variables
n_d <-2 #set number of dichotomus
#r
cormatrix_CFPS <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d), dimnames = list(colnames(SES_mental_CFPS)))
colnames(cormatrix_CFPS) <- dimname[[1]] #rename the matix
cormatrix_CFPS
#p
pmatrix_CFPS <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d), dimnames = list(colnames(SES_mental_CFPS)))
colnames(pmatrix_CFPS)  <-dimname[[1]]
pmatrix_CFPS

######################################
#calculate the spearman correlation matrix of all ordinal variables
library("correlation")
cor_ses_mental_cfps_ordinal <- Hmisc::rcorr(as.matrix(SES_mental_CFPS_ordinal), type = "spearman")
cor_ses_mental_cfps_ordinal
library("psych")
corrtest <-corr.test(SES_mental_CFPS_ordinal, y = NULL, use = "pairwise",method="spearman",adjust="holm", 
                                     alpha=.05,ci=TRUE,minlength=5)

#insert correlation matrix into the big matrix
#r
cormatrix_CFPS[1:n_o, 1:n_o] <- cor_ses_mental_cfps_ordinal$r
cormatrix_CFPS
#p
pmatrix_CFPS[1:n_o, 1:n_o] <- cor_ses_mental_cfps_ordinal$P
pmatrix_CFPS<-round(pmatrix_CFPS, digits = 5)
#ci
round(as.matrix(corrtest$ci),digits = 6)

#########biserial correlation##########
#?I try to use polycor::polyserial but SES_leo_cfps seem not fit it 
#polycor::polyserial(SES_mental_CFPS$SES_betan_cfps, SES_mental_CFPS$SES_leo_cfps, std.err = TRUE)
#so I use ltm::biserial.cor instead
library("ltm")
#install.packages("magicfor")
#use magicfor to store result of for loop
library(magicfor)               
#calculate correlation between leo and other ordinal variable
#r
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
e1_biserial_cortest[[1]] #see result

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
e2_biserial_cortest #see result

# extract p-value from cor.test result
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(e2_biserial_cortest[[i]]$p.value)
}
e2_biserial_p <- magic_result_as_dataframe() 
e2_biserial_p <- e2_biserial_p[,2]
e2_biserial_p<-round(e2_biserial_p, digits = 5) #see result

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
pmatrix_CFPS[1:n_o,(n_o +1)] <- e1_biserial_p
pmatrix_CFPS[1:n_o,(n_o +2)] <- e2_biserial_p
pmatrix_CFPS

# rows
pmatrix_CFPS[(n_o +1), 1:(n_o)]<- e1_biserial_p
pmatrix_CFPS[(n_o +2), 1:(n_o)]<-e2_biserial_p
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

# install.packages("ggcorrplot")
library("ggcorrplot")
library("corrplot")
head(cormatrix_CFPS)

corrplot_CFPS<-corrplot.mixed(cormatrix_CFPS, p.mat = pmatrix_CFPS, insig = "blank",sig.level = 0.05,
               cl.lim = c(-0.04, 1), tl.cex = 0.8, number.cex = 0.8)
cormatrix_CFPS_SES <- cormatrix_CFPS[3:11, 3:11]
pmatrix_CFPS_SES <- pmatrix_CFPS[3:11, 3:11]
corrplot_CFPS<-corrplot.mixed(cormatrix_CFPS_SES, p.mat = pmatrix_CFPS_SES, insig = "blank",sig.level = 0.05,
                              cl.lim = c(-0.04, 1), tl.cex = 0.8, number.cex = 0.8)

##install.packages("PerformanceAnalytics")
#library(PerformanceAnalytics)
#chart.Correlation(SES_mental_CFPS, histogram=TRUE, density = TRUE, method = "spearman")

##################################### psid matrix##########################################
##### correlation PSID######
# write a merge function for psid
merge_SES_PSID <- function(x, y){
  df <- merge(x, y, by= c("pid", "fid"),all.x= TRUE, all.y= TRUE)
  return(df)
}

#merge all SES PSID and mental health
mental_psid_child <- merge(psid_child, mental_psid, by = c("pid", "fid"))
mental_psid_child <- mental_psid_child[,c("depression", "life_satisfaction", "pid", "fid")]
SES_mental_PSID <- Reduce(merge_SES_PSID, list(mental_psid_child, SES_betan_child_psid, SES_moog_child_psid, SES_romeo2_child_psid,
                                               SES_qiu_child_psid, SES_kim_child_psid, SES_hanson_child_psid, 
                                                SES_leo_child_psid, SES_ozer_child_psid))
SES_mental_PSID <- SES_mental_PSID[, -c(1:2)] #delete pid column
head(SES_mental_PSID)
names(SES_mental_PSID) <- c("dep", "LS","c1", "c2", "c6", #composite 1,2,6
                              "i1", "i2","i3", #income 1-3
                              "e1", "e2") #education 1-2
summary(SES_mental_PSID)

SES_mental_PSID_ordinal <- SES_mental_PSID[, c("dep", "LS","c1", "c2", "c6", "i1", "i2","i3")] 
SES_mental_PSID_dich <- SES_mental_PSID[,c("e1", "e2")]
#extract colnames of SES_mental_PSID
dimname <- list(colnames(SES_mental_PSID))
dimname #see the names
#build a matrix for corrrelation result
n_o <-8 #set number of ordinal variables
n_d <-2 #set number of dichotomus
#r
cormatrix_PSID <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d), dimnames = list(colnames(SES_mental_PSID)))
colnames(cormatrix_PSID) <- dimname[[1]] #rename the matix
cormatrix_PSID
#p
pmatrix_PSID <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d), dimnames = list(colnames(SES_mental_PSID)))
colnames(pmatrix_PSID)  <-dimname[[1]]
pmatrix_PSID
#calculate the spearman correlation matrix of all ordinal variables
library("correlation")
summary(SES_mental_PSID_ordinal)
#cor_ses_mental_psid_ordinal<- Hmisc::rcorr(as.matrix(SES_mental_PSID_ordinal), type = "spearman")
cor_ses_mental_psid_ordinal<- corr.test(as.matrix(SES_mental_PSID_ordinal), use = "pairwise",method="spearman",adjust="holm", 
          alpha=.05,ci=TRUE,minlength=5)

#insert correlation matrix into the big matrix
#r
cormatrix_PSID[1:n_o, 1:n_o] <- cor_ses_mental_psid_ordinal$r
cormatrix_PSID
#p
pmatrix_PSID[1:n_o, 1:n_o] <- cor_ses_mental_psid_ordinal$P
pmatrix_PSID<-round(pmatrix_PSID, digits = 5)
pmatrix_PSID
#ci
cor_ses_mental_psid_ordinal$ci

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

#install.packages("ggcorrplot")
library("ggcorrplot")
library("corrplot")

cormatrix_PSID
corrplot_PSID <-corrplot.mixed(cormatrix_PSID, p.mat = pmatrix_PSID, insig = "blank", sig.level = 0.05,
         cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)
cormatrix_PSID_SES <-cormatrix_PSID[3:8, 3:8]
pmatrix_PSID_SES<- pmatrix_PSID[3:8, 3:8]
corrplot_PSID <-corrplot.mixed(cormatrix_PSID_SES, p.mat = pmatrix_PSID_SES, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)

#calculate ci between e1 and dep, e1 and LS, e1 and i2 by hand
#formula from http://www.real-statistics.com/correlation/biserial-correlation/
#e1 and dep
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
n<- 2825
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

##install.packages("PerformanceAnalytics")

#library(PerformanceAnalytics)
#chart.Correlation(SES_mental_PSID, histogram=TRUE, density = TRUE)

#create figure of two correlational matrix in one pdf
pdf("Correlational matrix.pdf",width=15,height=9)
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
corrplot.mixed(cormatrix_CFPS, p.mat = pmatrix_CFPS, insig = "blank",sig.level = 0.05,
                              cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1)
corrplot.mixed(cormatrix_PSID, p.mat = pmatrix_PSID, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix PSID", side = 1, line = -1)
par(opar)
dev.off()

pdf("Correlational matrix SES.pdf",width=15,height=9)
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
corrplot.mixed(cormatrix_CFPS_SES, p.mat = pmatrix_CFPS_SES, insig = "blank",sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1)
corrplot.mixed(cormatrix_PSID_SES, p.mat = pmatrix_PSID_SES, insig = "blank", sig.level = 0.05,
               cl.lim = c(0, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix PSID", side = 1, line = -1)
par(opar)
dev.off()

###########################################################
###################Betancourt, L, 2016:###################
#CFPS: SES and mental health/cognition
betan_child_cfps_mental <-merge(betan_child_cfps, mental_children_cfps, by = "pid")
#correlation
#SES and mental health
cor.test(betan_child_cfps_mental$SES_betan_cfps, betan_child_cfps_mental$depression)
ggplot(betan_child_cfps_mental,aes(x=SES_betan_cfps,y=depression)) + stat_binhex()+scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 
#SES and cognition
cor.test(betan_child_cfps_mental$SES_betan_cfps, betan_child_cfps_mental$cognition)
ggplot(betan_child_cfps_mental,aes(x=SES_betan_cfps,y=cognition)) + stat_binhex()+scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 

#PSID: SES and mental health/life satisfaction/cognition
betan_psid_mental <- merge(betan_child_psid, mental_psid, by = c("pid", "fid"))
betan_psid_mental <- drop_na(betan_psid_mental) #2761 left
#correlation
#SES and mental health
cor.test(betan_psid_mental$SES_betan_psid, betan_psid_mental$depression)
ggplot(betan_psid_mental,aes(x=SES_betan_psid,y=depression)) + stat_binhex()+scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 

#SES and life satisfaction
cor.test(betan_psid_mental$SES_betan_psid, betan_psid_mental$life_satisfaction)
ggplot(betan_psid_mental,aes(x=SES_betan_psid,y=life_satisfaction)) + stat_binhex()+scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 

#SES and cognition
#install.packages("polycor")
library(polycor)
polyserial(betan_psid_mental$SES_betan_psid, betan_psid_mental$cognition,std.err = TRUE)
ggplot(betan_psid_mental,aes(x=SES_betan_psid,y= cognition)) + stat_binhex()+scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 

###################Moog, 2008###################
#CFPS
moog_child_cfps_mental <- merge(moog_child_cfps, mental_children_cfps, by = "pid")
moog_child_cfps_mental<-drop_na(moog_child_cfps_mental)
#correlation
#SES and mental health
cor.test(moog_child_cfps_mental$SES_moog_cfps, moog_child_cfps_mental$depression)
ggplot(moog_child_cfps_mental,aes(x=SES_moog_cfps,y=depression)) + stat_binhex()+scale_fill_gradientn(colours=c("white","black")) + geom_smooth(method = "lm", colour = "red", se = T) 

#SES and cognition
cor.test(moog_child_cfps_mental$SES_moog_cfps, moog_child_cfps_mental$cognition)
ggplot(moog_child_cfps_mental,aes(x=SES_moog_cfps,y=cognition)) + stat_binhex()+ scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 

#PSID 
moog_child_psid_mental<- merge(moog_child_psid, mental_psid, by = c("pid","fid"))
moog_child_psid_mental<- drop_na(moog_child_psid_mental)
#correlation
#SES and mental health
cor.test(moog_child_psid_mental$SES_moog_psid, moog_child_psid_mental$depression)
ggplot(moog_child_psid_mental,aes(x=SES_moog_psid,y=depression)) + stat_binhex()+ scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 

#SES and life satisfaction
cor.test(moog_child_psid_mental$SES_moog_psid, moog_child_psid_mental$life_satisfaction)
ggplot(moog_child_psid_mental,aes(x=SES_moog_psid,y=life_satisfaction)) + stat_binhex()+ scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 

#SES and cognition
library(polycor)
polyserial(moog_child_psid_mental$SES_moog_psid, moog_child_psid_mental$cognition,std.err = TRUE)

##################yu, 2018###################
#CFPS
yu_cfps_mental <- merge(yu_cfps, mental_adult_cfps, by = "pid")
yu_cfps_mental <- drop_na(yu_cfps_mental)
#correlation
#SES and mental health
cor.test(yu_cfps_mental$SES_yu_cfps, yu_cfps_mental$depression)
ggplot(yu_cfps_mental,aes(x=SES_yu_cfps,y=depression)) + stat_binhex()+ scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 

#SES and cognition
cor.test(yu_cfps_mental$SES_yu_cfps, yu_cfps_mental$cognition)
ggplot(yu_cfps_mental,aes(x=SES_yu_cfps,y=cognition)) + stat_binhex()+ scale_fill_gradientn(colours=c("white","black"))+ geom_smooth(method = "lm", colour = "red", se = T) 

#################SES correlation######################
#betan & moog (CFPS)
betan_moog_cfps <- merge(moog_child_cfps, betan_child_cfps, by = c("pid", "fid", "pid_m"))
cor.test(betan_moog_cfps$SES_betan_cfps, betan_moog_cfps$SES_moog_cfps)
ggplot(betan_moog_cfps,aes(x=SES_betan_cfps,y=SES_moog_cfps)) + stat_binhex()+ geom_smooth(method = "lm", colour = "red", se = T) 

#betan & moog (PSID)
betan_moog_psid <-  merge(moog_child_psid, betan_child_psid, by =  c("pid", "fid", "pid_m","sex_m","sex", "relation_rp_m", "age"))
cor.test(betan_moog_psid$SES_moog_psid, betan_moog_psid$SES_betan_psid)
ggplot(betan_moog_psid,aes(x=SES_moog_psid,y=SES_betan_psid)) + stat_binhex()+ geom_smooth(method = "lm", colour = "red", se = T) 