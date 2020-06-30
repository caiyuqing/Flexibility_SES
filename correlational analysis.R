# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(ggcorrplot)) {install.packages("ggcorrplot",repos = "http://cran.us.r-project.org"); require(ggcorrplot)}
if (!require(corrplot)) {install.packages("corrplot",repos = "http://cran.us.r-project.org"); require(corrplot)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
if (!require(correlation)) {install.packages("correlation",repos = "http://cran.us.r-project.org"); require(correlation)}
if (!require(ltm)) {install.packages("ltm",repos = "http://cran.us.r-project.org"); require(ltm)}
if (!require(magicfor)) {install.packages("magicfor",repos = "http://cran.us.r-project.org"); require(magicfor)}

#################################mental health###################
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

# Mental health and cognition psid
mental_PSID <- df.PSID_child %>%
  dplyr::select(depression, #sum for depression: very healthy 0----24 very depressed
                life_satisfaction, #life satisfaction: completed satisfied 1----5 not at all satisfied
                pid) %>%
  dplyr::mutate(depression = ifelse(depression == 99, NA, depression),
                life_satisfaction = ifelse(life_satisfaction<1 | life_satisfaction>5, NA, life_satisfaction)) %>%
  # reverse score for depresion and life_satisfaction (higher-better mental health)
  dplyr::mutate(depression = -depression + 24,
                life_satisfaction = -life_satisfaction + 6) 

####################################### extract data ############################################
names_dataframe_cfps <- list(betan_CFPS, moog_CFPS,
                          jed_CFPS, mcder_CFPS,
                          romeo1_CFPS, romeo2_CFPS, qiu_CFPS, kim_CFPS, 
                          hanson_CFPS, leo_CFPS, ozer_CFPS)
names_dataframe_psid <- list(betan_PSID, moog_PSID,
                            romeo2_PSID, qiu_PSID, kim_PSID, 
                             hanson_PSID, leo_PSID, ozer_PSID)
#names_SES_cfps <- c("SES_betan_cfps", "SES_moog_cfps", "SES_jed_cfps",
#                    "SES_mcder_cfps", "SES_romeo1_cfps", "SES_romeo2_cfps", 
#                    "SES_qiu_cfps", "SES_kim_cfps", "SES_hanson_cfps", "SES_leo_cfps", "SES_ozer_cfps") 
names_paper_cfps <- c("betan", "moog","jed", "mcder",
                 "romeo1", "romeo2", "qiu", "kim", 
                 "hanson", "leo", "ozer")
names_paper_psid <- c("betan", "moog", "romeo2", "qiu", "kim", 
                 "hanson", "leo", "ozer")
# extract columns of pid and SES from all the dataframes of SES
SES_mental_CFPS_data <- for (i in 1:11) {
  dataframes <- names_dataframe_cfps[[i]][, c("pid", paste0("SES_", names_paper_cfps[i], "_cfps"))]
  print(dataframes)
}

SES_mental_PSID_data <- for (i in 1:8) {
  dataframes <- names_dataframe_psid[[i]][, c("pid", paste0("SES_", names_paper_psid[i], "_psid"))]
  print(dataframes)
}

# write a merge function to merge all the data
merge_SES <- function(x, y){
  df <- merge(x, y, by= "pid", all.x= TRUE, all.y= TRUE)
  return(df)
}
##################################### cfps matrix ##########################################
##### correlation CFPS######
# merge all ordinal and continuous SES cfps and mental health 
SES_mental_CFPS<- Reduce(merge_SES, SES_mental_CFPS_data$dataframes) %>%
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
# library("psych")
CFPS_omega <- psych::omega(SES_mental_CFPS[,3:13])
print(c(CFPS_omega$omega_h, CFPS_omega$omega.tot))

#extract colnames of SES_mental_CFPS
dimname <- colnames(SES_mental_CFPS)
dimname #see the names
###############
storage.vector <- rep(NA,10)
for(i in 1:10){
  storage.vector[i] <- i^2
  print(storage.vector)
}
storage.vector



#build a matrix for correlation result
Correlations <- as.data.frame(matrix(ncol = 4, nrow = 169)) %>%
  dplyr::rename(variable1 = V1, variable2 =V2, correlation =V3, p-value = V4) %>%
  dplyr::mutate(variable1 = rep(dimname, each = 13))
Correlations <- data.frame(variable1 = rep(dimname, each = 13),
                           variable2 = rep(dimname, 13),
                           correlation = rep(NA, 169),
                           p = rep(NA, 169)) %>%
  dplyr::mutate(correlation = SES_mental_CFPS$)

Correlations[1:13] <- dimname[1]
Correlations[1:26] <- dimname[2]
x<-cor.test(SES_mental_CFPS[,1], SES_mental_CFPS[,1])

output <- matrix(ncol=1, nrow=169)
output <- as.data.frame(output)
names(output) <- "corvalue"
colnames(SES_mental_CFPS[1])
output[2, "corvalue"] 
Correlations[1,] <- c(1,2,3)
library(magicfor)

magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:13) {
  for(j in 1:13) {
   #a<- cor.test(SES_mental_CFPS[,i], SES_mental_CFPS[,j], use = "pairwise.complete.obs")
   #b<- a$estimate
   x<-colnames(SES_mental_CFPS[i])
   y<-colnames(SES_mental_CFPS[j])
    print(c(x,y))
    }}
tmp2 <- magic_result_as_dataframe()  


Correlations[[11]]
Correlations
tmp$Correlations
tmp
result <- data.frame(matrix(nrow = 169, ncol = 2))
colnames(result) <- c("v1", "v2")
result <- vector("numeric",  169)
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:13) {
    for (j in 1:13) {
  a <- colnames(SES_mental_CFPS[i])
  b <- colnames(SES_mental_CFPS[j])
  c <- list(a,b)
  print(c)
  #a <- cor.test(x, y, use = "pairwise.complete.obs")
  #output[i+13*(j-1),"corvalue"] <- a$estimate
  #Correlations[i+(j-1)*13, 1]  <- colnames(SES_mental_CFPS[i])
  #Correlations[i+(j-1)*13, 2]  <- colnames(SES_mental_CFPS[j])
    }}
magic_result_as_dataframe()     # get the result

result
a<-c(1,2)
b<-c(2,3)
c<-c(a,b)
c<- list(a,b)
colnames(SES_mental_CFPS[1])
colnames(SES_mental_CFPS[, i])
typeof(cor)
iterations = 10
variables = 2
?magic_for()
output <- matrix(ncol=variables, nrow=iterations)

for(i in 1:iterations){
  output[i,] <- runif(2)
  
}

output
Correlations[1+(2-1)*13, 3] <-1
for (i in 1:4) {
  for (j in 1:4) {
    print(paste(i,j))
  }
  
}
tmp$result
colnames(SES_mental_CFPS[1])
colnames()
cor.test(SES_mental_CFPS$e2,SES_mental_CFPS$cog)
n_o <-11 #set number of ordinal variables
n_d <-2 #set number of dichotomous
#r
cormatrix_CFPS <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d), dimnames = list(colnames(SES_mental_CFPS)))
colnames(cormatrix_CFPS) <- dimname[[1]] #rename the matrix
cormatrix_CFPS
#p
pmatrix_CFPS <- matrix(data = NA, nrow = (n_o+n_d), ncol = (n_o+n_d), dimnames = list(colnames(SES_mental_CFPS)))
colnames(pmatrix_CFPS)  <-dimname[[1]]
pmatrix_CFPS

######################################
# calculate the Spearman correlation matrix of all ordinal variables
# library("correlation")
cor_ses_mental_cfps_ordinal <- Hmisc::rcorr(as.matrix(SES_mental_CFPS_ordinal), type = "spearman")
cor_ses_mental_cfps_ordinal

# library("psych")
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
ci_cfps <-round(as.matrix(corrtest$ci),digits = 4)
write.csv(ci_cfps[2:19, 1:3], file = "ci_cfps1.csv")

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

# extract p-value from cor.test result
magic_for(print, silent = TRUE) # call magic_for()
for (i in 1:n_o) {
  print(e2_biserial_cortest[[i]]$p.value)
}
e2_biserial_p <- magic_result_as_dataframe() 
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

head(cormatrix_CFPS)

corrplot_CFPS<-corrplot.mixed(cormatrix_CFPS, p.mat = pmatrix_CFPS, insig = "blank",sig.level = 0.05,
               cl.lim = c(-0.111, 1), tl.cex = 0.8, number.cex = 0.8)
cormatrix_CFPS_SES <- cormatrix_CFPS[3:13, 3:13]
pmatrix_CFPS_SES <- pmatrix_CFPS[3:13, 3:13]
corrplot_CFPS<-corrplot.mixed(cormatrix_CFPS_SES, p.mat = pmatrix_CFPS_SES, insig = "blank", sig.level = 0.05,
                              cl.lim = c(-0.04, 1), tl.cex = 0.8, number.cex = 0.8)

##################################### psid matrix##########################################
# merge all SES PSID and mental health
SES_mental_PSID <- Reduce(merge_SES, list(mental_psid, SES_betan_child_psid, SES_moog_child_psid, SES_romeo2_child_psid,
                                         SES_qiu_child_psid, SES_kim_child_psid, SES_hanson_child_psid, 
                                         SES_leo_child_psid, SES_ozer_child_psid)) %>%
  dplyr::select(-pid) %>%
  dplyr::rename(dep = depression,
                LS = life_satisfaction,
                c1 = SES_betan_psid,
                c2 = SES_moog_psid,
                c6 = SES_romeo2_psid,
                i1 = SES_qiu_psid,
                i2 = SES_kim_psid,
                i3 = SES_hanson_psid,
                e1 = SES_leo_psid,
                e2 = SES_ozer_psid)

SES_mental_PSID_ordinal <- SES_mental_PSID[, c("dep", "LS","c1", "c2", "c6", "i1", "i2","i3")] 
SES_mental_PSID_dich <- SES_mental_PSID[,c("e1", "e2")]

# McDonald’s omega
PSID_omega <- psych::omega(SES_mental_PSID[,3:10])

print(c(PSID_omega$omega_h, PSID_omega$omega.tot))
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

#install.packages("ggcorrplot")
library("ggcorrplot")
library("corrplot")

corrplot_PSID <-corrplot.mixed(cormatrix_PSID, p.mat = pmatrix_PSID, insig = "blank", sig.level = 0.05,
         cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)
cormatrix_PSID_SES <-cormatrix_PSID[3:10, 3:10]
pmatrix_PSID_SES<- pmatrix_PSID[3:10, 3:10]
corrplot_PSID <-corrplot.mixed(cormatrix_PSID_SES, p.mat = pmatrix_PSID_SES, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.08, 1), tl.cex = 0.8, number.cex = 0.8)

#calculate ci between e1 and dep, e1 and LS, e1 and i2 by hand
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

##install.packages("PerformanceAnalytics")

#library(PerformanceAnalytics)
#chart.Correlation(SES_mental_PSID, histogram=TRUE, density = TRUE)

#create figure of two correlational matrix in one pdf
pdf("Correlational matrix.pdf",width=15,height=9)
opar<-par(no.readonly=T)
par(mfrow=c(1,2))
corrplot.mixed(cormatrix_CFPS, p.mat = pmatrix_CFPS, insig = "blank",sig.level = 0.05,
                              cl.lim = c(-0.11, 1), tl.cex = 0.8, number.cex = 0.8)
mtext("Correlation matrix CFPS", side = 1, line = -1)
corrplot.mixed(cormatrix_PSID, p.mat = pmatrix_PSID, insig = "blank", sig.level = 0.05,
                               cl.lim = c(-0.11, 1), tl.cex = 0.8, number.cex = 0.8)
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

table_ses_mental_cfps <- cormatrix_CFPS[3:13,1:2]
table_ses_mental_cfps_p <-pmatrix_CFPS[3:13,1:2]
table_ses_mental_cfps
table_ses_mental_cfps_p
table_ses_mental_psid <- cormatrix_PSID[3:10,1:2]
table_ses_mental_psid_p<- pmatrix_PSID[3:10, 1:2]
table_ses_mental_psid
table_ses_mental_psid_p

write.csv(round(table_ses_mental_cfps, digits = 3), file = "table_ses_cfps.csv")
write.csv(round(table_ses_mental_psid, digits = 3), file = "table_ses_psid.csv")
