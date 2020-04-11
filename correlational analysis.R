#################################mental health###################
#Mental health and cognition CPFS
#children
mental_children_cfps <- df.children %>%     
  dplyr::select(wn401,	#Feel depressed and cannot cheer up
                wn402,	#Feel nervous
                wn403,	#Feel agitated or upset and cannot remain calm 
                wn404,	#Feel hopeless about the future 
                wn405,	#Feel that everything is difficult 
                wn406,	#Think life is meaningless 
                ###cognitive ability	
                wordtest,	
                mathtest, pid) 
mental_children_cfps[mental_children_cfps == -8] <- NA
mental_children_cfps <- mental_children_cfps %>%
  dplyr::mutate(depression = wn401+wn402+wn403+wn404+wn405+wn406,
                cognition = wordtest + mathtest) %>%
  dplyr::select(pid, depression, cognition)
mental_children_cfps <- drop_na(mental_children_cfps) #3360 left
#adult
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

#######################################extract data############################################
#extract personal coding and SES indicator for each study 
#betan
SES_betan_child_cfps <- betan_child_cfps[,c("pid", "SES_betan_cfps")]
SES_betan_child_psid <- betan_child_psid[,c("pid", "fid", "SES_betan_psid")]
#moog
SES_moog_child_cfps <- moog_child_cfps[,c("pid", "SES_moog_cfps")]
SES_moog_child_psid <- moog_child_psid[,c("pid", "fid", "SES_moog_psid")]
#Yu
SES_yu_yadult_cfps <- yu_cfps[,c("pid", "SES_yu_cfps")]
SES_yu_yadult_psid <- yu_yadult_psid[,c("pid", "fid", "SES_yu_psid")]
#Jed
SES_jed_child_cfps <- jed_cfps[,c("pid", "SES_jed_cfps")]
#mcder
SES_mcder_child_cfps <- mcder_child_cfps[,c("pid", "SES_mcder_cfps")]
#romeo1
SES_romeo1_child_cfps <- romeo1_child_cfps[,c("pid", "SES_romeo1_cfps")]
#romeo2
SES_romeo2_child_cfps <- romeo2_child_cfps[,c("pid", "SES_romeo2_cfps")]
SES_romeo2_child_psid <- romeo2_child_psid[,c("pid", "SES_romeo2_psid", "fid")]
#qiu
SES_qiu_child_cfps <- qiu_child_cfps[,c("pid","SES_qiu_cfps")]
SES_qiu_child_psid <- qiu_child_psid[,c("pid", "fid", "SES_qiu_psid")]
#kim
SES_kim_child_cfps <- kim_child_cfps[,c("pid","SES_kim_cfps")]
SES_kim_child_psid <- kim_child_psid[,c("pid", "fid", "SES_kim_psid")]
#hanson
SES_hanson_child_cfps <- hanson_child_cfps[, c("pid", "SES_hanson_cfps")]
SES_hanson_child_psid <- hanson_child_psid[,c("pid", "fid", "SES_hanson_psid")]
#leonard
SES_leo_child_cfps <- leo_child_cfps[, c("pid", "SES_leo_cfps")]
SES_leo_child_psid <- leo_child_psid[, c("pid", "fid", "SES_leo_psid")]
#ozernov
SES_ozer_child_cfps <- ozer_child_cfps[, c("pid", "SES_ozer_cfps")]
SES_ozer_child_psid <- ozer_child_psid[, c("pid", "fid", "SES_ozer_psid")]

#####################################cfps matrix##########################################
#write a merge function for cfps
merge_SES_CFPS <- function(x, y){
  df <- merge(x, y, by= "pid", all.x= TRUE, all.y= TRUE)
  return(df)
}
#####correlation CFPS######
#merge all SES cfps and mental health
SES_mental_CFPS <- Reduce(merge_SES_CFPS, list(SES_betan_child_cfps, SES_moog_child_cfps, SES_jed_child_cfps,
                                          SES_mcder_child_cfps, SES_romeo1_child_cfps,SES_romeo2_child_cfps,
                                          SES_qiu_child_cfps,SES_kim_child_cfps,SES_hanson_child_cfps, SES_leo_child_cfps,
                                          SES_ozer_child_cfps,
                                          mental_children_cfps))
SES_mental_CFPS <- SES_mental_CFPS[, -1] #delete pid column
#SES_mental_CFPS <- drop_na(SES_mental_CFPS)
#correltaion of cfps
library("Hmisc")
cor_ses_mental_cfps<- rcorr(as.matrix(SES_mental_CFPS))
cor_ses_mental_cfps
##install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(SES_mental_CFPS, histogram=TRUE, density = TRUE)

#####################################psid matrix##########################################
#####correlation PSID######
#write a merge function for psid
merge_SES_PSID <- function(x, y){
  df <- merge(x, y, by= c("pid", "fid"),all.x= TRUE, all.y= TRUE)
  return(df)
}

#merge all SES cfps and mental health
mental_psid_child <- merge(psid_child, mental_psid, by = c("pid", "fid"))
mental_psid_child <- mental_psid_child[,c("depression", "life_satisfaction", "pid", "fid")]
SES_mental_PSID <- Reduce(merge_SES_PSID, list(SES_betan_child_psid, SES_moog_child_psid, SES_romeo2_child_psid,
                                               SES_qiu_child_psid, SES_kim_child_psid, SES_hanson_child_psid, SES_leo_child_psid,
                                               mental_psid_child))
SES_mental_PSID <- SES_mental_PSID[, -c(1:2)] #delete pid column
summary(SES_mental_PSID)
#correltaion of cfps
library("Hmisc")
cor_ses_mental_psid<- rcorr(as.matrix(SES_mental_PSID))
cor_ses_mental_psid
##install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(SES_mental_PSID, histogram=TRUE, density = TRUE)


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


