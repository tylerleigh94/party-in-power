####Libraries####
library(easypackages)
libs<-c("haven", "tidyverse", "car", "labelled", "psych", "naniar", "plm")
libraries(libs)
####Read in the Data####
dat.1.orig <- read_sav("Data/GfK 2012/UPenn_NAES Pre Election 2012_Main_Client.sav")
dat.2.orig <- read_sav("Data/GfK 2012/UPenn_NAES Post Election 2012_Main_Client.sav")
####Merging####

#Change endings to _1 and _2

dat.1<-dat.1.orig %>%
  rename_at(vars(ends_with("_7")), funs(str_replace(., "_7$", "_W1")))

dat.2<-dat.2.orig %>%
  rename_at(vars(ends_with("_8")), funs(str_replace(., "_8$", "_W2")))

# Create longitundinal indicator and merge
dat.1$wave_W1<-1
dat.2$wave_W2<-1

##Merge
dat <- dat.1 %>% full_join(dat.2, by="MNO")

## Drop Folks in only one wave
dat$long<-ifelse(dat$wave_W1+dat$wave_W2==2, 1, 0)
dat <- dat %>%
  replace_na(list(long=0))
dat.p<-dat[dat$long==1,]

####Make into a long dataframe####

# Make party a stable characteristic
dat.p$party3_W1<-dat.p$partyid3_W1
dat.p$party3_W2<-dat.p$partyid3_W1

#Pivot Longer and recode NAs
dat.p<-remove_labels(dat.p, user_na_to_na = F)
na.values<-c(-1, -2)
dat.l<-dat.p %>%
  select(MNO, party3_W1, party3_W2, PPETHM_W1, PPETHM_W2,
         POS7_W1, POS7_W2, FFB_1_W1, FFB_1_W2, FFB_2_W1, FFB_2_W2, FFB_3_W1, FFB_3_W2, FFB_4_W1, FFB_4_W2,
         FFBa_W1, FFBa_W2, DETH1_A_W1, DETH1_A_W2, DETH1_B_W1, DETH1_B_W2, DETH1_C_W1, DETH1_C_W2, DETH2_A_W1,
         DETH2_A_W2, DETH2_B_W1, DETH2_B_W2, DETH2_C_W1, DETH2_C_W2, DETH3_A_W1, DETH3_A_W2, DETH3_B_W1, 
         DETH3_B_W2, DETH3_C_W1, DETH3_C_W2, DETH3_D_W1, DETH3_D_W2, PRR1_W1, PRR1_W2, CP3_W1, CP3_W2, EP1_W1, 
         EP1_W2, EP3_W1, EP3_W2, EP4_W1, EP4_W2, SOC1_W1, SOC1_W2, PER1_W1, PER1_W2, NEWOUT3_W1, NEWOUT3_W2, 
         RE15_W1, RE15_W2, RE17_W1, RE17_W2, RE19_W1, RE19_W2, RE22_W1, RE22_W2, RE24_1_W1, RE24_1_W2, RE24_2_W1,
         RE24_2_W2, RE24_3_W1, RE24_3_W2, RE24_4_W1, RE24_4_W2, RE25_1_W1, RE25_1_W2, RE25_2_W1, RE25_2_W2, 
         RE25_3_W1, RE25_3_W2, RE25_4_W1, RE25_4_W2, RE25_5_W1, RE25_5_W2) %>%
  replace_with_na_all(~.x %in% na.values) %>%
  pivot_longer(-MNO, names_to = c(".value", "wave"), names_sep="_W")


####Cleaning and Recodes####

# POS7 
dat.l$POS7.r<- car::recode(dat.l$POS7, "1=3; 2=2; 3=1; 4=0")

#FFB Index
##Recodes
dat.l$FFB_1.r <- car::recode(dat.l$FFB_1, "1=3; 2=2; 3=1; 4=0")
dat.l$FFB_2.r<-car::recode(dat.l$FFB_2, "1=3; 2=2; 3=1; 4=0")
dat.l$FFB_3.r<-car::recode(dat.l$FFB_3, "1=3; 2=2; 3=1; 4=0")
dat.l$FFB_4.r<-car::recode(dat.l$FFB_4, "1=3; 2=2; 3=1; 4=0")
dat.l$FFBa.r<-car::recode(dat.l$FFB_4, "4=3; 3=2; 2=1; 1=0")

##Index and reliability
index.ff<-grep("FFB", colnames(dat.l))[c(6:9)]
alpha.ff<-psych::alpha(dat.l[,index.ff])
dat.l$ffb.ind<-rowMeans(dat.l[,index.ff])

# Perceived Discrimination (NOTE: DETH3_D EXCLUDED BC UNSURE HOW IT FITS INTO INDEX)
dat.l <- dat.l %>%
  mutate(DETH1_A.r=100-DETH1_A, DETH1_B.r=100-DETH1_B, DETH1_C.r=100-DETH1_C,
         DETH2_A.r=100-DETH2_A, DETH2_B.r=100-DETH2_B, DETH2_C.r=100-DETH2_C,
         DETH3_A.r=100-DETH3_A, DETH3_B.r=100-DETH3_B, DETH3_C.r=100-DETH3_C, DETH3_D.r=100-DETH3_D)

dat.l$PD.ind<-NA
for(x in 1:nrow(dat.l)){
  if(dat.l$PPETHM[x]==1) {dat.l$PD.ind[x]=mean(dat.l$DETH1_A.r[x], dat.l$DETH1_B.r[x], 
                                              dat.l$DETH1_C.r[x], na.rm=T)-
    mean(dat.l$DETH2_A.r[x], dat.l$DETH2_B.r[x], dat.l$DETH2_C.r[x], 
         dat.l$DETH3_A.r[x], dat.l$DETH3_B.r[x], dat.l$DETH3_C.r[x], na.rm=T)}
  
  if(dat.l$PPETHM[x]==2) {dat.l$PD.ind[x]=mean(dat.l$DETH2_A.r[x], dat.l$DETH2_B.r[x], 
                                               dat.l$DETH2_C.r[x], na.rm=T)-
    mean(dat.l$DETH1_A.r[x], dat.l$DETH1_B.r[x], dat.l$DETH1_C.r[x], 
         dat.l$DETH3_A.r[x], dat.l$DETH3_B.r[x], dat.l$DETH3_C.r[x], na.rm=T)}
  
  if(dat.l$PPETHM[x]==4) {dat.l$PD.ind[x]=mean(dat.l$DETH3_A.r[x], dat.l$DETH3_B.r[x], 
                                               dat.l$DETH3_C.r[x], na.rm=T)-
    mean(dat.l$DETH2_A.r[x], dat.l$DETH2_B.r[x], dat.l$DETH2_C.r[x], 
         dat.l$DETH1_A.r[x], dat.l$DETH1_B.r[x], dat.l$DETH1_C.r[x], na.rm=T)}
}

#PRR1
dat.l$PRR1.r<-car::recode(dat.l$PRR1, "1=4; 2=3; 5=2; 3=1; 4=0")

#CP3
dat.l$CP3.r<-car::recode(dat.l$CP3, "1=3; 2=2; 3=1; 4=0")

#EP1, EP3, EP4 (NOTE: UNSURE IF THEY COMBINE INTO AN INDEX)
dat.l$EP1.r<-car::recode(dat.l$EP1, "1=2; 2=1; 3=0")
dat.l$EP3.r<-car::recode(dat.l$EP3, "1=2; 2=0; 3=1")
dat.l$EP4.r<-car::recode(dat.l$EP4, "1=4; 2=3; 3=2; 4=1; 5=0")

#SOC1
dat.l$SOC1.r<-car::recode(dat.l$SOC1, "1=4; 2=3; 5=2; 3=1; 4=0")

#PER1
dat.l$PER1.r<-car::recode(dat.l$PER1, "1=4; 2=3; 5=2; 4=1; 3=0")

#NEWOUT3
dat.l$NEWOUT3.r<-car::recode(dat.l$NEWOUT3, "1=3; 2=2; 3=1; 4=0")

#RE15, 17, 19, 22
dat.l$RE15.r<-car::recode(dat.l$RE15, "1=3; 2=2; 3=1; 4=0")
dat.l$RE17.r<-car::recode(dat.l$RE17, "1=3; 2=2; 3=1; 4=0")
dat.l$RE19.r<-car::recode(dat.l$RE19, "1=3; 2=2; 3=1; 4=0")
dat.l$RE22.r<-car::recode(dat.l$RE22, "1=3; 2=2; 3=1; 4=0")

#RE24
dat.l$RE24_1.r<-car::recode(dat.l$RE24_1, "1=4; 2=3; 3=2; 4=1; 5=0")
dat.l$RE24_2.r<-dat.l$RE24_2-1
dat.l$RE24_3.r<-car::recode(dat.l$RE24_3, "1=4; 2=3; 3=2; 4=1; 5=0")
dat.l$RE24_4.r<-dat.l$RE24_4-1

##Index and Reliability
index.re24<-grep("RE24_", colnames(dat.l))[-c(1:4)]
alpha.re24<-psych::alpha(dat.l[,index.re24])
dat.l$re24.ind<-rowMeans(dat.l[,index.re24], na.rm=T)

#RE25
dat.l$RE25_1.r<-1-dat.l$RE25_1
dat.l$RE25_2.r<-car::recode(dat.l$RE25_2, "1=4; 2=3; 3=2; 4=1; 5=0")
dat.l$RE25_3.r<-1-dat.l$RE25_3
dat.l$RE25_4.r<-car::recode(dat.l$RE25_4, "1=4; 2=3; 3=2; 4=1; 5=0")
dat.l$RE25_5.r<-1-dat.l$RE25_5

##Index and Reliability
index.re25<-grep("RE25_", colnames(dat.l))[-c(1:5)]
alpha.re25<-psych::alpha(dat.l[,index.re25])
dat.l$re25.ind<-rowMeans(dat.l[,index.re25], na.rm=T)

# Standardize all recodes and indices
dat.final<-dat.l %>%
  mutate(party3=2-party3, wave=wave, POS7.s=POS7.r/3, FFB_1.s=FFB_1.r/3, FFB_2.s=FFB_2.r/3,
         FFB_3.s=FFB_3.r/3, FFB_4.s=FFB_4.r/3, FFBa.s=FFBa.r/3, ffb.ind.s=ffb.ind/3, 
         DETH1_A.s=DETH1_A.r/100, DETH1_B.s=DETH1_B.r/100, DETH1_C.s=DETH1_C.r/100, 
         DETH2_A.s=DETH2_A.r/100, DETH2_B.s=DETH2_B.r/100, DETH2_C.s=DETH2_C.r/100,
         DETH3_A.s=DETH3_A.r/100, DETH3_B.s=DETH3_B.r/100, DETH3_C.s=DETH3_C.r/100, 
         DETH3_D.s=DETH3_D.r/100, PD.ind.s=PD.ind/100, PRR1.s=PRR1.r/4, CP3.s=CP3.r/3,
         )






