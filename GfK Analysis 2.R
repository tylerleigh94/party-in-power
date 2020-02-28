####Libraries####
library(easypackages)
libs<-c("haven", "tidyverse", "car", "labelled", "psych", "naniar", "plm")
libraries(libs)

####Import Data####
dat.orig.3 <- read_sav("Data/GfK 2008/Annenberg_Wave3_Final_Client.sav")
dat.orig.4 <- read_sav("Data/GfK 2008/W4_Annenberg_Client.sav")
dat.orig.5 <- read_sav("Data/GfK 2008/W5_Annenberg_Client.sav")

####Longitudinal Indicator and Merge Waves####

dat.3 <- dat.orig.3
dat.4 <- dat.orig.4
dat.5 <- dat.orig.5

#Wave variable
dat.3$wave_3 <- 1
dat.4$wave_4 <- 1
dat.5$wave_5 <- 1

#Select variables
dat.3<- dat.3 %>%
  select(MNO, partyid3_3, FFB_1_3, FFB_2_3, FFB_3_3, FFB_4_3, FFBa_3)

dat.4 <- dat.4 %>%
  select(MNO, partyid3_4, POS1_4, POS2_4, POS3_4, POS4_4, DETH1_A_4, DETH1_B_4, DETH1_C_4, DETH2_A_4,
         DETH2_B_4, DETH2_C_4, PRR1_4, PPETHM_4)

#Merge 3 and 4 and standardize endings and party
dat.34 <- dat.3 %>%
  full_join(dat.4, by='MNO')

dat.34$party3_W1 <- ifelse(is.na(dat.34$partyid3_3), dat.34$partyid3_4, dat.34$partyid3_3)

dat.34 <- dat.34 %>%
  select(-c(partyid3_3, partyid3_4)) %>%
  rename_at(vars(ends_with("_4")), funs(str_replace(., "_4$", "_W1"))) %>%
  rename_at(vars(ends_with("_3")), funs(str_replace(., "_3$", "_W1")))

#Merge into Wave 5 and keep party constant

dat <- dat.5 %>%
  select(MNO, FFB_1_5, FFB_2_5, FFB_3_5, FFB_4_5, FFBa_5, POS1_5, POS2_5, POS3_5, POS4_5, DETH1_A_5, 
         DETH1_B_5, DETH1_C_5, DETH2_A_5, DETH2_B_5, DETH2_C_5, PRR1_5, PPETHM_5) %>%
  inner_join(dat.34)  %>%
  rename_at(vars(ends_with("_5")), funs(str_replace(., "_5$", "_W2")))

dat$party3_W2 <- dat$party3_W1

# Replace NAs and pivot
na.values <- c(-1:-3)

dat.l <- dat %>%
  replace_with_na_all(~.x %in% na.values) %>%
  pivot_longer(-MNO, names_to = c(".value", "wave"), names_sep="_W")

####Variable Recodes and Cleaning####
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

# Perceived Discrimination (Only for White- and Black-identifying respondents)
dat.l <- dat.l %>%
  mutate(DETH1_A.r=100-DETH1_A, DETH1_B.r=100-DETH1_B, DETH1_C.r=100-DETH1_C,
         DETH2_A.r=100-DETH2_A, DETH2_B.r=100-DETH2_B, DETH2_C.r=100-DETH2_C)

dat.l$PD.ind<-NA
for(x in 1:nrow(dat.l)){
  if(is.na(dat.l$PPETHM[x])) {dat.l$PD.ind[x] <- NA}
  else if(dat.l$PPETHM[x]==1) {dat.l$PD.ind[x]=mean(dat.l$DETH1_A.r[x], dat.l$DETH1_B.r[x], 
                                               dat.l$DETH1_C.r[x], na.rm=T)-
    mean(dat.l$DETH2_A.r[x], dat.l$DETH2_B.r[x], dat.l$DETH2_C.r[x], na.rm=T)}
  
  else if(dat.l$PPETHM[x]==2) {dat.l$PD.ind[x]=mean(dat.l$DETH2_A.r[x], dat.l$DETH2_B.r[x], 
                                               dat.l$DETH2_C.r[x], na.rm=T)-
    mean(dat.l$DETH1_A.r[x], dat.l$DETH1_B.r[x], dat.l$DETH1_C.r[x], na.rm=T)}
  
}

#PRR1
dat.l$PRR1.r<-car::recode(dat.l$PRR1, "1=4; 2=3; 5=2; 3=1; 4=0")

# Position Scales

dat.l <- dat.l %>%
  mutate(POS1.r=3-POS1, POS2.r=3-POS2, POS3.r=3-POS3, POS4.r=4-POS4)

# Standardize all recodes and indices
dat.l <-dat.l %>%
  mutate(MNO.s=MNO, party3.s=party3, wave.s=wave, FFB_1.s=FFB_1.r/3, FFB_2.s=FFB_2.r/3,
         FFB_3.s=FFB_3.r/3, FFB_4.s=FFB_4.r/3, FFBa.s=FFBa.r/3, ffb.ind.s=ffb.ind/3, 
         DETH1_A.s=DETH1_A.r/100, DETH1_B.s=DETH1_B.r/100, DETH1_C.s=DETH1_C.r/100, 
         DETH2_A.s=DETH2_A.r/100, DETH2_B.s=DETH2_B.r/100, DETH2_C.s=DETH2_C.r/100,
         PD.ind.s=PD.ind/100, PRR1.s=PRR1.r/4, POS1.s=POS1.r/2, POS2.s=POS2.r/2, POS3.s=POS3.r/2,
         POS4.s=POS4.r/3)

####Analyses####
# Set up vector of variables
index.s<-grep(".s", colnames(dat.l))[-c(1:3)]
index.names<-colnames(dat.l[,index.s])

# Make wave and party factors
dat.l$party3.s<-factor(dat.l$party3.s)
dat.l$wave.s<-factor(dat.l$wave.s)

# Set up loop and capture output
out.08<-matrix(NA, ncol=3, nrow=length(index.names))
colnames(out.08)<-c("var", "est_08", "sig_08")
for(x in 1:length(index.names)){
  txt<-paste("model<-summary(plm(formula=", index.names[x], "~wave.s+party3.s*wave.s, data=dat.l, model='within', index='MNO.s', type='individual'))", sep="")
  eval(parse(text=txt))
  cat("-------------", "\n", "\n", paste("DV = ", index.names[x], sep=""), "\n", "\n")
  print(model)
  cat("\n")
  out.08[x,1]<-index.names[x]
  out.08[x,2]<-model$coefficients[3,1]
  out.08[x,3]<-model$coefficients[3,4]
}



