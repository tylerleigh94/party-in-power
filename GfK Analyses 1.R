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

#Pivot Longer
dat.p<-remove_labels(dat.p, user_na_to_na = F)
dat.l<-dat.p %>%
  select(MNO, party3_W1, party3_W2,
         POS7_W1, POS7_W2, FFB_1_W1, FFB_1_W2, FFB_2_W1, FFB_2_W2, FFB_3_W1, FFB_3_W2, FFB_4_W1, FFB_4_W2,
         FFBa_W1, FFBa_W2, DETH1_A_W1, DETH1_A_W2, DETH1_B_W1, DETH1_B_W2, DETH1_C_W1, DETH1_C_W2, DETH2_A_W1,
         DETH2_A_W2, DETH2_B_W1, DETH2_B_W2, DETH2_C_W1, DETH2_C_W2, DETH3_A_W1, DETH3_A_W2, DETH3_B_W1, 
         DETH3_B_W2, DETH3_C_W1, DETH3_C_W2, DETH3_D_W1, DETH3_D_W2, PRR1_W1, PRR1_W2, CP3_W1, CP3_W2, EP1_W1, 
         EP1_W2, EP3_W1, EP3_W2, EP4_W1, EP4_W2, SOC1_W1, SOC1_W2, PER1_W1, PER1_W2, NEWOUT3_W1, NEWOUT3_W2, 
         RE15_W1, RE15_W2, RE17_W1, RE17_W2, RE19_W1, RE19_W2, RE22_W1, RE22_W2, RE24_1_W1, RE24_1_W2, RE24_2_W1,
         RE24_2_W2, RE24_3_W1, RE24_3_W2, RE24_4_W1, RE24_4_W2, RE25_1_W1, RE25_1_W2, RE25_2_W1, RE25_2_W2, 
         RE25_3_W1, RE25_3_W2, RE25_4_W1, RE25_4_W2, RE25_5_W1, RE25_5_W2) %>%
  pivot_longer(-MNO, names_to = c(".value", "wave"), names_sep="_W")

####Cleaning and Recodes####





