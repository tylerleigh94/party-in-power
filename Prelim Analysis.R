####Libraries####
library(easypackages)
libs<-c("haven", "tidyverse", "car", "labelled", "psych", "naniar")
libraries(libs)
####Read in the Data####
dat <- readRDS("~/Desktop/R Programs/Research--Diana/party-in-power/Data/Amerispeak/waves1234all.rds")
####Cleaning####

# Keep only panelists
dat$long<-NA
for(x in 1:nrow(dat)){
  if(dat$wave_1[x]==1 & dat$wave_2[x]==1 &dat$wave_3[x]==1 & dat$wave_4[x]==1) {dat$long[x]=1}
  else{dat$long[x]=0}
}

dat.p<-dat[dat$long==1,]
#####Recodes####

#Vectors of NA
na.2<-c(77, 98, 99)

#PT4
dat.p<-dat.p %>%
  replace_with_na_at(.vars=c('PT4_W1', 'PT4_W2', 'PT4_W3', 'PT4_W4'), ~.x %in% na.2)
  
dat.p$pt4r_1<-car::recode(dat.p$PT4_W1, "1=4; 2=3; 3=2; 4=1")
dat.p$pt4r_2<-car::recode(dat.p$PT4_W2, "1=4; 2=3; 3=2; 4=1")
dat.p$pt4r_3<-car::recode(dat.p$PT4_W3, "1=4; 2=3; 3=2; 4=1")
dat.p$pt4r_4<-car::recode(dat.p$PT4_W4, "1=4; 2=3; 3=2; 4=1")

#PT5
dat.p<-dat.p %>%
  replace_with_na_at(.vars=c('PT5_W1', 'PT5_W2', 'PT5_W3', 'PT5_W4'), ~.x %in% na.2)

dat.p$pt5r_1<-car::recode(dat.p$PT5_W1, "1=4; 2=3; 3=2; 4=1")
dat.p$pt5r_2<-car::recode(dat.p$PT5_W2, "1=4; 2=3; 3=2; 4=1")
dat.p$pt5r_3<-car::recode(dat.p$PT5_W3, "1=4; 2=3; 3=2; 4=1")
dat.p$pt5r_4<-car::recode(dat.p$PT5_W4, "1=4; 2=3; 3=2; 4=1")

# PP1
dat.p$pp1r_1<-dat.p$PP1_W1
dat.p$pp1r_2<-dat.p$PP1_W2
dat.p$pp1r_3<-dat.p$PP1_W3
dat.p$pp1r_4<-dat.p$PP1_W4

# PP2
dat.p$pp2r_1<-dat.p$PP2_W1
dat.p$pp2r_2<-dat.p$PP2_W2
dat.p$pp2r_3<-dat.p$PP2_W3
dat.p$pp2r_4<-dat.p$PP2_W4

#SL2
dat.p$sl2r_1<-dat.p$SL2_W1
dat.p$sl2r_2<-dat.p$SL2_W2
dat.p$sl2r_3<-dat.p$SL2_W3
dat.p$sl2r_4<-dat.p$SL2_W4

#ISO1
dat.p$iso1r_1<-dat.p$ISO1_W1
dat.p$iso1r_2<-dat.p$ISO1_W2
dat.p$iso1r_3<-dat.p$ISO1_W3
dat.p$iso1r_4<-dat.p$ISO1_W4

#ISO3
dat.p$iso3r_1<-dat.p$ISO3_W1
dat.p$iso3r_2<-dat.p$ISO3_W2
dat.p$iso3r_3<-dat.p$ISO3_W3
dat.p$iso3r_4<-dat.p$ISO3_W4

#ISO5
dat.p$iso5r_1<-dat.p$ISO5_W1
dat.p$iso5r_2<-dat.p$ISO5_W2
dat.p$iso5r_3<-dat.p$ISO5_W3
dat.p$iso5r_4<-dat.p$ISO5_W4

#SD2
dat.p$sd2r_1<-dat.p$SD2_W1
dat.p$sd2r_4<-dat.p$SD2_W4

#SD4
dat.p$sd4r_1<-dat.p$SD4_W1
dat.p$sd4r_4<-dat.p$SD4_W4

# Make the dataset long
##Remove Labels and Drop Difficult variables
dat.p<-remove_labels(dat.p, user_na_to_na = F)
dat.long<- dat.p %>%
  select(ecoperc_1, ecoperc_2, perecoperc_1, perecoperc_2, ST1r_1, ST1r_2, st2r_1, st2r_2, st3r_1,
         st3r_2, mkt1r_1, mkt1r_4, mkt2r_1, mkt2r_4, mkt3r_1, mkt3r_4, im1r_1, im1r_2, im3r_1, 
         im3r_2, hc1r_1, hc1r_2, tr1r_1, tr1r_2, tr2r_1, tr2r_2, pt1r_1, pt1r_2, pt2r_1, pt2r_2,
         pt3r_1, pt3r_2, pt4r_1, pt4r_2, pt5r_1, pt5r_2, pp1r_1, pp1r_2, pp2r_1, pp2r_2, pp3r_1,
         pp3r_2, pp4r_1, pp4r_2, sl1r_1, sl1r_2, sl2r_1, sl2r_2, sl3r_1, sl3r_2, iso1r_1, iso1r_2,
         iso2r_1, iso2r_2, iso3r_1, iso3r_2, iso4r_1, iso4r_2, iso5r_1, iso5r_2, sd1r_1, sd1r_4,
         sd2r_1, sd2r_4, sd3r_1, sd3r_4, sd4r_1, sd4r_4, dis1r_1, dis1r_2, dis2r_1, dis2r_2, 
         dis3r_1, dis3r_2, dis4r_1, dis4r_2, dis5r_1, dis5r_2, dis6r_1, dis6r_2, dis7r_1, dis7r_2,
         chinatri_1, chinatri_2, ns1r_1, ns1r_2, ns2r_1, ns2r_2, ns3r_1, ns3r_2) %>%
  pivot_longer(-CaseId, names_to = c(".value", "wave"), names_sep="_W")

####Analyses####
# Set up vector of variables