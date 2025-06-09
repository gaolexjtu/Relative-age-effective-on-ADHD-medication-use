library(dplyr)
library(lubridate)
library("survival")
library(readxl)
library(pbapply)
library(tableone)
library(data.table)

mc_01to23 <- readRDS("M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/mc_01to23.rds")
length(unique(mc_01to23$Baby.s.Reference.Key.))

bb_death1 <- readRDS("M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_death.rds")
length(unique(bb_death1$Reference.Key.))
bb_death <- bb_death1 %>% 
  arrange(Reference.Key.,Date.of.Registered.Death.) %>% 
  filter(duplicated(Reference.Key.)==F) %>% 
  select(Reference.Key.,Date.of.Birth..yyyy.mm.dd..,Date.of.Registered.Death.,Sex.)
length(unique(bb_death1$Reference.Key.))#653712

bb0117 <- merge(mc_01to23 %>% 
                  mutate(m_age=as.numeric((as.Date(Maternity.Episode..Delivery.Date..yyyy.mm.dd..)-as.Date(Date.of.Birth..yyyy.mm.dd..)+1)/365.25)) %>% 
                  select(-Date.of.Birth..yyyy.mm.dd..,-Date.of.Registered.Death.),
                bb_death %>% distinct(),by.x = "Baby.s.Reference.Key.",by.y = "Reference.Key.",all.x = T) %>%
  mutate(Sex.=ifelse(is.na(Sex.),Baby.s.Info..Sex.,Sex.),
         Date.of.Birth..yyyy.mm.dd..=if_else(is.na(Date.of.Birth..yyyy.mm.dd..),as.Date(Maternity.Episode..Delivery.Date..yyyy.mm.dd..),Date.of.Birth..yyyy.mm.dd..)) %>% 
  mutate(Date.of.Birth..yyyy.mm.dd..=as.Date(Date.of.Birth..yyyy.mm.dd..),
         Date.of.Registered.Death.=as.Date(Date.of.Registered.Death.),
         Baby.s.Info..Birth.Weight..gm..=as.numeric(Baby.s.Info..Birth.Weight..gm..),
         Maternity.Episode..Gestation.weeks.1=ifelse(is.na(Maternity.Episode..Gestation.weeks.),39,Maternity.Episode..Gestation.weeks.),
         preterm=ifelse(Maternity.Episode..Gestation.weeks.1<37,1,0),
         Baby.s.Info..Apgar.Score.at.5.min.=if_else(Baby.s.Info..Apgar.Score.at.5.min.==99,as.numeric(NA),as.numeric(Baby.s.Info..Apgar.Score.at.5.min.))) %>% 
  mutate(md=substring(Date.of.Birth..yyyy.mm.dd..,6,10),
         DOB1 = ifelse(md=="02-29",
                       as.character(paste0(year(Date.of.Birth..yyyy.mm.dd..),"-02-28")),
                       as.character(Date.of.Birth..yyyy.mm.dd..)),
         DOB=as.Date(DOB1),
         age6=as.Date(DOB)+years(6),
         age13=as.Date(DOB)+years(13),
         age18=as.Date(DOB)+years(18),
         parity=Maternity.Episode..OBS.Hx...Cx..Parity.) %>% 
  filter(year(DOB)>=2001,
         year(DOB)<=2010) %>% #276023
  filter(!is.na(Baby.s.Reference.Key.), #230056
         Sex.!="U") %>% #230052
  filter(is.na(Date.of.Registered.Death.)|Date.of.Registered.Death.>age6) #229124
length(unique(bb0117$Baby.s.Reference.Key.))
bb0117$apgar <- "missing"
bb0117$apgar[which(bb0117$Baby.s.Info..Apgar.Score.at.5.min.>=7)] <- "0reassuring"
bb0117$apgar[which(bb0117$Baby.s.Info..Apgar.Score.at.5.min.>=4&bb0117$Baby.s.Info..Apgar.Score.at.5.min.<=6)] <- "moderately abnormal"
bb0117$apgar[which(bb0117$Baby.s.Info..Apgar.Score.at.5.min.<=3)] <- "illness"

bb0117$parity[which(bb0117$parity==">= 5")] <- 5

sga <- bb0117 %>%
  mutate(gestation_week1=ifelse(Maternity.Episode..Gestation.weeks.1<=28,28,Maternity.Episode..Gestation.weeks.1)) %>% 
  group_by(gestation_week1) %>% 
  mutate(gw_mean=mean(as.numeric((Baby.s.Info..Birth.Weight..gm..))),
         gw_sd=sd(as.numeric((Baby.s.Info..Birth.Weight..gm..))),
         sga=gw_mean-2*gw_sd) %>% 
  filter(duplicated(Maternity.Episode..Gestation.weeks.1)==F) %>% 
  ungroup() %>% 
  dplyr::select(Maternity.Episode..Gestation.weeks.1,sga)

bb0117<- merge(bb0117,sga,by="Maternity.Episode..Gestation.weeks.1") %>% 
  mutate(lb=ifelse(as.numeric(Baby.s.Info..Birth.Weight..gm..)<sga,1,0))


table(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district..)
bb0117$ses <- NA
bb0117$ses[which(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district.. %in% c("CENTRAL & WESTE","WANCHAI","SAI KUNG excl.","TSEUNG KWAN O","EASTERN"))] <- "HIGH"
bb0117$ses[which(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district.. %in% c("SOUTHERN","KOWLOON CITY","MONGKOK","YAU TSIM","TSUEN WAN","TAI PO"))] <- "Midium1" # upper middle
bb0117$ses[which(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district.. %in% c("SHATIN","OTHERS","ISLANDS excl. N","NORTH LANTAU","YUEN LONG","NORTH"))] <- "Midium2"#lower middle
bb0117$ses[which(bb0117$District.of.Residence.on.Latest.Selected.Encounter..district.. %in% c("WONG TAI SIN","TUEN MUN","SHAM SHUI PO","KWAI TSING","KWUN TONG"))] <- " LOW"


table(bb0117$Baby.s.Info..Mode.of.Delivery.)
bb0117$d_mode <- "Other"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("Breech"))] <- "Breech"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("Classical CS","LSCS"))] <- "CS"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("Forceps"))] <- "Forceps"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("NSD"))] <- "NSD"
bb0117$d_mode[which(bb0117$Baby.s.Info..Mode.of.Delivery. %in% c("V/E"))] <- "V/E"


mm_dx_01to23 <- readRDS("F:/data_no_share_20200506/17.relative age/mm_dx_01to23.rds")


dx_code <- list(c("anxiety","^293.84|^300"),
                c("depression","^296.[23]|^311|^309.[01]"),
                c("scz","^295"),
                c("epilepsy","^345"),
                c("bp","296.[0145678]"),
                c("personality","^301"),
                c("somke_dx","^305.1|^V15.82|^649.0"),
                c("alcohol_dx","^291|^303|^305.0|^357.5|^425.5|^535.3|^571.0|^571.1|^571.2|^571.3|^980|^V11.3"),
                c("adhd_asd","^314|^299")
)

func_dx <- function(z){
  x <- z[1]
  y <- z[2]
  
  #find the dx using icd code
  mm_dx <- mm_dx_01to23 %>% 
    filter(grepl(y,All.Diagnosis.Code..ICD9..)==T) %>% 
    mutate(Reference.Date.==as.Date(Reference.Date.))
  #merge dx data with main database, select mother with x dx before pregnancy date
  mc_01to17_dx <- merge(bb0117,
                        mm_dx %>% select(Reference.Key.,Reference.Date.),by="Reference.Key.",all.x = T) %>% 
    mutate(x2=ifelse((!is.na(Reference.Date.)&Reference.Date.<DOB),1,0)) %>% 
    select(-c(Reference.Date.)) %>% 
    arrange(id,-x2) %>% 
    group_by(id) %>% 
    filter(duplicated(id)==F) %>% ungroup() %>% 
    arrange(id) %>% 
    select(id,x2) %>%
    `colnames<-` (c("id",x))
  
  return(mc_01to17_dx)
}

all_mc_01to17_dx <- bind_cols(lapply(dx_code,func_dx))
all_mc_01to17_dx1 <- all_mc_01to17_dx %>% 
  select(-id...3,-id...5,-id...7,-id...9,-id...11,-id...13,-id...15,-id...17)
bb0117_1 <- merge(bb0117,all_mc_01to17_dx1,by="id",by.y = "id...1",all.x = T)



mm_rx_01to23 <- readRDS("F:/data_no_share_20200506/17.relative age/mm_rx_01to23.rds")


rx_code <- list(c("antipsychotics","^4.2"),
                c("sedatives","^4.1"),
                c("antidepressants","^4.3"),
                c("antiepileptics","^4.8"),
                c("stimulants","^4.4"))

func_rx <- function(z){
  x <- z[1]
  y <- z[2]
  
  #find the medication using BNF code
  mm_drug <- mm_rx_01to23 %>% 
    filter(grepl(y,Therapeutic.Classification..BNF..Principal..)==T) %>% 
    mutate(Dispensing.Date..yyyy.mm.dd..==as.Date(Dispensing.Date..yyyy.mm.dd..))
  #merge rx data with main database, select mother with x rx use before pregnancy date
  mc_01to17_rx <- merge(bb0117_1,mm_drug %>% select(Reference.Key.,Dispensing.Date..yyyy.mm.dd..),by="Reference.Key.",all.x = T) %>% 
    mutate(x1=ifelse((!is.na(Dispensing.Date..yyyy.mm.dd..)&Dispensing.Date..yyyy.mm.dd..<DOB),1,0)) %>% 
    select(-c(Dispensing.Date..yyyy.mm.dd..)) %>% 
    arrange(id,-x1) %>% 
    group_by(id) %>% 
    filter(duplicated(id)==F) %>% ungroup() %>% 
    arrange(id) %>% 
    select(id,x1) %>%
    `colnames<-` (c("id",x))
  
  
  return(mc_01to17_rx)
}
all_mc_01to17_rx <- bind_cols(lapply(rx_code,func_rx))
all_mc_01to17_rx1 <- all_mc_01to17_rx %>% 
  select(-id...3,-id...5,-id...7,-id...9)
bb0117_2 <- merge(bb0117_1,all_mc_01to17_rx1,by.x="id",by.y = "id...1")



mm_social_nursing_s <- mm_sn_01to23 %>% 
  filter(PAS..Smoking.status.=="Ex-smoker"|PAS..Smoking.status.=="Smoker") %>% 
  select(Reference.Key.,PAS..Created.Date..yyyy.mm.dd..)

bb0117_3 <- bb0117_2 %>% 
  mutate(smoke_sn=ifelse(Reference.Key.%in%mm_social_nursing_s$Reference.Key.,1,0)) %>% 
  mutate(smoke=ifelse(smoke_sn+somke_dx==0,0,1)) %>% 
  select(-somke_dx,-smoke_sn)


mm_social_nursing_d <- mm_sn_01to23 %>% 
  filter(PAS..Alcohol.use.status.=="Ex-drinker"|PAS..Alcohol.use.status.=="Drinker") %>% 
  select(Reference.Key.,PAS..Created.Date..yyyy.mm.dd..)

bb0117_4 <- bb0117_3 %>% 
  mutate(drink_sn=ifelse(Reference.Key.%in%mm_social_nursing_d$Reference.Key.,1,0)) %>% 
  mutate(drink=ifelse(drink_sn+alcohol_dx==0,0,1)) %>% 
  select(-drink_sn,-alcohol_dx)



bb_rx <- readRDS("M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_rx.rds")
rx_adhd <- bb_rx %>% 
  mutate(rxst=if_else(is.na(Prescription.Start.Date.),
                      as.Date(Dispensing.Date..yyyy.mm.dd..),
                      as.Date(Prescription.Start.Date.))) %>% 
  filter(grepl("^4.4",Therapeutic.Classification..BNF..Principal..)==T) %>% 
  mutate(adhd_date=rxst) %>% 
  select(Reference.Key.,adhd_date)

dx_rx_adhd <- rbind(rx_adhd) %>% 
  arrange(Reference.Key.,adhd_date) %>% 
  filter(duplicated(Reference.Key.)==F)

final1 <- merge(bb0117_4,dx_rx_adhd,by.x = "Baby.s.Reference.Key.",by.y = "Reference.Key.",all.x = T) %>% 
  filter(is.na(adhd_date)|adhd_date>=age6) %>% #228028
  mutate(edate=pmin(age13-1,
                    Date.of.Registered.Death.,
                    ymd('2023-12-31'),
                    na.rm = TRUE)) %>% 
  # filter(edate>=age6) %>% 
  mutate(event=ifelse(is.na(adhd_date)|adhd_date>edate,0,1)) %>% 
  mutate(event=if_else(is.na(event),0,event),
         fu_days=ifelse(event==1,
                        as.numeric(adhd_date-age6+1),
                        as.numeric(edate-age6+1)),
         birth_mn=month(Date.of.Birth..yyyy.mm.dd..),
         birth_yr=year(Date.of.Birth..yyyy.mm.dd..))#229341
length(unique(final1$Baby.s.Reference.Key.))
table(final1$birth_mn)

# fm <- readRDS("M:/Personal/Gao Le/to_fm/fm_study1.rds") %>% 
#   filter(!is.na(expo))

dat <- final1 %>% 
  filter(birth_mn==1|birth_mn==2) %>%
  mutate(exp=ifelse(birth_mn==1,0,1),
         m_age_cat=ifelse(m_age<35,0,1))
table(dat$exp)
py <- dat %>% 
  group_by(exp) %>% 
  mutate(indiv_sum=length(Baby.s.Reference.Key.),
         event_sum=sum(event),
         py_sum=sum(fu_days)/365.25) %>% 
  filter(duplicated(exp)==F) %>%
  select(exp,indiv_sum,event_sum,py_sum) %>% 
  mutate(inci=event_sum/indiv_sum*100,
         ir=event_sum/py_sum*1000)
table(dat$exp,dat$event)




# ph assupmtion test 
library("survival")
library("survminer")
# library(adjustedCurves)
# library(riskRegression)
library("WeightIt")



#cox regression
#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat)
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink, 
                     data =dat)

summary(result.cox1)






#Analysis 3----
final1 <- merge(bb0117_4,dx_rx_adhd,by.x = "Baby.s.Reference.Key.",by.y = "Reference.Key.",all.x = T) %>% 
  filter(adhd_date<=age13,
         adhd_date>=age6,
         !is.na(Sex.)) %>% 
  filter(is.na(Date.of.Registered.Death.)|Date.of.Registered.Death.>age13)#11750

exp_mph_bb <- rx_adhd %>% 
  mutate(Reference.Key.=as.numeric(Reference.Key.)) %>% 
  select(Reference.Key.,adhd_date) %>% 
  distinct(Reference.Key.,adhd_date)

final2 <- merge(final1 %>% select(-adhd_date),
                exp_mph_bb,by.x = "Baby.s.Reference.Key.",by.y = "Reference.Key.",all.x = T)%>% 
  mutate(event=if_else(adhd_date>=age13&adhd_date<=age18,1,0)) %>%
  mutate(event=if_else(is.na(event),0,event)) %>% 
  group_by(Baby.s.Reference.Key.) %>% 
  arrange(Baby.s.Reference.Key.,-event,adhd_date) %>% 
  filter(duplicated(Baby.s.Reference.Key.)==F,
         !is.na(Sex.)) %>% 
  mutate(edate=pmin(age18-1,
                    Date.of.Registered.Death.,
                    ymd('2023-12-31'),
                    na.rm = TRUE)) %>% 
  mutate(fu_days=ifelse(event==1,
                        as.numeric(adhd_date-age13+1),
                        as.numeric(edate-age13+1)),
         birth_mn=month(Date.of.Birth..yyyy.mm.dd..),
         birth_yr=year(Date.of.Birth..yyyy.mm.dd..))
length(unique(final2$Baby.s.Reference.Key.))
dat <- final2 %>% 
  filter(birth_mn==1|birth_mn==2) %>%
  mutate(exp=ifelse(birth_mn==1,0,1))

py <- dat %>% 
  group_by(exp) %>% 
  mutate(indiv_sum=length(Baby.s.Reference.Key.),
         event_sum=sum(event),
         py_sum=sum(fu_days)/365.25) %>% 
  filter(duplicated(exp)==F) %>%
  select(exp,indiv_sum,event_sum,py_sum) %>% 
  mutate(inci=event_sum/indiv_sum*100,
         ir=event_sum/py_sum*1000)
table(dat$exp,dat$event)


#cox regression
#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat)
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink, 
                     data =dat)
summary(result.cox1)


