library(dplyr)
library(lubridate)
library("survival")
library(readxl)
library(pbapply)
library(tableone)
library(data.table)

#Analysis 1 
# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/1.mother-child"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# mc_01to23 <- do.call(rbind.data.frame, All)
# colnames(mc_01to23) <- make.names(colnames(mc_01to23)) #708020
# saveRDS(mc_01to23,"M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/mc_01to23.rds")
mc_01to23 <- readRDS("F:/data_no_share_20200506/17.relative age/mc_01to23.rds")
length(unique(mc_01to23$Baby.s.Reference.Key.))
# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/3.child/3.death/"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# bb_death <- do.call(rbind.data.frame, All)
# colnames(bb_death) <- make.names(colnames(bb_death)) #708020
# saveRDS(bb_death,"M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_death.rds")
bb_death1 <- readRDS("F:/data_no_share_20200506/17.relative age/bb_death.rds")
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
  mutate(lb=ifelse(as.numeric(Baby.s.Info..Birth.Weight..gm..)<sga,1,0),
         id=1:length(Baby.s.Reference.Key.))


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


#combine mother dx
# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/2.mother/1.dx/"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# mm_dx_01to23 <- do.call(rbind.data.frame, All)
# colnames(mm_dx_01to23) <- make.names(colnames(mm_dx_01to23)) #708020
# saveRDS(mm_dx_01to23,"F:/data_no_share_20200506/17.relative age/mm_dx_01to23.rds")
mm_dx_01to23 <- readRDS("F:/data_no_share_20200506/17.relative age/mm_dx_01to23.rds")

# psy_dx <- mm_dx_01to23 %>% 
#   filter(grepl('^293.84|^300|^296.[23]|^311|^309.[01]|314|^299|^295|^296.[0145678]|^301',All.Diagnosis.Code..ICD9..)==T) %>% 
#   select(Reference.Key.,Reference.Date.) %>% 
#   unique()


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



#combine mother rx
# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/2.mother/2.rx/"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# All1 <- lapply(All, function(x) x[c('Reference Key\n',"Institution (EIS)\n",
#                                     "Dispensing Specialty (PHS)\n...3","Dispensing Date (yyyy-mm-dd)\n","Dispensing Specialty (PHS)\n...6",                                  
#                                     "Prescription Start Date\n","Prescription End Date\n","Multiple dosage indicator\n",
#                                     "Drug Item Code\n","Drug Name\n" ,"Route\n" , "Drug Strength\n",
#                                     "Dosage\n","Dosage Unit\n","Drug Frequency\n","Dispensing Duration\n","Dispensing Duration Unit\n" ,                                       
#                                     "Quantity (Named Patient)\n","Base Unit\n","Action Status\n","Therapeutic Classification (BNF, Principal)\n",
#                                     "Therapeutic Classification (Principal BNF no. + BNF Description)\n","Therapeutic Classification (BNF)\n",                               
#                                     "Type of Patient (Drug)\n")])
# 
# mm_rx_01to23 <- do.call(rbind.data.frame, All1)
# colnames(mm_rx_01to23) <- make.names(colnames(mm_rx_01to23)) 
# length(unique(mm_rx_01to23$Reference.Key.))#57632
# saveRDS(mm_rx_01to23,"F:/data_no_share_20200506/17.relative age/mm_rx_01to23.rds")
mm_rx_01to23 <- readRDS("F:/data_no_share_20200506/17.relative age/mm_rx_01to23.rds")

# psy_rx <- mm_rx_01to23 %>% 
#   filter(grepl('^4.[12348]',Therapeutic.Classification..BNF..Principal..)==T) %>% 
#   mutate(Reference.Date.=as.Date(Dispensing.Date..yyyy.mm.dd..)) %>% 
#   select(Reference.Key.,Reference.Date.) %>% 
#   unique()


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




# bb0117_3 <- bb0117_2 %>% 
#   mutate(m_psy=if_else(Reference.Date.<DOB,1,0),
#          m_psy=if_else(is.na(m_psy),0,m_psy)) %>% 
#   select(Baby.s.Reference.Key.,m_psy)
#   
# 
# bb0117_2 <- merge(bb0117,bb0117_1 %>% mutate(m_psy=ifelse(is.na(m_psy),0,m_psy)),by="Baby.s.Reference.Key.") 



#smoke and alcohol data
#combine social nursing data
# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/2.mother/5.social nursing/"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# mm_sn_01to23 <- do.call(rbind.data.frame, All)
# colnames(mm_sn_01to23) <- make.names(colnames(mm_sn_01to23))
# length(unique(mm_sn_01to23$Reference.Key.))#57632
# saveRDS(mm_sn_01to23,"F:/data_no_share_20200506/17.relative age/mm_sn_01to23.rds")

#combine dx and sn together
#find out somkers and ex-smokers from social nursing form
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


# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/3.child/1.dx/"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- lapply(filenames_list,function(filename){
#   print(paste("Merging",filename,sep = " "))
#   read_xlsx(filename,sheet = "Data")
# })
# 
# bb_dx <- do.call(rbind.data.frame, All)
# colnames(bb_dx) <- make.names(colnames(bb_dx)) #708020
# saveRDS(bb_dx,"M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_dx.rds")

# bb_dx <- readRDS("M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_dx.rds")
# dx_adhd <- bb_dx %>% 
#   filter(grepl("^314",All.Diagnosis.Code..ICD9..)==T) %>% 
#   mutate(adhd_date=as.Date(Reference.Date.)) %>% 
#   select(Reference.Key.,adhd_date)
# length(unique(dx_adhd$Reference.Key.))

# path <- "M:/Cohort Raw Data (do not edit)/DIAMOND/2.convert_update2023/3.child/2.rx/"
# 
# filenames_list <- list.files(path=path, pattern="*.xlsx",full.names=TRUE)
# 
# All <- pblapply(filenames_list,function(filename){
#   read_xlsx(filename,sheet = "Data")
# })
# 
# All1 <- lapply(All, function(x) x[c('Reference Key\n','Dispensing Date (yyyy-mm-dd)\n','Prescription Start Date\n','Prescription End Date\n','Therapeutic Classification (BNF, Principal)\n','Drug Name\n')])
# bb_rx <- do.call(rbind.data.frame, All1)
# colnames(bb_rx) <- make.names(colnames(bb_rx)) #708020
# saveRDS(bb_rx,"M:/Cohort Raw Data (do not edit)/DIAMOND/3.combined_2023/bb_rx.rds")



bb_rx <- readRDS("F:/data_no_share_20200506/17.relative age/bb_rx.rds")
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
  filter(birth_mn==12|birth_mn==11|birth_mn==1|birth_mn==2) %>%
  mutate(exp=ifelse(birth_mn==1|birth_mn==2,0,1),
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


dat0 <- dat %>% 
  mutate(y2001=ifelse(birth_yr==2001,1,0),
         y2002=ifelse(birth_yr==2002,1,0),
         y2003=ifelse(birth_yr==2003,1,0),
         y2004=ifelse(birth_yr==2004,1,0),
         y2005=ifelse(birth_yr==2005,1,0),
         y2006=ifelse(birth_yr==2006,1,0),
         y2007=ifelse(birth_yr==2007,1,0),
         y2008=ifelse(birth_yr==2008,1,0),
         y2009=ifelse(birth_yr==2009,1,0),
         y2010=ifelse(birth_yr==2010,1,0),
         apgar0=ifelse(apgar=="0reassuring",1,0),
         apgar1=ifelse(apgar=="moderately abnormal",1,0),
         apgar2=ifelse(apgar=="illness",1,0),
         apgar3=ifelse(apgar=="missing",1,0),
         ses_l=ifelse(ses==" LOW",1,0),
         ses_m1=ifelse(ses=="Midium2",1,0),
         ses_m2=ifelse(ses=="Midium1",1,0),
         ses_u=ifelse(ses=="HIGH",1,0),
         mode_n=ifelse(d_mode=="NSD",1,0),
         mode_v=ifelse(d_mode=="V/E",1,0),
         mode_b=ifelse(d_mode=="Breech",1,0),
         mode_c=ifelse(d_mode=="CS",1,0),
         mode_f=ifelse(d_mode=="Forceps",1,0),
         mode_o=ifelse(d_mode=="Other",1,0),
         parity_0=ifelse(parity==0,1,0),
         parity_1=ifelse(parity==1,1,0),
         parity_2=ifelse(parity==2,1,0),
         parity_3=ifelse(parity==3,1,0),
         parity_4=ifelse(parity==4,1,0),
         parity_5=ifelse(parity==5,1,0)) 
varsp <- c("event","Sex.","ses_l","ses_m1","ses_m2","ses_u","y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010","m_age","adhd_asd","anxiety","bp","depression","epilepsy","personality","scz","stimulants","antidepressants","antiepileptics","antipsychotics","sedatives","smoke","drink","parity_0","parity_1","parity_2","parity_3","parity_4","parity_5","lb","preterm","Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..","apgar0","apgar1","apgar2","apgar3","mode_b","mode_c","mode_f","mode_n","mode_v","mode_o")
catvarsp<- c("event","Sex.","ses_l","ses_m1","ses_m2","ses_u","y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010","adhd_asd","anxiety","bp","depression","epilepsy","personality","scz","stimulants","antidepressants","antiepileptics","antipsychotics","sedatives","smoke","drink","parity_0","parity_1","parity_2","parity_3","parity_4","parity_5","lb","preterm","Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..","apgar0","apgar1","apgar2","apgar3","mode_b","mode_c","mode_f","mode_n","mode_v","mode_o")
basTable<-CreateTableOne( data = dat0,strata = "exp",vars = varsp, factorVars = catvarsp,includeNA=T)
baseline<-as.data.frame(as.data.table(print(basTable,smd = T,test = T,noSpaces = T,showAllLevels=F,catDigits = 2),keep.rownames = T))
write.csv(baseline,"F:/data_no_share_20200506/17.relative age/baseline1.csv")


# fm <- readRDS("C:/Users/XJTU/Documents/xwechat_files/wxid_y4t668gk2qyd21_6e65/msg/file/2025-06/dt_study1.rds")
# 
# check <- merge(dat %>% select(Baby.s.Reference.Key.,anxiety) ,
#                fm %>% select(reference_key,mom_anxiety_disorder),by.x="Baby.s.Reference.Key.",by.y="reference_key") %>% 
#   mutate(dif=anxiety-mom_anxiety_disorder) 
# 

options(digits = 10)


# ph assupmtion test 
library("survival")
library("survminer")
# library(adjustedCurves)
# library(riskRegression)
library("WeightIt")


# result.cox1 <- coxph(Surv(fu_days, event) ~ exp+Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+as.factor(m_psy)+as.factor(m_age_cat)+as.factor(parity),
#                      data =dat,x=T)
# result.cox2 <- coxph(Surv(fu_days, event) ~ exp+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+as.factor(m_psy)+as.factor(m_age_cat)+as.factor(parity),
#                      data =dat %>% filter(Sex.=="M"),x=T)
# test.ph <- cox.zph(result.cox2)
# test.ph
# 
# ggcoxzph(test.ph)
# adjsurv <- adjustedsurv(data=dat %>% mutate(exp=as.factor(exp)),
#                         variable="exp",
#                         ev_time="fu_days",
#                         event="event",
#                         method="direct",
#                         outcome_model=result.cox1,
#                         conf_int=TRUE)
# km_sz<-survfit(Surv(fu_days, event) ~ exp+Sex.+strata(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar), 
#                data =dat)
#                weights = w,)
weigthts_weight <- weightit(exp ~ Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink,
                           data = dat,
                           estimand = "ATE", # Find the ATE
                           method = "ps")
dat1 <- dat %>% 
  mutate(ipw = weigthts_weight$weights)


km_sz<-survfit(Surv(fu_days, event) ~ exp, 
               weights = ipw,data=dat1)
# saveRDS(km_sz,"M:/Personal/Gao Le/to_fm/km_sz.rds")
fm <- ggsurvplot(km_sz,conf.int=T,
                 fun = "event",risk.table = T,
                 xlab = "",ylab="Cumulative incidence (per 100 children)",
                 palette=c("#B24745", "#394E55"),
                 legend.title="",legend.labs = c("Relatively old group (ref)","Relatively young group"),
                 break.x.by = 365)+ #xscale="d_y",
  theme_survminer(font.x = c(18, "plain", "black"),
                  font.y = c(18, "plain", "black"),
                  font.legend = c(18, "plain", "black"))
fm1 <- fm$plot +
  scale_x_continuous(breaks = c(0:16)*(365/2),
                     labels = c(6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14))+
  scale_y_continuous(breaks = c(0,0.02,0.04,0.06,0.08),
                     labels = c(0,2,4,6,8))




# # new method --------------------------------------------------------------
# 
# fm <- ggsurvplot(km_sz,conf.int=T,
#                   fun = "event",risk.table = T,
#                   xlab = "",ylab="Cumulative incidence",
#                   palette=c("#B24745", "#394E55"),
#                   legend.title="",legend.labs = c("Relatively old group","Relatively young group"),
#                   xscale="d_y",break.x.by = 365)+
#   theme_survminer(font.x = c(18, "plain", "black"),
#                   font.7 = c(18, "plain", "black"),
#                   font.legend = c(18, "plain", "black"))
# # fm$data.survtable$n.risk[1] <- 1
# # fm$table$data$n.risk[1] <- 1
# # fm$table$data$llabels[1] <- 1
# # fm$table$data$fmlabel <- 1:16*100
# # fm$table$labels$label <- "fmlabel"
# 
# temp_table <- ggplot_build(fm$table)
# # temp_table$data[[1]]$label <- 1:16*100
# fm$table <- ggplotify::as.ggplot(ggplot_gtable(temp_table))
# 
# fm
options(digits = 10)


output0<- fm$data.survplot %>% 
  filter(exp==0) %>% 
  mutate(incidence_0=(1-surv)*100) %>% 
  select(exp,time,incidence_0) %>% 
  mutate(age=ceiling(time/365.25)+5) %>% 
  arrange(time) %>% 
  filter(duplicated(age)==F)


output1<- fm$data.survplot %>% 
  filter(exp==1) %>% 
  mutate(incidence_1=(1-surv)*100) %>% 
  select(exp,time,incidence_1) %>% 
  mutate(age=ceiling(time/365.25)+5) %>% 
  arrange(time) %>% 
  filter(duplicated(age)==F)

output_all <- merge(output0,output1,by="age") %>% 
  mutate(rd=incidence_1-incidence_0)

# ggsave("D:/OneDrive - The University Of Hong Kong/other share/5.PhD project/38.relative age/1. ADHD persistence/km_aim1.emf",plot = fm,width = 9, height = 6)
library(devEMF)
png("F:/data_no_share_20200506/17.relative age/primary1.png",width = 9, height = 6, units = 'in',res = 600)
fm1
dev.off()


set.seed(1234)
library(boot)
id <- 0
rdnnt <- function(data, ii){
  dd <- data[ii,] # allows boot to select a sample
  weigthts_weight <- weightit(exp ~ Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink,
                              data = dd,
                              estimand = "ATE", # Find the ATE
                              method = "ps")
  id <<- id+1  #<<-means put this in the global enviroment
  cat(id,"\r")
  
  dat1 <- dd %>% 
    mutate(ipw = weigthts_weight$weights)
  
  km_sz<-survfit(Surv(fu_days, event) ~ exp, 
                 weights = ipw,data=dat1)
  
  fm <- ggsurvplot(km_sz,conf.int=T,
                   fun = "event")
  
  
  output0<- fm$data.survplot %>% 
    filter(exp==0) %>% 
    mutate(incidence_0=(1-surv)*100) %>% 
    select(exp,time,incidence_0) %>% 
    mutate(age=ceiling(time/365.25)+5) %>% 
    arrange(time) %>% 
    filter(duplicated(age)==F)
  
  
  output1<- fm$data.survplot %>% 
    filter(exp==1) %>% 
    mutate(incidence_1=(1-surv)*100) %>% 
    select(exp,time,incidence_1) %>% 
    mutate(age=ceiling(time/365.25)+5) %>% 
    arrange(time) %>% 
    filter(duplicated(age)==F)
  
  output_all <- merge(output0,output1,by="age") %>% 
    mutate(rd=incidence_1-incidence_0) %>% 
    pull(rd)

  # cat('.')
  return(output_all)
}

results <- boot(data=dat,
                statistic=rdnnt, R=5000)

apply(results$t, 2, function(x) quantile(x,c(0.05,0.95)))


# dt <- data.table::data.table(x = seq(0,2600,365),
#                              y = c("0.0013\n(-0.0066-0.0067)",
#                                    "0.3952\n(0.3065-0.4910)",
#                                    "1.0736\n(0.9138-1.2279)",
#                                    "1.6712\n(1.4729-1.8628)",
#                                    "1.9423\n(1.7172-2.1642)",
#                                    "1.9815\n(1.7367-2.2235)",
#                                    "2.0073\n(1.7534-2.2557)",
#                                    "2.0466\n(1.7868-2.3030)"))
# b <-
#   ggplot(dt) +
#   geom_text(aes(x = x, y = factor(1), label = y))+
#   ylab("")+xlab("Age (Year)")+ggtitle("Incidence difference")+
#   theme(axis.title.x = element_text(size = 12),
#         plot.title = element_text(size = 12),
#         # text=element_text(size=18),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         # panel.border = element_blank(),
#         panel.background = element_blank(),
#         # text = element_text(size = 18),
#         axis.text.x = element_text(size = 12),
#         axis.text.y=element_blank())+
#   scale_x_continuous(breaks = c(0:16)*(365/2),
#                      labels = c(6,"",7,"",8,"",9,"",10,"",11,"",12,"",13,"",14))
# fm$table <- b
# fm




#cox regression
#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat)
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink, 
                     data =dat)

summary(result.cox1)


#FEMALE
py <- dat %>% 
  filter(Sex.=="F") %>% 
  group_by(exp) %>% 
  mutate(indiv_sum=length(Baby.s.Reference.Key.),
         event_sum=sum(event),
         py_sum=sum(fu_days)/365.25) %>% 
  filter(duplicated(exp)==F) %>%
  select(exp,indiv_sum,event_sum,py_sum) %>% 
  mutate(inci=event_sum/indiv_sum*100,
         ir=event_sum/py_sum*1000)
table(dat$exp,dat$event)

#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat%>% filter(Sex.=="F"))
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+strata(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink, 
                     data =dat%>%filter(Sex.=="F"))

summary(result.cox1)



#MALE
py <- dat %>% 
  filter(Sex.=="M") %>% 
  group_by(exp) %>% 
  mutate(indiv_sum=length(Baby.s.Reference.Key.),
         event_sum=sum(event),
         py_sum=sum(fu_days)/365.25) %>% 
  filter(duplicated(exp)==F) %>%
  select(exp,indiv_sum,event_sum,py_sum) %>% 
  mutate(inci=event_sum/indiv_sum*100,
         ir=event_sum/py_sum*1000)
table(dat$exp,dat$event)

#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat%>% filter(Sex.=="M"))
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+strata(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink, 
                     data =dat%>%filter(Sex.=="M"))

summary(result.cox1)



#Analysis 2---- SECONDARY

final1 <- merge(bb0117_2,dx_rx_adhd,by.x = "Baby.s.Reference.Key.",by.y = "Reference.Key.",all.x = T) %>% 
  filter(is.na(adhd_date)|adhd_date>=age6) %>% 
  filter(is.na(Date.of.Registered.Death.)|Date.of.Registered.Death.>age13) %>% 
  filter(is.na(adhd_date)|adhd_date>=age13,
         !is.na(Sex.)) %>% 
  mutate(edate=pmin(age18-1,
                    Date.of.Registered.Death.,
                    ymd('2023-12-31'),
                    na.rm = TRUE)) %>% 
  mutate(event=ifelse(is.na(adhd_date)|adhd_date>edate,0,1),
         fu_days=ifelse(event==1,
                        as.numeric(adhd_date-age13+1),
                        as.numeric(edate-age13+1)),
         birth_mn=month(Date.of.Birth..yyyy.mm.dd..),
         birth_yr=year(Date.of.Birth..yyyy.mm.dd..))#216278
length(unique(final1$Baby.s.Reference.Key.))

dat <- final1 %>% 
  filter(birth_mn==12|birth_mn==11|birth_mn==1|birth_mn==2) %>%
  mutate(exp=ifelse(birth_mn==1|birth_mn==2,0,1))
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

#cox regression
#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat)
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+Sex.+strata(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink,  
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
  filter(birth_mn==12|birth_mn==11|birth_mn==1|birth_mn==2) %>%
  mutate(exp=ifelse(birth_mn==1|birth_mn==2,0,1))

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

table(dat$exp)
summary(coxph(Surv(fu_days, event) ~ exp+Sex.+as.factor(birth_yr)+lb+preterm, data =  dat))
# ggsurvplot(
#   fit = survfit(Surv(fu_days/30, event) ~ exp, data =  dat), 
#   fun = "cumhaz",
#   xlab = "Months since age 6", 
#   ylab = "Cumulative hazard of ADHD",
#   ggtheme = theme_classic2(base_size=20)
# )

dat0 <- dat %>% 
  mutate(y2001=ifelse(birth_yr==2001,1,0),
         y2002=ifelse(birth_yr==2002,1,0),
         y2003=ifelse(birth_yr==2003,1,0),
         y2004=ifelse(birth_yr==2004,1,0),
         y2005=ifelse(birth_yr==2005,1,0),
         y2006=ifelse(birth_yr==2006,1,0),
         y2007=ifelse(birth_yr==2007,1,0),
         y2008=ifelse(birth_yr==2008,1,0),
         y2009=ifelse(birth_yr==2009,1,0),
         y2010=ifelse(birth_yr==2010,1,0),
         apgar0=ifelse(apgar=="0reassuring",1,0),
         apgar1=ifelse(apgar=="moderately abnormal",1,0),
         apgar2=ifelse(apgar=="illness",1,0),
         apgar3=ifelse(apgar=="missing",1,0),
         ses_l=ifelse(ses==" LOW",1,0),
         ses_m1=ifelse(ses=="Midium2",1,0),
         ses_m2=ifelse(ses=="Midium1",1,0),
         ses_u=ifelse(ses=="HIGH",1,0),
         mode_n=ifelse(d_mode=="NSD",1,0),
         mode_v=ifelse(d_mode=="V/E",1,0),
         mode_b=ifelse(d_mode=="Breech",1,0),
         mode_c=ifelse(d_mode=="CS",1,0),
         mode_f=ifelse(d_mode=="Forceps",1,0),
         mode_o=ifelse(d_mode=="Other",1,0),
         parity_0=ifelse(parity==0,1,0),
         parity_1=ifelse(parity==1,1,0),
         parity_2=ifelse(parity==2,1,0),
         parity_3=ifelse(parity==3,1,0),
         parity_4=ifelse(parity==4,1,0),
         parity_5=ifelse(parity==5,1,0)) 
varsp <- c("event","Sex.","ses_l","ses_m1","ses_m2","ses_u","y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010","m_age","adhd_asd","anxiety","bp","depression","epilepsy","personality","scz","stimulants","antidepressants","antiepileptics","antipsychotics","sedatives","smoke","drink","parity_0","parity_1","parity_2","parity_3","parity_4","parity_5","lb","preterm","Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..","apgar0","apgar1","apgar2","apgar3","mode_b","mode_c","mode_f","mode_n","mode_v","mode_o")
catvarsp<- c("event","Sex.","ses_l","ses_m1","ses_m2","ses_u","y2001","y2002","y2003","y2004","y2005","y2006","y2007","y2008","y2009","y2010","adhd_asd","anxiety","bp","depression","epilepsy","personality","scz","stimulants","antidepressants","antiepileptics","antipsychotics","sedatives","smoke","drink","parity_0","parity_1","parity_2","parity_3","parity_4","parity_5","lb","preterm","Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..","apgar0","apgar1","apgar2","apgar3","mode_b","mode_c","mode_f","mode_n","mode_v","mode_o")
basTable<-CreateTableOne( data = dat0,strata = "exp",vars = varsp, factorVars = catvarsp,includeNA=T)
baseline<-as.data.frame(as.data.table(print(basTable,smd = T,test = T,noSpaces = T,showAllLevels=F,catDigits = 2),keep.rownames = T))
write.csv(baseline,"F:/data_no_share_20200506/17.relative age/baseline2.csv")



#km
weigthts_weight <- weightit(exp ~ Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink,
                            data = dat,
                            estimand = "ATE", # Find the ATE
                            method = "ps")
dat1 <- dat %>% 
  ungroup() %>%  
  mutate(ipw = weigthts_weight$weights)


km_sz<-survfit(Surv(fu_days, event) ~ exp, 
               weights = ipw,data=dat1)
# saveRDS(km_sz,"M:/Personal/Gao Le/to_fm/km_sz.rds")
fm <- ggsurvplot(km_sz,conf.int=T,
                 fun = "event",risk.table = T,
                 xlab = "",ylab="Cumulative incidence (per 100 children)",
                 palette=c("#B24745", "#394E55"),
                 legend.title="",legend.labs = c("Relatively old group (ref)","Relatively young group"),
                 xscale="d_y",break.x.by = 365)+
  theme_survminer(font.x = c(18, "plain", "black"),
                  font.7 = c(18, "plain", "black"),
                  font.legend = c(18, "plain", "black"))
fm1 <- fm$plot +
  scale_x_continuous(breaks = c(0:10)*(365/2),
                     labels = c(13,"",14,"",15,"",16,"",17,"",18))+
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8),
                     labels = c(0,20,40,60,80))





output0<- fm$data.survplot %>% 
  filter(exp==0) %>% 
  mutate(incidence_0=(1-surv)*100) %>% 
  select(exp,time,incidence_0) %>% 
  mutate(age=ceiling(time/365.25)+5) %>% 
  arrange(time) %>% 
  filter(duplicated(age)==F)


output1<- fm$data.survplot %>% 
  filter(exp==1) %>% 
  mutate(incidence_1=(1-surv)*100) %>% 
  select(exp,time,incidence_1) %>% 
  mutate(age=ceiling(time/365.25)+5) %>% 
  arrange(time) %>% 
  filter(duplicated(age)==F)

output_all <- merge(output0,output1,by="age") %>% 
  mutate(rd=incidence_1-incidence_0)

# ggsave("D:/OneDrive - The University Of Hong Kong/other share/5.PhD project/38.relative age/1. ADHD persistence/km_aim1.emf",plot = fm,width = 9, height = 6)
library(devEMF)
png("F:/data_no_share_20200506/17.relative age/primary2.png",width = 9, height = 6, units = 'in',res = 600)
fm1
dev.off()


set.seed(1234)
library(boot)
fm <- 0
rdnnt <- function(data, ii){
  dd <- data[ii,] # allows boot to select a sample
  weigthts_weight <- weightit(exp ~ Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink,
                              data = dd,
                              estimand = "ATE", # Find the ATE
                              method = "ps")
  fm <<- fm+1  #<<-means put this in the global enviroment
  cat(fm,"\r")
  
  dat1 <- dd %>% 
    ungroup() %>% 
    mutate(ipw = weigthts_weight$weights)
  
  km_sz<-survfit(Surv(fu_days, event) ~ exp, 
                 weights = ipw,data=dat1)
  
  fm <- ggsurvplot(km_sz,conf.int=T,
                   fun = "event")
  
  
  output0<- fm$data.survplot %>% 
    filter(exp==0) %>% 
    mutate(incidence_0=(1-surv)*100) %>% 
    select(exp,time,incidence_0) %>% 
    mutate(age=ceiling(time/365.25)+12) %>% 
    arrange(time) %>% 
    filter(duplicated(age)==F)
  
  
  output1<- fm$data.survplot %>% 
    filter(exp==1) %>% 
    mutate(incidence_1=(1-surv)*100) %>% 
    select(exp,time,incidence_1) %>% 
    mutate(age=ceiling(time/365.25)+12) %>% 
    arrange(time) %>% 
    filter(duplicated(age)==F)
  
  output_all <- merge(output0,output1,by="age") %>% 
    mutate(rd=incidence_1-incidence_0) %>% 
    select(age,rd)
  
  age_group <- data.frame(age=c(13,14,15,16,17,18))
  
  output_all1 <- merge(age_group,output_all,by="age",all.x = T) %>% 
    pull(rd)
  
  # print(ii)
  # cat('.')

  return(output_all1)
  
}

results <- boot(data=dat,
                statistic=rdnnt, R=5000)

apply(results$t, 2, function(x) quantile(x,c(0.05,0.95),na.rm = T))


#cox regression
#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat)
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink, 
                     data =dat)
summary(result.cox1)


#FEMALE
py <- dat %>%
  filter(Sex.=="F") %>% 
  group_by(exp) %>% 
  mutate(indiv_sum=length(Baby.s.Reference.Key.),
         event_sum=sum(event),
         py_sum=sum(fu_days)/365.25) %>% 
  filter(duplicated(exp)==F) %>%
  select(exp,indiv_sum,event_sum,py_sum) %>% 
  mutate(inci=event_sum/indiv_sum*100,
         ir=event_sum/py_sum*1000)
#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat %>% filter(Sex.=="F"))
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+as.factor(birth_yr)+lb+preterm+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink, 
                     data =dat%>% filter(Sex.=="F"))
summary(result.cox1)


#MALE
py <- dat %>%
  filter(Sex.=="M") %>% 
  group_by(exp) %>% 
  mutate(indiv_sum=length(Baby.s.Reference.Key.),
         event_sum=sum(event),
         py_sum=sum(fu_days)/365.25) %>% 
  filter(duplicated(exp)==F) %>%
  select(exp,indiv_sum,event_sum,py_sum) %>% 
  mutate(inci=event_sum/indiv_sum*100,
         ir=event_sum/py_sum*1000)
#crude
result.cox1 <- coxph(Surv(fu_days, event) ~ exp, 
                     data =dat %>% filter(Sex.=="M"))
summary(result.cox1)
#adjusted
result.cox1 <- coxph(Surv(fu_days, event) ~ exp+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink, 
                     data =dat%>% filter(Sex.=="M"))
summary(result.cox1)





#boot strap error method
rdnnt <- function(data, ii){
  dd <- data[ii,] # allows boot to select a sample
  weigthts_weight <- weightit(exp ~ Sex.+as.factor(birth_yr)+lb+preterm+Baby.s.Info..With.Birth.Trauma.mentioned..Y.N..+as.factor(apgar)+as.factor(ses)+as.factor(d_mode)+m_age+as.factor(parity)+adhd_asd+anxiety+bp+depression+epilepsy+personality+scz+stimulants+antidepressants+antiepileptics+antipsychotics+sedatives+smoke+drink,
                              data = dd,
                              estimand = "ATE", # Find the ATE
                              method = "ps")
  fm <<- fm+1
  cat(fm,"\r")
  
  dat1 <- dd %>% 
    ungroup() %>% 
    mutate(ipw = weigthts_weight$weights)
  
  
  km_sz <- tryCatch({
    survfit(Surv(fu_days, event) ~ exp, 
            weights = ipw,data=dat1)
  }, error = function(e) {
    message("Model failed at bootstrap ", fm, ": ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(km_sz)) {
    print(c(NA,NA,NA,NA,NA,NA))
    return(list(NA,NA,NA,NA,NA,NA))
  }
  
  
  # Calculate the incidence rate for each group
  
  fm <- tryCatch({
    ggsurvplot(km_sz,conf.int=T,
               fun = "event")
  }, error = function(e) {
    message("Error in creating data.survplot: ",fm, conditionMessage(e))
    return(NULL)
  })
  if (is.null(fm)) {
    print(c(NA,NA,NA,NA,NA,NA))
    return(list(NA,NA,NA,NA,NA,NA))
  }
  
  output0<- fm$data.survplot %>% 
    filter(exp==0) %>% 
    mutate(incidence_0=(1-surv)*100) %>% 
    select(exp,time,incidence_0) %>% 
    mutate(age=ceiling(time/365.25)+12) %>% 
    arrange(time) %>% 
    filter(duplicated(age)==F)
  
  
  output1<- fm$data.survplot %>% 
    filter(exp==1) %>% 
    mutate(incidence_1=(1-surv)*100) %>% 
    select(exp,time,incidence_1) %>% 
    mutate(age=ceiling(time/365.25)+12) %>% 
    arrange(time) %>% 
    filter(duplicated(age)==F)
  
  output_all <- merge(output0,output1,by="age") %>% 
    mutate(rd=incidence_1-incidence_0) %>% 
    pull(rd)
  ifelse(length(output_all)!=6,
         print(output_all),NA)
  # print(ii)
  # cat('.')
  # print(output_all)
  return(output_all)
  
}
