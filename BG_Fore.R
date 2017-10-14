library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)
library(HSAUR3)
library(KernSmooth)
library(flexmix)
library(boot)
library(mclust)
library(gridExtra)


NINE <- read.csv("//KNX3IT/AWG Management/RGR/Activation Needs Dashboard/Data/NINE.csv", stringsAsFactors=FALSE)
Tracker <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/Tracker.csv", stringsAsFactors=FALSE)
ARMASTER <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv", stringsAsFactors=FALSE)

NINE$ED_AWG_CREDIT_AR <- as.character(NINE$ED_AWG_CREDIT_AR)
ARMASTER$desk <- as.character(ARMASTER$desk)
ARMASTER <- ARMASTER %>%
  rename(ED_AWG_CREDIT_AR=desk) %>%
  dplyr::select(EMPNUM,A.R,ED_AWG_CREDIT_AR)
Tracker <- Tracker %>%
  rename(CM_FILENO = TFILE) 
Tracker$CM_FILENO <- as.character(Tracker$CM_FILENO)
NINE$CM_FILENO <- as.character(NINE$CM_FILENO)

NINE <- NINE %>%
  dplyr::select(ED_ACCT_ACT_DATE,CM_FILENO,ED_AWG_CREDIT_AR) %>%
  left_join(ARMASTER,by='ED_AWG_CREDIT_AR') %>%
  left_join(Tracker,by=c('CM_FILENO','EMPNUM'))
rm(ARMASTER); rm(Tracker)

NINE$ACT_Month <- as.Date(paste('01',format(as.Date(NINE$ED_ACCT_ACT_DATE,'%m/%d/%Y'),'%B %Y')),'%d %B %Y')
NINE$RHB_Month <- as.Date(NINE$SetupMonth,'%m/%d/%Y')
NINE$ED_ACCT_ACT_DATE <- as.Date(NINE$ED_ACCT_ACT_DATE,'%m/%d/%Y')
NINE$SetupMonth <- as.Date(NINE$SetupMonth,'%m/%d/%Y')

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

NINE <- NINE %>%
  mutate(Month_Setup = elapsed_months(RHB_Month,ACT_Month))

RHB <- NINE %>% 
  filter(!is.na(Month_Setup))

qplot(RHB$Month_Setup)

DF <- NINE
names <- c("BOLTON, TROY","BOWIE, RACHEL","BULLY, ADAM","DIVERS, SHALLINA","DORAN, JOSHUA","GARRETT, BROCK",
           "LOWHORN, JEREMIAH","MCCARTY, ROBIN","NEWMAN, ANDREW","ROBINSON, JAMARCUS","WISECARVER, JOSHUA")
DF <- DF %>%
  filter(!A.R %in% names)

NINE$Month_Setup[is.na(NINE$Month_Setup)] <- 'Not Setup'

NINE <- NINE %>%
  mutate(Month1 = ifelse(Month_Setup == '0',1,0),
         Month2 = ifelse(Month_Setup == '1',1,0),
         Month3 = ifelse(Month_Setup == '2',1,0),
         Month4 = ifelse(Month_Setup == '3',1,0),
         Month5 = ifelse(Month_Setup == '4',1,0),
         Month6 = ifelse(Month_Setup == '5',1,0),
         Month7 = ifelse(Month_Setup == '6',1,0),
         Month8 = ifelse(Month_Setup == '7',1,0),
         Month9 = ifelse(Month_Setup == '8',1,0),
         Month10 = ifelse(Month_Setup == '9',1,0),
         Month11 = ifelse(Month_Setup == '10',1,0),
         Not_Act = ifelse(!Month_Setup %in% c('0','1','2','3','4','5','6','7','8','9','10','Not Setup'),1,0)
         )
  
aggregate <- NINE %>%
  group_by(ACT_Month) %>%
  summarize(Activations = n(),
            M1_Setup = as.numeric(sum(Month1)/Activations),
            M2_Setup = sum(Month2)/Activations,
            M3_Setup = sum(Month3)/Activations,
            M4_Setup = sum(Month4)/Activations,
            M5_Setup = sum(Month5)/Activations,
            M6_Setup = sum(Month6)/Activations,
            M7_Setup = sum(Month7)/Activations,
            M8_Setup = sum(Month8)/Activations,
            EveryThingElse = (sum(Month9)+sum(Month10)+sum(Month11)+sum(Not_Act))/Activations)


M1Mean <- max(aggregate[,3]$M1_Setup)
M2Mean <- max(aggregate[,4]$M2_Setup)
M3Mean <- max(aggregate[,5]$M3_Setup)
M4Mean <- max(aggregate[,6]$M4_Setup)
M5Mean <- max(aggregate[,7]$M5_Setup)
M6Mean <- max(aggregate[,8]$M6_Setup)
M7Mean <- max(aggregate[,9]$M7_Setup)
M8Mean <- max(aggregate[,10]$M8_Setup)



Forecast <- aggregate %>%
  mutate(Projected_Rehabs = M1Mean * Activations[ACT_Month == '2017-09-01'] +
                            M2Mean * Activations[ACT_Month == '2017-08-01'] +
                            M3Mean * Activations[ACT_Month == '2017-07-01'] +
                            M4Mean * Activations[ACT_Month == '2017-06-01'] +
                            M5Mean * Activations[ACT_Month == '2017-05-01'] +
                            M6Mean * Activations[ACT_Month == '2017-04-01'] +
                            M7Mean * Activations[ACT_Month == '2017-03-01'] +
                            M8Mean * Activations[ACT_Month == '2017-02-01'] 
           )

lapply(M1Mean,mean,na.rm=T)





Tracker <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/Tracker.csv", stringsAsFactors=FALSE)
ARMASTER <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv", stringsAsFactors=FALSE)

Tracker <- Tracker %>%
  left_join(ARMASTER,by="EMPNUM") %>%
  filter(desk >= 800) %>%
  filter(desk < 900)

Tracker$SetupMonth <- as.Date(Tracker$SetupMonth,'%m/%d/%Y')


TRK <- Tracker %>%
  group_by(SetupMonth) %>%
  summarize(Rehabs = trunc(n()))

rm(ARMASTER);rm(Tracker); rm(names)






average_act <- DF %>%
  group_by(ACT_Month) %>%
  summarize(Activations = n(),
            Ars = n_distinct(A.R),
            Activations_Per_Ar = n()/n_distinct(A.R))









