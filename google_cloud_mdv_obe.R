# CONNECTING TO GOOGLE CLOUD ----------------------------------------------

# Make sure the APIs are enabled
# Get the JSON file with keys and put it in whichever folder you run the requests
# Aprove the request on the browser
# The details bellow are specific to this BUCKET

library(googleAuthR)
library(DBI)
library(bigrquery)

GOOGLE_APPLICATION_CREDENTIALS="gc_json_key.json"

con <- dbConnect(
  bigrquery::bigquery(),
  project = "***********",
  dataset = "******",
  billing = "**************"   # mdv-t2d
)

dbListTables(con)


library(data.table)
library(tidyverse)
library(lubridate)
library(viridis)
library(ggsci)
library(scales)
library(RColorBrewer)

options(scipen = 999)

Obesity_Disease_Data <- read.table("Obesity_Disease_Data.csv", 
                                   header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

Act_Data_Obesity_A08 <- read.table("Act_Data_Obesity_A08.csv", 
                               header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

Obesity_FF1Data <- read.table("Obesity_FF1Data.csv", 
                               header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


Obesity_Disease_Data_unique_IDS <- unique(Obesity_Disease_Data$patientid)
length(Obesity_Disease_Data_unique_IDS)
Unique_Obesity_IDs <- as.data.frame(Obesity_Disease_Data_unique_IDS)

write.csv(Unique_Obesity_IDs, "Unique_Obesity_IDs.csv", row.names = F)

rm(Unique_Obesity_IDs)
rm(Obesity_Disease_Data_unique_IDS)




Obesity_Disease_Data %>%
  select(icd10code) %>%
  group_by(icd10code) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(icd10code,n), y=n))+
  geom_bar(position="dodge", stat="identity", fill="midnightblue", alpha=0.8)+
  ylim(0,1700000)+
  scale_x_discrete(labels=c("E661" = "Drug-induced \n Obesity (E661)", 
                            "R638" = "Deviated Food \n Habit (R638)", 
                            "R635" = "Weight Gain \n (R635)", 
                            "R632" = "Bulimia (R632)", 
                            "E662" = "Obesity with Alveloar \n Hypoventilation Syndrome (E662)", 
                            "E668" = "Morbid Obesity \n (E668)",
                            "E669" = "Obesity (E669)"))+
  geom_text(aes(label = n, vjust = -1))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Number of patient-visits")+
  xlab("")+
  labs(title = "N. of patient-visits, obesity-related disorders")


Obesity_Disease_Data %>%
  select(icd10code) %>%
  group_by(icd10code) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(icd10code,n), y=n))+
  geom_bar(position="dodge", stat="identity", fill="firebrick", alpha=0.8)+
  scale_y_log10(limits=c(1,2000000))+
  scale_x_discrete(labels=c("E661" = "Drug-induced \n Obesity (E661)", 
                            "R638" = "Deviated Food \n Habit (R638)", 
                            "R635" = "Weight Gain \n (R635)", 
                            "R632" = "Bulimia (R632)", 
                            "E662" = "Obesity with Alveloar \n Hypoventilation Syndrome (E662)", 
                            "E668" = "Morbid Obesity \n (E668)",
                            "E669" = "Obesity (E669)"))+
  geom_text(aes(label = n, vjust = -1))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Log 10 (patient-visits)")+
  xlab("")+
  labs(title = "N. of patient-visits, obesity-related disorders")



Obesity_Disease_Data %>%
  select(nyugaikbn) %>%
  mutate(nyugaikbn = as.factor(nyugaikbn)) %>%
  group_by(nyugaikbn) %>%
  summarise(n = n())  %>%
  ggplot(aes(x=nyugaikbn, y=n, fill=nyugaikbn))+
  geom_bar(position="dodge", stat="identity", alpha=0.8, show.legend = FALSE)+
  ylim(0, 1750000)+
  scale_fill_manual(values=c("firebrick", "midnightblue"))+
  scale_x_discrete(labels=c("1" = "1 - Outpatient", "2" = "2 - Inpatient"))+
  geom_text(aes(label = n, vjust = -1))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Number of patient-visits")+
  xlab("")+
  labs(title = "Outpatient vs Inpatient care \n (Apr 2008 - Oct 2021)")



ObsDisDat <- Obesity_Disease_Data %>%
  select(datamonth) %>%
  mutate(datamonth = as.Date(datamonth, origin=lubridate::origin)) %>%
  group_by(datamonth) %>%
  summarise(n = n())  %>%
  arrange(datamonth)

ObsDisDat <- as.data.frame(ObsDisDat)

write.csv(ObsDisDat, "Obesity_Hospital_Visits_Month.csv", row.names = F)
rm(ObsDisDat)


ObDisDat_plot1 <- Obesity_Disease_Data %>%
  select(datamonth) %>%
  mutate(datamonth = as.Date(datamonth, origin=lubridate::origin)) %>%
  group_by(datamonth) %>%
  summarise(n = n())  %>%
  arrange(datamonth) %>%
  ggplot(aes(x=datamonth, y=n, color=datamonth))



ObDisDat_plot1 + 
  ylim(0, 27000)+
  geom_line(color="firebrick", size = 3, alpha = 0.75)+
  scale_x_date(breaks=date_breaks("6 months"), 
               labels=date_format("%b %y")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("") + ylab("Number of patient'visits \n") +
  ggtitle("Number of obesity-related patient visits to the hospital / month \n (Apr 2008 - Oct 2021)")


DiseaseDatDist <- Obesity_Disease_Data %>%
  select(patientid) %>%
  group_by(patientid) %>%
  summarise(n=n()) %>%
  arrange(patientid) %>%
  mutate(typevar= typeof(patientid)) %>%
  ggplot(aes(x=typevar, y=n, color=n))



DiseaseDatDist + geom_jitter(width = 0.6, size = 2, alpha = 0.4, show.legend = F)+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  ylab("Log 10 (N.)") +
  scale_y_log10()+
  ggtitle("Number of visits per patient")



number_visits_per_patient <- Obesity_Disease_Data %>%
  select(patientid) %>%
  group_by(patientid) %>%
  summarise(n=n()) %>%
  arrange(patientid)



finaldate <- as.Date("2021-10-01")


follow_up_days <- Obesity_Disease_Data %>%
  select(patientid, datamonth) %>%
  mutate(datamonth = as.Date(datamonth)) %>%
  group_by(patientid) %>%
  arrange(patientid, datamonth) %>%
  summarise(min = min(datamonth)) %>%
  mutate(followup = finaldate - min)



visits_by_followup <- follow_up_days %>% 
  left_join(number_visits_per_patient, by= "patientid")


visits_by_followup_normalized <- visits_by_followup %>%
  mutate(followup = (followup / 12)) %>%
  mutate(followup = as.numeric(followup)) %>%
  mutate(visitspermonth = (n / followup))

visits_by_followup_normalized_plot <- visits_by_followup_normalized %>%
  select(patientid, visitspermonth) %>%
  mutate(typevar= typeof(patientid)) %>%
  ggplot(aes(x=typevar, y=visitspermonth, color=visitspermonth))


visits_by_followup_normalized_plot + 
  geom_jitter(width = 0.6, size = 2, alpha = 0.3, show.legend = F)+
  geom_violin(alpha = 0.5, fill="yellow", color="yellow", show.legend = F)+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggtitle("N. of visits per patient per month of follow-up")


rm(finaldate, visits_by_followup_normalized, visits_by_followup)
rm(visits_by_followup_normalized_plot)
rm(ObDisDat_plot1)
rm(follow_up_days)
rm(DiseaseDatDist)
rm(number_visits_per_patient)


Obesity_FF1Data <- read.table("Obesity_FF1Data.csv", 
                              header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

M_KaCode <- 
  read.table("D:/Paulo_Bastos/PFIZER_JAPEN_T2_DIABETES_OBESITY/pfizer_dmo_20220131/pfizer_dmo_20220131/M_KaCode.txt", 
                               header = TRUE, sep="\t",  colClasses = "character", stringsAsFactors = FALSE)

Obesity_FF1Data_unique_IDS <- unique(Obesity_FF1Data$patientid)
length(Obesity_FF1Data_unique_IDS)


Obesity_FF1Data <-
  Obesity_FF1Data %>% 
  filter(!is.na(Obesity_FF1Data$weight) & !is.na(Obesity_FF1Data$height) & Obesity_FF1Data$weight != 0 &  Obesity_FF1Data$height != 0)



Obesity_FF1_Data_w_DEPARTMENT <- Obesity_FF1Data %>% left_join(M_KaCode, by="kacodeuni")



Obesity_FF1_Data_unique_IDS <- unique(Obesity_FF1_Data_w_DEPARTMENT$patientid)
length(Obesity_FF1_Data_unique_IDS)  #

str(Obesity_FF1_Data_w_DEPARTMENT)

rm(M_KaCode, Obesity_FF1Data, Obesity_FF1_Data_unique_IDS)



Obesity_FF1_Data_w_DEPARTMENT$weight <- as.numeric(Obesity_FF1_Data_w_DEPARTMENT$weight)
Obesity_FF1_Data_w_DEPARTMENT$height <- as.numeric(Obesity_FF1_Data_w_DEPARTMENT$height)

Obesity_FF1_Data_w_DEPARTMENT <- Obesity_FF1_Data_w_DEPARTMENT %>%
  select(patientid, weight, height, kacodeuni, kaname_eng) %>%
  mutate(BMI = weight / (height / 100)^2 )
  

Obesity_FF1_Data_unique_DEPS <- unique(Obesity_FF1_Data_w_DEPARTMENT$kacodeuni)
length(Obesity_FF1_Data_unique_DEPS)


Obesity_FF1_Data_w_DEPARTMENT <- Obesity_FF1_Data_w_DEPARTMENT %>%  
  filter(BMI > 10)

range(Obesity_FF1_Data_w_DEPARTMENT$BMI)

Obesity_FF1_Data_w_DEPARTMENT %>%  
  select(patientid, BMI) %>%
  mutate(typevar= typeof(patientid)) %>%
  ggplot(aes(x=typevar, y=BMI, color=BMI)) + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  geom_violin(alpha = 0.6, fill="yellow", show.legend = F, colour = "white")+
  geom_jitter(width = 0.5, size = 2, alpha = 0.6, show.legend = F)+
  ggtitle("BMIs, obesity-related disorder patients \n (Hospital visits Apr 2008 - Oct 2021)")



Obesity_FF1_Data_w_DEPARTMENT %>%  
  select(patientid, kaname_eng) %>%
  group_by(kaname_eng) %>%
  arrange(kaname_eng) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  ggplot(aes(x=reorder(kaname_eng, n), y=n, fill = n))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of registered visits by Medical Department")+
  scale_fill_viridis_c() 

rm(Obesity_FF1_Data_w_DEPARTMENT, Obesity_FF1_Data_unique_DEPS, Obesity_FF1Data_unique_IDS)




Act_Data_Obesity <- read.table("Act_Data_Obesity.csv", 
                               header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


M_DataKbn <- 
  read.table("D:/Paulo_Bastos/PFIZER_JAPEN_T2_DIABETES_OBESITY/pfizer_dmo_20220131/pfizer_dmo_20220131/M_DataKbn.txt", 
             header = TRUE, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

Act_Data_Obesity %>%
  select(patientid, datakbn) %>%
  left_join(M_DataKbn, by = "datakbn") %>%
  group_by(datakbnname_eng) %>%
  arrange(datakbnname_eng) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  ggplot(aes(x=reorder(datakbnname_eng, n), y=n, fill = n))+
  geom_bar(stat = "identity", show.legend = FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  ggtitle("N. of registered medical treatment claims")+
  geom_text(aes(label = n, vjust = -1))+
  ylim(0,8000)+
  scale_fill_viridis_c() 


rm(Act_Data_Obesity)
rm(M_DataKbn)



Obesity_Disease_Data <- read.table("Obesity_Disease_Data.csv", 
                                   header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)



all_obesity_age_sex <- read.table("all_obesity_age_sex.csv", 
                                 header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)



Obesity_Disease_Data_demographics <- 
  Obesity_Disease_Data %>% 
  inner_join(all_obesity_age_sex, by = c("patientid" = "patientid", "datamonth" = "datamonth"))


rm(all_obesity_age_sex, Obesity_Disease_Data)



Obesity_Disease_Data_demographics$age <- as.numeric(Obesity_Disease_Data_demographics$age)
range(Obesity_Disease_Data_demographics$age)
sort(Obesity_Disease_Data_demographics$age)


data.frame(Obesity_Disease_Data_demographics %>%
  group_by(age) %>%
  summarise(n = n()))

Obesity_Disease_Data_demographics$sex = as.factor(Obesity_Disease_Data_demographics$sex)

Obesity_Disease_Data_demographics %>%
  group_by(sex) %>%
  summarise(n = n())


labs <- c(paste(seq(0, 95, by = 5), 
                seq(0 + 5 - 1, 100 - 1, by = 5), sep = "-"), 
          paste(100, "+", sep = ""))

Obesity_Disease_Data_demographics$AgeGroup <- cut(Obesity_Disease_Data_demographics$age, 
                                                  breaks = c(seq(0, 100, by = 5), Inf), 
                                                  labels = labs, right = FALSE)

head(Obesity_Disease_Data_demographics[c("age", "AgeGroup")], 100)


print(Obesity_Disease_Data_demographics %>%
  select(icd10code, AgeGroup) %>%
  group_by(icd10code, AgeGroup) %>%
  summarise(n = n()), n=140)


myCols <- colorRampPalette(brewer.pal(9, "Accent"))
mySetCols <- myCols(length(unique(Obesity_Disease_Data_demographics$AgeGroup)))


Obesity_Disease_Data_demographics %>%
  #filter(age >= 18) %>%
  select(icd10code, AgeGroup) %>%
  group_by(icd10code, AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(icd10code,n), y=n, fill=AgeGroup))+
  geom_bar(position="stack", stat="identity", alpha=0.9)+
  ylim(0,1700000)+
  scale_x_discrete(labels=c("E661" = "Drug-induced \n Obesity (E661)", 
                            "R638" = "Deviated Food \n Habit (R638)", 
                            "R635" = "Weight Gain \n (R635)", 
                            "R632" = "Bulimia (R632)", 
                            "E662" = "Obesity with Alveloar \n Hypoventilation Syndrome (E662)", 
                            "E668" = "Morbid Obesity \n (E668)",
                            "E669" = "Obesity (E669)"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Number of patient-visits")+
  xlab("")+
  labs(title = "N. of patient-visits, obesity-related disorders")+
  scale_fill_manual(values= mySetCols)


Obesity_Disease_Data_demographics %>%
  #filter(age >= 18) %>%
  select(icd10code, AgeGroup) %>%
  group_by(icd10code, AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(icd10code,n), y=n, fill=AgeGroup))+
  geom_bar(position="stack", stat="identity", alpha=1)+
  ylim(0,1700000)+
  scale_x_discrete(labels=c("E661" = "Drug-induced \n Obesity (E661)", 
                            "R638" = "Deviated Food \n Habit (R638)", 
                            "R635" = "Weight Gain \n (R635)", 
                            "R632" = "Bulimia (R632)", 
                            "E662" = "Obesity with Alveloar \n Hypoventilation Syndrome (E662)", 
                            "E668" = "Morbid Obesity \n (E668)",
                            "E669" = "Obesity (E669)"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Number of patient-visits")+
  xlab("")+
  labs(title = "N. of patient-visits, obesity-related disorders")+
  scale_fill_viridis_d()


levels(Obesity_Disease_Data_demographics$sex) <- c("Male", "Female")

Obesity_Disease_Data_demographics %>%
  select(icd10code, sex) %>%
  group_by(icd10code, sex) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=reorder(icd10code,n), y=n, fill=sex))+
  geom_bar(position="stack", stat="identity", alpha= 0.9)+
  scale_x_discrete(labels=c("E661" = "Drug-induced \n Obesity (E661)", 
                            "R638" = "Deviated Food \n Habit (R638)", 
                            "R635" = "Weight Gain \n (R635)", 
                            "R632" = "Bulimia (R632)", 
                            "E662" = "Obesity with Alveloar \n Hypoventilation Syndrome (E662)", 
                            "E668" = "Morbid Obesity \n (E668)",
                            "E669" = "Obesity (E669)"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  ylab("Number of patient-visits")+
  xlab("")+
  labs(title = "N. of patient-visits, obesity-related disorders")+
  scale_fill_nejm()



ObsDisDat <- Obesity_Disease_Data_demographics %>%
  filter(age >= 18) %>%
  select(datamonth, AgeGroup) %>%
  mutate(datamonth = as.Date(datamonth, origin=lubridate::origin)) %>%
  group_by(datamonth, AgeGroup) %>%
  summarise(n = n())  %>%
  arrange(datamonth)

ObsDisDat <- as.data.frame(ObsDisDat)


ObDisDat_plot1 <- Obesity_Disease_Data_demographics %>%
  #filter(age >= 18) %>%
  select(datamonth, AgeGroup) %>%
  mutate(datamonth = as.Date(datamonth, origin=lubridate::origin)) %>%
  group_by(datamonth, AgeGroup) %>%
  summarise(n = n())  %>%
  arrange(datamonth) %>%
  ggplot(aes(x=datamonth, y=n, color=AgeGroup))



ObDisDat_plot1  + geom_line( size = 1.5, alpha = 0.9)+
  scale_x_date(breaks=date_breaks("6 months"), 
               labels=date_format("%b %y")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("") + ylab("Number of patient'visits \n") +
  ggtitle("Number of obesity-related patient visits to the hospital / month \n (Apr 2008 - Oct 2021)")+
  scale_color_viridis_d()

rm(ObDisDat_plot1, ObsDisDat, mySetCols, myCols)





write.csv(Obesity_Disease_Data_demographics, "Obesity_Disease_Data_demographics.csv", row.names = F)

data.frame(
  Obesity_Disease_Data_demographics %>%
    select(patientid, age) %>%
    group_by(patientid) %>%
    arrange(patientid, age))


UniquePatient_MaxAge <- Obesity_Disease_Data_demographics %>%
       select(patientid, age) %>%
       group_by(patientid) %>%
       arrange(patientid, age) %>%
       summarize(across(everything(), max))


UniquePatient_MaxAge$AgeGroup <- 
  cut(UniquePatient_MaxAge$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)

UniquePatient_MaxAge %>%
  filter(age >= 18) %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n , fill = AgeGroup))+
  geom_bar(stat="identity") +
  ylim(0,13000)+
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("Number of unique patients per Age Group")


Act_Data_Obesity <- read.table("Act_Data_Obesity.csv", 
                                   header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


Obesity_Disease_Data_demographics <- Obesity_Disease_Data_demographics %>%
  select(-c(fromdate, nyugaikbn, icd10code, diseasecode, diseasename_eng))
  
length(unique(Obesity_Disease_Data_demographics$patientid))  # all good, 


Obesity_Demographics_with_DRUG_label <- Obesity_Disease_Data_demographics %>%
  left_join(Act_Data_Obesity, by = c("patientid" = "patientid", "datamonth" = "datamonth"))


Obesity_Demographics_with_DRUG_label <- Obesity_Demographics_with_DRUG_label %>% distinct()

Obesity_DRUG_label_by_month <- Obesity_Demographics_with_DRUG_label %>%
  arrange(datamonth)

Obesity_DRUG_label_by_month <- Obesity_DRUG_label_by_month %>%
  mutate(datakbn = ifelse(is.na(datakbn), "OFF Drug", "ON Drug"))

Obesity_DRUG_label_by_month$datamonth <- as.Date(Obesity_DRUG_label_by_month$datamonth)

Cohort_Until_Oct_2012 <- Obesity_DRUG_label_by_month %>%
  filter(datamonth < "2012-10-01")

# Oct 2012 to Apr 2017
Cohort_Oct_2012_April_2017 <- Obesity_DRUG_label_by_month %>%
  filter(datamonth >= "2012-10-01" & datamonth < "2017-04-01")

# From Apr 2017 onwars
Cohort_April_2017_Oct_2021 <- Obesity_DRUG_label_by_month %>%
  filter(datamonth >= "2017-04-01")


Cohort_Until_Oct_2012 <- Cohort_Until_Oct_2012 %>%
  select(-c(AgeGroup, sex))

Cohort_Oct_2012_April_2017 <- Cohort_Oct_2012_April_2017 %>%
  select(-c(AgeGroup, sex))

Cohort_April_2017_Oct_2021 <- Cohort_April_2017_Oct_2021 %>%
  select(-c(AgeGroup, sex))



UniquePatient_MaxAge_Cohort_Until_Oct_2012 <- Cohort_Until_Oct_2012 %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

UniquePatient_MaxAge_Cohort_Oct_2012_April_2017 <- Cohort_Oct_2012_April_2017 %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

UniquePatient_MaxAge_Cohort_April_2017_Oct_2021 <- Cohort_April_2017_Oct_2021 %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))



UniquePatient_MaxAge_Cohort_Until_Oct_2012$AgeGroup <- 
  cut(UniquePatient_MaxAge_Cohort_Until_Oct_2012$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)

UniquePatient_MaxAge_Cohort_Oct_2012_April_2017$AgeGroup <- 
  cut(UniquePatient_MaxAge_Cohort_Oct_2012_April_2017$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)

UniquePatient_MaxAge_Cohort_April_2017_Oct_2021$AgeGroup <- 
  cut(UniquePatient_MaxAge_Cohort_April_2017_Oct_2021$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)





UniquePatient_MaxAge_Cohort_Until_Oct_2012$datakbn <- as.factor(UniquePatient_MaxAge_Cohort_Until_Oct_2012$datakbn)

UniquePatient_MaxAge_Cohort_Until_Oct_2012 %>%
  filter(datakbn == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF DRUG / Age Group (Apr 2008 - Oct 2012)")

names(UniquePatient_MaxAge_Cohort_Until_Oct_2012)
UniquePatient_MaxAge_Cohort_Until_Oct_2012 %>%
  filter(datakbn == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON DRUG / Age Group (Apr 2008 - Oct 2012)")



UniquePatient_MaxAge_Cohort_Oct_2012_April_2017$datakbn <- as.factor(UniquePatient_MaxAge_Cohort_Oct_2012_April_2017$datakbn)

UniquePatient_MaxAge_Cohort_Oct_2012_April_2017 %>%
  filter(datakbn == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.7, fill = "deeppink4")+
  ylim(0,5000)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF DRUG / Age Group (Oct 2012 - Apr 2017)")


UniquePatient_MaxAge_Cohort_Oct_2012_April_2017 %>%
  filter(datakbn == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.7, fill = "aquamarine4")+
  ylim(0,5000)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON DRUG / Age Group (Oct 2012 - Apr 2017)")



UniquePatient_MaxAge_Cohort_April_2017_Oct_2021$datakbn <- as.factor(UniquePatient_MaxAge_Cohort_April_2017_Oct_2021$datakbn)

UniquePatient_MaxAge_Cohort_April_2017_Oct_2021 %>%
  filter(datakbn == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.7, fill = "deeppink4")+
  ylim(0,9000)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF DRUG / Age Group (Aprl 2017 - Oct 2021)")


UniquePatient_MaxAge_Cohort_April_2017_Oct_2021 %>%
  filter(datakbn == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.7, fill = "aquamarine4")+
  ylim(0,9000)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON DRUG / Age Group (Aprl 2017 - Oct 2021)")




How_many_OFF_2017_to_2021 <- data.frame(UniquePatient_MaxAge_Cohort_April_2017_Oct_2021 %>%
                                          filter(datakbn == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_2017_to_2021 <- data.frame(UniquePatient_MaxAge_Cohort_April_2017_Oct_2021 %>%
                                         filter(datakbn == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))

How_many_OFF_vs_ON <- How_many_OFF_2017_to_2021 %>% left_join(How_many_ON_2017_to_2021, by = "AgeGroup")
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)
How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)

How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "darkkhaki")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with Mazindol Prescribed upon Hospital Visit (Apr 2017 - Oct 2021)")



Obesity_Disease_Data_demographics <- read.table("Obesity_Disease_Data_demographics.csv", 
                                                header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)


Act_Data_Obesity <- read.table("Act_Data_Obesity.csv", 
                               header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

Obesity_Disease_Data_demographics <- Obesity_Disease_Data_demographics %>%
  select(-c(fromdate, nyugaikbn, icd10code, diseasecode, diseasename_eng))

length(unique(Obesity_Disease_Data_demographics$patientid))  # all good, 

Obesity_Demographics_with_DRUG_label <- Obesity_Disease_Data_demographics %>%
  left_join(Act_Data_Obesity, by = c("patientid" = "patientid", "datamonth" = "datamonth"))

Obesity_Demographics_with_DRUG_label <- Obesity_Demographics_with_DRUG_label %>% distinct()


Obesity_DRUG_label_by_month <- Obesity_Demographics_with_DRUG_label %>%
  arrange(datamonth)

Obesity_DRUG_label_by_month <- Obesity_DRUG_label_by_month %>%
  mutate(datakbn = ifelse(is.na(datakbn), "OFF Drug", "ON Drug"))


Obesity_DRUG_label_by_month$datamonth <- as.Date(Obesity_DRUG_label_by_month$datamonth)

Cohort_Year_Minus_1 <-Obesity_DRUG_label_by_month %>%
  filter(datamonth >= "2020-10-01")

Cohort_Year_Minus_1 <- Cohort_Year_Minus_1 %>%
  select(-c(AgeGroup, sex))

UniquePatient_MaxAge_Cohort_Year_Minus_1 <- Cohort_Year_Minus_1 %>%
  group_by(patientid) %>%
  arrange(patientid, age) %>%
  summarize(across(everything(), max))

UniquePatient_MaxAge_Cohort_Year_Minus_1$age <- as.numeric(UniquePatient_MaxAge_Cohort_Year_Minus_1$age)

UniquePatient_MaxAge_Cohort_Year_Minus_1$AgeGroup <- 
  cut(UniquePatient_MaxAge_Cohort_Year_Minus_1$age,
      breaks = c(seq(0, 100, by = 5), Inf), 
      labels = labs, right = FALSE)

UniquePatient_MaxAge_Cohort_Year_Minus_1$datakbn <- as.factor(UniquePatient_MaxAge_Cohort_Year_Minus_1$datakbn)

UniquePatient_MaxAge_Cohort_Year_Minus_1 %>%
  filter(datakbn == "OFF Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "deeppink4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients OFF DRUG / Age Group (Year -1)")

UniquePatient_MaxAge_Cohort_Year_Minus_1 %>%
  filter(datakbn == "ON Drug") %>%
  group_by(AgeGroup) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = AgeGroup, y = n, fill = AgeGroup))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "aquamarine4")+
  #ylim(0,900)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("") +
  geom_text(aes(label = n, vjust = -1))+
  ggtitle("N. of unique patients ON DRUG / Age Group (Year -1)")

How_many_OFF_Year_Minus_1 <- data.frame(UniquePatient_MaxAge_Cohort_Year_Minus_1 %>%
                                          filter(datakbn == "OFF Drug") %>%
                                          group_by(AgeGroup) %>%
                                          summarise(n = n()))

How_many_ON_Year_Minus_1 <- data.frame(UniquePatient_MaxAge_Cohort_Year_Minus_1 %>%
                                         filter(datakbn == "ON Drug") %>%
                                         group_by(AgeGroup) %>%
                                         summarise(n = n()))


How_many_OFF_vs_ON <- How_many_OFF_Year_Minus_1 %>% left_join(How_many_ON_Year_Minus_1, by = "AgeGroup")

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% replace_na(list(n.x = 0, n.y = 0))

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(sumrow= n.x + n.y)

How_many_OFF_vs_ON <- How_many_OFF_vs_ON %>% mutate(proportion = (n.y / sumrow) * 100)


How_many_OFF_vs_ON %>%
  ggplot(aes(x = AgeGroup, y = proportion, fill = AgeGroup, label=sprintf("%0.2f", round(proportion, digits = 2))))+
  geom_bar(position = "stack", stat="identity", alpha = 0.8, fill = "firebrick")+
  ylim(0,10)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("") + ylab("%") +
  geom_text(aes(vjust = -1))+
  ggtitle("Percentage (%) of Unique Patients with Mazindol Prescribed upon Hospital Visit (Last 12 months)")



Act_Data_Obesity_All_Drugs <- read.table("Act_Data_Obesity_All_Drugs.csv", 
                               header = TRUE, sep=",", colClasses = "character", stringsAsFactors = FALSE)

Act_Data_Obesity_All_Drugs_unique_IDS <- unique(Act_Data_Obesity_All_Drugs$patientid)
length(Act_Data_Obesity_All_Drugs_unique_IDS)

Act_Data_Obesity_All_Drugs_unique_drugs <- unique(Act_Data_Obesity_All_Drugs$receiptcode)
length(Act_Data_Obesity_All_Drugs_unique_drugs)

xx <- data.frame(Act_Data_Obesity_All_Drugs %>%
                   select(patientid, receiptcode) %>%
  group_by(patientid, receiptcode) %>%
  summarise(n = n())) 

M_Drug  <- 
  fread("M_Drug.txt", 
             header = TRUE, sep="\t", colClasses = "character", stringsAsFactors = FALSE)

M_Drug <- M_Drug %>% select(-c("drugusagecode", "genericcode"))

xx <- xx %>%
  inner_join(M_Drug, by = "receiptcode") 


Drugs_Obesity_Patients_N <- data.frame(xx %>% group_by(druggeneralname_eng) %>% summarise(n = n())) %>%
  arrange(n)

write.csv(Drugs_Obesity_Patients_N, "Drugs_Obesity_Patients_N_LastYear.csv")

