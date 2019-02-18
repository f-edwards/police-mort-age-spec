### Read in and format NVSS multiple cause mortality files

library(tidyverse)

pos<-
  fwf_positions(
    c(69, 77, 154, 445, 484), #begin
    c(69, 78, 156, 446, 486), #end
    c("sex",
      "age_27",
      "cause_113",
      "race",
      "hisorgin") #colname
  )

files<-list(
  "./data/VS17MORT.DUSMCPUB",
  "./data/VS16MORT.DUSMCPUB",
  "./data/VS15MORT.DUSMCPUB",
  "./data/VS14MORT.DUSMCPUB",
  "./data/VS13MORT.DUSMCPUB",
  "./data/VS12MORT.DUSMCPUB",
  "./data/VS11MORT.DUSMCPUB",
  "./data/VS10MORT.DUSMCPUB",
  "./data/VS09MORT.DUSMCPUB",
  "./data/Mort2008us.dat")

mort<-lapply(
  files, 
  function(x){
    read_fwf(x, pos)
  }
)
years<-2017:2008
for(i in 1:length(mort)){
  mort[[i]]$year<-years[[i]]
}

mort<-bind_rows(mort)

### convert 27 cat age into character

mort<-mort%>%
  mutate(
    age = 
      case_when(
        age_27 %in% c("01", "02") ~ "0",
        age_27 %in% c("03", "04", "05", "06") ~ "1-4",
        age_27 == "07" ~ "5-9",
        age_27 == "08" ~ "10-14",
        age_27 == "09" ~ "15-19",
        age_27 == "10" ~ "20-24",
        age_27 == "11" ~ "25-29",
        age_27 == "12" ~ "30-34",
        age_27 == "13" ~ "35-39",
        age_27 == "14" ~ "40-44",
        age_27 == "15" ~ "45-49",
        age_27 == "16" ~ "50-54",
        age_27 == "17" ~ "55-59",
        age_27 == "18" ~ "60-64",
        age_27 == "19" ~ "65-69",
        age_27 == "20" ~ "70-74",
        age_27 == "21" ~ "75-79",
        age_27 == "22" ~ "80-84",
        age_27 %in% c("23", "24", "25", "26") ~ "85+",
        age_27 == "27" ~ "Missing")
  )

### convert race into character, compress race/hispanic ethnicity

mort<-mort%>%
  mutate(
    race= ifelse(
      race == "01" & hisorgin <200, "white",
      ifelse(race == "02", "black",
             ifelse(race == "03", "amind",
                    ifelse(hisorgin >200 & hisorgin<996, "latino",
                           "asian")))))

### make sex and race more verbose for presentation

mort<-mort%>%
  mutate(
    sex = case_when(
      sex=="M"~"Male",
      sex=="F"~"Female")
  )%>%
  mutate(race=
           case_when(
             race=="amind" ~ "American Indian/AK Native",
             race=="asian" ~ "Asian/Pacific Islander",
             race=="black" ~ "African American",
             race=="latino"~ "Latinx",
             race=="white" ~ "White"
           ))

### make age/gender/race total death rate, death by cause rate

total_mort<-mort%>%
  group_by(year, age, race, sex)%>%
  summarise(deaths = n())

mort_cause<-mort%>%
  group_by(year, age, race, sex, cause_113)%>%
  summarise(deaths = n())%>%
  arrange(age, race, sex, deaths)

### fill in zeroes

mort_cause<-mort_cause%>% 
  ungroup()%>%
  tidyr::complete(year, age, race, sex, cause_113, 
                  fill = list(deaths = 0))

### list of 51 rankable causes based on 2016 leading causes report from NVSS
### https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_06.pdf
mort_cause<-mort_cause%>%
  mutate(cause_113 = as.numeric(cause_113))%>%
  mutate(cause_50 = 
           case_when(
             cause_113 == 1 ~ "Salmonella infections",
             cause_113 == 2 ~ "Shigellosis and amebiasis",
             cause_113 %in% 5:6 ~ "Tuberculosis",
             cause_113 == 7 ~ "Whooping cough",
             cause_113 == 8 ~ "Scarlet fever and erysipelas",
             cause_113 == 9 ~ "Meningococcal infection",
             cause_113 == 10 ~ "Septicemia",
             cause_113 == 11 ~ "Syphilis",
             cause_113 == 12 ~ "Acute poliomyelitis",
             cause_113 == 13 ~ "Arthropod-borne viral encephalitis",
             cause_113 == 14 ~ "Measles",
             cause_113 == 15 ~ "Viral hepatitis",
             cause_113 == 16 ~ "Human immunodeficiency virus (HIV) disease",
             cause_113 == 17 ~ "Malaria",
             cause_113 %in% 20:43 ~ "Malignant neoplasms",
             cause_113 == 44 ~ "In situ neoplasms, benign neoplasms and neoplasms of uncertain or unknown behavior",
             cause_113 == 45 ~ "Anemias",
             cause_113 == 46 ~ "Diabetes mellitus",
             cause_113 %in% 48:49 ~ "Nutritional deficiencies",
             cause_113 == 50 ~ "Meningitis",
             cause_113 == 51 ~ "Parkinson's disease",
             cause_113 == 52 ~ "Alzheimer's disease",
             cause_113 %in% 55:68 ~ "Diseases of heart",
             cause_113 == 69 ~ "Essential (primary) hypertension and hypertensive renal disease",
             cause_113 == 70 ~ "Cerebrovascular diseases",
             cause_113 == 71 ~ "Atherosclerosis",
             cause_113 == 73 ~ "Aortic aneurysm and dissection",
             cause_113 %in% 77:78 ~ "Influenza and pneumonia",
             cause_113 == 80 ~ "Acute bronchitis and bronchiolitis",
             cause_113 %in% 83:86 ~ "Other acute lower respiratory infections",
             cause_113 == 87 ~ "Pneumoconioses and chemical effects",
             cause_113 == 88 ~ "Pneumonitis due to solids and liquids",
             cause_113 == 90 ~ "Peptic ulcer",
             cause_113 == 91 ~ "Diseases of appendix",
             cause_113 == 92 ~ "Hernia",
             cause_113 %in% 94:95 ~ "Chronic liver disease and cirrhosis",
             cause_113 == 96 ~ "Cholelithiasis and other disorders of gallbladder",
             cause_113 %in% 98:101 ~ "Nephritis, nephrotic syndrome and nephrosis",
             cause_113  == 102 ~ "Infections of kidney",
             cause_113 == 103 ~ "Hyperplasia of prostate",
             cause_113 == 104 ~ "Inflammatory diseases of female pelvic organs",
             cause_113 %in% 106:107 ~ "Pregnancy, childbirth and the puerperium",
             cause_113 == 108 ~ "Certain conditions originating in the perinatal period",
             cause_113 == 109 ~ "Congenital malformations, deformations and chromosomal abnormalities",
             cause_113 %in% 113:123 ~ "Accidents (unintenional injuries)",
             cause_113 %in% 125:126 ~ "Intentional self-harm (suicide)",
             cause_113 %in% 128:129 ~ "Assault (homicide)",
             cause_113 == 130 ~ "Legal intervention",
             cause_113 == 134 ~ "Operations of war and their sequelae",
             cause_113 == 135 ~ "Complications of medical and surgical care"
           ))%>%
  group_by(year, age, race, sex, cause_50)%>%
  summarise(deaths = sum(deaths))%>%
  mutate(cause_50 = ifelse(is.na(cause_50), "Other", cause_50))

## output files
write_csv(total_mort, "./data/total_mort.csv")
write_csv(mort_cause, "./data/mort_cause.csv")
