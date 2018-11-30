nvss_dat<-read.csv("./data/mort_cause.csv",
              stringsAsFactors = FALSE)

### list of 51 rankable causes based on 2016 leading causes report from NVSS
### https://www.cdc.gov/nchs/data/nvsr/nvsr67/nvsr67_06.pdf
nvss_dat<-nvss_dat%>%
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
           ))

#### filter for only legal intervention deaths

nvss_dat<-nvss_dat%>%
  #filter(cause_113 == 130)%>%
  #select(-cause_113)%>%
  filter(!(is.na(pop)))

make_life_table<-function(nat_nvss_dat){
  nat_nvss_dat<-nat_nvss_dat%>%
    ungroup(nat_nvss_dat)
  ### create proportions of pop with each TPR outcome by age/year
  nat_nvss_dat<-nat_nvss_dat%>%
    mutate(m = deaths / pop)
  
  ### convert to probability 
  ### age_period (n) = 1 for all cases
  ### a = 0.5 (avg age of TPR for ppl in year, within-period survival)
  nat_nvss_dat<-nat_nvss_dat%>%
    mutate(q = 1 * m / (1 + (1 - 0.5) * m),
           p = 1 - q)
  ### make cumulative survival
  nat_nvss_dat<-nat_nvss_dat%>%
    mutate(lx = 1e5 * cumprod(c(1, p))[-nrow(nat_nvss_dat)])
  ### deaths
  nat_nvss_dat<-nat_nvss_dat%>%
    mutate(d = -c(diff(lx),0))
  ## person-years in each group
  nat_nvss_dat<-nat_nvss_dat%>%
    mutate(L = (lx - d) * 1 + d * 0.5,
           t = sum(L)- cumsum(L) + L)
  ## life expectancy (time to TPR)
  nat_nvss_dat<-nat_nvss_dat%>%
    mutate(e = t/lx)
  ### cum prevalence
  nat_nvss_dat<-nat_nvss_dat%>%
    mutate(c = 1-lx/1e5)
  return(nat_nvss_dat)
}

### make pooled mortatlity tables
tables<-list()
races<-unique(nvss_dat$race)
sexs<-unique(nvss_dat$sex)
causes<-unique(nvss_dat$cause_50)
k<-0
for(h in 1:length(causes)){
  for(j in 1:length(races)){
    for(g in 1:length(sexs)){
      k<-k+1
      tables[[k]]<-make_life_table(nvss_dat%>%
                                     filter(race==races[j])%>%
                                     filter(sex==sexs[g])%>%
                                     filter(cause_50==causes[h])%>%
                                     group_by(race, sex, age, cause_50)%>%
                                     summarise(pop = sum(pop), deaths=sum(deaths)))
    }
  }
}

nvss_tables<-bind_rows(tables)