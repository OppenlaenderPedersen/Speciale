#OMKODNING AF DATA

#Først sættes working directory.
setwd("C:/Users/niels/Documents/R-filer_Speciale")

#Herefter loades relevante pakker.
library(tidyverse)
library(haven)
library(labelled)
library(readxl)
recode = dplyr::recode # Specification needed as "dplyr" is in conflict with another package.

#Nu indlæeses data.
Data = read_excel("Conjoint_Excel-fil.xlsx")
names(Data)

#Herefter fjernes respondenter, som har gennemført eksperimentet på under 3 minutter,
#da disse sansynligvis ikke har tænkt nok over informationen, og derfor kan være
#udtryk for survey satisficing (Berinsky et al., 2014).
Data = Data %>%
  filter(RESPONDENT_LENGTH_OF_INTERVIEW_SECONDS > 180)
#151 respondenter fjernes på denne måde.

#Variablene sammensættes på baggrund af survey variable.
Data = Data %>%
  mutate(køn = case_when(Q2_KØN_KVINDE==1 ~ "Kvinde",
                         Q2_KØN_MAND==1 ~ "Mand"),
         alder = Q3_ALDER,
         region = case_when(Q4_REGION_NORDJYLLAND==1 ~ "Nordjylland", 
                           Q4_REGION_MIDTJYLLAND==1 ~ "Midtjylland",
                           Q4_REGION_SYDDANMARK==1 ~ "Syddanmark",
                           Q4_REGION_SJÆLLAND==1 ~ "Sjælland",
                           Q4_REGION_HOVEDSTADEN==1 ~ "Hovedstaden"),
         bystørrelse = case_when(Q5_BYSTØRRELSE_KOBENHAVN_STORKOBENHAVN==1 ~ "København/Storkøbenhavn",
                                 Q5_BYSTØRRELSE_AARHUS_AALBORG_ELLER_ODENSE==1 ~ "Aarhus, Aalborg eller Odense",
                                 Q5_BYSTØRRELSE_OVER_40.000_INDBYGGERE==1 ~ "Over 40.000 indbyggere",
                                 Q5_BYSTØRRELSE_20.000_39.999_INDBYGGERE==1 ~ "20.000-39.999 indbyggere",
                                 Q5_BYSTØRRELSE_10.000_19.999_INDBYGGERE==1 ~ "10.000-19.999 indbyggere",
                                 Q5_BYSTØRRELSE_3.000_9.999_INDBYGGERE==1 ~ "3.000-9.999 indbyggere",
                                 Q5_BYSTØRRELSE_1.000_2.999_INDBYGGERE==1 ~ "1.000-2.999 indbyggere",
                                 Q5_BYSTØRRELSE_200_999_INDBYGGERE==1 ~ "200-999 indbyggere",
                                 Q5_BYSTØRRELSE_UNDER_200_INDBYGGERE==1 ~ "Under 200 indbyggere"),
         uddannelsesniveau = case_when(Q6_UDDANNELSESNIVEAU_GRUNDSKOLE==1 ~ "Grundskole",
                                       Q6_UDDANNELSESNIVEAU_STUDENTEREKSAMEN==1 ~ "Studentereksamen",
                                       Q6_UDDANNELSESNIVEAU_ERHVERVSFAGLIG==1 ~ "Erhvervsuddannelse",
                                       Q6_UDDANNELSESNIVEAU_KORT_VIDEREGAENDE==1 ~ "Kort videregående",
                                       Q6_UDDANNELSESNIVEAU_MELLEMLANG_VIDEREGAENDE==1 ~ "Mellemlang videregående",
                                       Q6_UDDANNELSESNIVEAU_BACHELORUDDANNELSE==1 ~ "Bacheloruddannelse",
                                       Q6_UDDANNELSESNIVEAU_LANG_VIDEREGAENDE==1 ~ "Lang videregående",
                                       Q6_UDDANNELSESNIVEAU_ANDET==1 ~ "Andet"),
         hensyn_vs_hastighed = case_when(Q10_HENSYN_VS_HASTIGHED_LOKALSAMFUNDET_HELT_KLART_VIGTIGST==1
                                         ~ "Hensyn til lokalsamfundet er helt klart vigtigst",
                                         Q10_HENSYN_VS_HASTIGHED_LOKALSAMFUNDET_LIDT_MERE_VIGTIGT==1
                                         ~ "Hensyn til lokalsamfundet er lidt mere vigtigt",
                                         Q10_HENSYN_VS_HASTIGHED_HASTIGHEDEN_LIDT_MERE_VIGTIG==1
                                         ~ "Hastighed for opsætningen af sol og vind er lidt mere vigtig",
                                         Q10_HENSYN_VS_HASTIGHED_HASTIGHEDEN_HELT_KLART_VIGTIGST==1
                                         ~ "Hastighed for opsætningen af sol og vind er helt klart vigtigst"),
         screener_1_CO2_gevinst = case_when(Q11_SCREENER_CO2_GEVINST_SOLCELLEPARKERNE_HAVDE_DEN_STORSTE_CO2_GEVINST==1
                                            ~ "Solceller havde den største CO2-gevinst",
                                            Q11_SCREENER_CO2_GEVINST_VINDMOLLEPARKERNE_HAVDE_DEN_STORSTE_CO2_GEVINST==1
                                            ~ "Vindmøller havde den største CO2-gevinst",
                                            Q11_SCREENER_CO2_GEVINST_BEGGE_SAMME_CO2_GEVINST==1
                                            ~ "Begge slags projekter havde den samme CO2-gevinst",
                                            Q11_SCREENER_CO2_GEVINST_VED_IKKE==1 ~ "Ved ikke"),
         screener_2_areal = case_when(Q12_SCREENER_AREAL_SOLCELLEPARKERNE_FYLDTE_DET_STORSTE_AREAL==1
                                      ~ "Solceller fyldte det største areal",
                                      Q12_SCREENER_AREAL_VINDMOLLEPARKERNE_FYLDTE_DET_STORSTE_AREAL==1
                                      ~ "Vindmøller fyldte det største areal",
                                      Q12_SCREENER_AREAL_BEGGE_SLAGS_PROJEKTER_FYLDTE_DET_SAMME_AREAL==1
                                      ~ "Begge slags projekter fyldte det samme areal",
                                      Q12_SCREENER_AREAL_VED_IKKE==1 ~ "Ved ikke"))

#Omkodning af variable for hypotetiske VE-projekter.
Data$Profil_afstand = factor(Data$Profil_afstand,
                             labels = c("1 km",
                                        "3 km",
                                        "5 km"))
Data$Profil_type = factor(Data$Profil_type,
                          labels = c("Solcellepark",
                                     "Vindmøllepark"))
Data$Profil_naturefredningsforening = factor(Data$Profil_naturefredningsforening,
                                             labels = c("Ikke involveret_n",
                                                        "Støtter projektet_n"))
Data$Profil_klimaborgerting = factor(Data$Profil_klimaborgerting,
                                     labels = c("Ikke involveret_k",
                                                "Støtter projektet_k"))
Data$Profil_medbestemmelse = factor(Data$Profil_medbestemmelse,
                                    labels = c("Ændringsforslag er ikke muligt",
                                               "Ændringsforslag er muligt"))
Data$Profil_lokalsamfundspulje = factor(Data$Profil_lokalsamfundspulje,
                                        levels = c("Ingen penge til lokalsamfundet",
                                                   "300.000 kr. årligt til det nære lokalområde"),
                                        labels = c("Ingen penge til lokalsamfundet",
                                                   "300.000 kr. årligt til lokalsamfundet"))
Data$Profil_investering = factor(Data$Profil_investering,
                                 labels = c("Ikke muligt at investere",
                                            "Lokale borgere kan investere"))

#Herefter konstrueres aldersvariablen rigtigt, så den går fra årstal til alder.
#Samtidig fjernes respondenter, som er under 18 år eller over 90, som der er 25 af.
Data$alder = 2023 - Data$alder
Data = Data %>%
  filter(alder > 17 & alder < 91)
table(Data$alder)

#Nu tjekkes alle variablene efter.
#Først baggrundvariable for respondenter.
table(Data$køn)
table(Data$alder)
table(Data$region)
table(Data$bystørrelse)
table(Data$uddannelsesniveau)
table(Data$hensyn_vs_hastighed)
table(Data$screener_1_CO2_gevinst)
table(Data$screener_2_areal)

#Data for hypotetiske VE-projekter.
table(Data$Profil_afstand)
table(Data$Profil_type)
table(Data$Profil_naturefredningsforening)
table(Data$Profil_klimaborgerting)
table(Data$Profil_medbestemmelse)
table(Data$Profil_lokalsamfundspulje)
table(Data$Profil_investering)

#Class tjekkes for variable.
#Baggrundvariable for respondenter.
class(Data$køn) #character
class(Data$alder) #numeric
class(Data$region) #character
class(Data$bystørrelse) #character
class(Data$uddannelsesniveau) #character
class(Data$hensyn_vs_hastighed) #character
class(Data$screener_1_CO2_gevinst) #character
class(Data$screener_2_areal) #character

#Data for hypotetiske VE-projekter.
class(Data$Profil_afstand) #factor
class(Data$Profil_type) #factor
class(Data$Profil_naturefredningsforening) #factor
class(Data$Profil_klimaborgerting) #factor
class(Data$Profil_medbestemmelse) #factor
class(Data$Profil_lokalsamfundspulje) #factor
class(Data$Profil_investering) #factor

#Nu omkodes kategoriske variable til at være factor frem for character.
Data$køn = factor(Data$køn, levels = c("Mand",
                                       "Kvinde"))
Data$region = factor(Data$region, levels = c("Nordjylland",
                                             "Midtjylland",
                                             "Syddanmark",
                                             "Sjælland",
                                             "Hovedstaden"))
Data$bystørrelse = factor(Data$bystørrelse, levels = c("Under 200 indbyggere",
                                                       "200-999 indbyggere",
                                                       "1.000-2.999 indbyggere",
                                                       "3.000-9.999 indbyggere",
                                                       "10.000-19.999 indbyggere",
                                                       "20.000-39.999 indbyggere",
                                                       "Over 40.000 indbyggere",
                                                       "Aarhus, Aalborg eller Odense",
                                                       "København/Storkøbenhavn"))
Data$uddannelsesniveau = factor(Data$uddannelsesniveau, levels = c("Grundskole",
                                                                   "Studentereksamen",
                                                                   "Erhvervsuddannelse",
                                                                   "Kort videregående",
                                                                   "Mellemlang videregående",
                                                                   "Bacheloruddannelse",
                                                                   "Lang videregående",
                                                                   "Andet"))
Data$hensyn_vs_hastighed = factor(Data$hensyn_vs_hastighed, levels =
                                  c("Hensyn til lokalsamfundet er helt klart vigtigst",
                                    "Hensyn til lokalsamfundet er lidt mere vigtigt",
                                    "Hastighed for opsætningen af sol og vind er lidt mere vigtig",
                                    "Hastighed for opsætningen af sol og vind er helt klart vigtigst"))
Data$screener_1_CO2_gevinst = factor(Data$screener_1_CO2_gevinst, levels =
                                     c("Solceller havde den største CO2-gevinst",
                                       "Vindmøller havde den største CO2-gevinst",
                                       "Begge slags projekter havde den samme CO2-gevinst",
                                       "Ved ikke"))
Data$screener_2_areal = factor(Data$screener_2_areal, levels =
                               c("Solceller fyldte det største areal",
                                 "Vindmøller fyldte det største areal",
                                 "Begge slags projekter fyldte det samme areal",
                                 "Ved ikke"))

#Herefter fjernes respondenter, som kommer fra Region Hovedstaden eller en by,
#der har over 40.000 indbyggere. 309 respondenter frakodes grundet disse variable.
table(Data$region)
table(Data$bystørrelse)
Data = Data %>%
  filter(region != "Hovedstaden",
         bystørrelse != "Over 40.000 indbyggere",
         bystørrelse != "Aarhus, Aalborg eller Odense",
         bystørrelse != "København/Storkøbenhavn")

#Hensyn vs. hastighed-variablen gøres dikotom.
table(Data$hensyn_vs_hastighed)
Data$new_hensyn_vs_hastighed = as.numeric(Data$hensyn_vs_hastighed)
Data$new_hensyn_vs_hastighed = dplyr::recode(Data$new_hensyn_vs_hastighed, 
                                            `1` = 0L,
                                            `2` = 0L,
                                            `3` = 1L,
                                            `4` = 1L)
Data = Data %>%
  set_value_labels(new_hensyn_vs_hastighed =c("Hensyn er vigtigst" = 0,
                                              "Hastighed er vigtigst" = 1))
Data$new_hensyn_vs_hastighed
table(Data$new_hensyn_vs_hastighed)
#Det fremgår her, at 639 mener, at hastighed er vigtigst, mens 1836 mener, at
#hensynet til lokalområdet er vigtigst ift. opsætningen af VE-projekter.

#Til sidst sammensættes de to screenere til en dummy-variabel alt efter, om der
#er svaret rigtigt eller forkert.
table(Data$screener_1_CO2_gevinst)
table(Data$screener_2_areal)
Data = Data %>%
  mutate(screener_1_CO2_gevinst = as.numeric(screener_1_CO2_gevinst),
         screener_2_areal = as.numeric(screener_2_areal)) %>%
  set_value_labels(screener_1_CO2_gevinst = c("Solceller havde den største CO2-gevinst" = 1,
                                              "Vindmøller havde den største CO2-gevinst" = 2,
                                              "Begge slags projekter havde den samme CO2-gevinst" = 3,
                                              "Ved ikke" = 4),
                   screener_2_areal = c("Solceller fyldte det største areal" = 1,
                                        "Vindmøller fyldte det største areal" = 2,
                                        "Begge slags projekter fyldte det samme areal" = 3,
                                        "Ved ikke" = 4))
Data$passed_screenere = 0
Data$passed_screenere[Data$screener_1_CO2_gevinst == 3 & Data$screener_2_areal == 3] <- 1
Data = Data %>%
  set_value_labels(passed_screenere = c("Mindst ét forkert svar" = 0,
                                        "Begge rigtige svar" = 1))

Data$passed_screenere
table(Data$passed_screenere)
#Det fremgår, at der er 561 respondenter, der har svaret rigtigt på begge screenere,
#mens 1914 respondenter har svaret forkert på mindst én screener.

#På baggrund af ovenstående omkodning har vi nu 34650 effektive observationer
#svarende til 2475 respondenter i vores undersøgelse.

