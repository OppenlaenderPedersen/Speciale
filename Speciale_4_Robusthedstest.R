#ROBUSTHEDSTEST

#Først sættes working directory.
setwd("C:/Users/niels/Documents/R-filer_Speciale")

#Herefter loades relevante pakker.
library(gridExtra)
library(grid)
library(ggplot2)
library(cregg)
library(xtable)
library(cowplot)

###ROBUSTHEDSTEST###

#I det følgende testes der for fem typer af robusthed:
#1: Randomisering
#2: Balance
#3: Opgavenummer
#4: Profilplacering
#5: Screener

###TEST AF RANDOMISERING

#Ny dataframe til denne del.
Robust_data = Data

#Specificerer variable i dataframe.

#Afstand til VE-projekter.
Profil_afstand = Robust_data %>%
  group_by(Profil_afstand) %>%
  tally() %>%
  mutate(andel = (n / nrow(Robust_data))*100,
         feature = "Afstand til husstand",
         level = as.factor(Profil_afstand))

#Typen af VE-projekt.
Profil_type = Robust_data %>%
  group_by(Profil_type) %>%
  tally() %>%
  mutate(andel = (n / nrow(Robust_data))*100,
         feature = "Type af projekt",
         level = as.factor(Profil_type))

#Anbefaling fra naturfredningsforening.
Profil_naturefredningsforening = Robust_data %>%
  group_by(Profil_naturefredningsforening) %>%
  tally() %>%
  mutate(andel = (n / nrow(Robust_data))*100,
         feature = "Naturfrednings",
         level = as.factor(Profil_naturefredningsforening))

#Anbefaling fra klimaborgerting.
Profil_klimaborgerting = Robust_data %>%
  group_by(Profil_klimaborgerting) %>%
  tally() %>%
  mutate(andel = (n / nrow(Robust_data))*100,
         feature = "Klimaborgerting",
         level = as.factor(Profil_klimaborgerting))

#Medbestemmelse i planlægningen.
Profil_medbestemmelse = Robust_data %>%
  group_by(Profil_medbestemmelse) %>%
  tally() %>%
  mutate(andel = (n / nrow(Robust_data))*100,
         feature = "Medbestemmelse",
         level = as.factor(Profil_medbestemmelse))

#Lokalsamfundspulje.
Profil_lokalsamfundspulje = Robust_data %>%
  group_by(Profil_lokalsamfundspulje) %>%
  tally() %>%
  mutate(andel = (n / nrow(Robust_data))*100,
         feature = "Lokalpulje",
         level = as.factor(Profil_lokalsamfundspulje))

#Mulighed for at investere.
Profil_investering = Robust_data %>%
  group_by(Profil_investering) %>%
  tally() %>%
  mutate(andel = (n / nrow(Robust_data))*100,
         feature = "Investering",
         level = as.factor(Profil_investering))

#Nu kombineres variablene i en række.
rand_data = bind_rows(Profil_afstand,
                      Profil_type,
                      Profil_naturefredningsforening,
                      Profil_klimaborgerting,
                      Profil_medbestemmelse,
                      Profil_lokalsamfundspulje,
                      Profil_investering)

#Fastlæggelse af levels og rækkefølge.
rand_data = rand_data %>%
  mutate(feature = factor(feature,
                          levels = c("Afstand til husstand",
                                     "Type af projekt",
                                     "Naturfrednings",
                                     "Klimaborgerting",
                                     "Medbestemmelse",
                                     "Lokalpulje",
                                     "Investering")),
         level = factor(level,
                        levels = c("1 km",
                                   "3 km",
                                   "5 km",
                                   "Solcellepark",
                                   "Vindmøllepark",
                                   "Ikke involveret_n",
                                   "Støtter projektet_n",
                                   "Ikke involveret_k",
                                   "Støtter projektet_k",
                                   "Ændringsforslag er ikke muligt",
                                   "Ændringsforslag er muligt",
                                   "Ingen penge til lokalsamfundet",
                                   "300.000 kr. årligt til lokalsamfundet",
                                   "Ikke muligt at investere",
                                   "Lokale borgere kan investere")))

#Visualisering af resultater for randomisering.
Plot_robust_1_randomisering = rand_data %>%
  ggplot(., aes(x = andel,
                y = reorder(level, desc(level)),
                label = round(andel,1))) +
  geom_col(fill = "black") +
  theme(axis.text = element_text(size = 12)) +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 100)) +
  geom_text(position = position_dodge(width = .9),
            hjust = -0.2, 
            size = 3) +
  ylab("") +
  xlab("Procent") +
  labs(title = "Fordelinger") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") + 
  theme_bw() +
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", face = "plain"),
        axis.text.y = element_text(color = "black", face = "plain"))
Plot_robust_1_randomisering

#Gemmer plot.
ggsave(filename = "Plot_18_Speciale_Robusthed_Randomisering.png",
       plot = Plot_robust_1_randomisering, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 10, 
       height = 8.5,
       dpi = 500)

###BALANCETEST

#Ny dataframe til denne del.
balance_data = Robust_data

#Omkodning af enkelte labels for bedre visualisering i plots.
attr(balance_data$Profil_type, "label") = "Type af projekt"
attr(balance_data$Profil_naturefredningsforening, "label") = "Naturfrednings"
attr(balance_data$Profil_lokalsamfundspulje, "label") = "Lokalpulje"
attr(balance_data$Profil_investering, "label") = "Investering"

#I det følgende tester vi balance på tre kategorier: (1) Køn, (2) Alder og (3) Videregående uddannelse,
#som vi sammensætter i et plot til sidst, da vi har brugbare data på disse variable.

#TEST FOR KØN
table(balance_data$køn)
balance_data = balance_data %>%
  mutate(køn = ifelse(køn=="Kvinde", 1, 0))

#Marginal means model.
mm_balance_køn = cj(data = balance_data,
                    formula = køn ~ Profil_afstand +
                                    Profil_type +
                                    Profil_naturefredningsforening +
                                    Profil_klimaborgerting +
                                    Profil_medbestemmelse +
                                    Profil_lokalsamfundspulje +
                                    Profil_investering,
                    id = ~ RESPONDENT_ID,
                    estimate = "mm",
                    h0 = mean(balance_data$køn))

#Visualisering af resultater.
Plot_robust_2_balance_1 = mm_balance_køn %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.3,
                 color = "black") +
  geom_vline(xintercept = 0.4842,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means (kvinde)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", face = "plain"),
        axis.text.y = element_text(color = "black", face = "plain"))
Plot_robust_2_balance_1

#TEST FOR ALDER
table(balance_data$alder)

#Marginal means model.
mm_balance_alder = cj(data = balance_data,
                      formula = alder ~ Profil_afstand +
                                        Profil_type +
                                        Profil_naturefredningsforening +
                                        Profil_klimaborgerting +
                                        Profil_medbestemmelse +
                                        Profil_lokalsamfundspulje +
                                        Profil_investering,
                      id = ~ RESPONDENT_ID,
                      estimate = "mm",
                      h0 = mean(balance_data$alder))

#Visualisering af resultater.
Plot_robust_2_balance_2 = mm_balance_alder %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.3,
                 color = "black") +
  geom_vline(xintercept = 51.34,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means (gennemsnitlig alder)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", face = "plain"),
        axis.text.y = element_blank())
Plot_robust_2_balance_2

#TEST FOR VIDEREGÅENDE UDDANNELSE
table(balance_data$uddannelsesniveau)

#Dikotom indikator for videregående uddannelse.
balance_data = balance_data %>%
  mutate(uddannelsesniveau = ifelse(uddannelsesniveau=="Kort videregående" |
                                    uddannelsesniveau=="Mellemlang videregående" |
                                    uddannelsesniveau=="Bacheloruddannelse" |
                                    uddannelsesniveau=="Lang videregående", 1, 0))

#Marginal means model.
mm_balance_uddannelse = cj(data = balance_data,
                           formula = uddannelsesniveau ~ Profil_afstand +
                                                         Profil_type +
                                                         Profil_naturefredningsforening +
                                                         Profil_klimaborgerting +
                                                         Profil_medbestemmelse +
                                                         Profil_lokalsamfundspulje +
                                                         Profil_investering,
                           id = ~ RESPONDENT_ID,
                           estimate = "mm",
                           h0 = mean(balance_data$uddannelsesniveau))

#Visualisering af resultater.
Plot_robust_2_balance_3 = mm_balance_uddannelse %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)))) +
  geom_point(fill = "black") +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 height = 0.3,
                 color = "black") +
  geom_vline(xintercept = 0.55,
             linetype = "longdash",
             color = "black") +
  xlab("Marginal means (videregående uddannelse)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", face = "plain"),
        axis.text.y = element_blank())
Plot_robust_2_balance_3

#Samling af de tre plots til én figur.
Plot_robust_2_balance = plot_grid(Plot_robust_2_balance_1,
                                  Plot_robust_2_balance_2,
                                  Plot_robust_2_balance_3,
                                  nrow=1,
                                  rel_widths = c(4.5/9, 2.5/9, 2.5/9))
Plot_robust_2_balance

#Gemmer plot.
ggsave(filename = "Plot_19_Speciale_Robusthed_Balancetest.png",
       plot = Plot_robust_2_balance, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 10, 
       height = 9,
       dpi = 500)

###TEST FOR FRAVÆR AF OVERFØRSELSEFFEKTER (opgavenummer)

#Først opretter vi en indikator for opgavenummer.
table(Robust_data$CHOICE_SET)
Robust_data = Robust_data %>%
  mutate(conjointnr = case_when(CHOICE_SET==1 ~ "Opgave nr. 1",
                                CHOICE_SET==2 ~ "Opgave nr. 2",
                                CHOICE_SET==3 ~ "Opgave nr. 3",
                                CHOICE_SET==4 ~ "Opgave nr. 4",
                                CHOICE_SET==5 ~ "Opgave nr. 5",
                                CHOICE_SET==6 ~ "Opgave nr. 6",
                                CHOICE_SET==7 ~ "Opgave nr. 7"))

#Nu sættes rækkefølgen for de enkelte niveauer.
Robust_data$conjointnr = factor(Robust_data$conjointnr, levels =c("Opgave nr. 1",
                                                                  "Opgave nr. 2",
                                                                  "Opgave nr. 3",
                                                                  "Opgave nr. 4",
                                                                  "Opgave nr. 5",
                                                                  "Opgave nr. 6",
                                                                  "Opgave nr. 7"))

#Definerer variablen som faktor.
Robust_data$conjointnr = factor(Robust_data$conjointnr)

##ANOVA-test
anova_conjointnr = cj_anova(data = Robust_data,
                            formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                         Profil_type +
                                                         Profil_naturefredningsforening +
                                                         Profil_klimaborgerting +
                                                         Profil_medbestemmelse +
                                                         Profil_lokalsamfundspulje +
                                                         Profil_investering,
                            id = ~ RESPONDENT_ID,
                            by = ~ conjointnr)
anova_conjointnr

#Tabel med estimater.
write.table(format(anova_conjointnr, digits = 4), file = "Tabel_15_Robusthed_anova_opgavenummer.txt",
            sep = ";", quote = FALSE, row.names = F)

#Labels til de enkelte projektkarakteristika.
attr(Robust_data$Profil_afstand, "label") = "Afstand til husstand"
attr(Robust_data$Profil_type, "label") = "Type af VE-projekt"
attr(Robust_data$Profil_naturefredningsforening, "label") = "Naturfredningsforening"
attr(Robust_data$Profil_klimaborgerting, "label") = "Klimaborgerting"
attr(Robust_data$Profil_medbestemmelse, "label") = "Medbestemmelse"
attr(Robust_data$Profil_lokalsamfundspulje, "label") = "Lokalsamfundspulje"
attr(Robust_data$Profil_investering, "label") = "Mulighed for at investere"

#Marginal means model for test af overførselseffekt
mm_conjointnr = cj(data = Robust_data,
                   formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                Profil_type +
                                                Profil_naturefredningsforening +
                                                Profil_klimaborgerting +
                                                Profil_medbestemmelse +
                                                Profil_lokalsamfundspulje +
                                                Profil_investering,
                   id = ~ RESPONDENT_ID,
                   by = ~ conjointnr,
                   estimate = "mm",
                   h0 = 0.5)

#Visualisering af resultater
Plot_robust_3_opgavenummer = mm_conjointnr %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                colour = conjointnr)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Støtte til VE-projekter på tværs af karakteristika blandt respondenterne (marginal means)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(color = "black", face = "plain"),
        axis.text.y = element_text(color = "black", face = "plain")) +
  guides(shape = guide_legend(nrow = 2,
                              byrow = TRUE)) + 
  scale_shape_discrete(breaks = c("Opgave nr. 1",
                                  "Opgave nr. 2",
                                  "Opgave nr. 3",
                                  "Opgave nr. 4",
                                  "Opgave nr. 5",
                                  "Opgave nr. 6",
                                  "Opgave nr. 7"))
Plot_robust_3_opgavenummer

#Gemmer plot.
ggsave(filename = "Plot_20_Speciale_Robusthed_Opgavenummer.png",
       plot = Plot_robust_3_opgavenummer, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 10, 
       height = 12,
       dpi = 500)

###TEST AF PROFILPLACERINGSEFFEKTER (PROFIL TIL VENSTRE OG HØJRE)
Robust_data = Robust_data %>%
  mutate(profilnr = case_when(LABEL==1 ~ "Profil til venstre",
                              LABEL==2 ~ "Profil til højre"))

#Definerer variablen som faktor.
Robust_data$profilnr = factor(Robust_data$profilnr)

##ANOVA-test
anova_profilnr = cj_anova(data = Robust_data,
                          formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                       Profil_type +
                                                       Profil_naturefredningsforening +
                                                       Profil_klimaborgerting +
                                                       Profil_medbestemmelse +
                                                       Profil_lokalsamfundspulje +
                                                       Profil_investering,
                          id = ~ RESPONDENT_ID,
                          by = ~ profilnr)
anova_profilnr

#Tabel med estimater.
write.table(format(anova_profilnr, digits = 4), file = "Tabel_16_Robusthed_anova_profilnummer.txt",
            sep = ";", quote = FALSE, row.names = F)

#Sæt rækkefølge for levels.
Robust_data$profilnr = factor(Robust_data$profilnr,
                              levels = c("Profil til venstre",
                                         "Profil til højre"))

#Marginal means model for test af betydning af profilnumemr.
mm_profilnr = cj(data = Robust_data,
                 formula = CHOICE_INDICATOR ~ Profil_afstand +
                                              Profil_type +
                                              Profil_naturefredningsforening +
                                              Profil_klimaborgerting +
                                              Profil_medbestemmelse +
                                              Profil_lokalsamfundspulje +
                                              Profil_investering,
                 id = ~ RESPONDENT_ID,
                 by = ~ profilnr,
                 estimate = "mm",
                 h0 = 0.5)

#Visualisering af resultater
Plot_robust_4_profilnummer = mm_profilnr %>%
  ggplot(., aes(x = estimate, 
                y = reorder(level, desc(level)),
                color = profilnr)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2) +
  geom_errorbarh(aes(xmin = lower, 
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Støtte til VE-projekter på tværs af karakteristika blandt respondenterne (marginal means)") +
  ylab("") +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(color = "black", face = "plain"),
        axis.text.y = element_text(color = "black", face = "plain")) +
  guides(shape = guide_legend(nrow = 2,
                              byrow = TRUE)) + 
  scale_shape_discrete(breaks = c("Profil til venstre",
                                  "Profil til højre"))
Plot_robust_4_profilnummer

#Gemmer plot.
ggsave(filename = "Plot_21_Speciale_Robusthed_Profilnummer.png",
       plot = Plot_robust_4_profilnummer, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 10, 
       height = 11.5,
       dpi = 500)

###OVERORDNET CONJOINTANALYSE KUN MED BESTÅEDE SCREENER

#Først filtreres data, så kun opmærksomme respondenter beholdes.
table(Robust_data$passed_screenere)
Screener_data = Robust_data %>% 
  filter(passed_screenere==1)

#Nu dannes en marginal means model for den samlede conjointanalyse kun med beståede
#screenere.
screener_mm = cj(data = Screener_data,
                 formula = CHOICE_INDICATOR ~ Profil_afstand +
                   Profil_type +
                   Profil_naturefredningsforening +
                   Profil_klimaborgerting +
                   Profil_medbestemmelse +
                   Profil_lokalsamfundspulje +
                   Profil_investering,
                 id = ~ RESPONDENT_ID,
                 estimate = "mm",
                 h0 = 0.5)

#Visualisering af screener conjoint estimater.
Plot_robust_5_screener = screener_mm %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.3) +
  geom_point(size = 1.5, alpha = 1, fill = "black", color = "black") +
  xlab("Støtte til VE-projekter på tværs af karakteristika blandt respondenterne (marginal means + screener)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y", space = "free_y") +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 12)) +
  theme(axis.text.x = element_text(color = "black", size = 11, face = "plain")) +
  theme(axis.text.y = element_text(color = "black", size = 10, face = "plain"))
Plot_robust_5_screener

#Gemmer plot.
ggsave(filename = "Plot_22_Speciale_Robusthed_Screener.png",
       plot = Plot_robust_5_screener, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 10, 
       height = 12,
       dpi = 500)

#Tabel med estimater.
tabel_17 = screener_mm %>% 
  select(level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_17, digits = 2), file = "Tabel_17_Robusthed_screener.txt",
            sep = ";", quote = FALSE, row.names = F)

