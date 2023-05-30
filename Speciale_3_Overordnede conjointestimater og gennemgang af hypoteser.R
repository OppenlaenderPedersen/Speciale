#OVERORDNEDE CONJOINTESTIMATER OG GENNEMGANG AF HYPOTESER

#Først sættes working directory.
setwd("C:/Users/niels/Documents/R-filer_Speciale")

#Herefter loades relevante pakker.
library(tidyverse)
library(haven)
library(stargazer)
library(sandwich)
library(lmtest)
library(labelled)
library(ggplot2)
library(ggpubr)
library(broom)
library(psych)
library(forcats)
library(expss)
library(equatiomatic)
library(cregg)
library(stringr)
library(cjoint)
library(emmeans)
library(bookdown)
library(viridis)
library(estimatr)
library(grid)
library(gridExtra)
library(knitr)
library(extrafont)

#Data fra conjoint-profilerne med VE-projekter tjekkes igennem
table(Data$Profil_afstand)
table(Data$Profil_type)
table(Data$Profil_naturefredningsforening)
table(Data$Profil_klimaborgerting)
table(Data$Profil_medbestemmelse)
table(Data$Profil_lokalsamfundspulje)
table(Data$Profil_investering)

#Conjointanalyse: effekten af attributter for valg af kandidater.

#Først sættes labels på de enkelte projektkarakteristika i undersøgelsen.
attr(Data$Profil_afstand, "label") = "Afstand til husstand"
attr(Data$Profil_type, "label") = "Type af VE-projekt"
attr(Data$Profil_naturefredningsforening, "label") = "Naturfredningsforening"
attr(Data$Profil_klimaborgerting, "label") = "Klimaborgerting"
attr(Data$Profil_medbestemmelse, "label") = "Medbestemmelse"
attr(Data$Profil_lokalsamfundspulje, "label") = "Lokalsamfundspulje"
attr(Data$Profil_investering, "label") = "Mulighed for at investere"

#Nu dannes en marginal means model for den samlede conjointanalyse.
conjoint_mm = cj(data = Data,
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

#Visualisering af de samlede conjoint estimater.
Plot_1 = conjoint_mm %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.3) +
  geom_point(size = 1.5, alpha = 1, fill = "black", color = "black") +
  xlab("Støtte til VE-projekter på tværs af karakteristika blandt respondenterne (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y", space = "free_y") +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 12)) +
  theme(axis.text.x = element_text(color = "black", size = 11, face = "plain")) +
  theme(axis.text.y = element_text(color = "black", size = 10, face = "plain"))
Plot_1

#Gemmer plot.
ggsave(filename = "Plot_1_Speciale_Conjointplot_mm_samlet.png",
       plot = Plot_1, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 10, 
       height = 12,
       dpi = 500)

#Tabel med estimater.
tabel_1 = conjoint_mm %>% 
  select(level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_1, digits = 2), file = "Tabel_1_Conjointplot_mm_samlet.txt",
            sep = ";", quote = FALSE, row.names = F)

#Nu opstilles en model for den forudsagte sandsynlighed for valget af hvert VE-projekt
#ved 1., 25., 50., 75. og 99. percentil i data.
Model_1 = lm_robust(formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                 Profil_type +
                                                 Profil_naturefredningsforening +
                                                 Profil_klimaborgerting +
                                                 Profil_medbestemmelse +
                                                 Profil_lokalsamfundspulje +
                                                 Profil_investering,
                    data = Data,
                    clusters = RESPONDENT_ID)

###Opstilling af den forudsagte sandsynlighed for valget af hvert VE-projekt.
predicted = bind_cols(Data, as.data.frame(predict(object = Model_1,
                                                  newdata = Data,
                                                  se.fit = TRUE,
                                                  interval = "confidence")))

###Definition af percentilerne, hvor det specificeres, hvilke der skal medtages i modellen.
percentiles = quantile(predicted$fit.fit, c(0.01, 0.25, 0.50, 0.75, 0.99))

###Beregning af afstand til percentilerne.
predicted = predicted %>%
  mutate(dist1 = abs(percentiles[1] - fit.fit),
         dist25 = abs(percentiles[2] - fit.fit),
         dist50 = abs(percentiles[3] - fit.fit),
         dist75 = abs(percentiles[4] - fit.fit),
         dist99 = abs(percentiles[5] - fit.fit))

###Nu vælges de profiler for VE-projekter, der ligger tættest på de enkelte percentiler.
profiles = predicted %>%
  filter(dist1==min(dist1) |
         dist25==min(dist25) |
         dist50==min(dist50) |
         dist75==min(dist75) |
         dist99==min(dist99)) %>%
  select(Profil_afstand,
         Profil_type,
         Profil_naturefredningsforening,
         Profil_klimaborgerting,
         Profil_medbestemmelse,
         Profil_lokalsamfundspulje,
         Profil_investering,
         fit.fit, fit.lwr, fit.upr, se.fit) %>%
  distinct() %>%
  arrange(fit.fit) %>%
  mutate(percentiles = c(1, 25, 50, 75, 99),
         feature = "Profiltyper for VE-projekter")

###Justering af konfidensintervaller således, at de ikke går under nul.
profiles = profiles %>%
  mutate(fit.lwr = ifelse(fit.lwr<0, 0.00, fit.lwr))

###Tilføjelse af labels og tekster.
profiles = profiles %>%
  mutate(label = paste(paste(Profil_afstand),
                       paste(Profil_type),
                       paste(Profil_naturefredningsforening),
                       paste(Profil_klimaborgerting),
                       paste(Profil_medbestemmelse),
                       paste(Profil_lokalsamfundspulje),
                       paste(Profil_investering),
                       sep = "\n"))

text1 = textGrob(label = profiles$label[1],
                 gp = gpar(fontsize = 7))
text25 = textGrob(label = profiles$label[2],
                  gp = gpar(fontsize = 7))
text50 = textGrob(label = profiles$label[3],
                  gp = gpar(fontsize = 7))
text75 = textGrob(label = profiles$label[4],
                  gp = gpar(fontsize = 7))
text99 = textGrob(label = profiles$label[5],
                  gp = gpar(fontsize = 7))

#Visualisering af plottet med percentilerne for profilerne.
Plot_2 = profiles %>%
  ggplot(., aes(x = percentiles,
                y = fit.fit)) +
  geom_point(fill = "black",
             size = 2) +
  geom_errorbar(aes(ymin = fit.lwr,
                    ymax = fit.upr),
                width = 2,
                color = "black") +
  scale_x_continuous("Percentiler",
                     breaks = c(1, 25, 50, 75, 99),
                     expand = c(0.05, 0.2)) +
  scale_y_continuous("Tilslutning til VE-projekter med bestemte karakteristika",
                     breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) +
  facet_grid(feature ~ .,
             scales = "free_y",
             space = "free_y") +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 12.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0),
                                    size = 11)) +
  theme(text = element_text(color = "black", size = 11, face = "plain")) +
  annotation_custom(text1, 
                    xmin = 1,
                    xmax = 1,
                    ymin = -0.22,
                    ymax = -0.22) + 
  annotation_custom(text25, 
                    xmin = 25,
                    xmax = 25,
                    ymin = -0.22,
                    ymax = -0.22) + 
  annotation_custom(text50,
                    xmin = 50, 
                    xmax = 50,
                    ymin = -0.22,
                    ymax = -0.22) + 
  annotation_custom(text75,
                    xmin = 75,
                    xmax = 75,
                    ymin = -0.22,
                    ymax = -0.22) + 
  annotation_custom(text99,
                    xmin = 99,
                    xmax = 99,
                    ymin = -0.22,
                    ymax = -0.22) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1, 2, 7.2, 1), "lines"),
        panel.grid.minor.y = element_blank())
Plot_2

#Gemmer plot.
ggsave(filename = "Plot_2_Speciale_Percentilplot.png",
       plot = Plot_2,
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 10,
       height = 6.5,
       dpi = 500)

#Tabel med output (finder fit.fit her).
tabel_2 = profiles %>% 
  select(percentiles,
         fit.fit)

write.table(format(tabel_2, digits = 2), file = "Tabel_2_Percentilplot.txt",
            sep = ";", quote = FALSE, row.names = F)

##########Hypotese 1##########

#Vores første hypotese lyder, at 'en kortere afstand mellem borgerens husstand
#og det foreslåede VE-projekt har en negativ indvirkning på tilslutningen til
#projektet'.

#Der laves i den forbindelse en lille marginal means model til at vise dette.
Model_2_H1_H4 = cj(data = Data,
                formula = CHOICE_INDICATOR ~ Profil_afstand +
                                             Profil_type +
                                             Profil_naturefredningsforening +
                                             Profil_klimaborgerting +
                                             Profil_medbestemmelse +
                                             Profil_lokalsamfundspulje +
                                             Profil_investering,
                id = ~ RESPONDENT_ID, #klyngerobuste standardfejl
                estimate = "mm",
                h0 = 0.5)

#Nu dannes plottet.
Plot_H1 = Model_2_H1_H4 %>%
  filter(feature=="Afstand til husstand") %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.3) +
  geom_point(size = 1.5, alpha = 1, fill = "black", color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af afstand til husstand (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"))
Plot_H1

#Gemmer plot.
ggsave(filename = "Plot_3_Speciale_Conjointplot_mm_afstand.png",
       plot = Plot_H1, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 3.5,
       dpi = 500)

##########Hypotese 2##########

#Vores anden hypotese lyder, at 'det har en positiv effekt på tilslutningen til
#et VE-projekt, når der udvises naturhensyn, så den lokale natur ikke lider skade'.

#Vi kan til denne hypotese genbruge den første model og blot filtrere påny på variablen.
#Nu dannes plottet.
Plot_H2 = Model_2_H1_H4 %>%
  filter(feature=="Naturfredningsforening") %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.3) +
  geom_point(size = 1.5, alpha = 1, fill = "black", color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af naturfredningsforening (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"))
Plot_H2

#Gemmer plot.
ggsave(filename = "Plot_4_Speciale_Conjointplot_mm_naturfredningsforening.png",
       plot = Plot_H2, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 3,
       dpi = 500)

#Selvom vi sådan set godt kan konkludere på hypotesen nu, så laver nogle ekstra
#grafer til at supplere vores resultater, hvor afstandsvariablen opdeles alt efter,
#værdien på naturfredningsforeningsvariablen.
Model_natur_mm = cj(data = Data,
                    formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                 Profil_type +
                                                 Profil_klimaborgerting +
                                                 Profil_medbestemmelse +
                                                 Profil_lokalsamfundspulje +
                                                 Profil_investering,
                    by = ~ Profil_naturefredningsforening,
                    id = ~ RESPONDENT_ID, #klyngerobuste standardfejl
                    estimate = "mm",
                    h0 = 0.5)

#Visualisering af plottet.
Plot_H2_Ekstra_1 = Model_natur_mm %>%
  filter(feature=="Afstand til husstand") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_naturefredningsforening)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af afstand til husstand og opdelt\nefter støtte fra naturfredningsforening (marginal means)") +
  ylab("") +
  scale_x_continuous(breaks = c(0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65)) +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Ikke involveret_n",
                                  "Støtter projektet_n"))
Plot_H2_Ekstra_1

#Gemmer plot.
ggsave(filename = "Plot_5_Speciale_Conjointplot_mm_naturfredningsforening_ekstra_1.png",
       plot = Plot_H2_Ekstra_1, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 5,
       dpi = 500)

#Tabel med estimater.
tabel_3 = Model_natur_mm %>%
  filter(feature=="Afstand til husstand") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_3, digits = 2), file = "Tabel_3_Conjointplot_mm_naturfredningsforening_ekstra_1.txt",
            sep = ";", quote = FALSE, row.names = F)

#Der laves nu afslutningsvis til hypotese 2 en supplerende graf, der undersøger
#naturfredningsforeningsvariablen afhængig af værdien for afstand på baggrund af
#en AMCE-model, hvorved vi kan afgøre, om der er tale om nogle former for interaktion.
AMCE_model_natur = cj(data = Data,
                      formula = CHOICE_INDICATOR ~ Profil_naturefredningsforening,
                      by = ~ Profil_afstand,
                      id = ~ RESPONDENT_ID)

#Visualisering af resultater.
Plot_H2_Ekstra_2 = AMCE_model_natur %>%
  filter(feature=="Naturfredningsforening") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_afstand)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af støtte fra naturfredningsforening\nog opdelt efter afstand til husstand (AMCE)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("1 km",
                                  "3 km",
                                  "5 km"))
Plot_H2_Ekstra_2

#Gemmer plot.
ggsave(filename = "Plot_6_Speciale_Conjointplot_amce_naturfredningsforening_ekstra_2.png",
       plot = Plot_H2_Ekstra_2, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 4,
       dpi = 500)

#Tabel med estimater.
tabel_4 = AMCE_model_natur %>%
  filter(feature=="Naturfredningsforening") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_4, digits = 2), file = "Tabel_4_Conjointplot_amce_naturfredningsforening_ekstra_2.txt",
            sep = ";", quote = FALSE, row.names = F)

##########Hypotese 3##########

#Vores tredje hypotese lyder, at 'det har en positiv effekt på tilslutningen til
#et VE-projekt, når de lokale borgere inddrages i planlægningsprocessen'.

#Vi kan også genbruge den tidligere model til denne hypotese og blot filtrere
#påny på variablene for klimaborgerting og medbestemmelse.

#Nu dannes plottene med klimaborgerting først.
Plot_H3_1 = Model_2_H1_H4 %>%
  filter(feature=="Klimaborgerting") %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.3) +
  geom_point(size = 1.5, alpha = 1, fill = "black", color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af klimaborgerting (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"))
Plot_H3_1

#Gemmer plot.
ggsave(filename = "Plot_7_Speciale_Conjointplot_mm_klimaborgerting.png",
       plot = Plot_H3_1, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 3,
       dpi = 500)

#Nu dannes plottet for medbestemmelse.
Plot_H3_2 = Model_2_H1_H4 %>%
  filter(feature=="Medbestemmelse") %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.3) +
  geom_point(size = 1.5, alpha = 1, fill = "black", color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af medbestemmelse (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"))
Plot_H3_2

#Gemmer plot.
ggsave(filename = "Plot_8_Speciale_Conjointplot_mm_medbestemmelse.png",
       plot = Plot_H3_2, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 3,
       dpi = 500)

#Selvom vi sådan set godt kan konkludere på hypotesen nu, så laver vi igen nogle
#ekstra grafer til at supplere vores resultater, hvor afstandsvariablen opdeles
#alt efter værdien på variablene for borgerinddragelse.

#Først kigges på klimaborgerting.
Model_klimaborger_mm = cj(data = Data,
                       formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                    Profil_type +
                                                    Profil_naturefredningsforening +
                                                    Profil_medbestemmelse +
                                                    Profil_lokalsamfundspulje +
                                                    Profil_investering,
                       by = ~ Profil_klimaborgerting,
                       id = ~ RESPONDENT_ID, #klyngerobuste standardfejl
                       estimate = "mm",
                       h0 = 0.5)

#Visualisering af plottet.
Plot_H3_Ekstra_1 = Model_klimaborger_mm %>%
  filter(feature=="Afstand til husstand") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_klimaborgerting)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af afstand til husstand og opdelt\nefter støtte fra klimaborgerting (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Ikke involveret_k",
                                  "Støtter projektet_k"))
Plot_H3_Ekstra_1

#Tabel med estimater.
tabel_5 = Model_klimaborger_mm %>%
  filter(feature=="Afstand til husstand") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_5, digits = 2), file = "Tabel_5_Conjointplot_mm_klimaborgerting_ekstra_1.txt",
            sep = ";", quote = FALSE, row.names = F)

#Så kigges på medbestemmelse i planlægningen.
Model_medbestemmelse_mm = cj(data = Data,
                          formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                       Profil_type +
                                                       Profil_naturefredningsforening +
                                                       Profil_klimaborgerting +
                                                       Profil_lokalsamfundspulje +
                                                       Profil_investering,
                          by = ~ Profil_medbestemmelse,
                          id = ~ RESPONDENT_ID, #klyngerobuste standardfejl
                          estimate = "mm",
                          h0 = 0.5)

#Visualisering af plottet.
Plot_H3_Ekstra_2 = Model_medbestemmelse_mm %>%
  filter(feature=="Afstand til husstand") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_medbestemmelse)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af afstand til husstand og opdelt\nefter medbestemmelse i planlægningen (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Ændringsforslag er ikke muligt",
                                  "Ændringsforslag er muligt"))
Plot_H3_Ekstra_2

#Tabel med estimater.
tabel_6 = Model_medbestemmelse_mm %>%
  filter(feature=="Afstand til husstand") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_6, digits = 2), file = "Tabel_6_Conjointplot_mm_medbestemmelse_ekstra_1.txt",
            sep = ";", quote = FALSE, row.names = F)

#Samling af de to plots til visualisering med marginal means.
Plot_H3_Ekstra_3 = grid.arrange(Plot_H3_Ekstra_1, Plot_H3_Ekstra_2, nrow=2)

#Gemmer plot.
ggsave(filename = "Plot_9_Speciale_Conjointplot_mm_borgerinddragelse_ekstra_1.png",
       plot = Plot_H3_Ekstra_3, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 7,
       dpi = 500)

#Der laves nu afslutningsvis til hypotese 3 igen en supplerende graf, der undersøger
#borgerinddragelsesvariablene afhængig af værdierne for afstand og på baggrund af
#en AMCE-model, hvorved vi kan afgøre, om der er tale om lineære effekter eller en
#form for interaktionseffekt.

#Først kigges på klimaborgerting.
AMCE_model_klimaborger = cj(data = Data,
                            formula = CHOICE_INDICATOR ~ Profil_klimaborgerting,
                            by = ~ Profil_afstand,
                            id = ~ RESPONDENT_ID)

#Visualisering af resultater.
Plot_H3_Ekstra_4 = AMCE_model_klimaborger %>%
  filter(feature=="Klimaborgerting") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_afstand)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af støtte fra klimaborgerting\nog opdelt efter afstand til husstand (AMCE)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("1 km",
                                  "3 km",
                                  "5 km"))
Plot_H3_Ekstra_4

#Tabel med estimater.
tabel_7 = AMCE_model_klimaborger %>%
  filter(feature=="Klimaborgerting") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_7, digits = 2), file = "Tabel_7_Conjointplot_amce_klimaborgerting_ekstra_2.txt",
            sep = ";", quote = FALSE, row.names = F)

#Så kigges der på medbestemmelse i planlægningen.
AMCE_model_medbestemmelse = cj(data = Data,
                               formula = CHOICE_INDICATOR ~ Profil_medbestemmelse,
                               by = ~ Profil_afstand,
                               id = ~ RESPONDENT_ID)

#Visualisering af resultater.
Plot_H3_Ekstra_5 = AMCE_model_medbestemmelse %>%
  filter(feature=="Medbestemmelse") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_afstand)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af medbestemmelse i planlægningen\nog opdelt efter afstand til husstand (AMCE)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("1 km",
                                  "3 km",
                                  "5 km"))
Plot_H3_Ekstra_5

#Tabel med estimater.
tabel_8 = AMCE_model_medbestemmelse %>%
  filter(feature=="Medbestemmelse") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_8, digits = 2), file = "Tabel_8_Conjointplot_amce_medbestemmelse_ekstra_2.txt",
            sep = ";", quote = FALSE, row.names = F)

#Samling af de to plots til visualisering med AMCE.
Plot_H3_Ekstra_6 = grid.arrange(Plot_H3_Ekstra_4, Plot_H3_Ekstra_5, nrow=2)

#Gemmer plot.
ggsave(filename = "Plot_10_Speciale_Conjointplot_amce_borgerinddragelse_ekstra_2.png",
       plot = Plot_H3_Ekstra_6, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 8, 
       height = 6,
       dpi = 500)

##########Hypotese 4##########

#Vores fjerde hypotese lyder, at 'det har en positiv effekt på tilslutningen til
#et VE-projekt, når der uddeles en økonomisk kompensation til lokalsamfundet'.

#Vi kan også genbruge den tidligere model til denne hypotese og blot filtrere
#påny på variablene for lokalsamfundspulje og mulighed for at investere.

#Nu dannes plottene med lokalsamfundspulje først.
Plot_H4_1 = Model_2_H1_H4 %>%
  filter(feature=="Lokalsamfundspulje") %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.3) +
  geom_point(size = 1.5, alpha = 1, fill = "black", color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af lokalsamfundspulje (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"))
Plot_H4_1

#Gemmer plot.
ggsave(filename = "Plot_11_Speciale_Conjointplot_mm_lokalsamfundspulje.png",
       plot = Plot_H4_1, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 3,
       dpi = 500)

#Nu dannes plottet for mulighed for at investere.
Plot_H4_2 = Model_2_H1_H4 %>%
  filter(feature=="Mulighed for at investere") %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.3) +
  geom_point(size = 1.5, alpha = 1, fill = "black", color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af mulighed for at investere (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"))
Plot_H4_2

#Gemmer plot.
ggsave(filename = "Plot_12_Speciale_Conjointplot_mm_investering.png",
       plot = Plot_H4_2, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 3,
       dpi = 500)

#Selvom vi sådan set godt kan konkludere på hypotesen nu, så laver vi igen nogle
#ekstra grafer til at supplere vores resultater, hvor afstandsvariablen opdeles
#alt efter værdien på variablene for økonomisk kompensation.

#Først kigges på lokalsamfundspulje.
Model_lokalpulje_mm = cj(data = Data,
                                formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                             Profil_type +
                                                             Profil_naturefredningsforening +
                                                             Profil_klimaborgerting +
                                                             Profil_medbestemmelse +
                                                             Profil_investering,
                                by = ~ Profil_lokalsamfundspulje,
                                id = ~ RESPONDENT_ID, #klyngerobuste standardfejl
                                estimate = "mm",
                                h0 = 0.5)

#Visualisering af plottet.
Plot_H4_Ekstra_1 = Model_lokalpulje_mm %>%
  filter(feature=="Afstand til husstand") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_lokalsamfundspulje)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af afstand til husstand og opdelt\nefter lokalsamfundspulje (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Ingen penge til lokalsamfundet",
                                  "300.000 kr. årligt til lokalsamfundet"))
Plot_H4_Ekstra_1

#Tabel med estimater.
tabel_9 = Model_lokalpulje_mm %>%
  filter(feature=="Afstand til husstand") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_9, digits = 2), file = "Tabel_9_Conjointplot_mm_lokalsamfundspulje_ekstra_1.txt",
            sep = ";", quote = FALSE, row.names = F)

#Så kigges på muligheden for at investere.
Model_investering_mm = cj(data = Data,
                                 formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                              Profil_type +
                                                              Profil_naturefredningsforening +
                                                              Profil_klimaborgerting +
                                                              Profil_medbestemmelse +
                                                              Profil_lokalsamfundspulje,
                                 by = ~ Profil_investering,
                                 id = ~ RESPONDENT_ID, #klyngerobuste standardfejl
                                 estimate = "mm",
                                 h0 = 0.5)

#Visualisering af plottet.
Plot_H4_Ekstra_2 = Model_investering_mm %>%
  filter(feature=="Afstand til husstand") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_investering)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af afstand til husstand og opdelt efter\nmuligheden for at lokale borgere kan investere (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Ikke muligt at investere",
                                  "Lokale borgere kan investere"))
Plot_H4_Ekstra_2

#Tabel med estimater.
tabel_10 = Model_investering_mm %>%
  filter(feature=="Afstand til husstand") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_10, digits = 2), file = "Tabel_10_Conjointplot_mm_investering_ekstra_1.txt",
            sep = ";", quote = FALSE, row.names = F)

#Samling af de to plots til visualisering med marginal means.
Plot_H4_Ekstra_3 = grid.arrange(Plot_H4_Ekstra_1, Plot_H4_Ekstra_2, nrow=2)

#Gemmer plot.
ggsave(filename = "Plot_13_Speciale_Conjointplot_mm_økonomisk_kompensation_ekstra_1.png",
       plot = Plot_H4_Ekstra_3, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 7,
       dpi = 500)

#Der laves nu afslutningsvis til hypotese 4 igen en supplerende graf, der undersøger
#for økonomisk kompensation afhængig af værdierne på afstandsvariablen og på baggrund
#af en AMCE-model, hvorved vi kan afgøre, om der er tale om lineære effekter eller
#en form for interaktionseffekt.

#Først kigges på lokalsamfundspulje.
AMCE_model_lokalpulje = cj(data = Data,
                                  formula = CHOICE_INDICATOR ~ Profil_lokalsamfundspulje,
                                  by = ~ Profil_afstand,
                                  id = ~ RESPONDENT_ID)

#Visualisering af resultater.
Plot_H4_Ekstra_4 = AMCE_model_lokalpulje %>%
  filter(feature=="Lokalsamfundspulje") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_afstand)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af lokalsamfundspulje\nog opdelt efter afstand til husstand (AMCE)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("1 km",
                                  "3 km",
                                  "5 km"))
Plot_H4_Ekstra_4

#Tabel med estimater.
tabel_11 = AMCE_model_lokalpulje %>%
  filter(feature=="Lokalsamfundspulje") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_11, digits = 2), file = "Tabel_11_Conjointplot_amce_lokalsamfundspulje_ekstra_2.txt",
            sep = ";", quote = FALSE, row.names = F)


#Så kigges der på muligheden for at lokale borgere kan investere.
AMCE_model_investering = cj(data = Data,
                                   formula = CHOICE_INDICATOR ~ Profil_investering,
                                   by = ~ Profil_afstand,
                                   id = ~ RESPONDENT_ID)

#Visualisering af resultater.
Plot_H4_Ekstra_5 = AMCE_model_investering %>%
  filter(feature=="Mulighed for at investere") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_afstand)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af muligheden for at lokale borgere\nkan investere og opdelt efter afstand til husstand (AMCE)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("1 km",
                                  "3 km",
                                  "5 km"))
Plot_H4_Ekstra_5

#Tabel med estimater.
tabel_12 = AMCE_model_investering %>%
  filter(feature=="Mulighed for at investere") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_12, digits = 2), file = "Tabel_12_Conjointplot_amce_investering_ekstra_2.txt",
            sep = ";", quote = FALSE, row.names = F)

#Samling af de to plots til visualisering med AMCE.
Plot_H4_Ekstra_6 = grid.arrange(Plot_H4_Ekstra_4, Plot_H4_Ekstra_5, nrow=2)

#Gemmer plot.
ggsave(filename = "Plot_14_Speciale_Conjointplot_amce_økonomisk_kompensation_ekstra_2.png",
       plot = Plot_H4_Ekstra_6, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 8, 
       height = 6,
       dpi = 500)

##########Undersøgelse af VE-projekter##########

#Selvom vi ikke har en teoretisk hypotese knyttet til typen af VE-projekt, så vælger
#vi nu at undersøge dette karakteristika nærmere, fordi det er yderst interessant at
#se, hvordan borgerne reagerer på hhv. solcelle- og vindmølleprojekter på land,
#og om det er muligt at identificere nogle forskelle.

#Vi starter med at gengive den overordnede conjoint-graf fokuseret på typen af VE-projekt.
Plot_VE_type_1 = Model_2_H1_H4 %>%
  filter(feature=="Type af VE-projekt") %>%
  ggplot(., aes(x = estimate, y = reorder(level, desc(level)))) +
  geom_vline(xintercept = 0.5, linetype = "longdash") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.3) +
  geom_point(size = 1.5, alpha = 1, fill = "black", color = "black") +
  xlab("Tilslutning på baggrund af typen af VE-projekt (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 9.5),
        axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"))
Plot_VE_type_1

#Gemmer plot.
ggsave(filename = "Plot_15_Speciale_Conjointplot_mm_VE_type_1.png",
       plot = Plot_VE_type_1, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 3.5,
       dpi = 500)

#Vi vil nu i tråd med tidligere hypoteser undersøge, om der er forskel på tilslutningen
#til hhv. solcelle- og vindmølleprojekter på baggrund af afstand, og om det er muligt
#at spore en interaktionseffekt, eller om der er tale om en lineær sammenhæng.
Model_VE_type_mm = cj(data = Data,
                             formula = CHOICE_INDICATOR ~ Profil_afstand +
                                                          Profil_naturefredningsforening +
                                                          Profil_klimaborgerting +
                                                          Profil_medbestemmelse +
                                                          Profil_lokalsamfundspulje +
                                                          Profil_investering,
                             by = ~ Profil_type,
                             id = ~ RESPONDENT_ID, #klyngerobuste standardfejl
                             estimate = "mm",
                             h0 = 0.5)

#Visualisering af plottet.
Plot_VE_type_2 = Model_VE_type_mm %>%
  filter(feature=="Afstand til husstand") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_type)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0.5,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til VE-projekter på baggrund af afstand til husstand og opdelt\nefter typen af VE-projekt (marginal means)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("Solcellepark",
                                  "Vindmøllepark"))
Plot_VE_type_2

#Gemmer plot.
ggsave(filename = "Plot_16_Speciale_Conjointplot_mm_VE_type_2.png",
       plot = Plot_VE_type_2, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 5,
       dpi = 500)

#Tabel med estimater.
tabel_13 = Model_VE_type_mm %>%
  filter(feature=="Afstand til husstand") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_13, digits = 2), file = "Tabel_13_Conjointplot_mm_VE_type.txt",
            sep = ";", quote = FALSE, row.names = F)

#Der laves nu atter en supplerende graf, der undersøger typen af VE-projekt afhængig
#af afstandsvariablen og på baggrund af en AMCE-model, hvorved vi kan afgøre, om
#der er tale om nogle former for interaktion eller ej.
AMCE_model_VE_type = cj(data = Data,
                               formula = CHOICE_INDICATOR ~ Profil_type,
                               by = ~ Profil_afstand,
                               id = ~ RESPONDENT_ID)

#Visualisering af resultater.
Plot_VE_type_3 = AMCE_model_VE_type %>%
  filter(feature=="Type af VE-projekt") %>%
  ggplot(., aes(x = estimate,
                y = reorder(level, desc(level)),
                shape = Profil_afstand)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 3,
             alpha = 1,
             fill = "black",
             color = "black") +
  geom_errorbarh(aes(xmin = lower,
                     xmax = upper),
                 position = position_dodge(width = 0.5),
                 height = 0.3) +
  geom_vline(xintercept = 0,
             linetype = "longdash",
             color = "black") +
  xlab("Tilslutning til hhv. solcelle- og vindmølleparker på baggrund af\nafstand til husstand (AMCE)") +
  ylab("") +
  facet_grid(feature ~.,
             scales = "free_y",
             space = "free_y") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0),
                                    size = 11.5),
        axis.text.x = element_text(color = "black", size = 10, face = "plain"),
        axis.text.y = element_text(color = "black", size = 10, face = "plain"),
        panel.grid.minor.x = element_blank()) +
  guides(shape = guide_legend(nrow = 1,
                              byrow = TRUE)) +
  scale_shape_discrete(breaks = c("1 km",
                                  "3 km",
                                  "5 km"))
Plot_VE_type_3
#Det kunne godt ligne en interaktionseffekt.

#Gemmer plot.
ggsave(filename = "Plot_17_Speciale_Conjointplot_amce_VE_type_3.png",
       plot = Plot_VE_type_3, 
       path = "/Users/niels/Documents/R-filer_Speciale",
       width = 7, 
       height = 4,
       dpi = 500)

#Tabel med estimater.
tabel_14 = AMCE_model_VE_type %>%
  filter(feature=="Type af VE-projekt") %>%
  select(BY,
         level, 
         estimate, 
         std.error,
         z,
         p,
         lower,
         upper) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

write.table(format(tabel_14, digits = 2), file = "Tabel_14_Conjointplot_amce_VE_type.txt",
            sep = ";", quote = FALSE, row.names = F)


