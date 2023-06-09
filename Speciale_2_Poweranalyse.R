#POWERANALYSE

#F�rst s�ttes working directory.
setwd("C:/Users/niels/Documents/R-filer_Speciale")

#Herefter loades relevante pakker.
library(devtools)
library(cjpowR)

#Herefter foretages poweranalyser p� baggrund af cjpowR fra Schuessler og Freitag (2020).

#Amce er den forventede effektst�rrelse.
#Power er powerniveauer, som typisk er 0.8.
#Levels er det st�rste antal af niveauer ved variablene.
#Alpha er signifikansniveauet.

#Da det ikke har v�ret muligt at finde rapporterede effektst�rrelser fra tidl. studier
#med fokus p� VE-projekter, som kan anvendes til poweranalysen, har vi i stedet
#besluttet at foretage vores poweranalyse p� baggrund af angivne meta-st�rrelser
#fra Schuessler & Freitag (2020) og Stefanelli & Lukac (2020). Disse bruges derfor
#som et forudg�ende sk�n p� det n�dvendige antal effektive observationer.

#Poweranalyse for to niveauer med meta-effektst�rrelser (amce).
cjpowr_amce(amce=0.02, power = 0.80, levels = 2, alpha=0.05)
cjpowr_amce(amce=0.03, power = 0.80, levels = 2, alpha=0.05)
cjpowr_amce(amce=0.05, power = 0.80, levels = 2, alpha=0.05)
cjpowr_amce(amce=0.1, power = 0.80, levels = 2, alpha=0.05)

#Poweranalyse for tre niveauer med meta-effektst�rrelser (amce).
cjpowr_amce(amce=0.02, power = 0.80, levels = 3, alpha=0.05)
cjpowr_amce(amce=0.03, power = 0.80, levels = 3, alpha=0.05)
cjpowr_amce(amce=0.05, power = 0.80, levels = 3, alpha=0.05)
cjpowr_amce(amce=0.1, power = 0.80, levels = 3, alpha=0.05)

#Poweranalyse for fire niveauer med meta-effektst�rrelser (amce).
cjpowr_amce(amce=0.02, power = 0.80, levels = 4, alpha=0.05)
cjpowr_amce(amce=0.03, power = 0.80, levels = 4, alpha=0.05)
cjpowr_amce(amce=0.05, power = 0.80, levels = 4, alpha=0.05)
cjpowr_amce(amce=0.1, power = 0.80, levels = 4, alpha=0.05)

#Efter vores dataindsamling er vores stikpr�ve landet p� N=2475, hvilket giver
#os 34650 effektive observationer. Vi tjekker derfor efter Stefanelli & Lukacs
#(2020) Shiny App for at se, om vi overholder vores powerniveau, hvilket vi g�r
#i alle scenarier: https://mblukac.shinyapps.io/conjoints-power-shiny/

