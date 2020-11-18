#importo il dataset CALCEMIA 
#importo il dataset infosedute 
load("C:/Users/fabrizio/Desktop/progetto/dati originali/Ionico_generale.RData")
load("C:/Users/fabrizio/Desktop/progetto/dati originali/InfoSedute_dialisi_AS.Rdata")
load("C:/Users/fabrizio/Desktop/progetto/dati originali/Anagrafica_dialisi_AS.Rdata")
library(lme4)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

unione=merge(CALCEMIA,infosedute_AS,by=c('Codice_paziente','Seduta_n'))
unione=merge(unione,anagrafica_AS,by=c('Codice_paziente'))

names(unione)

# boxplot(unione$stacco_tot~unione$CAL)
# Cal_1=subset(unione$CAL,unione$CAL=='1')
# Cal_0=subset(unione$CAL,unione$CAL=='0')
boxplot(unione$stacco_tot~unione$Ospedale)


Z=na.omit(unione[,c(4,7,13,16,17,18,19,22,25,31,32)] )
CORR=data.frame(cor(Z))
CORR=round(CORR,2)
#CORR
#modello con due effetti random: paziente e ospedale
meps=lmer(stacco_tot ~ Na_presc+ Eta +Totale_volumeUF.corretto+ attacco_tot+Terapia+ (1|Ospedale) + (1|Codice_paziente),data=unione)
summary(meps)



#boxplot(unione$stacco_tot~unione$Codice_paziente)
# Random effects:
#   Groups          Name        Variance Std.Dev.
# Codice_paziente (Intercept) 0.1477   0.3843  
# Ospedale        (Intercept) 0.2054   0.4532  
# Residual                    0.1259   0.3548   
# Number of obs: 745, groups:  Codice_paziente, 129; Ospedale, 4


plot(meps,type = c("p", "smooth"))
plot_model(meps,type='est')
#plot_model(meps,type='eff')
#plot_model(meps,type="slope")
plot_model(meps,type="re") 
plot_model(meps,type='diag')









