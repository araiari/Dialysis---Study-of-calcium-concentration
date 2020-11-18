#setwd("C:/Users/990840/Desktop/UNI/applied stat/Progetto/Dialysis Project Applied Statistics")

load("~/aa POLIMI/AA_Applied_Statistics/Dialysis Project/Script/dati_uniti_mezzogiorno.Rdata")


attach(dati)

lm=lm(stacco_tot ~ Na_presc + volumeUF + Eta + attacco_tot + Terapia)
summary(lm)

#R^2=0.6186 and R^2 adj=0.6029, residual std error: 0.4247


x11();par(mfrow=c(2,2));plot(lm)
shapiro.test(residuals(lm))


library(lme4)

lmm=lmer(stacco_tot ~ Na_presc + volumeUF + Eta + attacco_tot +Terapia + (1|Ospedale))
summary(lmm)

# random effects: std dev ospedale=0.3893, std residuals=0.3645 (migliora rispetto a lm sopra)
#frazione di std dev spiegata dai random effects:0.559


x11()
boxplot(stacco_tot ~ Ospedale)

x11()
boxplot(stacco_tot ~ Terapia)

x11()
boxplot(stacco_tot ~ Ospedale*Terapia)


library(ggplot2)
library(sjlabelled)
library(sjmisc)
library(sjPlot)

x11()
plot_model(lmm, type='re')
plot(lmm,type = c("p", "smooth"))
plot_model(lmm,type='est')
x11()
plot_model(lmm,type='diag')

#lugano non ha la terapia shd (vedi boxplot con interazione), se si guardano i boxplot della terapia si vede che 
#shd ha media piu alta del livello di calcio allo stacco. e quindi è ovvio che dal grafico del random effect lugano
#abbia un effetto negativo.

detach(dati)

