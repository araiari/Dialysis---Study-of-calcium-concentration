# PCA DELLE VARIABILI CONTINUE NELL'ULITMO DATASET PER IL MODELLO
# SUL CALCIO TOTALE

load("InfoSedute_dialisi_AS.Rdata")
load("dati_modificati.RData")
load("Anagrafica_dialisi_AS.Rdata")

Uc_i=merge(CALCEMIA,infosedute_AS,by=c("Codice_paziente","Seduta_n"))


n=unique(Uc_i$Codice_paziente)     #138 pazienti
n=data.frame(n)

a=Uc_i[,c(1,4:9,13,16:20,22:28)]
a=a[,-c(11,16:20)]

col_na=NULL
for(i in 1:14){
  col_na[i]=sum(is.na(a[i]))
}

names(a)
col_na



namae=names(a)

v=tapply(a[,2],a[,1],mean)
v=cbind(v)
v=cbind(rownames(v),v)
colnames(v)=c("Codice_paziente","attacco_tot")


for( i in 3:14) {
  
  s=tapply(a[,i],a[,1],mean)
  s=cbind(s)
  s=cbind(rownames(s),s)
  colnames(s)=c(namae[1],namae[i])
  v=merge(s,v,by="Codice_paziente")
  
}

temp=na.omit(v)

dati_anag <- anagrafica_AS
names(dati_anag)

# rimuoviamo le colonne cardiopatia perchè troppi NA
dati_anag <- dati_anag[ , -c(8)]
dati_anag <- data.frame(dati_anag)
dati_anag <- na.omit(dati_anag)
# da 141 na passo a 125

#uniamo i due dataset 
dati_uniti <- merge(temp, dati_anag)
p <- dim(dati_uniti)[2]
N <- dim(dati_uniti)[1]


dati_uniti <- data.frame(dati_uniti)

# DA QUA HO IL DATASET dati_uniti -----------------------------------------

# prendo solo le variabili continue
cont=dati_uniti[,-c(1,9:11,16,19:26)]
N <- dim(cont)[1]
sapply(cont, class)
# ci sono alcune 
for(i in 1:13){
  cont[,i]=as.character(cont[,i])
  cont[,i]=as.numeric(cont[,i])
}
sapply(cont, class)


p <- dim(cont)[2]
N <- dim(cont)[1]

cont <- data.frame(cont)
names(cont)
cont <- cont[-c(8,9)]

PCA <- princomp(~ ., data = cont, cor = TRUE)
summary(PCA)

# ANALISI PCA -------------------------------------------------------------

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(PCA$sdev^2, las=2, main='Principal Components', ylab='Variances')
barplot(sapply(cont,sd)^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(PCA$sdev^2)/sum(PCA$sde^2), type='b', axes=F, xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(cont),labels=1:ncol(cont),las=2)

loads <- PCA$loadings
loads

loads[,1:5]

# graphical representation of the loadings of the first six principal components
x11()
par(mar = c(2,4,3,2), mfrow = c(6,1))
for(i in 1:5) barplot(loads[,i], ylim = c(-1, 1))



# Standardizzo le variabili e ripeto l'analisi

dati2 <- scale(cont)
dati2 <- data.frame(dati2)
PCA2 <- princomp(~ ., data = dati2, cor = TRUE)
summary(PCA2)


x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(PCA2$sdev^2, las=2, main='Principal Components', ylab='Variances')
barplot(sapply(dati2,sd)^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(PCA2$sdev^2)/sum(PCA2$sde^2), type='b', axes=F, xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(dati2),labels=1:ncol(dati2),las=2)

loads <- PCA2$loadings
loads

loads[,1:5]

# graphical representation of the loadings of the first six principal components
x11()
par(mar = c(2,4,3,2), mfrow = c(6,1))
for(i in 1:5) barplot(loads[,i], ylim = c(-1, 1))


# ora provo a togliere le variabili che mi sembrano correlate tra loro
names(dati2)
diff_pesi <- dati2[2] - dati2[3]
dati2 <- dati2[ -c(2, 3)]

dati2[10] <- diff_pesi
names(dati2)[10] <- "diff_pesi"

PCA3 <- princomp(~ ., data = dati2, cor = TRUE)
summary(PCA3)

# ANALISI PCA -------------------------------------------------------------

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(PCA3$sdev^2, las=2, main='Principal Components', ylab='Variances')
barplot(sapply(dati2,sd)^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(PCA3$sdev^2)/sum(PCA3$sde^2), type='b', axes=F, xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(dati2),labels=1:ncol(dati2),las=2)

loads <- PCA$loadings
loads

loads[,1:5]

# graphical representation of the loadings of the first six principal components
x11()
par(mar = c(2,4,3,2), mfrow = c(6,1))
for(i in 1:5) barplot(loads[,i], ylim = c(-1, 1))


