# Multivariate Correspondence Analysis for anagrafica_as

# MCA is an extension of the simple correspondence analysis for summarizing and 
# visualizing a data table containing more than two categorical variables. 
# It can also be seen as a generalization of principal component analysis when 
# the variables to be analyzed are categorical instead of quantitative.

# MCA:
#PCA applicata a variabili categoriche
# Per ogni variabile categorica crea una colonna per label
# (Se binaria una colonna per 0 e una colonna per 1)
# Mantenendo una riga per ogni dato, si ottiene matrice X
# Sommando tutti i valori ottiene il "grand total" che diviso la matrice X da 
# nuova matrice Z, ottengo autovalori e scores
# Posso avere analisi per riga e colonna 
# vanno eliminate dall'analisi le variabili con osservazioni sbilanciate 
# per problemi singolarità matrice



# I will procede with a brief analysis on the data, to ensure that each variable
# has enought variability to be taken into account.
# Then, I will perform MCA on the data.frame


# Prepare the data ---------------------------------------------------------------


library(mvtnorm)
library(rgl)
library(MASS)
library(car)

setwd("C:/Users/Utente/Documents/aa POLIMI/AA_Applied_Statistics/Dialysis Project/Script")
load("~/aa POLIMI/AA_Applied_Statistics/Dialysis Project/Anagrafica_dialisi_AS.Rdata")

data_cat <- anagrafica_AS[,c(3,6:14)]


names(data_cat)

data_cat <- data_cat[-which(is.na(data_cat$Betabloccanti)),]

n <- dim(data_cat)[1] #128 pazienti
p <- dim(data_cat)[2] #10 covariate

data_cat<- data.frame(data_cat)

#### Multiple Correspondence Analysis


# Check the frequency for each var ----------------------------------------
# and Remove NA

summary(data_cat)

x11(); par(mfrow=c(2,5))
for (j in 1:p) {
  hist(as.numeric(data_cat[,j]),  main=colnames(data_cat)[j],
       ylab = "Count", col="steelblue") 
  }
#cardiac_amylosis and corticosteroids should be removed since have too few "yes"
data_cat <- data_cat[, -c(5,9)] 

#Caridiopathy must be removed due to too many NAs
data_cat <- data_cat[, -4]

n <- dim(data_cat)[1] #128
p <- dim(data_cat)[2] #7

names(data_cat)

for ( i in 1:dim(data_cat)[2]){
  data_cat[,i] <- as.factor(data_cat[,i])
}

names(data_cat) <- c("Gender", "Therapy", "Diabetes", "Arterial_Hypertension", 
                     "Insulin", "Beta_blockers", "Hospital" )



# MCA on the new data.frame ---------------------------------------------------------------------


library(FactoMineR)
library(factoextra)

mca.data_cat <- MCA(data_cat, graph=F)
# print(mca.data_cat)

#summary of dimensions and variability
mca.data_cat$eig         

#plot to see the percentage of variability due to each dimension
x11(); fviz_eig(mca.data_cat,addlabels = T)

#how much each variable contiubuite to total variability
#mca.data_cat$var$contrib 

#plot the contribution of each variavle for dim1->axes=1, dim2->axes=2
#layout(matrix(c(1,2),1,2,byrow=T))
x11()
fviz_contrib(mca.data_cat, choice = "var", axes =1, top = 15)

x11()
fviz_contrib(mca.data_cat, choice = "var", axes =2 ,top = 15)

x11()
fviz_contrib(mca.data_cat, choice = "var", axes =3 ,top = 15)

x11()
fviz_contrib(mca.data_cat, choice = "var", axes =4 ,top = 15)



#plot the influence of variables on variablity in the plane dim1/dim2
# x11()
# fviz_mca_var(mca.data_cat, choice = "mca.cor",repel = TRUE)

#plot to see how variables (+yes,no) behave wrt each other in the space dim1/dim2
x11()
fviz_mca_var(mca.data_cat, repel=T, col.var='contrib',
                gradient.cols = c("#4585c4", "#edce4f", "#d35454"))

x11() #plot to see how variables and data (patients) are in the plane dim1/dim2
fviz_mca_biplot(mca.data_cat,repel=T,col.var="contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) #(slow)

