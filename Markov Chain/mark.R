## markov chain con basso, medio, alto standard

## basso < 4.65mg/dl; alto > 5.28 mg/dl
low <- 4.65
high <- 5.28

## loading data

library(markovchain)


# medie per paziente dei calci --------------------------------------------
# Prendo tutti i dati di tutte le sedute divise per paziente, 
# ovvero prendo le medie per paziente dei calci

calcio <- DATI122[, c(6,5,4,3,2)]

mat <- as.matrix(calcio) 

for (i in 1:122){
  for (j in 1:5){
    mat.temp <- as.numeric(mat[i,j])
    if (mat.temp <= low)
      mat[i,j]<-1
    if (mat.temp > low & mat.temp < high)
      mat[i,j]<- 2
    if (mat.temp >=high)
      mat[i,j]<- 3
  }
}


m.trans <- cbind(c(0,0,0),c(0,0,0),c(0,0,0))

for (i in 1:122){ # Riempio la matrice di transizione
  for (j in 1:4){
    prima <- as.numeric(mat[i,j])
    dopo <- as.numeric(mat[i,j+1])
    m.trans[prima,dopo] <- m.trans[prima,dopo]+1
  }
}

m.trans

for (i in 1:3){ # normalizzo per righe
  somma <- sum(m.trans[i,])
  m.trans[i,] <- 1/somma*m.trans[i,]
}

m.trans

nome.stati <- c("Basso", "Medio", "Alto")
MC.mean.standard<- new("markovchain", transitionMatrix=m.trans, states=nome.stati)

# Vera parte di markov: RISULTATI
summary(MC.mean.standard)
steadyStates(MC.mean.standard)
MC.mean.standard^4


# tutti i calci -----------------------------------------------------------
# Prendo tutti i calci, senza distiguere da paziente a paziente

calcio <- CALCIO_IONICO[,4:8]

mat <- as.matrix(calcio)


for (i in 1:dim(calcio)[1]){
  for (j in 1:5){
    mat.temp <- mat[i,j]
    if (mat.temp <= low)
      mat[i,j]<-1
    if (mat.temp > low & mat.temp < high)
      mat[i,j]<- 2
    if (mat.temp >=high)
      mat[i,j]<- 3
    rm(mat.temp)
  }
}


m.transizione <- matrix(0,3,3)

for (i in 1:dim(mat)[1]){ # riempio la matrice di transizione
  for (j in 1:(dim(mat)[2]-1)){
    
    prima <- mat[i,j]
    dopo <- mat[i,j+1]
    m.transizione[prima,dopo] <- m.transizione[prima,dopo]+1
    
  }
}

m.transizione

for (i in 1:3){ # normalizziamo per righe
  somma <- sum(m.transizione[i,])
  m.transizione[i,] <- 1/somma*m.transizione[i,]
}

m.transizione 

# creiamo la vera markov: RISULTATI
state_names = (c("Basso","Medio","Alto"))
MC.all.standard<-new("markovchain", transitionMatrix=m.transizione,
         states=(state_names))

summary(MC.all.standard)
steadyStates(MC.all.standard)
MC.all.standard^4
