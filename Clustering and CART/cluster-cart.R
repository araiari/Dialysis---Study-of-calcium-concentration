load("dati_ti_prego_ultimi.R")

library(tree)
library(rgl)
library(KernSmooth)

dati <- dati_ti_prego_ultimi
rm(dati_ti_prego_ultimi)

calcio_ionico <- dati[ , c(1, 6, 5, 4, 3, 2)]


for (i in 2:6) {
  calcio_ionico[ ,i] <- as.numeric(as.character(calcio_ionico[ ,i]))
}

# notiamo che le curve di crescita di calcio ionico nel sangue differiscono sopratttutto
# nel comportamento iniziale, crescendo molto o poco.
# perciò impostiamo una prima clusterizzazione 1d guardando la differenza tra il calcio 
# ionico iniziale e il calcio ionico alla prima misurazione in dialisi

differenze <- calcio_ionico[, 3] - calcio_ionico[ ,2]
x11(); hist(differenze, breaks = 32)

dati_cluster <- cbind(differenze)
cluster1d <- kmeans(dati_cluster, centers = 3)

gruppo_basso <- which(cluster1d$cluster == 1)
gruppo_medio <- which(cluster1d$cluster == 2)
gruppo_alto <- which(cluster1d$cluster == 3)

x11()
par(mfrow = c(1, 3))
plot(1, 1, xlim = c(1, 5), ylim = c(3.3, 6.6), main = 'Group 1')

for(i in gruppo_basso)
{
  ca_temp <- calcio_ionico[i, 2:6]
  lines(smooth.spline(1:5, ca_temp, spar = 0), col = 2)
}

plot(1, 1, xlim = c(1, 5), ylim = c(3.3, 6.6), main = 'Group 2')

for(i in gruppo_medio)
{
  ca_temp <- calcio_ionico[i, 2:6]
  lines(smooth.spline(1:5, ca_temp, spar = 0), col = 3)
}
plot(1, 1, xlim = c(1, 5), ylim = c(3.3, 6.6), main = 'Group 3')

for(i in gruppo_alto)
{
  ca_temp <- calcio_ionico[i, 2:6]
  lines(smooth.spline(1:5, ca_temp, spar = 0), col = 4)
}

# il risultato non è soddisfacente


# noi sappiamo che il valore finale del calcio ionico è dipendente dal valore di
# calcio ionico che è stato messo nel bagno (perchè alla fine si ha equilibrio osmotico)
# ed è quindi ragionevole che dipenda dall'ospedale
# inoltre abbiamo l'evidenza del modello lineare di fabri e gu

x11()
plot(calcio_ionico[ ,6], col = as.numeric(dati$Ospedale), pch = 19, 
            main = 'Final calcium according to hospitals',
            xlab = "", ylab = "calcium concentration")
x11();
boxplot(calcio_ionico[,6] ~ dati$Ospedale, col=2:5,
        main = 'Final calcium according to hospitals',
        xlab = "", ylab = "calcium concentration")


# provo quindi ad aggiungere una terza covariata e fare un plot in 2d con il valore allo stacco
# questo in qualche modo equivale a clusterizzare per gli ospedali


dati_cluster <- cbind(differenze, calcio_ionico[ ,6])

cluster2d <- kmeans(dati_cluster, centers = 3)

x11(); plot(dati_cluster, xlab = 'differenze', ylab = 'stacco', col = cluster2d$cluster , pch = 19, main = 'Cluster')


gruppo_basso <- which(cluster2d$cluster == 1)
gruppo_medio <- which(cluster2d$cluster == 2)
gruppo_alto <- which(cluster2d$cluster == 3)

x11()
par(mfrow = c(1, 3))
plot(1, 1, xlim = c(1, 5), ylim = c(3.3, 6.6), main = 'Group 1')

for(i in gruppo_basso)
{
  ca_temp <- calcio_ionico[i, 2:6]
  lines(smooth.spline(1:5, ca_temp, spar = 0), col = 2)
}
lines(smooth.spline(colMeans(calcio_ionico[gruppo_basso,2:6]), spar = 0), col = 1, lwd = 1.5)
plot(1, 1, xlim = c(1, 5), ylim = c(3.3, 6.6), main = 'Group 2')

for(i in gruppo_medio)
{
  ca_temp <- calcio_ionico[i, 2:6]
  lines(smooth.spline(1:5, ca_temp, spar = 0), col = 3)
}
lines(smooth.spline(colMeans(calcio_ionico[gruppo_medio,2:6]), spar = 0), col = 1, lwd = 1.5)
plot(1, 1, xlim = c(1, 5), ylim = c(3.3, 6.6), main = 'Group 3')

for(i in gruppo_alto)
{
  ca_temp <- calcio_ionico[i, 2:6]
  lines(smooth.spline(1:5, ca_temp, spar = 0), col = 4)
}
lines(smooth.spline(colMeans(calcio_ionico[gruppo_alto,2:6]), spar = 0), col = 1, lwd = 1.5)
# questo cluster sembra molto buono!!
# provo a fare un cart per il dataset
# 
# attach(dati)
# dati_per_cart <- dati[ ,c('Eta', 'attacco_P', 'Flusso_sangue_prescritto' )]
# detach(dati)
# 
# for (i in 1:dim(dati_per_cart)[2]) {
#   dati_per_cart[ ,i] <- as.numeric(as.character(dati_per_cart[ ,i]))
# }
# 
# cart <- tree(factor(cluster2d$cluster) ~ ., data = dati_per_cart)
# summary(cart)
# 
# x11()
# plot(cart, type = 'proportional')
# text(cart, title(main = 'Cart da cluster su differenze e stacco calcio') ,splits = TRUE, label = "yval", all = T , digits = 1,pretty = 0)
# 
# cart.p <- prune.tree(cart, best = 6)
# summary(cart.p)
# 
# x11()
# plot(cart.p, type = 'proportional')
# text(cart.p, title(main = 'Cart da cluster su differenze e stacco calcio') ,splits = TRUE, label = "yval", all = T , digits = 1,pretty = 0)

# betabloccanti diabete insulina

# provo a disegnare le curve in base a questi

attach(dati)
dati_per_cart_cat <- dati[ ,c('Eta', 'attacco_P', 'Flusso_sangue_prescritto', 'Insulina', 'Diabete', 'Betabloccanti')]
detach(dati)

for (i in 1:(dim(dati_per_cart_cat)[2] - 3)) {
  dati_per_cart_cat[ ,i] <- as.numeric(as.character(dati_per_cart_cat[ ,i]))
}

cart_cat <- tree(factor(cluster2d$cluster) ~ ., data = dati_per_cart_cat)
summary(cart_cat)

x11()
plot(cart_cat, type = 'proportional')
text(cart_cat, title(main = 'Cart da cluster su differenze e stacco calcio') ,splits = TRUE, label = "yval", all = T , digits = 1,pretty = 0)

cart_cat.p <- prune.tree(cart_cat, best = 8)
summary(cart_cat.p)

x11()
plot(cart_cat.p, type = 'proportional')
text(cart_cat.p, title(main = 'pruned') ,splits = TRUE, label = "yval", all = T , digits = 1,pretty = 0)

attach(dati_per_cart_cat)
gruppo1 <- intersect(which(Betabloccanti > 0.5), which(Flusso_sangue_prescritto > 316))
A <- intersect(which(Betabloccanti < 0.5), which(Eta > 59.5))
B1 <- intersect(which(Betabloccanti < 0.5), which(Eta < 59.5))
B <- intersect(B1, which(attacco_P < 2.9))
gruppo2 <- union(A, B)
#### ma con betabloccanti e i suoi na come si faaaaaa??

# gruppo3 <- intersect(which(Betabloccanti < 0.5), which(Eta < 59.5))
D <- intersect(which(Betabloccanti > 0.5), which(Flusso_sangue_prescritto < 316))
C1 <- intersect(which(Betabloccanti < 0.5), which(Eta < 59.5))
C <- intersect(C1, which(attacco_P > 2.9))
gruppo3 <- union(C, D)


# ora provo a individuare i missclassificati

x11()
par(mfrow = c(1, 3))
plot(1, 1, xlim = c(1, 5), ylim = c(3.5, 5.6), , main = 'Group 1 from tree')

for(i in gruppo1)
{
  ca_temp <- calcio_ionico[i, 2:6]
  lines(smooth.spline(1:5, ca_temp, spar = 0), col = 2)
}

lines(smooth.spline(colMeans(calcio_ionico[gruppo1,2:6]), spar = 0), col = 1, lwd = 1.5)

plot(1, 1, xlim = c(1, 5), ylim = c(3.5, 5.6), main='Group 2 from tree')
for(i in gruppo3)
{
  ca_temp <- calcio_ionico[i, 2:6]
  lines(smooth.spline(1:5, ca_temp, spar = 0), col = 3)
}
lines(smooth.spline(colMeans(calcio_ionico[gruppo3,2:6]), spar = 0), col = 1, lwd = 1.5)


plot(1, 1, xlim = c(1, 5), ylim = c(3.5, 5.6), main = 'Group 3 from tree')
for(i in gruppo2)
{
  ca_temp <- calcio_ionico[i, 2:6]
  lines(smooth.spline(1:5, ca_temp, spar = 0), col = 4)
}
lines(smooth.spline(colMeans(calcio_ionico[gruppo2,2:6]), spar = 0), col = 1, lwd = 1.5)