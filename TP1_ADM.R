wine=read.csv('wine.csv')
#Mean and standard-deviation of the 29 quantitative variable in wine;
M = unname(colMeans(wine[4:32]))
V = unname(sapply(wine[4:32],sd))
print(V)
print(M)

#Centering and reducing
CR=wine
for (i in 1:29)
{
  CR[,3+i] <- (wine[,3+i]-M[i])/(V[i])
}
#Check if variables ar indeed recuded and centered
Barycentre=(colMeans(CR[4:32]))
print(Barycentre)
Variance=diag(var(CR[4:32]))
print(Variance)

#inertia
Inertie=sum(Variance)
print(Inertie)

"Q2"
chinon=CR[CR$Label == 'Chinon',]
saumur=CR[CR$Label == 'Saumur',]
bourgueuil=CR[CR$Label == 'Bourgueuil',]

#poids
pchi=nrow(chinon)/nrow(CR)
psau=nrow(saumur)/nrow(CR)
pbou=nrow(bourgueuil)/nrow(CR)
#barycentre
mchi= unname(colMeans(chinon[4:32]))
msau= unname(colMeans(saumur[4:32]))
mbou= unname(colMeans(bourgueuil[4:32]))
#carrée des normes euclidiennes des barycentres
nchi=sum(mchi^2)
nsau=sum(msau^2)
nbou=sum(mbou^2)
#Inertie externe
Inex=pchi*nchi+psau*nsau+pbou*nbou
#R2 pour le partionnement en appelation A CORRIGER
R=Inex/Inertie
print(100*R)

#R2 par variable
rchi=(sapply((chinon[4:32]),sd)^2)/sum(Variance)
rbou=(sapply((bourgueuil[4:32]),sd)^2)/sum(Variance)
rsau=(sapply((saumur[4:32]),sd)^2)/sum(Variance)
trichi=rchi[order(unlist(rchi))]
tribou=rbou[order(unlist(rbou))]
trisau=rsau[order(unlist(rsau))]

plot(trichi,xaxt='n')
axis(1,at=1:29,labels=names(rchi))

plot(tribou,xaxt='n')
axis(1,at=1:29,labels=names(rbou))

plot(trisau,xaxt='n')
axis(1,at=1:29,labels=names(rsau))

Rr=1-(sum(rchi))
print(Rr)
rm(red)

#PART2

#Q2
#def (a faire label)
w=1/nrow(wine) * diag(1,nrow(wine))
m=1/ncol(wine[4:32]) * diag(1,ncol(wine[4:32]))
X=as.matrix(CR[4:32])
Y=cbind(ifelse(CR$Label == 'Bourgueuil',1,0),ifelse(CR$Label == 'Chinon',1,0),ifelse(CR$Label == 'Saumur',1,0))
Z=cbind(ifelse(CR$Soil == 'Env1',1,0),ifelse(CR$Soil == 'Env2',1,0),ifelse(CR$Soil == 'Reference',1,0),ifelse(CR$Soil == 'Env4',1,0))

#piYw
Py= Y %*% solve(t(Y) %*% w %*% Y) %*% t(Y) %*% w
print(Py)

#piXjm
j = 1
rm(Px)
Px=list()
for (j in 1:21) {
Px[[j]] = X[j,] %*% solve(t(X[j,]) %*% m %*% X[j,]) %*% t(X[j,]) %*% m
}
#tr
T=sum(diag(Px[[1]]*Py))

Test=Py%*%as.matrix(CR[4])
print(CR[4])
Va = unname(sapply(bourgueuil[4],mean))


#QC
Rma=X %*% m %*% t(X) %*% w
sum(diag(Rma %*% Py))

Pz= Z %*% solve(t(Z) %*% w %*% Z) %*% t(Z) %*% w

sum(diag(Rma %*% Pz))