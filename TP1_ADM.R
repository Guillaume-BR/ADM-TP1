library(xtable)
wine=read.csv('~/ADM/ADM-TP1/wine.csv')
xtable(wine[1:4,1:5], type = "latex", file = "wine.tex",digits = 3,
        caption = "Extrait du tableau")

#Mean and standard-deviation of the 29 quantitative variable in wine;
M = unname(colMeans(wine[4:32]))
V = unname(sapply(wine[4:32],sd))*21/20 #variance corrigée ?? facteur (21/20) ?
print(V)
print(M)

#Centering and reducing
CR=wine
for (i in 1:29)
{
  CR[,3+i] <- (wine[,3+i]-M[i])/(V[i])
}
#Check if variables ar indeed recuded and centered
Barycentre=colMeans(CR[4:32])
xtable(t(as.matrix(Barycentre[,1:5])), type = "latex", file = "barycentre.tex",digits = 3,
       caption="extrait du tableau des barycentres")
Variance=t(diag(var(CR[4:32])))
print(Variance)
xtable(t(as.matrix(Variance[,1:5])), type = "latex", file = "barycentre.tex",digits = 3,
       caption="extrait du tableau des variances")

#inertia
Inertie=sum(Variance)
print(Inertie)



"Q2"
chinon=CR[CR$Label == 'Chinon',]
xtable(chinon[1:3,1:5], type = "latex", file = "barycentre.tex",digits = 3,
       caption="extrait du tableau de l'appelation Chinon")

saumur=CR[CR$Label == 'Saumur',]
bourgueuil=CR[CR$Label == 'Bourgueuil',]

#poids
pchi=nrow(chinon)/nrow(CR)
psau=nrow(saumur)/nrow(CR)
pbou=nrow(bourgueuil)/nrow(CR)

#barycentre des classes 
mchi= unname(colMeans(chinon[4:32]))
print(mchi)
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
RVariables=pchi*mchi^2+psau*msau^2+pbou*mbou^2
print(RVariables)

pchi*mchi[1]^2 + pbou*mbou[1]^2 + psau*msau[1]^2
sum(1/29*RVariables)

rchi=(sapply((chinon[4:32]),sd)^2)/sum(Variance) #pas bon ici
rchibis=chinon[4:32]
rbou=(sapply((bourgueuil[4:32]),sd)^2)/sum(Variance)
rsau=(sapply((saumur[4:32]),sd)^2)/sum(Variance)
trichi=rchi[order(unlist(rchi))]
tribou=rbou[order(unlist(rbou))]
trisau=rsau[order(unlist(rsau))]

print(as.matrix(tribou))
print(as.matrix(trichi))
print(as.matrix(trisau))

plot(trichi,xaxt='n')
axis(1,at=1:29,labels=names(rchi))

plot(tribou,xaxt='n')
axis(1,at=1:29,labels=names(rbou))

plot(trisau,xaxt='n')
axis(1,at=1:29,labels=names(rsau))

Rr=1-(sum(rchi))
print(Rr)

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