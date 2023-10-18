library(xtable)
wine=read.csv('~/ADM/ADM-TP1/wine.csv')
xtable(wine[1:4,1:5], type = "latex", file = "wine.tex",digits = 3,
       caption = "Extrait du tableau")

#Mean and standard-deviation of the 29 quantitative variable in wine;
M = unname(colMeans(wine[4:32]))
V = unname(sapply(wine[4:32],sd))*sqrt(20/21) #variance corrigée ?? facteur (21/20) ?
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
Variance=diag(var(CR[4:32])*20/21)
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

#barycentre des classes 
mchi= (colMeans(chinon[4:32]))
msau= (colMeans(saumur[4:32]))
mbou= (colMeans(bourgueuil[4:32]))

#carrée des normes euclidiennes des barycentres
nchi=sum(mchi^2)
nsau=sum(msau^2)
nbou=sum(mbou^2)

#Inertie externe
Inex=pchi*nchi+psau*nsau+pbou*nbou
Inex
#R2 pour le partitionement en appellation
R=Inex/Inertie
print(100*R)

#R2 par variable
rvar=pchi*mchi^2+psau*msau^2+pbou*mbou^2  #via normes euclidiennes
print(rvar)
trirvar=rvar[order(unlist(rvar))]
trirvar
plot(trirvar,xaxt='n')
axis(1,at=1:29,labels=FALSE,srt=90)
text(0.5:28.5,rep(-0.05,2),labels=names(trirvar),srt = 45,xpd=NA,adj=c(1,1))
sum(rvar*1/29)# c'est bon ici !!

#PART2

#Q1
#def (a faire label)
w=1/nrow(wine) * diag(1,nrow(wine))
m=1/ncol(wine[4:32]) * diag(1,ncol(wine[4:32]))
X=as.matrix(CR[4:32])
Y=cbind(ifelse(CR$Label == 'Bourgueuil',1,0),ifelse(CR$Label == 'Chinon',1,0),ifelse(CR$Label == 'Saumur',1,0))
Z=cbind(ifelse(CR$Soil == 'Env1',1,0),ifelse(CR$Soil == 'Env2',1,0),ifelse(CR$Soil == 'Reference',1,0),ifelse(CR$Soil == 'Env4',1,0))

#piYw
Py= Y %*% solve(t(Y) %*% w %*% Y) %*% t(Y) %*% w
print(Py)

#piXjw
rm(j)
Px=list()
for (j in 1:29) {
  Px[[j]] = X[,j] %*% solve(t(X[,j]) %*% w %*% X[,j]) %*% t(X[,j]) %*% w
}
#tr
for (i in 1:29)
{
  T[i]=sum(diag(Px[[i]]%*%Py))
}

print(T)

xtable(as.matrix(t(T[1:4]), type = "latex", file = "wine.tex",digits = 3,
       caption = "Extrait du tableau")

print(unname(rvar))
#C'est bon, il coïncide avec rvar de la première partie !

#QC
Rma=X %*% m %*% t(X) %*% w
sum(diag(Rma %*% Py))

#Q2
Pz= Z %*% solve(t(Z) %*% w %*% Z) %*% t(Z) %*% w
sum(diag(Rma %*% Pz))

