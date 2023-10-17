library(xtable)
wine=read.csv('~/ADM/ADM-TP1/wine.csv')
xtable(wine[1:4,1:5], type = "latex", file = "wine.tex",digits = 3,
       caption = "Extrait du tableau")


#Moyenne M et deviation-standard V des 29 variables quantitatives de Wine
M = unname(colMeans(wine[4:32]))
V = unname(sapply(wine[4:32],sd))*sqrt(20/21) #correction du facteur sqrt(1/(n-1)) utilisé par l'estimateur sd()

#Tableau CR des 29 variables quantitatives de Wine centrée-réduite 
CR=wine
for (i in 1:29)
{
  CR[,3+i] <- (wine[,3+i]-M[i])/(V[i])
}

#Confirmation que CR soit centré-réduit
Barycentre=colMeans(CR[4:32])
Variance=diag(var(CR[4:32])*20/21) #correction du facteur 1/(n-1) utilisé de l'estimateur var()
print(Variance)
print(Barycentre)

#Inertie
Inertie=sum(Variance)
print(Inertie)

#Partition de CR en 3 tableaux d'appellation
chinon=CR[CR$Label == 'Chinon',]
saumur=CR[CR$Label == 'Saumur',]
bourgueuil=CR[CR$Label == 'Bourgueuil',]

#poids de chaques classes d'apellation
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

#Inertie inter-appelation
Inex=pchi*nchi+psau*nsau+pbou*nbou

#R2 de le partitionement en appellation
R=Inex/Inertie
print(100*R)

#Tableau rvar du R2 de chaques variables sensorielles
rvar=pchi*mchi^2+psau*msau^2+pbou*mbou^2

#Graphique des variables en fonction de leur R2, classées par ordre croissant
trirvar=rvar[order(unlist(rvar))]
par(mar=c(10,5,2,5))
plot(trirvar,xaxt='n',xlab='',ylab ='R²')
axis(1,at=1:29,labels=FALSE,srt=90)
text(0.5:28.5,rep(-0.05,2),labels=names(trirvar),adj = c(0.6,3.2),srt = 90,xpd=NA,cex=0.75)

#Moyenne arithmétiques des R2
moy_ravr=sum(rvar*1/29)

#Creation des matrices W,M,X,Y,Z
w=1/nrow(wine) * diag(1,nrow(wine))
m=1/ncol(wine[4:32]) * diag(1,ncol(wine[4:32]))
X=as.matrix(CR[4:32])
Y=cbind(ifelse(CR$Label == 'Bourgueuil',1,0),ifelse(CR$Label == 'Chinon',1,0),ifelse(CR$Label == 'Saumur',1,0))
Z=cbind(ifelse(CR$Soil == 'Env1',1,0),ifelse(CR$Soil == 'Env2',1,0),ifelse(CR$Soil == 'Reference',1,0),ifelse(CR$Soil == 'Env4',1,0))

#Matrice Py du projecteur de Y
Py= Y %*% solve(t(Y) %*% w %*% Y) %*% t(Y) %*% w
print(Py)

#Liste Px des projecteurs Px[j] de chaques Xj
Px=list()
for (j in 1:29) {
  Px[[j]] = X[,j] %*% solve(t(X[,j]) %*% w %*% X[,j]) %*% t(X[,j]) %*% w
}
#Tableau Ty des Tr(Px[j] * Py)
Ty=c()
for (i in 1:29)
{
  Ty[i]=sum(diag(Px[[i]]%*%Py))
}
Ty=as.matrix(t(Ty))
colnames(Ty) <- colnames(CR[4:32])

#Comparaison avec rvar
print((unname(Ty)))
print(unname(rvar))

#Calcule de Tr(XMX'W*Py)
Rma=X %*% m %*% t(X) %*% w
Rmat=sum(diag(Rma %*% Py))

#Creation de la matrice Pz du projecteur de Z
Pz= Z %*% solve(t(Z) %*% w %*% Z) %*% t(Z) %*% w

#Calcule de Tr(XMX'W*Pz)
Rs=sum(diag(Rma %*% Pz))

#Tableau Tz des Tr(Px[j] * Pz)
Tz=c()
for (i in 1:29)
{
  Tz[i]=sum(diag(Px[[i]]%*%Pz))
}