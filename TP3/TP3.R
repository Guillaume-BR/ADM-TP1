library(Factoshiny)
library(FactoMineR)
#1)a) Chargement du dataframe
villesbrut = read.table("~/ADM/ADM-TP1/TP3/villes.csv", header = TRUE, row.names=1)
villes = Factoshiny(villesbrut)

villes_rang = apply(villesbrut, MARGIN = 2 , FUN = rank)
acp_rang = Factoshiny(villes_rang)

villes_eco = villesbrut[,c('Chomage', "ChomageJeunes", "ChomageLong", "EvoluEmploiCree", "Activite", "EmploiFeminin",
                       "EmploiCommune", "DefaillEntreprise", "SalaireAnnuel", "ImpotRevenu", "ImpotFortune", "Imposables",
                       "MetreCarreAncien", "TaxeHabitation", "FoncierBati", "MetreCubeEau", "EvolDemographique", "Vieillissement",
                       "AttiranceGlobale", "AttiranceActifs", "Proprietaires", "LogtSup4pieces", "LogtInsalubre", "LogtVacant",
                       "LogtConstruction")]

acp_eco = 