library(Factoshiny)
library(FactoMineR)
#1)a) Chargement du dataframe
villesbrut = read.table("~/ADM/ADM-TP1/TP3/villes.csv", header = TRUE, row.names=1)

villes_rang = apply(villesbrut, MARGIN = 2 , FUN = rank)

villes_eco = villesbrut[,c('Chomage', "ChomageJeunes", "ChomageLong", "EvoluEmploiCree", "Activite", "EmploiFeminin",
                       "EmploiCommune", "DefaillEntreprise", "SalaireAnnuel", "ImpotRevenu", "ImpotFortune", "Imposables",
                       "MetreCarreAncien", "TaxeHabitation", "FoncierBati", "MetreCubeEau", "EvolDemographique", "Vieillissement",
                       "AttiranceGlobale", "AttiranceActifs", "Proprietaires", "LogtSup4pieces", "LogtInsalubre", "LogtVacant",
                       "LogtConstruction")]

villes_ris = villesbrut[,c('Criminalite', 'EvolutionCrimes', 'SecuriteRoutiere', 'Inondations', 'TerrainsPollues', 'UsinesRisques',
                           'MortaliteInfantile', 'MortaliteCancerPoumon', 'MortaliteAlcool', 'DecesInfarctus', 'TauxSuicide',
                           'MortaliteGlobale', 'TailleClassesPrimaires', 'Retard6eme', 'Retard3eme', 'RetardTerminale')]

villes_nat = villesbrut[ , c('Mer', 'Ski', 'Soleil', 'Pluie', 'Temperature', 'MarcheAPied')]

villes_nat_rang = apply(villes_nat, MARGIN = 2 , FUN = rank)

acp_nat_rang = Factoshiny(villes_nat_rang)
