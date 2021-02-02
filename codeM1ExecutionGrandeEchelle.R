# les packages nécessaires
require(cubt) 
require(mclust)
require(caret)
require(pdist)
require("doParallel")
getDoParWorkers() # affiche nombre de coeurs alloué
ncore <- detectCores() # 40 pour la machine distante
registerDoParallel(ncore-3) # alloue le nombre de coeurs souhaité
getDoParWorkers() # Pour vérifer qu'on utilise bien tous les coeurs

# Les paramètres
# Faire varier le nombre d'observation N
#vectObs <<- seq(4000,20000, by=4000)
vectObs <<- c(4000, 40000, 400000)
#sizeOpt_N = c(78, 150, 9)
# minsize en fonction de la taille d'échantillon
# sizeOpt_N <<- c(3, 4, 4) le 1er qui marche à distance
sizeOpt_N <<- c(10, 10, 100) # par minsize

# Nombre de  répétition à faire pour chaque taille d'échantion
nbRep <<- 5

mod <<- 1                    ## le modèle M1
dima <<- 2                   ## la dimension des données
SigMa <<- 0.19               ## la dispersion des données
nbcl <<- 4                   ## le nombre de classe

## le nombre de bloc à effectuer pour chaque taille d'échantillon
#nbech <<- c(10, 50, 80)
nbech_m <<- list(c(10,100), c(10, 100, 1000), c(10, 100, 1000, 10000))
## minesize optimale pour chaque bloc de données ainsi que pour chaque taille d'échantillon
# taille_opt <<- list(c(5, 5, 5, 2), c(35, 19, 7, 4), c(49, 7, 5, 9)) le 1er qui marche à distance
taille_opt <<- list(c(10, 2), c(8, 5, 3), c(10, 8, 5, 3)) # ajout arbitraire des deux derniers vecteurs
MinSplit2 <<-list(c(8,2), c(10, 6, 5), c(10, 8, 6, 5))
## minesize optimale pour la combinaison des centres issues de chaque bloc
#sizeOpt_c <<- c(3, 3, 3) # ajout arbitraire de 3,3
sizeOpt_cm <<- list(c(2,4), c(3, 5, 8), c(3, 5, 8, 10))
MinSplit_cm <<-list(c(2,4), c(4, 6, 10), c(4, 6, 10, 15))
## la profondeur de l'arbre
MinDev = 0.01
profond1 <<- 6
profond2 <<- 6
MinSplit1 <<- 4
MinDist <<- 0.3
## le nombre de feuille souhaité après pruning
nf <<- nbcl + 2
longDim <<- seq_len(dima)              ## le vecteur de longueur égale à la dimension nécessaire pour mettre ensemble les centres
longClass <<- seq_len(nbcl)            ## le vecteur de longueur égale au nombre de classe idem qu'en haut
nomCol <<- paste0("x", longDim)        ## le vecteur pour renommer les colonnes de la réunion des centres des différents blocs


#---------------------------------------
# Fonction predict.clus.fus
source('mes_fonctions.R')
source("Code_simulation.R")