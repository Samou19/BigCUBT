
#########################################"
# les packages nécessaires
require(cubt) 
require(mclust)
require(caret)
require(pdist)
# calcul parallèle
require("doParallel")
getDoParWorkers() # affiche nombre de coeurs alloué
ncore <- detectCores() # 40 pour la machine distante
registerDoParallel(ncore-3) # alloue le nombre de coeurs souhaité
getDoParWorkers() # Pour vérifer qu'on utilise bien tous les coeurs

# Les paramètres
# Faire varier le nombre d'observation N
vectObs <<- c(3000, 30000, 300000)
#sizeOpt_N = c(78, 150, 9)
# minsize en fonction de la taille d'échantillon
sizeOpt_N <<- c(8, 545, 545) # obtenu par opt.minesize et lp = 9

# Nombre de  répétition à faire pour chaque taille d'échantion
nbRep <<- 5

mod <<- 2                   ## le modèle M2
dima <<- 5                  ## la dimension des données
SigMa <<- 0.19               ## la dispersion des données
nbcl <<- 10                   ## le nombre de classe

## le nombre de bloc à effectuer pour chaque taille d'échantillon
#nbech <<- c(10, 50, 80)
nbech_m <<- list(c(10,100), c(10, 100, 1000), c(10, 100, 1000, 10000))
## minesize optimale pour chaque bloc de données ainsi que pour chaque taille d'échantillon
taille_opt <<- list(c(10, 2), c(8, 15, 3), c(545, 8, 15, 3)) # par minsize
MinSplit2 <<-list(c(8,2), c(10, 15, 5), c(25, 10, 20, 5))
# taille_opt <<- list(c(4, 4, 4, 4), c(4, 4, 4, 4), c(4, 4, 4, 4)) # arbitraire car la version précédente les avait fixé tous à 2

## minesize optimale pour la combinaison des centres issues de chaque bloc
#sizeOpt_c <<- c(3, 3, 3) #  car la version précédente les avait fixé tous à 3
sizeOpt_cm <<- list(c(2,40), c(4, 6, 9), c(4, 6, 9, 100))
MinSplit_cm <<-list(c(8, 40), c(4, 6, 10), c(4, 6, 10, 15))

## la profondeur de l'arbre
MinDev = 0.01
profond1 <<- 9
profond2 <<- 9
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