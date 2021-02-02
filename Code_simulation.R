library(lubridate)
# Les vecteurs réponses resp. kmeans, cubt affectation et prévision
res.mean.kmeans <- NULL
res.mean.cubt.Aff <- NULL


# Pour chaque taille d'échantillon faire ceci:


for (nb in seq_len(length(vectObs))) { # Boucle sur taille d'échantillon
  #----------------------------------------------------------------------------------------
	nbObs <<- vectObs[nb]        ## la taille de l'échantillon
	print(paste("N = ", nbObs))
	sizeOpt_m <<- unlist(taille_opt[nb])
	MinSplit2_m <<- unlist(MinSplit2[nb])
	MinSplit_c <<- unlist(MinSplit_cm[nb])
	nbech <<- unlist(nbech_m[nb])
	sizeOpt_c <<- unlist(sizeOpt_cm[nb])
  # les indicateurs de performance de chaque modèle
  # Pour Kmeans
	tot.error.kmeans <- NULL
  # Pour Cubt affectation
	tot.error.cubt.Aff <- NULL
  # Pour chaque répétition faire ceci:
	for (i in seq_len(nbRep)) {
		print(paste("Repetion", i))
    # 1-Générer les données
		data1 <- gendata( mod,dim = dima,N = nbObs,sigma = SigMa,ncl = nbcl)
    ## Conversion de data1 en matrice sans label
		data = as.matrix(data1[,-1], drop = FALSE)
		yobs = data1[,1]
		
    # 2: Sans fusion
    ## 2-A: Kmeans
		print("Kmeans exact")
		TT0 = Sys.time()
		obj.kmeans <- kmeans(data, nstart = 20, centers = nbcl)
		clas.kmeans <- obj.kmeans$cluster
    # le temps de kmeans sur tout l'échantillon
		t.kmeans = difftime(Sys.time(), TT0, units = 'secs')
    tot.t1.k <- rep(t.kmeans, length(nbech))
    ## 2-B: Cubt
		print("Cubt exact")
		TT1 = Sys.time()
		arb.cub <- cubt(data, mindev = MinDev, minsplit = MinSplit1, minsize = sizeOpt_N[nb], lp = profond1)
    # pruning stage mindist = 0.3 or 0.5 and  = 0.2, 0.4 or 0.6
		prun.cub = prune.cubt(arb.cub, data, nleaves = nf, mindist = MinDist)
		gc()
		gc()
    # join leaves
		jon.cub = join2.cubt(prun.cub, data, nclass = nbcl, alph = alph)
		cub.class <- predict.cubt(jon.cub)
    # le temps de cubt sur tout l'échantillon
		t.cubt = difftime(Sys.time(), TT1, units = 'secs')
		tot.t1.c <- rep(t.cubt, length(nbech))
    #--------------------------------------
    ## 3- Fusion
		print("Pour la fusion")
		tot.t22.k =tot.t22.caf= numeric() # temps de calcul pour kmeans et cubt
		err.k = err.c = ind.k = ind.c = NULL # erreurs et ari pour kmeans et cubt
    
		for (j in seq_len(length(nbech))) { # boucle sur les nombres de blocs
      # Etape 1: Diviser l'échantillon en m sous-échantillon de taille l
			n <- nrow(data)
			m <- nbech[j]  # m sous-echantillon
			print(paste("m = ", m))
			bloc = cv(n,m)
      #------------------------------------------------------------------------------------------
# Etape 2: A chaque sous-echantillon, appliquer Kmeans
# Kmeans
			print("Kmeans Fusion en parallèle")
			T11 = Sys.time()
			res_par_foreach <- foreach(i = seq_len(m)) %dopar% myfonction_centerKmeans(i, centers = nbcl)
			UCenntre1 = do.call(rbind,res_par_foreach)
# Etape 3et4: Appliquer Kmeans à UCenntre
			print("Kmeans Fusion sur union des centres de bloc")
			UkmeansD <-kmeans(UCenntre1, nstart = 20, centers = nbcl)
			UCentresF <- UkmeansD$centers
      # Etape 5: Affectaion RFM
			clasFusion.kmeans <- predict.clus.fus(data, UCentresF)
			temps.Fusion.kmeans1 = difftime(Sys.time(), T11, units = 'secs')
# Cubt
			print("Cubt Fusion en parallèle")
			T22 = Sys.time()
			res_par_foreach <- foreach(i = seq_len(m)) %dopar% myfonction_centerCubt(i, mindev = MinDev, minsplit = MinSplit2_m[j], minsize = sizeOpt_m[j], 
			                                                                         lp = profond2, nclass = nbcl, nleaves = nf, mindist = MinDist, alph = alph)
			UCenntre2 = do.call(rbind,res_par_foreach)
			print("Cubt Fusion sur union des centres de bloc")
			arb1 <- cubt(UCenntre2, mindev = MinDev, minsplit = MinSplit_c[j], minsize = sizeOpt_c[j], lp = profond2)
			# ncla2 <- length(table(predict.cubt(arb1)))
			# cat("\n Le nombre de classe après cubt sur UCenntre2 est:", ncla2, "\n")
			
			prun1 = prune.cubt(arb1, UCenntre2, nleaves = nf, mindist = MinDist)
			gc()
			gc()
			ncla1 <- length(table(predict.cubt(prun1)))
			if (ncla1 < nbcl) {
				warning("Le nombre de classe souhaité n'est pas atteint sur les centres", immediate. = T)
				cat("\n Le nombre de classe après prunig est:", ncla1, "\n")
			}
			jon1 = join2.cubt(prun1, UCenntre2, nclass = nbcl, alph = alph)
      # recuperer les centres
			cl.cubt <- as.factor(predict.cubt(jon1))
			centreF = do.call(rbind,by(UCenntre2,cl.cubt,colMeans))
      # Etape 5: Affectaion RFM
			clasFusion.cubt.aff <- predict.clus.fus(data, centreF)
			temps.Fusion.cubt.Aff1 = difftime(Sys.time(), T22, units = 'secs')
#-------------------------------------------------------------------------------------------------------
# Evaluation
			print("Etape des évalutions")
# Kmeans
		# Entre Kmeans-verité(y)
		
			err.k = rbind(err.k, c(error(clas.kmeans, yobs)[1],error(clasFusion.kmeans, yobs)[1] ,error(clasFusion.kmeans, clas.kmeans)[1]))
			err.c = rbind(err.c,c(error(cub.class, yobs)[1], error(clasFusion.cubt.aff, yobs)[1], error(clasFusion.cubt.aff, cub.class)[1]))
			
			ind.k = rbind(ind.k, c(adjustedRandIndex(clas.kmeans, yobs),
						adjustedRandIndex(clasFusion.kmeans, yobs), adjustedRandIndex(clasFusion.kmeans, clas.kmeans)))
			ind.c = rbind(ind.c, c(adjustedRandIndex(cub.class, yobs),
						adjustedRandIndex(clasFusion.cubt.aff, yobs),adjustedRandIndex(clasFusion.cubt.aff, cub.class)))

			tot.t22.k <- c(tot.t22.k, temps.Fusion.kmeans1)
			tot.t22.caf <- c(tot.t22.caf, temps.Fusion.cubt.Aff1)
# Le temps total pour Fusion cubt prévision
		} #Fin boucle sur nombre de blocs
		aa.k <-cbind(tot.t1.k,tot.t22.k,err.k,ind.k)
		aaa <- matrix(c(rep(nrow(data), length(nbech)), nbech), ncol = 2)
		res.kmeans <-data.frame(aaa, aa.k)
		colnames(res.kmeans) = c("N", "m", "T1", "T2", "ME1", "ME2", "ME12", "ARI1", "ARI2", "ARI12")
		tot.error.kmeans = rbind(tot.error.kmeans, res.kmeans)
		aa.c.af <-cbind(tot.t1.c, tot.t22.caf,err.c,ind.c)
		res.cubt.Aff <-data.frame(aaa, aa.c.af)
		colnames(res.cubt.Aff)  = c("N", "m", "T1", "T2", "ME1", "ME2", "ME12", "ARI1", "ARI2", "ARI12")
		tot.error.cubt.Aff = rbind(tot.error.cubt.Aff, res.cubt.Aff)
		
	} # Fin boucle sur répétitions
  #------------------------------------------------------------------------------------------------------------------------------------------
  # Résultat de Kmenas et cubt
	bb.k <- tot.error.kmeans[, c(3:10)]
	bb.c <- tot.error.cubt.Aff[, c(3:10)]
	gr = tot.error.kmeans[,"m"]
	mean.k = cbind(nbObs,nbech,do.call(rbind,by(bb.k,gr,colMeans)))
	mean.c = cbind(nbObs,nbech,do.call(rbind,by(bb.c,gr,colMeans)))
	res.mean.kmeans <- rbind(res.mean.kmeans, mean.k)	
	res.mean.cubt.Aff <- rbind(res.mean.cubt.Aff, mean.c)
} # Fin Boucle sur taille d'échantillon

indd <- seq_len(length(unlist(nbech_m)))

colnames(res.mean.kmeans) <-c("N","m","T1", "T2","ME1","ME2","ME12","ARI1","ARI2","ARI12")
row.names(res.mean.kmeans) <- indd
colnames(res.mean.cubt.Aff) <-c("N", "m","T1", "T2","ME1","ME2","ME12","ARI1","ARI2","ARI12")
row.names(res.mean.cubt.Aff) <- indd

# Réponses
# if (mod ==1){
#   R1M1 <- round(res.mean.kmeans,3)
#   print(R1M1)
#   saveRDS(R1M1, file = paste("R1M1", date(), sep = "-"))
#   
#   r1 <- res.mean.kmeans
#   R1M1_k <- cbind(r1[, c(1:4)], r1[, c(5:10)]*100)
#   print(round(R1M1_k,3))
#   saveRDS(round(R1M1_k,3), file = "R1M1_k")
#   
#   R2M1 <- round(res.mean.cubt.Aff,3)
#   print(R2M1)
#   saveRDS(R2M1, file = paste("R2M1", date(), sep = "-"))
#   
#   r2 <- res.mean.cubt.Aff
#   R2M1_c <- cbind(r2[, c(1:4)], r2[, c(5:10)]*100)
#   print(round(R2M1_c,3))
#   saveRDS(round(R2M1_c,3), file = "R2M1_c")
#   
# } else{
#   R1M2 <- round(res.mean.kmeans,3)
#   print(R1M2)
#   saveRDS(R1M2, file = paste("R1M2", date(), sep = "-"))
#   
#   r1 <- res.mean.kmeans
#   R1M2_k <- cbind(r1[, c(1:4)], r1[, c(5:10)]*100)
#   print(round(R1M2_k,3))
#   saveRDS(round(R1M2_k,3), file = "R1M2_k")
#   
#   R2M2 <- round(res.mean.cubt.Aff,3)
#   print(R2M2)
#   saveRDS(R2M2, file = paste("R2M2", date(), sep = "-"))
#   
#   r2 <- res.mean.cubt.Aff
#   R2M2_c <- cbind(r2[, c(1:4)], r2[, c(5:10)]*100)
#   print(round(R2M2_c,3))
#   saveRDS(round(R2M2_c,3), file = "R2M2_c")
# }

if (mod ==3){
  R1M3 <- round(res.mean.kmeans,3)
  print(R1M3)
  saveRDS(R1M3, file = paste("R1M3", today(), sep = "-"))
  
  r1 <- res.mean.kmeans
  R1M3_k <- cbind(r1[, c(1:4)], r1[, c(5:10)]*100)
  print(round(R1M3_k,3))
  saveRDS(round(R1M3_k,3), file = paste("R1M3_k", today(), sep = "-"))
  
  R2M3 <- round(res.mean.cubt.Aff,3)
  print(R2M3)
  saveRDS(R2M3, file = paste("R2M3", today(), sep = "-"))
  
  r2 <- res.mean.cubt.Aff
  R2M3_c <- cbind(r2[, c(1:4)], r2[, c(5:10)]*100)
  print(round(R2M3_c,3))
  saveRDS(round(R2M3_c,3), file = paste("R2M3_c", today(), sep = "-"))
  
} else{
  R1M4 <- round(res.mean.kmeans,3)
  print(R1M4)
  saveRDS(R1M4, file = paste("R1M4", today(), sep = "-"))
  
  r1 <- res.mean.kmeans
  R1M4_k <- cbind(r1[, c(1:4)], r1[, c(5:10)]*100)
  print(round(R1M4_k,3))
  saveRDS(round(R1M4_k,3), file = paste("R1M4_k", today(), sep = "-"))
  
  R2M4 <- round(res.mean.cubt.Aff,3)
  print(R2M4)
  saveRDS(R2M4, file = paste("R2M4", today(), sep = "-"))
  
  r2 <- res.mean.cubt.Aff
  R2M4_c <- cbind(r2[, c(1:4)], r2[, c(5:10)]*100)
  print(round(R2M4_c,3))
  saveRDS(round(R2M4_c,3), file = paste("R2M4_c", today(), sep = "-"))
}

