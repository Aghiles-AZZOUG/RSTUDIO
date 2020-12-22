#etudiant Aghilas AZZOUG
#exo1
#elle suit une loi binomiale comme il y a un echantillonnage non exhaustif...
#Dans une population P, on étudie un caractère qualitatif C.
#Loi binomiale avec populationde  n=100 habitants, la probabilité pour qu'une
#personne souhaite être vaccinée contre la Covid est de p=0.8.
#x->B(n-100,p=0.8)
# on peut expliquer ainsi : on repette 100 fois une experience: demander si la personne souhaite vacciner ou paste(il  ya 2 issues: succes = oui , echec= non)
#les experiences sont indépendantes
#2: la probabilite entre 75 ET 85 personnes
sum(dbinom(75:85,100,0.8))  #resultat 0.8320809

#3(p()<0.05)
#pour ne pas manquer de vaccin:
  #il faudrait prendre 5%
  #80-6.25=73.75
  #avec 85% donc

####################################
#exo2
#loi de gama 
 X<-2
 alpha<-1
 beta<-2
 dgamma(X,shape=alpha,scale=beta)
 
# dessin graphe sur les 10 echantillon on peut desuir pour 1 eet 2 et 3
 plot(0, 0, xlim = c(0, 10), ylim = c(0, 1), type = "n")
 for(i in seq_along(k))
   curve(dgamma(x, shape = alpha, scale = beta), from = 0, to = 100, col = i, add = TRUE)
#resultat 0.1839397
 #schema gama_den
 > alpha<-3
 > beta<-2
 > dgamma(X,shape=alpha,scale=beta)
 #resultat 0.09196986
 # dessin graphe sur les 10 echantillon on peut desuir pour 1 eet 2 et 3
 plot(0, 0, xlim = c(0, 10), ylim = c(0, 1), type = "n")
 for(i in seq_along(k))
   curve(dgamma(x, shape = alpha, scale = beta), from = 0, to = 100, col = i, add = TRUE)
 #schema gama_den3_2
 
 
 #exo 3
 data<-read.txt("C:/Users/azzou/Documents/Yeux_Cheux.txt",header=TRUE)
 #1 il y a 593 individus effecit
 attach(data)
 x<-table(sexe) 
 #Trois variable
 #Pour visualiser noms de colonnes
 names(data)
 #type de variable caractere: "cheveux "yeux" "sexe"
 
 #exo4
 #importer le fichier : 
   nutri<-read.table("C:/Users/azzou/Documents/Nutrition.csv",sep=";",header=TRUE)
 #2 il y a 226
 attach(nutri)
 x<-table(situation) 
 #histogrme 
 set.seed(1)
 d <- sample(letters[1:226], 226, replace = TRUE)
 plot(table(d))
 
 # or
 barplot(table(taille))
 #image: taille 
 #commentaire : on ramarque que la majorité des personnes ont une entre l'interval [154 . 178]
 #et la taille max c'est 160
 
 #dessin  : TAILLe
 
 #Puis dessin: Histogramme pour la suite de derniere question
 ######################################"""
 #on fait  : 
 effectifs<-table(espece)  #effectif
 Proportion<-prop.table(x)   #frequence
 hauteurs = effectifs/sum(effectifs)/diff(classes) ## la densité de probabilité estimée
 ## d'être dans chacune des classes
 ## mon_histo (ci-dessous) est une liste « banale » avec presque
 ## tous les éléments d'une liste renvoyée par la fonction hist
 mon_histo = list(breaks=classes,
                  counts=effectifs,
                  density=hauteurs,
                  mids=centres)
 ## On rajoute un attribue de classe à mon_histo
 class(mon_histo) = "histogram"
 plot(mon_histo,
      xlab = "taille", ylab="Proportion",
      main="Histogramme des tailles par individus")
 
#boite de moustache 
 x <- c(1:10) ; y <- rnorm(100,5,2) ;z <- rnorm(100,5,2)
 boxplot(x)
 boxplot(x,y, z,names=c("X","Y","Z"))
 # x, y et z ici sont 3 listes (vecteurs) <=> cela donne 3 boîtes à moustaches
