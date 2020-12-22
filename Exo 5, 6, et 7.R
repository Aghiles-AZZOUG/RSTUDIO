


#################################################################
##  IC pour une proportion pi quand  n = 15 petit 
#################################################################

#~~~~~~~~~~~~~
##  Exercice 5
#~~~~~~~~~~~~~


M=3000; n1=15; p=0.45; alpha = 0.05

## 1) Méthode exacte 

compteur = 0  ## ceux qui ne contiennent pas p=0.45
for (i in 1:M){
  nb=sum(rbinom(n1,size=1, prob = p))# nb de "1"
  BInf = qbeta(alpha/2, nb, n1-nb+1) ; 
  BSup = qbeta(1-alpha/2, nb+1, n1-nb )
   if ( ( BInf > p)|(BSup < p) )  # "|" signifie "ou"
   # on compte les mauvais intervalles,   
     { compteur = compteur+1 }  
}
compteur/M  ##  0.0166 ; 0.0217

# ou utiliser le package "binom"

install.packages("binom") #     avec "  "
library(binom)
 
compteur=0
Binf=c(); Bsup=c()
for (i in 1:M){
   Echant=rbinom(n2, size=1, prob=p)
   IC=binom.confint(sum(Echant),n=n2, conf.level=0.95,method="exact")
   Binf[i] = as.numeric(IC[5]); Bsup[i] = as.numeric(IC[6])
   if ((Binf[i]<=p)&(Bsup[i] >=p)) # bon intervalle
    {compteur = compteur+1}
   }
compteur/M ##   0.9796667

mean(Binf); mean(Bsup) # 0.2101077 ; 0.7157901
#  remarques :  la sortie de IC fournit 6 valeurs avec leurs noms: 
# dont IC[5] est la valeur de Binf avec nom "lower"
# dont IC[6] est la valeur de Binf avec nom "upper". 
# la commande "as.numeric"
# permet de supprimer les noms donc calculer les moyennes.

compteur = 0  ## ceux qui ne contiennent pas p=0.45
for (i in 1:M){
  nombre=sum(rbinom(n1,size=1, prob = p)## nb de "1"
   IC=binom.confint(nombre, n=15, conf.level=0.95,method = "exact") 
  if ((IC[5]>p)|(IC[6]<p)) # IC est un vecteur de 6 composantes 
  # dont le 5ème et le 6ème fournient les bornes inf et sup
  { compteur = compteur+1 }  
}
compteur/M  ##  0.0166 ; 0.0217

## 2) Méthode Wald (asymptotique)
compteur2 = 0
for (i in 1:M){
  echant=rbinom(n1, size=1, prob = 0.3)
  nombre=sum(echant)
  IC=binom.confint(nombre, n=n1, conf.level=0.95,method="asymptotic") 
  if ((IC[5]>0.3)|(IC[6]<0.3)) # mauvais intervalle !!!  "|" signifie "ou"
  { compteur2 = compteur2+1 } }
compteur2/M  ##    0.05


# 3) méthode de  bayes
M1=50000; n1=15; p0=0.45
compteur=0
for (i in 1:M1){
  echant=sum(rbinom(1, n1, prob = p0))
  IC=binom.confint(sum(echant),n=n1, conf.level=0.95,method="asymptotic")
IC=binom.confint(nombre, n=n1, conf.level=0.95,method="bayes") 

  if ((IC[5]>p0)|(IC[6]<p0)) { compteur = compteur+1 } 
}
compteur/M1  ##  0.04986  ;   0.05168  WHY  ?????

## Par Théorème Central Limite (méthode de Wald) pour n  grand

M=50000; n1=15; p=0.4
(Cq=qnorm(0.975))  # 1.959964
compteur=0
for (i in 1:M){
    echant=rbinom(n1, size=1, prob=p)
    xbar =  mean(echant)
    Binf = xbar - 1.96* sqrt(xbar*(1-xbar)/n1)
    Bsup = xbar + 1.96* sqrt(xbar*(1-xbar)/n1)
     if ((Binf>p)|(Bsup <p)) { compteur=compteur+1 }
   }
compteur/M  ##  0.05004 ;  0.05066  POURQUOI WHY  ?????


#####################################################
## La raison ? S=sum(echant) suivant la loi  B(15, 0.4)
# la qualité d'approx de S par une v.a. normale N(mu, sigma^2)
# est bonne bien que n=15 soit assez petit !


##  0.91734  0.91866   POURQUOI ????

#####################################################

# assez mauvais !!!
###############################################################
###  Aproximation d'une loi binomiale par une loi normale   ###
###############################################################

5/30; 5/35;  5/40;  5/45;  5/50 ;  10/150
#  0.1666667; 0.1428571; 0.125; 0.1111111;  0.1 ;  0.06666667





## distance de Kolmogrov  sup{ |F1(x)-F2(x)|, x in R}


(kol.0=max(abs(vb-vn))) ##   0.1036146

(kol.c=max(abs(vb-vnc))) ##  0.003778221


##~~~~~~~~~~~~~~~~~~~~
# Exercice 6
##~~~~~~~~~~~~~~~~~~~~
install.packages("binom"); library(binom)
M=100; n3=1000; p=0.005
# 1) méthode exacte
compteur=0
Binf=c(); Bsup=c()
for (i in 1:M){
   Echant=rbinom(n3, size=1, prob=p)
    Binf[i] = qbeta( 0.025,  sum(Echant), n3-sum(Echant)+1); 
    Bsup[i] = qbeta(1-0.025, sum(Echant)+1, n3-sum(Echant)); 
   if ((Binf[i]<=p)&(Bsup[i] >=p)){compteur = compteur+1}
   }
compteur/M ##   0.98

# ou utiliser le package "binorm" : 
 
  # IC=binom.confint(sum(Echant),n=n3, conf.level=0.95,method="exact")
  # Binf[i] = as.numeric(IC[5]); Bsup[i] = as.numeric(IC[6]) # enlever l'étiquette
mean(Binf); mean(Bsup) # [1] 0.001777751 ; [1] 0.01160123

#  remarques :  la sortie de IC fournit 6 valeurs avec leurs noms: 
# dont IC[5] est la valeur de Binf avec nom "lower"
# dont IC[6] est la valeur de Bsup avec nom "upper". la commande "as.numeric(IC[5])"
# permet de supprimer les noms donc calculer la moyenne.


# 2) méthode asymptotique
M=100; n3=1000; p=0.005

Cq = qnorm(0.975)
compteur2 = 0; 
Binf=c(); Bsup=c(); Xbar=c()
for (i in 1:M){
   Echant=rbinom(n3, size=1, prob=p)
   Xbar[i] = mean(Echant)
   Binf[i] = Xbar[i] - Cq*sqrt(Xbar[i]*(1-Xbar[i])/n3) ; 
   Bsup[i] = Xbar[i] + Cq*sqrt(Xbar[i]*(1-Xbar[i])/n3) ;
   if ((Binf[i]<=p)&(Bsup[i] >=p))# les bons intervalles
  {compteur2 = compteur2+1}
   }
1 - compteur2/M ##   0.12

mean(Binf); mean(Bsup) #   0.0006221437 ;  0.008817856

#  POURQUOI ????

#####################################################
## La raison ? S=sum(echant) suivant la loi  B(1000, 0.005)
# la qualité d'approx de S par une v.a. normale N(mu, sigma^2)
# est  mauvaise bien que n=1000 soit très grand

## distance de Kolmogrov  sup{ |F1(x)-F2(x)|, x in R}

(kol.0=max(abs(vb-vn))) ##    0.115961
(kol.c=max(abs(vb-vnc))) ##   0.02874008



##~~~~~~~~~~~~~~~~~~~~
# Exercice 7
##~~~~~~~~~~~~~~~~~~~~

# Google avec les mots clés : r-bloggers, poisson distribution, americium 24
# puis copier :
 emissions = 0:19
 observed = c(1, 4, 13, 28, 56, 105, 126, 146, 164, 161, 123, 101, 74, 53, 23, 15, 9, 3, 1, 1)

#  1)  
(n=sum(observed)); # 1207
# comme il y a une fois 0, 4 fois 1, 13 fois 2... et 1 fois 19, 

# - la somme totale est donc égale à sum(emissions*observed)  #  10099
# - la moyenne est donc égale à 
(lambda= sum(emissions*observed)/n)  #  8.367026

# 2)  
(Eff.theo=dpois(emissions, 8.367026)*n)
 Eff.obs=observed

# 3)  
Effectifs= matrix(c(Eff.obs, Eff.theo),nrow=2,byrow=TRUE)
colnames(Effectifs) = 0:19
row.names(Effectifs) = c("Estimé","Théorique")
barplot(
   Effectifs, beside=TRUE,
 legend.text=TRUE ,args.legend = list(x=17,y=140),
  ylim=c(0,170),col=c("darkblue","red")
  )

# 4)
Proportions= matrix(c(Eff.obs/n, Eff.theo/n),nrow=2,byrow=TRUE)
colnames(Proportions) = 0:19
row.names(Proportions) = c("Proportion","Proba")
barplot(
   Proportions, beside=TRUE,
 legend.text=TRUE ,args.legend = list(x=17,y=0.16),
  ylim=c(0,0.17),col=c("darkblue","red")
  )




##~~~~~~~~~~~~~~~~~~~~
# Exercice 7
##~~~~~~~~~~~~~~~~~~~~

# Google avec les mots clés : r-bloggers, poisson distribution, americium 24
# puis copier :
 emissions = 0:19
 observed = c(1, 4, 13, 28, 56, 105, 126, 146, 164, 161, 123, 101, 74, 53, 23, 15, 9, 3, 1, 1)

#  1)  
(n=sum(observed)); # 1207
# comme il y a une fois 0, 4 fois 1, 13 fois 2... et 1 fois 19, 

# - la somme totale est donc égale à sum(emissions*observed)  #  10099
# - la moyenne est donc égale à 
(lambda= sum(emissions*observed)/n)  #  8.367026

# 2)  
(Eff.theo=dpois(emissions, 8.367026)*n)
 Eff.obs=observed

# 3)  
Effectifs= matrix(c(Eff.obs, Eff.theo),nrow=2,byrow=TRUE)
colnames(Effectifs) = 0:19
row.names(Effectifs) = c("Estimé","Théorique")
barplot(
   Effectifs, beside=TRUE,
 legend.text=TRUE ,args.legend = list(x=17,y=140),
  ylim=c(0,170),col=c("darkblue","red")
  )

# 4)
Proportions= matrix(c(Eff.obs/n, Eff.theo/n),nrow=2,byrow=TRUE)
colnames(Proportions) = 0:19
row.names(Proportions) = c("Proportion","Proba")
barplot(
   Proportions, beside=TRUE,
 legend.text=TRUE ,args.legend = list(x=17,y=0.16),
  ylim=c(0,0.17),col=c("darkblue","red")
  )


# 5) 
n
install.packages("DescTools")
library("DescTools")

s=sum(emissions*observed)
 ( IC=PoissonCI(s, n, conf.level = 0.90, sides ="two.sided", method = "exact"))

# Borne inf
   IC[2]; #   8.230549 

# Borne sup 

  IC[3]  #   8.505279





