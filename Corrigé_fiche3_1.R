

#############################################################################
###########      Chap III      Intervalle de confiance            ###########
#############################################################################


##~~~~~~~~~~~~~~~~~~~~
# Exercice 1  Crab
##~~~~~~~~~~~~~~~~~~~~


# On s'intéresse à la taille du lobe frontal ( largeur arrière ,: longueur de la carapace respectivement).
# des crabes (Leptograpsus variegatus).  On prend 200 valeurs de la varable la taille du lobe frontal "FL" 
# du fichier "crabes"  de la librairie "MASS" comme population (RW et CL respectivement). On note mu la moyenne 
# théorique de la variable FL. On souhaite tester, basé sur un échantillon de 
# taille 28, :
## 1) Lancer 
# install.packages("MASS"); non nécessaire
library(MASS) # ouverture de la bibliothèque

# 2) Sauvegarder ces 200 valeurs de "FL" du fichier "crabs" de la library("MASS") dans une variable nommée
#  Population.  On suppose dans la suite que la population admet une loi normale N( mu, sigma^2) avec les 
# deux paramètres inconnus.

Population=crabs$FL
length(Population)  ## 200
summary(Population)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   7.20   12.90   15.55   15.58   18.05   23.10 

# 3)  IC de l'espérance (mu=15.583 la moyenne "théorique de la population totale)
mu= mean(Population) # mu=15.583


#  Générer une réalisationn de l'échantillon (X1...Xn) n=28 , puis construire l'intervalle 
# de confiance de $\mu$ avec un niveau de confiance 1- alpha = 95%.
n=28; (Cq=qt(0.975, df=n-1)) ## 2.051831

echan=sample(Population, 28, replace=T)
(xbar=mean(echan)); (sc=sd(echan))
xbar-Cq*sc/sqrt(n); xbar+Cq*sc/sqrt(n);  ##   14.23836;  16.63307

## 4)  
M=100; n=28; mu= mean(Population) # mu=15.583

BInf=c();BSup=c()
compteur=0
for (i in 1:M){
   echant = sample(Population, 28, replace=T)
   BInf[i] = mean(echant) - Cq*sd(echant)/sqrt(n)
   BSup[i] = mean(echant) + Cq*sd(echant)/sqrt(n)
   if ((BInf[i]<=mu)&(BSup[i]>=mu)){compteur=compteur+1}
   }
1-compteur/M  # 0.06


## 5)  IC de la variance (sigma^2=12.2173  la variance "théorique de la population totale)
var(Population); ## sigma^2=12.2173

n=15; 
(C1=qchisq(0.025, df=n-1)); (C2=qchisq(0.975, df=n-1))#  5.628726 ; 26.11895

echan=sample(Population, 15, replace=T)
(n-1)*var(echan)/C2;   (n-1)*var(echan)/C1;
##] 7.744416 ; 35.93637



## 6) Générer 100 réalisationns de l'échantillon (X1...Xn) avec n=15.  

M=100; n=15; sigma2=12.2173; alpha=0.10;  1-alpha/2# = 0.95

(C1=qchisq(0.05, df=n-1)); (C2=qchisq(0.95, df=n-1))#   6.570631 ; 23.68479

BInf=c();BSup=c()
compteur=0
for (i in 1:M){
   echant = sample(Population, 15, replace=T)
   BInf[i] = (n-1)*var(echant)/C2;  
   BSup[i] = (n-1)*var(echant)/C1;
   if ((BInf[i]<=sigma2)&(BSup[i]>=sigma2)){compteur=compteur+1}
   }
1-compteur/M  # 0.04

##--------------------------------------------------

##----------------------
##~~~~~~~~~~~~~~~~~~~~
# Exercice 1' pour les économistes
##~~~~~~~~~~~~~~~~~~~~
##  Exemple réel

## returns of the Standard and Poors 500 index for all the trading days 
## in 1990, 1991, ..., 1999.
# Rendements de l’indice Standard and Poors 500 pour toutes les journées
# de bourse de 1990 à 1999.

# 1) install.packages("MASS") ;  
library(MASS)   # ouveture de la bibliothèque "MASS"

# 2)  Définir la population
Population = SP500
summary(SP500)

# espérance = 0.04575267  (la moyenne "théorique de la population totale)
(mu=mean(Population));# 0.04575267

# 3) 
n=28; ## 1-alpha=0.95, 1-alpha/2=0.975
(Cq=qt(0.975, df=n-1))  ##  2.051831

(echant=sample(Population, 28, replace=T))  # sample : échantillonnage
## une réalisation d'un échantillon
xbar=mean(echant); sc=sd(echant);
(Binf=xbar-Cq*sc/sqrt(n)); (Bsup=xbar+Cq*sc/sqrt(n))## formules à appliquer
#  -0.05792601;  0.8981628

# [-0.09734585; 0.717752]   

# 4)
M=100; n=28

mu=0.04575267 ## espérance


BInf=c();BSup=c();
compteur=0
for (i in 1:M){
   echant=sample(Population, 28, replace=T)# qui se renouvelle à chaque v. de i
   BInf[i]=mean(echant) - Cq*sd(echant)/sqrt(n)
   BSup[i]=mean(echant) + Cq*sd(echant)/sqrt(n)
   if ((BInf[i]<=mu)&(BSup[i]>=mu)){compteur=compteur+1}
   } 
1-compteur/M  ##  0.06


## 5)  var(Population)  ## = 0.8982233 (la variance "théorique de la population totale)

n=15;
C1=qchisq(0.05, df=n-1);C2=qchisq(0.95, df=n-1); # 1-alpha/2=0.95
echant=sample(Population, 15, replace=T)
sc2=var(echant)
(Binf= (n-1)*var(echant)/C2); (Bsup=(n-1)*var(echant)/C1);
#   0.3781531 ;  1.363108


## 6)

M=100; n=15  ## alpha =10% => alpha/2= 0.05, 1- alpha/2= 0.95

(C1=qchisq(0.05, df=n-1));(C2=qchisq(0.95, df=n-1));
#  6.570631; 23.68479

plot(range(c(0,6)), range(c(1,M)), type="n", xlab="",ylab="" )
abline(v=sigma2, col="red") 

BInf=numeric(M);BSup=numeric(M);
compteur=0
for (i in 1:M){
   Echant=sample(Population, 28, replace=T)
   BInf[i]=(n-1)*var(Echant)/C2
   BSup[i]=(n-1)*var(Echant)/C1
   if ((BInf[i]<=sigma2)&(BSup[i]>=sigma2)){compteur=compteur+1}
   }
1-compteur/M


summary(SP500)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -7.11275 -0.41413  0.04210  0.04575  0.54282  4.98869 

(m=min(SP500));(M=max(SP500))
# -7.112745 [1] 4.988693
brk1=seq(m, M, length=50)
hist(SP500, prob=T, , breaks=brk1)


##~~~~~~~~~~~~~~~~~~~~
##  Exercice 2  Loi gaussienne
##~~~~~~~~~~~~~~~~~~~~


###   Intervalles de conf pour l'espérance cas gaussien
#################################################################
###   Loi normale  N(-10, 1.5^2)  mu = -10, sigma = 1.5

# chercher BInf(x1...xn) et BSup(x1...xn) tels que
# P(BInf(x1...xn) <= theta <= BSup(x1...xn) )=1-alpha
# Une seule réalisation :

n = 20;  mu = -10; sigma = 1.5

Echant=rnorm(n, mean=mu, sd=sigma)
xbar = mean((Echant))
Sc=sqrt(var(Echant))
(Cq=qt(0.95, df=n-1)) # car alpha=0.10 => 1-alpha/2= 0.95

xbar - Cq*Sc/sqrt(n);xbar + Cq*Sc/sqrt(n);
# -10.61587 ; -9.548189


#################################################################
## 1) + 2) Intervalle de confiance de mu  au niveau  1-alpha
#################################################################

M=100; n=20; mu= -10; sigma = 1.5 

alpha=0.10;  1-alpha/2# = 0.95
(Cq=qt(0.95, df=n-1)) #  1.729133

plot(range(c(mu-1.5*sigma,mu+1.5*sigma)), range(c(1,M)), type="n", xlab="",
  	ylab=")" ) # définir le cadre du graphique
abline(v=mu, col="red") # tracer le seqment vertical représentant la valeur de mu

Xbar=c();  Sc=c(); BInf=c(); BSup=c()

compteur=0
for (i in 1:M){
   Echant=rnorm(n, mean=mu, sd=sigma)
   Xbar[i]=mean(Echant)
   Sc[i]=sd(Echant)
   BInf[i]=Xbar[i] - Cq*Sc[i]/sqrt(n) # borne inf
   BSup[i]=Xbar[i] + Cq*Sc[i]/sqrt(n) # borne sup
   if ((BInf[i]<=mu)&(BSup[i]>=mu)) {compteur = compteur + 1} 
   lines( c(BInf[i],BSup[i]), c(i,i), col="blue")} 
# tracer l'ième segment horizontal représenatnt l' ième réalisation de Interv confiance 

compteur/M # pourcentage de bons intervalles

#  0.90

#################################################################
## 3)+4)  Intervalle de confiance de sigma^2 au niveau  1-alpha
#################################################################

M=100; n=20; mu= -10; sigma = 1.5

(C1=qchisq(0.025, df=n-1)); (C2=qchisq(0.975, df=n-1));
##  8.906516 ; 32.85233

plot(range(c(0,10)), range(c(1,M)), type="n", xlab="",ylab="" )
abline(v=1.5^2, col="red") 

M=100; n=20;
Sc=c(); 
BInf=c();BSup=c(); longueur=c()

compteur2=0
for (i in 1:M){
   Echant=rnorm(n, mean = mu, sd = sigma)
   Sc[i]=sd(Echant)

   BInf[i]=(n-1)*Sc[i]^2/C2
   BSup[i]=(n-1)*Sc[i]^2/C1
   longueur[i]= BSup[i]- BInf[i]
   if ((BInf[i]<=1.5^2)&(BSup[i]>=1.5^2)){compteur2 = compteur2+1}
   lines( c(BInf[i],BSup[i]), c(i,i), col="blue")
}

mean(longueur)
## 3.36887

compteur2

(M-compteur2)/M  ##  0.06

summary(longueur)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.357   2.540   3.393   3.411   3.977   6.891 


## 5) 

(C1=qchisq(0.025, df=n-1)); (C2=qchisq(0.975, df=n-1));
##  8.906516 ; 32.85233

(C12=qchisq(0.04461, df=n-1)); (C22=qchisq(1-0.00539, df=n-1));
##  9.899141; 38.32776


BInf2=c();BSup2=c();
longueur2=c()
compteur2=0
for (i in 1:M){
   # Echant=rnorm(n, mean=10, sd=2)
   BInf2[i]=(n-1)*Sc[i]^2/C22
   BSup2[i]=(n-1)*Sc[i]^2/C12
   longueur2[i]= BSup2[i]- BInf2[i]
   if ((BInf2[i]<=1.5^2)&(BSup2[i]>=1.5^2)){compteur2 = compteur2+1}
   lines( c(BInf2[i],BSup2[i]), c(i,i), col="blue")}

mean(longueur)  ## 3.410528
(longueur > longueur2)
mean(longueur2) ## 3.08442

sum((longueur > longueur2)) ## 100
compteur2  ## 95
(M-compteur2)/M  ##  0.05
summary(longueur2)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.243   2.325   3.106   3.123   3.641   6.309 

##~~~~~~~~~~~~~~~~~~~~
##  fin
##~~~~~~~~~~~~~~~~~~~~


1-0.00539-0.04461 ##  0.95011

(1/C1-1/C2)  ## 0.0818381
(1/C12-1/C22) ##  0.07492811


##--------------------------------------------------
## comparaison



alpha=0.05; alpha/2; 1-alpha/2#  0.025;  0.975
##############################


#~~~~~~~~~~~~~
##  Exercice 3  Théorème central Limite
#~~~~~~~~~~~~~


## Loi  Beta B(0.5,0.5)

#  simulation de N=50000  v. a. de la loi  Beta(0.5,0.5)
n=50; M=1000; (N=n*M)
x=rbeta(N,0.5,0.5)

# 1) On redéfinit une répartition allant du minimum au maximum  pour un 
# histogramme plus fin
(mx=floor(min(x)));(Mx=ceiling(max(x)))
brk=c(seq(mx,Mx,length=101))         
hist(x, probability = TRUE, col = "light blue", breaks = brk)     
# probability = T   permet de remplacer les effectifs par les proportions

# 2) On superpose le graphique  de la densité théorique B(0.5,0.5)
curve(dbeta(x,0.5,0.5),col="red",lwd = 2,add=TRUE)    ## FALSE

# 3)  x reécrit en matrice "enchant" peut etre considérée comme M réalisations
#  d'un échantillon de taille  n=50 et que l'on peut obtenir M réalisation 
# de la variable     Z = sqrt(n)*(Xbar - mu)/sigma
# où mu= alpha/(alpha +beta) =1/2, 
# sigma = sqrt(alpha*beta)/(alpha +beta)^2/(alpha +beta +1))=sqrt((1/4)/2)=1/sqrt(8)
1/sqrt(8) # =0.3535534

Echant=matrix(x, nrow=1000, byrow=TRUE) # x est défini plus haut
  z = c();
for(k in 1:M){
      z[k]=sqrt(n)*(mean(Echant[k,])-0.5 )/(1/sqrt(8)) 
 }

hist(z,proba=T, breaks=0.1*(-55:55),xlim=c(-5,5), main="Xbar de  Beta(0.5,0.5)" )

# 4) 
curve(dnorm(x,mean=0,sd=1),col="red",lwd = 2,add=TRUE)
   

#~~~~~~~~~~~~~
##  Exercice 4 IC pour une proportion
#~~~~~~~~~~~~~

treering
summary(treering)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.0000  0.8370  1.0340  0.9968  1.1970  1.9080 

# 1) conversion en variable de Bernoulli
Population = ifelse((treering<=0.89), 1, 0)
length(Population) ##  7980

## 20 premières valeurs
Population[1:20]
# 0 0 0 0 0 0 1 0 1 1 1 1 1 1 1 0 1 1 1 1

## pi= mean(Population) le pourcentage de 1, paramètre inconnu
## que vous n'etes pas censé de connaitre

# 2) 
alpha=0.05; alpha/2; 1-alpha/2#  0.025;  0.975

n=65
echant=sample(Population, n, replace=T)
(S=sum(echant)); xbar=S/n #  18  est le nb de 1, 18/65 est la proportion de 1

# 3) méthode asymptotique
# par le Théorème Central Limite, on a 
(Cq=qnorm(0.975))  ## 1.959964
(Binf = xbar - Cq*sqrt(xbar*(1-xbar)/n)) ##   0.1681395
(Bsup = xbar + Cq*sqrt(xbar*(1-xbar)/n)) ##   0.3857066
# qui contient la valeur du paramètre pi : 
(pi= mean(Population)) ## 0.3065163

# 4) Méthod de Wald (asymptotique)
install.packages("binom")  #    avec " "
library(binom)

binom.confint(x=18, n=65, conf.level = 0.95, methods = "asymptotic")
#       method  x  n      mean     lower     upper
# 1 asymptotic 18 65 0.2769231 0.1681395 0.3857066
# on a le même résultat
 
# 5) méthode exacte
IC= binom.confint(18, n=65, conf.level=0.95,method="exact") 
IC
#  method  x  n      mean     lower     upper
#1  exact 18 65 0.2769231 0.1731004 0.4019012
IC[5]; IC[6]; 
# 1 0.1731004 ;   0.4019012

# 6) 
qbeta(0.025, 18, 65-18+1); qbeta(1 - 0.025, 18+1, 65-18)
#  0.1731004 ;   0.4019012
# on a le même résultat

# 7) méthode de Bayes
binom.confint(18,n=65, conf.level=0.95,method="bayes") 
#   method  x  n      mean     lower     upper
# 1  bayes 18 65 0.280303 0.1754592 0.3888583

# 11 méthodes différentes
binom.confint(x=18, n=65, conf.level = 0.95, methods = "all")
          method  x  n      mean     lower     upper
1  agresti-coull 18 65 0.2769231 0.1822507 0.3964915
2     asymptotic 18 65 0.2769231 0.1681395 0.3857066
3          bayes 18 65 0.2803030 0.1754592 0.3888583
4        cloglog 18 65 0.2769231 0.1748962 0.3884502
5          exact 18 65 0.2769231 0.1731004 0.4019012
6          logit 18 65 0.2769231 0.1819710 0.3973543
7         probit 18 65 0.2769231 0.1795943 0.3946957
8        profile 18 65 0.2769231 0.1781811 0.3929243
9            lrt 18 65 0.2769231 0.1781805 0.3929024
10     prop.test 18 65 0.2769231 0.1765404 0.4039010
11        wilson 18 65 0.2769231 0.1829358 0.3958064



#~~~~~~~~~~~~~
##  Exercice 5
#~~~~~~~~~~~~~


install.packages("binom") #     avec "  "
library(binom)


M=1000; n1=15; p=0.45

## a) Méthode exacte (???)

s=rbinom(1, n1, p);  (IC=binom.confint(s, n=n1, conf.level=0.95,method="exact") )
IC[5]>p; IC[6]<p; 

compteur = 0  ## ceux qui ne contiennent pas p=0.45

for (i in 1:M){
  nombre=rbinom(1, size=n1, prob = p)
  #nombre=sum(echant)  ## nb de "1"
  IC=binom.confint(nombre, n=15, conf.level=0.95,method="exact") 
  if ((IC[5]>p)|(IC[6]<p)) { compteur = compteur+1 }  }

compteur/M  ##  0.0166 ; 0.0217

## b) omis

## c) Méthode Wald (asymptotique)
compteur2 = 0
for (i in 1:M){
  echant=rbinom(n, size=1, prob = 0.3)
  nombre=sum(echant)
  IC=binom.confint(nombre, n=15, conf.level=0.95,method="asymptotic") 
  if ((IC[2]>0.3)|(IC[3]<0.3)) { compteur2 = compteur2+1 } }

compteur2/M  ##   0.0856 ;  0.082

# d) omis  e) méthode de  bayes
M1=50000; n1=15; p0=0.3

compteur=0
for (i in 1:M1){
  echant=rbinom(n1, size=1, prob = 0.3)
  IC=binom.confint(sum(echant),n=n1, conf.level=0.95,method="Wald")
IC=binom.confint(nombre, n=n1, conf.level=0.95,method="bayes") 

  if ((IC[2]>p0)|(IC[3]<p0)) { compteur = compteur+1 } 
}
compteur/M1  ##  0.04986  ;   0.05168  WHY  ?????


