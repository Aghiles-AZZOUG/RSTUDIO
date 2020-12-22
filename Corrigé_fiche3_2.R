

#####################################################

##~~~~~~~~~~~~~~~~~~~~
# Exercice 5
##~~~~~~~~~~~~~~~~~~~~
install.packages("binom"); library(binom)
M=3000; n2=15; p=0.45

# 1) méthode exacte
compteur=0
Binf=c(); Bsup=c()
for (i in 1:M){
   Echant=rbinom(n2, size=1, prob=p)
   IC=binom.confint(sum(Echant),n=n2, conf.level=0.95,method="exact")
   Binf[i] = as.numeric(IC[5]); Bsup[i] = as.numeric(IC[6])
   if ((Binf[i]<=p)&(Bsup[i] >=p)){compteur = compteur+1}
   }
compteur/M ##   0.9796667


mean(Binf); mean(Bsup) # 0.2101077 ; 0.7157901
#  remarques :  la sortie de IC fournit 6 valeurs avec leurs noms: 
# dont IC[5] est la valeur de Binf avec nom "lower"
# dont IC[6] est la valeur de Binf avec nom "upper". la commande "as.numeric"
# permet de supprimer les noms donc calculer les moyennes.


# 2) méthode asymptotique
M=3000; n=15; p=0.45

Cq = qnorm(0.975)
compteur2 = 0; 
Binf=c(); Bsup=c(); Xbar=c()
for (i in 1:M){
   Echant=rbinom(n, size=1, prob=p)
   Xbar[i] = mean(Echant)
   Binf[i] = Xbar[i] - Cq*sqrt(Xbar[i]*(1-Xbar[i])/n) ; 
   Bsup[i] = Xbar[i] + Cq*sqrt(Xbar[i]*(1-Xbar[i])/n) ;
   if ((Binf[i]<=p)&(Bsup[i] >=p)){compteur2 = compteur2+1}
   }
compteur2/M ##   0.9366667

mean(Binf); mean(Bsup) #  0.2065262; [1] 0.6927627

## méthode Wald  (approximation gaussienne)
##--------------
M=50000; n2=50; p=0.05

compteur2=0
for (i in 1:M){
   sample=rbinom(n2, size=1, prob = p)
  (IC=binom.ci(sum(sample),n=n2, conf.level=0.95,method="asymptotic"))
   if ( (IC[2]<=p)&(IC[3]>=p) ){compteur2 = compteur2+1}
   }
compteur2/M ##   0.91898   0.92068  POURQUOI ????

## Approximation gaussienne (Théorème Central Limite)

(Cq=qnorm(0.975)) #=1.959964  Wald =>  

compteur3=0

for (i in 1:M){
   Echant=rbinom(n2, size=1, prob = p)
   BInf=mean(Echant) - Cq*sqrt(mean(Echant)*(1-mean(Echant))/n2)
   BSup=mean(Echant) + Cq*sqrt(mean(Echant)*(1-mean(Echant))/n2)
    if ( (BInf<=p)&(BSup>=p) ){compteur3 = compteur3+1}
   }
compteur3/M

##  0.91734  0.91866   POURQUOI ????

#####################################################
## La raison ? S=sum(echant) suivant la loi  B(50, 0.05)
# la qualité d'approx de S par une v.a. normale N(mu, sigma^2)
# est  mauvaise bien que n=50 soit assez grand


# L'espérance et la variance de S suivant B(50,0.05)
# càd mu= n*p; sigma^2= n*p*(1-p)

50*0.05; 50*0.05*(1-0.05) ;## mu=2.5 ; sigma^2=2.375

x=0:10
y =dbinom(x, 50, prob=0.05)
plot(x,y, type="p", col="blue")
curve(dnorm((x), mean=2.5, sd=sqrt(2.375) ), col="red", add=T)
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

