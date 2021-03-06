


####################################################################
###########      Chap IV     Tests d'hypotheses          ###########
####################################################################
# Fiche 4  Tests d'hypoth�ses
##~~~~~~~~~~~~~~~~~~~~
# Exercice 1
##~~~~~~~~~~~~~~~~~~~~
  

#1) s'il s'agit du diagnostic d'une maladie grave : 

#a) H0 : "la maladie est pr�sente" contre H1 : "la maladie est absente"
#=> erreur de 1 esp�ce : la maladie grave ne sera pas trait�e � temps

#b) H0 : "la maladie est absente" contre H1 : "la maladie est pr�sente"
#=> erreur de 1 esp�ce : : traiter un sujet sain inutilement et douloureusement

#2) 1) s'il s'agit de la culpabilit� d'un d�fendeur devant le Tribunal correctionnel :

# a) H0 : "le d�fendeur est innocent " contre H1 : "le d�fendeur est coupable " 
#=> erreur de 1 esp�ce : mettre un innocent en prison

# b) H0 : "le d�fendeur est coupable" contre H1 : "le d�fendeur est innocent "
#=> erreur de 1 esp�ce :  rel�cher un coupable en liberant


#  En effet, H0 b�n�ficie de la pr�somption d�innocence, et ne sera condamn�e 
# que si on a suffisamment d��vidences. Sinon, le juge (le test) la
# rel�chera... ce qui n�est pas en soi une preuve d�innocence !


#L�hypoth�se H0, dite hypoth�se nulle,  joue un r�le particulier : 

# le but du test est de r�unir suffisamment de preuves au sein des donn�es pour 
# d�montrer qu�elle est fausse.  Si c�est le cas, l�hypoth�se nulle est rejet�e. 

# Dans le cas contraire, on ne rejette pas H0, � faute de preuves �, et on dit 
# sobrement que les donn�es ne sont pas incompatibles avec cette hypoth�se... 
# ce qui ne veut pas dire qu�elle est vraie !


##~~~~~~~~~~~~~~~~~~~~
# Exercice 2
##~~~~~~~~~~~~~~~~~~~~


L'�tiquette d'une bouteille de 75 cl de jus d'orange (d'une certaine marque) 
indique que le jus d'orange contient en moyenne, au plus un gramme de mati�re 
grasse. On pr�l�ve n = 30 bouteilles de la m�me marque, en trouve 

0.99 1.19 1.03 1.10 0.97 0.79 0.87 1.46 1.02 0.95 1.09 0.85 1.18 0.81 0.96 
1.22 0.72 1.13 1.23 1.05 1.36 1.32 1.21 1.02 1.36 0.97 1.21 1.06 1.31 1.01 

# t�l�charger les donn�es sur Moodle :
x=c(0.99,1.19,1.03,1.10,0.97,0.79,0.87,1.46,1.02,0.95,
1.09,0.85,1.18,0.81,0.96,1.22,0.72,1.13,1.23,1.05,
1.36,1.32,1.21,1.02,1.36,0.97,1.21,1.06,1.31,1.01)


## 0) Test de la normalit�
#  H0 : "il s'agit d'1 loi normale" contre H1: "loi non normale"

shapiro.test(x)
## data:  x
## W = 0.98359, p-value = 0.9108
## Comme p-valeur > le seuil, on conserve H0 (de la normalit�).

# 1) On souhaite tester 
#  H0 : mu <= 1 contre H1 :  mu > 1 au seuil de alpha = 0.05. 
(n=length(x)) ## 30

## 3)Statistique T = sqrt(n)*(Xbar-mu0)/Sc  qui suit, 
# si mu = 1, 1 loi de Student � n-1 degr�s de libert�
  (Ts = sqrt(n)*(mean(x) - 1)/sd(x))  # �viter d'utiliser T qui signifie "VRAI"
## 2.428621
sqrt(30)*(mean(x)-1)/sd(x)  # 2.428621

## 4) r�gion de rejet � droite :  [C, infini[  o� la valeur critique C =
qt(0.95, 29)  ##  1.699127   ## 1-alpha = 0.95

## 5)Comme la valeur t de T appartient � la r�gion de rejet, 
# le test est significatif : on rejette donc H0. 

## p-valeur  P(T>=2.428621)=  1- P(T <2.428621) = 1 - F(2.428621)
1 - pt(2.428621, df=29)  ## 0.01079386  < alpha = 0.5: 
# il n'y a plus de pr�omption prot�geant  H0, on rejette donc H0 => mu > 1.

################
 t.test(x, mu = 1, alternative = "greater")# <=> H0: mu <=1, H1: mu > 1
 t.test(x, mu = 1, alternative = "less")   # <=> H0: mu >=1, H1: mu < 1
 t.test(x, mu = 1, alternative = "two.sided")# <=> H0: mu =1, H1: mu <> 1

# data:  x
# t = 2.4286, df = 29, p-value = 0.01079
# alternative hypothesis: true mean is greater than 1
# 95 percent confidence interval:
# 1.02443     Inf
# sample estimates:
# mean of x 
# 1.081333 


##~~~~~~~~~~~~~~~~~~~~
# Exercice 3
##~~~~~~~~~~~~~~~~~~~~

Une universit� a re�u un envoi en masse de n = 400 mails de xx@xxxx.fr. On souhaite savoir si 
xx@xxxx.fr est un spammeur ( c'�d que le score > 2500). Si, se bas� sur les scores de ces 400
 mails, on a xbar = 2505 et s2c = 3293 , Que doit on conclure au seuil de a = 0.05 si on utilise : 
# (en suppopsant une loi gaussienne)
#  "spam" ssi "mu >= 2500",     donc on prend mu0 = 2500 :

# a) un test unilat�ral � gauche: 
# c'est � dire que les grandes valeurs contre les petites valeurs 

# 1) H0: mu >= 2500 contre H1: mu < 2500 au seuil alpha =0.05

# 3) On utilise Statistique T = sqrt(n)*(Xbar-mu0)/Sc 

# 4) region de rejet  ] -infini, C]  o� C = 
  -qt(0.95, df = 399)  ##  (C = -1.648682)
  qt(0.05, df = 399)  ##   calcul directe

sqrt(400)*(2505-2500)/sqrt(3293)
#  1.742626

# 5) Comme t de T n'appartient pas � la r�gion de rejet, on conserve H0.
# Conclusion : le test n'est pas significatif, on conserve H0. 
# Mais si on se trompe, la probabilit� de conserver H0 � tort 
# n'est pas contr�l�e.


# b) un test unilat�ral � droite : 

# 1) H0: mu <= 2500 contre H1: mu > 2500 au seuil alpha = 0.05

# 3) On utilise Statistique T = sqrt(n)*(Xbar-mu0)/Sc 

# 4) region de rejet [C, infini[   o� C =
  qt(0.95,  399)     ## 1.648682

sqrt(400)*(2505-2500)/sqrt(3293)
#  1.742626

# 5) Comme t de T appartient � la r�gion de rejet, le test est significatif:
# on rejette H0: il s'agit du spam !!!

# Si on se trompe, la probabilit� de rejeter H0 � tort est major�e par le seuil.
# ==> Le test unilat�ral � droite est pr�f�rable.



##~~~~~~~~~~~~~~~~~~~~
# Exercice 4
##~~~~~~~~~~~~~~~~~~~~



# On s'int�resse � la taille du lobe frontal des crabes. On prend la variable la taille du lobe frontal not�e "FL" du chier "crabs" (sans la lettre "e") de la librairie "MASS" avec 200 comme �tant le cardinal de la population . On note � l'esp�rance de la variable "FL". On souhaite, bas� sur un �chantillon de taille n = 18, tester l'hypoth�se nulle
# H0 : � = 14,5 contre H1 : � > 14,5 au seuil de a = 5%.
  library(MASS)
# Sauvegarder ces 200 valeurs de "FL" du fichier "crabs" dans un vecteur nomm� Population :
Population=crabs$FL

b) G�n�rer une r�alisation de l'�chantillon (X1,...,X30) de taille n = 30,
#  puis tester l'hypoth�se de la normalit� : 
# H0 : "il s'agit d'une loi normale" contre H1 : "il s'agit d'une loi non normale" au seuil de 10%.
# On utilise pour cela le 
echant = sample( Population, 30, replace = T)
shapiro.test(echant) 
# W = 0.97062, p-value = 0.5563

# la p-valeur = 0.5563 > 0,10

# On suppose dans la suite que la population suit une loi normale N(�,sigma^2) 
# avec les deux param�tres inconnus. On xe la taille de l'�chantillon � n = 18
#  dans toute la suite.
# c) G�n�rer M = 1000 r�alisations de l'�chantillon (X1,...,Xn) avec n = 18. 
# Calculer M = 1000 r�alisations de la statistique T du test en pr�cisant la 
# valeur critique C ainsi que la r�gion de rejet.
M=1000; n=18
(Cq = qt(0.95, 17)) #  1.739607
compteur = 0 nb de rejet de H0
Ts=c() # Eviter T qui signifie "VRAI"
for (i in 1:M){
  echant = sample(Population, n, replace = TRUE)
  Ts[i] = sqrt(n)*(mean(echant)- 14.5)/sd(echant)
  if (Ts[i] > Cq) {compteur = compteur + 1} 
}
compteur/M  ##  0.361
# � quoi correspond compteur/M  ???
mean(Population)  # mu = 15.583, donc H0 est fausse
# compteur/M correspond � la proba de rejeter H0 � RAISON, donc 
# la PUISSANCE du test
d) Tracer l'histogramme de ces M = 1000 r�alisations de la statistique T. Superposer avec la droite verticale x = C o� C est la valeur critique pour d�terminer la zone de rejet :

hist(Ts, col="green", ylim=c(0,0.45), prob=TRUE)
abline( v= Cq, add=T, col="black")

e) Calculer la vraie valeur de la moyenne th�orique � en utilisant la totalit� des 200 valeurs de la population. D�terminer la proportion des r�alisations du test (parmi M = 1000) qui conduisent � conserver H0.
f) Interpr�ter les r�sultats 

## a)

library(MASS)
Population=crabs$FL
(N=length(Population))  ## 200

## b)  test de la normalit�
echant=sample(Population, 30, replace=T)
shapiro.test(echant)
# W = 0.98243, p-value = 0.8859
# 0.8859 > seuil => On conserve l'hypo de la normalit�

# H0: mu <= 14.5 contre H1 : mu > 14.5  au seuil alpha =0.05
M=10000; n=18
## c)  r�gion de rejet [C, infini[, o� C = 
qt(0.95, df=n-1)  # 1.739607

compteur = 0 # nb de conservation de H0
Ts=c()
for (i in 1:M){ 
echant=sample(Population, 18, replace=TRUE)
Ts[i]= sqrt(18)*(mean(echant)-14.5)/sd(echant)
 if (Ts[i] <= 1.739607) { compteur = compteur +1}}
compteur/M  ##  0.6535

## d) histogramme
mt=min(Ts); Mt=max(Ts)
brk=seq(mt, Mt, length=100)
hist(Ts, breaks=brk, col= "green")
abline(v=1.739607, col="red")

## e) compteur/M  ##  0.6535
# mu =  15.583 => H0 est fausse
mean(Population)  ##  15.583
## f)
# le risque de 2eme esp�ce P( conserver H0 � tort ) --> 0.6535


##~~~~~~~~~~~~~~~~~~~~
# Exercice 5
##~~~~~~~~~~~~~~~~~~~~


On s'int�resse aux rendements journaliers de bourse de l'indice Standard and Poors 500 de 1990 � 1999. On prend 2780 valeurs du fichier "SP500" de la librairie "MASS" comme population. On note � et s2 la moyenne th�orique et la variance th�orique du rendement. On souhaite, bas� sur un �chantillon de taille n = 50, tester l'hypoth�se nulle
H0 � = 0.1 contre H1: � < 0.1 au seuil de a = 5%.

#a) 
library(MASS)
# Sauvegarder ces N = 2780 valeurs du rendement du fichier "SP500" dans une 
# variable nomm�e Population.
Population = SP500
(N=length(Population))  #  2780

# b) G�n�rer une r�alisation de l'�chantillon de taille 30 (X1,...,X30), *
# puis tester �'hypoth�se de la normalit� au seuil de 10% par (shapiro.test ).
# Donner la p-valeur du test et conclure.

echant= sample(Population, 30, replace=TRUE)
echant
 [1] -0.04286921 -0.53215085 -0.52024688  1.54064672  0.07183908  0.04248630
 [7]  0.35065568  0.37294573 -0.08653251  0.30327161  1.23183261 -0.12251408
[13]  1.35350772  0.08784873  0.56637497  0.14787999 -0.81520134  0.93021894
[19] -0.72680217  0.33071370  4.08820605 -1.03369472 -0.24956818  1.55280793
[25]  2.86336572  0.63362240  1.68434961 -2.25967799  0.97888159  1.28988820

# H0 : loi normale H1: non normale
shapiro.test(echant)
# W = 0.94436, p-value = 0.1192
# p-valeur > 0,10  
# Colclu : on peut consid�rer echant comme une r�alisation d'un echantillon 
# gaussien


# On suppose dans la suite que la population suit une loi normale N(�,sigma^2) avec les deux param�tres inconnus. On xe la taille de l'�chantillon � n = 28 dans toute la suite.


# c) G�n�rer M = 1000 r�alisations de l'�chantillon (X1,...,Xn). 
# Calculer M = 1000 r�alisations de la statistique T du test en pr�cisant 
# la valeur critique C ainsi que la r�gion de rejet.
# H0 : mu >= 0.1  contre H1 : mu <  0.1   (mu0=0.1)

M=10000; n=28
Ts=c() # Eviter d'utiliser T qui signifie "VRAI" !!!
(Cq=qt(0.05, 27)) # -1.703288
compteur = 0
for (i in 1:M){
   echant= sample(Population, 28, replace=TRUE)
   Ts[i] = sqrt(n)*(mean(echant) -0.1)/sd(echant)
   if ( Ts[i] < Cq) { compteur = compteur +1}
}
compteur/M # � quoi correspond ce pourcentage compteur/M ?
 #  0.1  ;  0.087;  0.0889
# Or l'esp�rance mu =
mean(Population)   0.04575267

# H0 �tant fausse, la prob de rejeter H0 � raison est approxi 0,10
# donc la prob de conserver H0 � tort est approxi 0,90, c�d
# le risque de 2�me esp�ce est approxi 0,90 !!!
  

# d) Tracer l'histogramme de ces M = 1000 r�alisations de la statistique T. Superposer avec la droite verticale x = C o� C est la valeur critique pour d�terminer la zone de rejet :
# abline( v= C, add=T)

hist(Ts, col="green", breaks=100)
abline( v= Cq, col = "red", add=T)
# e) Calculer la vraie valeur de la moyenne th�orique � en utilisant la totalit� des 200 valeurs de la population. D�terminer la proportion des r�alisations du test (parmi M = 1000) qui conduisent � conserver H0.
# f) Interpr�ter les r�sultats de e) en 

## a)
library(MASS)
Population=SP500
(N=length(Population))  ## 2780

## b)  test de la normalit� 
echant=sample(Population, 30, replace=T)
shapiro.test(echant)
# W = 0.95912, p-value = 0.2941 ;  0.7713
# 0.2941 > seuil => On conserve l'hypoth�se de la normalit�.

# H0: mu >= 0.1 contre H1 : mu < 0.1  au seuil alpha =0.05

M=10000; n=28
## c)  r�gion de rejet ] infini, -C[, o� -C = 
qt(0.05, df=n-1)  #   -1.703288

compteur2 = 0 ;  Ts=c()
for (i in 1:M){ 
echant=sample(Population, 28, replace=T)
Ts[i]= sqrt(28)*(mean(echant)-0.1)/sd(echant)
 if (Ts[i] >=  -1.703288) { compteur2 = compteur2 +1}}
compteur2/M  ##    0.9112
#  qui correspond � la proba de conserver H0.

## d) histogramme
mt=min(Ts); Mt=max(Ts)
brk=seq(mt, Mt, length=100)
hist(Ts, breaks=brk, col= "blue")
abline(v=-1.703288, col="red")

## e) 
compteur2/M  ##    0.9112

# mu = 0.04575267 => H0 (mu >= 0.1) est fausse
mean(Population)  ##   0.04575267

## f)
le risque de 2 esp�ce = P( conserver H0 � tort ) -->   0.9112


##~~~~~~~~~~~~~~~~~~~~
# Exercice 6
##~~~~~~~~~~~~~~~~~~~~
##########################################################
##  Etude des Risques de 1�re esp�ce et de 2�me esp�ce  ##
##########################################################

##  1. Test de la comparaison d'une esp�rance
## ~~~~~~~~~~~~~~~~
# Test unilat�ral � droite, n=16
# (X1,...,  Xn) �chantillon de loi N(mu, sigma^2)

## H0: mu <= 10  contre  H1: mu > 10  
## alpha =0.05
n = 16; 
# a)  La zone de rejet est [C, infini[, o� C = 
 qt(0.95, n-1) ## 1.75305 avec  n-1 =15

# b) # mu_0 = 10,  

# c) On utilise T= sqrt(n)*(Xbar-10)/Sc
n = 16; sigma = 5;

## Risques de 1�re esp�ce (On se place au cas o� H0 est vraie)
#~~~~~~~~~~~~~~~~~~~~~~~~
####  1) Loi de T sous H0, mu= 9.7  et son  histogramme

# erreur de 1 esp�ce = rejeter H0 � tort
# rejeter H0 ssi Ts dans la zone de rejet ssi Ts > 1.75305 
M=10000;  n = 16; sigma = 5; mu =  9.9; # < mu0=10
compteur=0
Ts=c()
for (i in 1:M){
   echant=rnorm(n, mu, sigma)
   Ts[i]=sqrt(n)*(mean(echant)-10)/sd(echant)  
   if ( Ts[i] > qt(0.95, n-1) ) #  = 1.75305
      { compteur = compteur+1}  }

##  loi Student non centr�e de delta ##  -0.24
compteur/M  #  0.0277 < alpha = 0.05
## valeur th�orique
round(1- pt(1.75305, ncp= -0.24, df=n-1),5); # 0.03044 < alpha = 0.05

##  histogramme

hist(Ts, proba=TRUE, main="histogramme de T", col="green", breaks=100)
abline(v = qt(0.95, n-1), col="black")
curve(dt(x, ncp= -0.24, df=n-1), col="black", add=TRUE) # avec mu = 9.7 sous H0
curve(dt(x,  df=n-1), col="red", add=TRUE)         ## loi sous H0 avec mu =10

## risque de premi�re esp�ce
compteur/M; 
#    0.0278   ##   0.03022

qt(0.95, df=n-1)
## valeur th�orique
round(1- pt(1.75305, ncp= -0.24, df=n-1),5); 
#  0.03044



## Risques de 2�me esp�ce  (les donn�es sont choisies de sorte  que H0 est fausse !!!)
#~~~~~~~~~~~~~~~~~~~~~~~~
# erreur de 2 esp�ce = conserver H0 � tort
# conserver H0 ssi Ts n'est pas dans la zone de rejet ssi Ts <= 1.75305

M=50000;  n = 16; sigma = 5;  mu1 = 12; mu0=10
compteur2=0
Xbar=c(); 
for (i in 1:M){
   echant=rnorm(n, mean=mu1, sd=sigma)
   Ts[i]=sqrt(n)*(mean(echant)-10)/sd(echant)  
  if ( Ts[i] <=  qt(0.95, n-1)) {compteur2 =compteur2 +1}}
compteur2/M  #  0.54938
round( pt(1.75305, ncp = 1.6, df=n-1), 5); # arrondi � 5 d�cimales
#  0.54674

###  loi Student non centr�e de delta =  1.6

##  histogramme

hist(Ts, proba=TRUE, main="histogramme de T", col="skyblue", breaks=100)
abline(v = qt(0.95, n-1), col="black")
curve(dt(x, ncp = 1.6, df=n-1), col="red", add=TRUE) ## loi sous H1 avec mu =12
curve(dt(x,  df=n-1), col="red", add=TRUE)         ## loi sous H0 avec mu =10

## risque approx. de deuxi�me esp�ce
compteur2/M; 
#   0.54988

## la valeur du risque th�orique de deuxi�me esp�ce
round( pt(1.75305, ncp = 1.6, df=n-1), 5); # arrondi � 5 d�cimales
#  0.54674


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######  Th�oriquement, les trois lois et les risques
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ? n = 16; sigma = 5;  mu1 = 12; mu0=10
curve(dt(x, df=n-1),xlim=c(sqrt(n)*(mu0-15)/sigma,sqrt(n)*(mu1-5)/sigma),  col="red")
abline(v=0,lty=2, col="green")
x=seq(qt(0.95, df=n-1),10,length=100)
y=dt(x, df=n-1)
polygon(c(qt(0.95, df=n-1),x,10),c(0,y,0),col="green")

# 2) # 1) Si, sous H0,  mu0 = 9.7
curve(dt(-x, ncp=sqrt(n)*(10-mu0)/sigma, df=n-1),col="orange",  add=T)
abline(v=-sqrt(n)*(10-mu0)/sigma,lty=2, col="blue")
x=seq(qt(0.95, df=n-1),8,length=100)
y= dt(-x,ncp=sqrt(n)*(10-mu0)/sigma, df=n-1)
polygon(c(qt(0.95, df=n-1),x,8),c(0,y,0),col="blue")


## Sous H1 : mu=12 par exemple

curve(dt(x, ncp=sqrt(n)*(mu1-10)/sigma, df=n-1),col="blue",  add=T)
abline(v=sqrt(n)*(mu1-10)/sigma,lty=1, col="brown")
x=seq(-5,qt(0.95, df=n-1),length=200)
y=dt(x, ncp=sqrt(n)*(mu1-10)/sigma, df=n-1)
polygon(c(-5, x, qt(0.95, df=n-1)),c(0,y,0),density=c(20,15,20),col="brown")
abline(v=qt(0.95, df=n-1), col="black")

legtxt=c(  paste("Sous Ho mu=", mu0),paste("Sous Ho mu=", 10),
paste("Sous H1 mu=", mu1), paste("Valeur critique=", 1.75305) )
legend("topleft",legtxt, lty=c(2,2,1,1), lwd=1, col=c("blue", "green", "brown", "black"),bty="n")


## Risques de 1�re esp�ce et de 2�me esp�ce
(alpha1= round(pt(-qt(0.95, df=n-1), ncp=sqrt(n)*(10-mu0)/sigma, df=n-1),3)); ## 0.03
(beta1=round(pt(qt(0.95, df=n-1), ncp=sqrt(n)*(mu1-10)/sigma, df=n-1),3))  ## 0.547


legtxt=c(  paste("Risque de 1e esp�ce=",alpha1),paste("Risque de 1e esp�ce=", 0.05),
paste("Risque de 2e esp�ce=", beta1) )
legend("topright",legtxt, lty=c(2,2,1), lwd=1, col=c("blue", "green", "brown"),bty="n")


##  Pour tout mu in [5,15]
n = 16; sigma = 5;
risk1=function(x){ round(pt(-qt(0.95, df=n-1), ncp=sqrt(n)*(10-x)/sigma, df=n-1),3)}

risk2=function(x){round(pt(qt(0.95, df=n-1), ncp=sqrt(n)*(x-10)/sigma, df=n-1),3)}
u=seq(5,10,le=100)
v=risk1(u)
plot(u,v,type="l", xlim=c(5,15), ylim=c(0,1),xlab="valeur de mu", ylab="Risques",col="blue")
legtxt=c( paste("Sous H0"),paste("mu <= 10", beta1), 
 paste("Risque de 1�re esp�ce") )
legend("topleft",legtxt, lty=c(1,1,1), lwd=1, col=c("red", "blue", "green"),bty="n")


curve(risk2(x), xlim=c(10,15), col="red", add=T)
abline( v=10, col="blue")
abline(h=0.547, v=12, col="black")
legtxt=c( paste("Sous H1"),paste("mu > 10", beta1), 
 paste("Risque de 2�me esp�ce") )
legend("topright",legtxt, lty=c(1,1,1), lwd=1, col=c("red", "blue", "green"),bty="n")




## Si n varie 

# (X1,...,  Xn) �chantillon de loi N(mu, sigma^2)
## H0: mu<=10 contre H1: mu > 10  

## alpha =0.05

# mu_refer = 10,  On utilise T= sqrt(n)*(Xbar-10)/Sc

 sigma = 5;

# Risque de 1�re esp�ce
risk1=function(x,n){ round(pt(-qt(0.95, df=n-1), ncp=sqrt(n)*(10-x)/sigma, df=n-1),3)}
u=seq(15,100,by=5) 
v=risk1(9.7, u)
plot(u,v,type="p", xlim=c(10,105), ylim=c(0,1),xlab="valeur de mu", ylab="Risques de 2�me esp�ce",col="blue")

##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Risques de 2�me esp�ce   en fonction de n 
##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
u=seq(15,100,by=5) 
risk2=function(x, n){round(pt(qt(0.95, df=n-1), ncp=sqrt(n)*(x-10)/sigma, df=n-1),3)}
v=risk2(12, u)
plot(u,v,type="p", xlim=c(10,105),xlab="valeur de n", ylab="Risques de 2�me esp�ce", ylim=c(0,1),col="red")
legtxt=c( paste("Sous H1"),paste("mu = 12"), 
 paste("Risque de 2�me esp�ce en fct de n") )
legend("topright",legtxt, lty=c(1,1,1), lwd=1, col=c("red", "blue", "red"),bty="n")

##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Risques de 2�me esp�ce   en fonction de sigma 
##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n=20
risk2=function(x, sigma){round(pt(qt(0.95, df=n-1), ncp=sqrt(n)*(x-10)/sigma, df=n-1),3)}
u=seq(0.5,10,le=100) 
v=risk2(12, u)
plot(u,v,type="l", xlim=c(0,6),xlab="Valeur de sigma", ylab="Risques de 2�me esp�ce",  ylim=c(0,0.8),col="red")
legtxt=c( paste("Sous H1"),paste("mu = 12"), 
 paste("Risque de 2�me esp�ce en fct de sigma ") )
legend("topright",legtxt, lty=c(1,1,1), lwd=1, col=c("red", "blue", "red"),bty="n")



#########################################
##~~~~~~~~~~~~~~~~~~~~
# Exercice 9  (Fiche 5)
##~~~~~~~~~~~~~~~~~~~~



##  2. Comparaison d'une variance
## ~~~~

# (X1, Xn) �chantillon de loi N(mu, sigma^2)
## H0: sigma^2 =2^2 contre H1: sigma^2 <> 2^2 (sigma^2 = (1,9) (3,5) ) 
## alpha =0.05
####   les deux risques


## quantile  si n-1 =19
n=20;
qchisq(0.025, df=19) #  8.906516
qchisq(0.975, df=19) ##  32.85233

## Sous Ho 
# 1) Si sigma^2 = 2^2

## Sous H1 : 
sigma12 = 3 ; sigma22 = 6   ## 3,5


####  1) Loi de K2 sous H1 sigma12 = 3 et son  histogramme

M=50000;  # sigma12 = 3
compteur=0
 Sc2=c();K2=c()
for (i in 1:M){
   echant=rnorm(n, mean=10, sd=sqrt(sigma12))
   Sc2[i]=var(echant)
   K2[i]=(n-1)*Sc2[i]/4
   if (( K2[i] >= 8.906516)&( K2[i] <= 32.85233)) {compteur=compteur+1}}

## risque de deuxi�me esp�ce
compteur/M  ##  0.88982
(beta1=round(pchisq(4*32.85233/sigma12 , df=19)-pchisq(4*8.906516/sigma12 , df=19),3)) 
##  0.89


###  loi K2  non calibr�e
## 2) histogramme
(mk=min(K2));(Mk= max(K2))
brk2=seq(mk,Mk, length=100)
hist(K2, proba=T, main="histogramme de K2", col="green", breaks=brk2)
curve(dchisq(4*x/sigma12 , df=19)*4/sigma12,col="red",  add=T)

abline(v=17*sigma12/4, lty=2, col="blue")
x=seq(8.906516,32.85233, length=100)
y= dchisq(4*x/sigma12 , df=19)*4/sigma12
polygon(c(8.906516,x,32.85233),c(0,y,0),density=c(15,20,20),col="blue")


M=50000;  # sigma12 = 6
compteur=0
 Sc2=c();K22=c()
for (i in 1:M){
   echant=rnorm(n, mean=10, sd=sqrt(sigma22))
   Sc2[i]=var(echant)
   K22[i]=(n-1)*Sc2[i]/4
   if (( K22[i] >= 8.906516)&( K22[i] <= 32.85233)) {compteur=compteur+1}}

## risque de deuxi�me esp�ce
compteur/M  ##   0.7097
(beta2=round(pchisq(4*32.85233/sigma22 , df=19)-pchisq(4*8.906516/sigma22 , df=19),3)) 
##  0.709

###  loi K2  non centr�e 
## 2) histogramme

(mk2=min(K22));(Mk2= max(K22))
brk3=seq(mk2,Mk2, length=100)
hist(K22, proba=T, main="histogramme de K2", col="green", breaks=brk3)
curve(dchisq(4*x/sigma22 , df=19)*4/sigma22,col="red",  add=T)


################


curve(dchisq(x, df=19),xlim=c(0,10*sigma22), ylim=c(0,0.11),  col="red")
abline(v=17, lty=2,col="green")
x=seq(0,8.906516,length=200)
y=dchisq(x, df=19)
polygon(c( 0,x,8.906516),c(0,y,0),density=c(15,20,20), col="green")
x=seq(32.85233,60,length=200)
y=dchisq(x, df=19)
polygon(c(32.85233,x,60),c(0,y,0),density=c(20,20,15), col="green")

# 2)sous H1,  Si sigma^2 = sigma12
curve(dchisq(4*x/sigma12 , df=19)*4/sigma12,col="orange",  add=T)
abline(v=17*sigma12/4, lty=2, col="blue")
x=seq(8.906516,32.85233, length=100)
y= dchisq(4*x/sigma12 , df=19)*4/sigma12
polygon(c(8.906516,x,32.85233),c(0,y,0),density=c(15,20,20),col="blue")

## Sous H1, si   sigma^2 = sigma22
curve(dchisq(4*x/sigma22, df=19)*4/sigma22,col="orange",  add=T)
abline(v=17*sigma22/4,lty=2, col="brown")
x=seq(8.906516,32.85233, length=100)
y= dchisq(4*x/sigma22, df=19)*4/sigma22
polygon(c(8.906516,x,32.85233),c(0,y,0),density=c(15,20,20),col="brown")
abline(v=8.906516, col="black")
abline(v=32.85233, col="black")

## Risques de  2�me esp�ce

(beta1=round(pchisq(4*32.85233/sigma12 , df=19)-pchisq(4*8.906516/sigma12 , df=19),3)) 
##  0.89
(beta2=round(pchisq(4*32.85233/sigma22 , df=19)-pchisq(4*8.906516/sigma22 , df=19),3)) 
##  0.709


legtxt=c(  paste("Sous Ho sigma2=", 4), paste("Valeurs critiques=", 8.906516),
paste("Valeurs critiques=", 32.85233), paste("Sous H1 sigma2=", sigma12),paste("Risque  2e esp�ce=", beta1),
paste("Sous H1 sigma2=", sigma22),paste("Risque  2e esp�ce=",beta2) )
legend("topright",legtxt, lty=c(2,2,2,1,1,1,1), lwd=1, col=c("green","black", "black",  "blue","blue",  "brown", "brown"),bty="n")





##~~~~~~~~~~~~~~~~~~~~
# Exercice 6   (Fiche 5)
##~~~~~~~~~~~~~~~~~~~~

  

# On s'int�resse � la taille du lobe frontal ( largeur arri�re ,: longueur de la carapace respectivement).
# des crabes (Leptograpsus variegatus).  On prend 200 valeurs de la varable la taille du lobe frontal "FL" 
# du fichier "crabes"  de la librairie "MASS" comme population (RW et CL respectivement). On note mu la moyenne 
# th�orique de la variable FL. On souhaite tester, bas� sur un �chantillon de 
# taille 18, l'hypoth�se nulle  H0 mu <= 14,5 contre H1: mu >14,5 au seuil de alpha = 5%.
:
## 1) Sauvegarder ces 200 valeurs de "FL" du fichier "crabes" de la library("MASS") dans une variable nomm�e
 Population.


library("MASS") 


help(crabs)
dim(crabs) ##  200   8
head(crabs)

summary(crabs)
N=200; 

crabs$sp
(Pop1.bleu=crabs$FL[crabs$sp=="B"])
(Pop2.oran=crabs$FL[crabs$sp=="O"])

mean(Pop1.bleu); sd(Pop1.bleu); 
##  14.056; [1] 3.01961  ;  

mean(Pop2.oran); sd(Pop2.oran); 
##  17.11 ; ## 3.275575 ;  
n1=28; n2=29;

qt(0.975, n1+n2-2)
##   2.004045

###############################
Population=crabs$FL;

(Pop1.bleu=crabs$FL[crabs$sp=="B"])
(Pop2.oran=crabs$FL[crabs$sp=="O"])

 # crabes oranges
 ## les indices
r1=c(7,13,12,78,57,34,15,73,56,9,23,74,29,74,26,70,82,73,24,68,77,4,87,13,82,68,15,64);
r2=c(50,13,43,68,90,99,76,62,46,65,23,14,30,35,100,87,85,36,10,46,84,85,69,73,84,10,
76,58,95);

## on obtient ainsi les r�alisations des deux �chantillons
(x=Pop1.bleu[r1])
(y=Pop2.oran[r2])

# Les 2 premiers : tests de la normalit� sur deux �chantillons: 

# H0: une loi normale  contre  H1: non normale  au seuil alpha =0.05
shapiro.test(x); shapiro.test(y);
# W = 0.95271, p-value = 0.2315   > seuil
# W = 0.95374, p-value = 0.2286   > seuil

## Les tests de la normalit�s �tant non significatifs, on peut supposer
# qu'il s'agit de N( mu1, sigma1^2) et N( mu2, sigma2^2).

(n1=length(x)); (n2=length(y));  ## 28 29
sd(x);sd(y);
(xbar=mean(x)); (ybar=mean(y)); (var.x=var(x)); (var.y=var(y))
#[1] 13.03929 #[1] 18.23793
#[1] 3.207659 #[1] 8.309581

## Le 3�me : test de l'�galit� de deux variances th�oriques
# au seuil alpha =0,05 (0.10)
# H0 : sigma1^2 = sigma2^2 contre H1 : sigma1^2 <> sigma2^2 

qf(0.025, n1-1, n2-1); qf(0.975, n1-1, n2-1)
##   0.4648612  [1] 2.139732
qf(0.05, n1-1, n2-1); qf(0.95, n1-1, n2-1)
#  0.5270029 ; 1.889424

# zone de rejet [0, 0,46[ et ]2.14,infini[

(f=var(x)/var(y))  ##  0.3860193
# comme f dans la zone de rejet: 
# Cl : il y a une diff significative entre les 2 variances th�oriques

## Maintenant on effectue le test principal :
 qf(0.05, n1-1, n2-1)
## H0: mu1>= mu2 contre H1 : mu1 < mu2  (unilat�ral � gauche)

## statistique de Welch   (car le 3 test est significatif!)

(T=(xbar-ybar)/sqrt(var(x)/n1+var(y)/n2))

## ddl 
(num=(var(x)/n1+ var(y)/n2)^2); (den=(var(x)/n1)^2/(n1-1)+(var(y)/n2)^2/(n2-1))
num/den
## 47.06333

## valeur critique : (test unilat�ral, 1-alpha=0.95)

qt(0.05, 47); -qt(0.95, 47);     ## non   qt(0.975, 47);
##   C= -1.677927        (non  2.011741; )

## zone de rejet ]�infini, -1.68[ et ]1.68, infini[

# la valeur de T vaut -8.2 qui est dans la zone de rejet :
# Conclusion : on rejette H0 =>
# => la taille des crabes "bleu" est plus petite que celle des crabes "orange"!




## t< C =>  on rejette H0:


t.test(x,y, var.equal=FALSE, paired=FALSE)
##         Welch Two Sample t-test
# data:  x and y
# t = -8.2085, df = 47.063, p-value = 1.234e-10
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -6.472679 -3.924611
sample estimates:
mean of x mean of y 
 13.03929  18.23793 



(ybar-xbar)/sqrt(((n1-1)*varx+(n2-1)*vary)/(n1+n2-2))/sqrt(1/n1+1/n2)
##  8.143847



(Indice=sample(1:N, n, replace=T))

Indice=c(175, 30, 105, 163,  70, 184,  34, 154, 196,  26,  47, 185,  46,  105,  64, 
 54, 181, 120,  65, 106,  70,  53, 131,  71,   36,  40, 145,  93,  50, 136,  2, 141,  11,  14, 127)
length(Indice)

x=Population[Indice]
shapiro.test(x)

## W = 0.94895, p-value = 0.1052
mean(Population); sd(Population)
## [1] 15.583;    3.495325


mean(x); sd(x)
## [1] 15.52857  ; [1] 3.680679








## 2) G�n�rer une r�alisationn de l'�chantillon (X1...Xn) n=38 , puis tester l'hypoth�se de la normlit�
au seuil de alpha = 10%.

## On suppose dans la suite que la population admet une loi normale N( mu, sigma^2) avec les 
deux param�tres inconnus.

## 3) G�n�rer 200 r�alisationns de l'�chantillon (X1...Xn) avec n=18.  Appliquer un test appropri� 
en pr�cisant chaque �tape de la proc�dure du test.


## 4) Calculer la moyenne th�orique en utilisant la totalit� de valeurs de la population
D�terminer la proportion des tests (parmi 200) qui conduisent � conserver  H0 � tort. 

## 5)  Interpr�ter les r�sultats de 4) en utilisant les termes sp�cifiques du test.

help(crabs)
 
Morphological Measurements on Leptograpsus Crabs
Description

The crabs data frame has 200 rows and 8 columns, describing 5 morphological measurements on
50 crabs each of two colour forms and both sexes, of the species Leptograpsus variegatus collected 
at Fremantle, W. Australia.

FL: taille du lobe frontal (mm).
RW: largeur arri�re (mm).
CL: longueur de la carapace (mm).
CW: largeur de la carapace (mm).
BD: la hauteur du corps (mm).

dim(crabs);##  200   8

(x=bleu[r1])
(y=oran[r2])

(n1=length(x)); (n2=length(y));  ## 28 29


r=c(sample(1:100, n1, replace=T))
bleu[r]
(x= c(41, 162,  37,  51 ,146,  47,  63, 109,  41 , 99))
(y=x^2)
# 1681 26244  1369  2601 21316  2209  3969 11881  1681  9801
mean(y); var(y)
## 8275.2  ;[1] 81163481

T=0
while (abs(T) <  2.004045)  {
r1=c(sample(1:100, n1, replace=T)); x=bleu[r1];
 r2=c(sample(1:100,n2 , replace=T)); y=oran[r2];(xbar=mean(x)); (ybar=mean(y)); (varx=var(x)); (vary=var(y))
(T=(ybar-xbar)/sqrt(((n1-1)*varx+(n2-1)*vary)/(n1+n2-2))/sqrt(1/n1+1/n2)) }
  r1; r2; x;y;T
y
x=bleu[r1];y=oran[r2]



x=c(11.1,12.6,12.3,13.7,10.1,16.4,12.8,12.8,9.8,11.8,15.0,13.0,15.9,13.0,15.2,12.6,15.0,12.8,15.0,12.0,13.4,9.6,15.2,12.6,15.0,12.0,12.8,11.6)
y=c(23.1,14.1,21.0,16.2,20.1,22.5,18.0,15.6,21.6,15.7,15.7,14.1,17.8,18.6,23.1,19


##~~~~~~~~~~~~~~~~~~~~
# Exercice 9
##~~~~~~~~~~~~~~~~~~~~


#c(paste("p=",p)
# ?hatch
#legtxt=c(expression(paste(italic(mu),"=8.5",)), expression(paste(italic(mu),"=10",)), 
#expression(paste(italic(mu),"=12")), expression(paste(italic(ValeurCritique),"=1.75"))) 
# polygon(c(8,x,12),c(0,y,0),col="gray")

pie(c(1,2,3),density=c(1,2,20))

hist(runif(200),density=c(10,20,30)


###  exemple 2 sapammeur

# Ho : mu >= 2500 contre H1 : mu <2500
xbar=2505 ; sc2=3293
qt(0.95, df=399)# 1.648682
sqrt(400)*(2505-2500)/sqrt(3293)# 1.742626


# H0 : mu <= 2500 contre H1 : mu >2500

qt(0.95, df=399)# 1.648682
sqrt(400)*(2505-2500)/sqrt(3293)# 1.742626  >1.649 


2500+1.65*sqrt(3293)/20  ## 2504.734

