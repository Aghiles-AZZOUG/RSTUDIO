##############################################################
########        Fiche 5     Tests (suite)
##############################################################



# Exercice 1
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1-a)
n=25
echant=rnorm(n, 19,5)  

# H0 : "normalement distribuée"    contre H1 : au contraire
# au seuil de 10%

res=as.numeric(shapiro.test(echant)[2]) # supprimer l'étiquette
# W = 0.97942, p-value = 0.8734
# Conclusion : p-valeur > 0,10 , on conserve H0.

# 1-b) 

M=10000; n=25
compteur =0
for (i in 1:M){ 
   echant=rnorm(n, 19,5)
   res=as.numeric(shapiro.test(echant)[2])# supprimer l'étiquette
   if ( res >0.10) { compteur = compteur +1}
}
compteur/M  #  0.898 ;  0.9019
# c'est à dire P( conserver la normalté à raison) = 0.90
# risque de 1ère espèce = 1 - P = 0.10

# Si lois exponentielles
# 2-a)
n=25;  echant=rexp(n, rate = 1)
# H0 : "normalement distribuée" contre H1 :

(res=as.numeric(shapiro.test(echant)[2]))
#  p-value =  0.001476319
# Conclusion : p-valeur < 0,10 , on rejette H0.

# 2-b) 
M=10000; n=25
compteur =0
for (i in 1:M){ 
   echant=rexp(n, rate=1)
   res = as.numeric(shapiro.test(echant)[2])# supprimer l'étiquette
   if ( res > 0.10) { compteur = compteur +1}
}
compteur/M  #  0.0364

# c'est à dire le risque de 2ème espèce = P( conserver la normalté à tort)=0.04
curve(dexp(x, rate=1), col="black")

# 3) si lois de Poisson avec petite valeur de lambda

M=10000; n = 25; lambda = 0.5
compteur =0
for (i in 1:M){ 
   echant=rpois(n, lambda)
   res = as.numeric(shapiro.test(echant)[2])# supprimer l'étiquette
   if ( res > 0.10) { compteur = compteur +1}
}
compteur/M  #  0
# c'est à dire P( conserver la normalté à tort) = 0
# donc le risque de 2ème espèce = 0.  Pourquoi ? parce que la différence est énorme 
barplot(dpois(0:25, 0.5), col="blue")

# 4) si lois de Poisson avec  grande valeur de lambda
M=10000; n = 25; lambda = 10
compteur =0
for (i in 1:M){ 
   echant=rpois(n, lambda)
   res=as.numeric(shapiro.test(echant)[2])# supprimer l'étiquette
   if ( res > 0.10) { compteur = compteur +1}
}
compteur/M  #  0.7875

# c'est à dire P( conserver la normalté à tort) = 0.79
# donc le risque de 2ème espèce = 79%
#  Pourquoi ? parce que la différence est faible
barplot(dpois(0:25, 10), col="blue")


# Exercice 2  test sur une proportion
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~  


# Exo 2 Le ministère de la santé souhaite évaluer l'impact de l'augmentation récente des 
prix des cigarettes mise en place en vue de réduire la proportion de fumeurs réguliers 
dans la population $p$. On sait  que la proportion de fumeurs réguliers s'élevait à 
$p_0=0.32$ avant la hausse des prix.  
On prélève (après la hausse des prix) un échantillon de 300 individus   dans lequel 
84 individus fument régulièrement des cigarettes.

# 1) Tester H0 : p <= p0 contre  H1 : p > p0  au seuil alpha=0.05 (p0=0.32)
#  la v.a. S suit ue loi hypergéométrique qui peut etre assimilée à une loi 
# binomiale B(300, p), puis approx N(300*p, 300*p*(1-p)).

# Pour la correction de continuité, on calcul
82/300 - 0.32; 0.5/300 #   -0.04666667  <  -0.001666667
# iii ème )  On utilise Z = sqrt(n)*(Xbar-0.32 + 0.5/300)/sqrt(0.32*0.68) # (p0=0.32)
# iv ) Selon le TCL, (n > 30, n*xbar >5 et n*(1-Xbar) > 5) , la région de rejet :
 [C, infini[ où C = 
qnorm(0.95) #  1.644854 
 (Z = sqrt(n)*(Xbar-0.32+ 0.5/300)/sqrt(0.32*0.68))
# v)
n=300; k=82
(z=sqrt(n)*(82/300-0.32 +1/600)/sqrt(0.32*0.68))
##  -1.670874
 comme z n'apaprtient pas à la region de rejet, on conserve H0. 
## p-valeur =P( Z > -1.67087) = 1 - P( Z <= -1.67087) =
1- pnorm(-1.670874)  ##    0.9526267 >  0.05 => conserver  H0.

z^2 #  2.79182

prop.test(82, 300, p=0.32, alternative ="greater",correct = TRUE)
data:  82 out of 300, null probability 0.32
X-squared = 2.7918, df = 1, p-value = 0.9526
alternative hypothesis: true p is greater than 0.32
95 percent confidence interval:
 0.2316047 1.0000000
sample estimates:
        p 
0.2733333 

# 2)  En coservant H0, seul risque possible, c'est la risque de 2ème espèce,
# c'est la raison pour laquelle on ne peut pas se fier à cette conclusion, on 
# ne peut pas dire que H0 est vraie

# 3) Tester H0 : p >= p0 contre  H1 : p < p0  au seuil alpha=0.05 (p0 = 0.32)

# Pour la correction de continuité
82/300 - 0.32; 0.5/300 #  -0.04 est négative, mais  82/300 - 0.32 + 0.5/300 < 0

# iii ème )  On utilise Z = sqrt(n)*(Xbar-0.32 + 0.5/300)/sqrt(0.32*0.68) # (p0 = 0.32)

# iv ) Selon le TCL, (n > 30, n*xbar >5 et n*(1-Xbar) > 5), la région de rejet :
 ] -infini, -C[ où -C = 
-qnorm(0.95) #  -1.644854 
n=300
(z=sqrt(n)*(82/300-0.32 +1/600)/sqrt(0.32*0.68)) ##   -1.670874
 # v) comme z apaprtient  à la region de rejet, on rejette  H0. 

# p-valeur = P(Z <= -1.67) # (pire dans le sens de H1) =
pnorm(-1.670874)  #  0.04737328

# 4)  En rejetant H0, seul risque possible, c'est la risque de 1ère espèce,

prop.test(82, 300, p=0.32,alternative ="less",correct = TRUE)
         1-sample proportions test with continuity correction
data:  82 out of 300, null probability 0.32
X-squared = 2.7918, df = 1, p-value = 0.04737
alternative hypothesis: true p is less than 0.32
95 percent confidence interval:
 0.0000000 0.3192725
sample estimates:
        p 
0.2733333 


# Exercice 3   test sur une proportion
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Un bureau d'une université declare que 20% d'étudiants  vont au cinéma chaque mois.
Une amicale décide de réaliser une enquête basée sur un échantillon de taille 
n = 150 dont on note S le nombre d'étudiants qui vont au cinéma mensuellement. 
Elle  constate sur un échantillon de taille n =150 , qu'il y 21 personnes
qui vont au cinéma chaque mois. 

1) Quelle est la loi exacte du nombre d'étudiants 
  dans l’échantillon de taille n = 150 qui vont au cinéma mensuellement ?

 Par quelle loi peut-on l’approcher et pourquoi ? En déduire une
approximation de la loi de Xbar = S/n.
# 
# 1) la v.a. S suit une loi hypergéométrique qui peut etre assimilée à une loi 
# binomiale B(150, p), puis loi de S est approx normale N(n*p, n*p*(1-p)).
# loi de Xbar approx N(p, p(1-p)/n)
# i ) Tester H0 : p = 0.20 contre  H1 : p <> 0.20 au seuil alpha=0.10
21/150 - 0.20; 0.5/150

# iii)  On utilise Z = sqrt(n)*(Xbar-0.20 +0.5/n)/sqrt(0.20*0.80)

# iv) région de rejet :
]-infini, -C][  Union  ]C, infini[ où C =

qnorm(0.95) #  1.644854, 
n=150; sqrt(n)*(21/150-0.2 +0.5/n)/sqrt(0.2*0.8) ##   -1.735055

# v) z appartient à la région de rejet (ou p-valeur < 0,10):
# on rejette H0 : p = 0.20

##p-valeur = P( Z < - 1.735055  ou Z >  1.735055)
2*pnorm(-1.735055)  ##  0.08273106  < alpha = 0.10  = > on rejette H0 au seuil de 10%. 


prop.test(21, 150, p=0.20,alternative ="two.sided",correct = TRUE)

data:  21 out of 150, null probability 0.2
X-squared = 3.0104, df = 1, p-value = 0.08273
alternative hypothesis: true p is not equal to 0.2
95 percent confidence interval:
 0.09070718 0.20832313
sample estimates:
   p 
0.14 


###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Exercice 4   comparaison d'une proportion
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Un chercheur pense que la proportion de garçons à la naissance augmente dans des 
conditions économiques difficiles.  
 Pour vérifier cette hypothèse, il a prélevé deux échantillons de la taille n= 5000.  
l'échantillon  parmi les  enregistrements de naissance des bébés nés au cours d'une 
période de récession économique a révélé que {\bf 52,56\%} des nouveau-nés étaient 
des garçons alors l'autre   {\bf 51,46 \%}.


n=5000;  (ns=2*n)#   10000
(s1=0.5256*5000) ##   2628
(s2=0.5146*5000)  ##  2573
(somme=s1+s2) #   5201

(Xbar = 0.5256) ; (Ybar= 0.5146)  
(p.hat=(n*Xbar + n*Ybar)/n/2)  ##  0.5201

# pour défendre l'hypothèse H1, on effectue un test uni à droite
H0:  p1 <= p2  contre  H1 p1 > p2

qnorm(0.90) ##  1.281552  -> 1.282

(numerateur=(Xbar - Ybar))  #  0.011

(denominateur=sqrt( p.hat*(1-p.hat)*(1/n+1/n)))##  0.009991917

(z= numerateur/denominateur)  ##  1.10089

au seuil de 10% 

# région de rejet : [1.282, infini[

# z n'appartient pas à la 
# on ne peut pas rejeter H0: 


# 3)
# conclusion inverse ssi
(Xbar - Ybar)/sqrt( p.hat*(1-p.hat)*(1/n+1/n))  >1.282
(Xbar - Ybar)^2/( p.hat*(1-p.hat)*(2/n))  >1.282^2


###  
n=14594;  (ns=2*n)# 29188
(s1=0.5146*n)  ##   7510.072

(s2=0.5256*n) ##    7670.606

 (somme=s1+s2) #  15180.68


(piChapo1 = s1/n);(piChapo2 = s2/n)  ##   0.5141026 ; 0.5259259

(piChapo=somme/n/2)  ##  0.5201

H0 p1 >= p2  contre  H1 p1 < p2

qnorm(0.97) ##  1.880794



n=5000;  (ns=2*n)#   10000
(s1=0.5256*5000) ##   2628
(s2=0.5146*5000)  ##  2573


 (somme=s1+s2) #   5201

(piChapo1 = s1/n);(piChapo2 = s2/n)  ##    0.5256 ; 0.5146

(piChapo=somme/n/2)  ##  0.5201

H0 p1 >= p2  contre  H1 p1 < p2

qnorm(0.90) ##  1.281552  -> 1.282
qnorm(0.95) 
(numerateur=(piChapo1-piChapo2))  #  0.011



(denominateur=sqrt( piChapo*(1-piChapo)*(1/n+1/n)))##  0.009991917
(denominateur2=sqrt( 0.52*0.48*(1/n+1/n)))
(z= numerateur/denominateur)  ##  1.10089
numerateur/denominateur2  ##  1.100881


(piChapo1-piChapo2)/sqrt( piChapo*(1-piChapo) < sqrt(2/n) * 1.880794
 ##   n>=

au seuil de 10% 
2*1.282^2*0.5201*0.4799/(0.5256-0.5146)^2  #  6780.446

2*1.282^2*0.52*0.48/(piChapo1-piChapo2)^2 ##   6780.446
(piChapo=somme/n/2)  ##  0.5201




# Exercice 5  comparaison de deux proportions
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Dans une étude  sur la qualité de deux publicités, chacune a été diffusée dans une 
zone test spécifique 6 fois en une semaine. La semaine suivante, une enquête 
téléphonique a identifié les personnes qui ont vu les publicités.  On a ensuite  
L'enquête a demandé à ces personnes d'énoncer le slogan de la publicité qu'ils 
avaient vue. Voici les résultats~: 

- Nombre de personnes ayant vu la publicité A : 150; nombre de personnes se 
souvenant du slogan 69. 
- Nombre de personnes ayant vu la publicité B : 200; nombre de personnes 
se souvenant du slogan 70. 

Tester l’hypothèse selon laquelle il n’y a pas d’écart 
entre les proportions de personnes se souvenant du slogan des publicités, 
au seuil de signification 0.05.
## Il s'agit d'un tirage sans remise, mais compte tenu du fait que
## les effectifs n1 et n2 sont très petits par rapport aux nombre de personnes 
## alors la différence entre tirage sans remise ou avec est négligeable.
## H0 : p_1 = p_2 contre H1 : p_1 <> p_2 au seuil de alpha =0,05

n1 = 150;  n2 = 200; n = n1 + n2
somme1 = 69 ; somme2 = 70; somme = somme1 + somme2
(Xbar = somme1/n1);(Ybar = somme2/n2)  ##  0.46;  0.35

(p.hat=somme/n)  ##  0.3971429 : 
# le meilleur estimateur du p = p_1=p_2, les deux échantillons confondus

# On souhaite tester
# H0 : p_1 = p_2 contre H1 : p_1 <> p_2 au seuil de alpha =0,05

## On utilise la statistique  statistique Z 
(numerateur=(Xbar - Ybar))  # 0.11
(denominateur=sqrt( p.hat*(1-p.hat)*(1/n1+1/n2)))##   0.05285109
(z= numerateur/denominateur)  ##    2.081358
(Cq=qnorm(0.975))  ## 1.959964

## Sous H0, Z suit approximativement une loi normale centrée réduite
##  puisque n1 et n2 sont très grands
donc 
(qc=qnorm(0.975));qnorm(0.025)  # 1.959964
Région de rejet = ]-infini, -1,96[  et ] 1,96, infini[
# Comme z appartient  à la region de rejet, on rejette donc H0 :
# il y a une différence significative entre les deux publicités au seuil de 5%
2*pnorm(-2.081358) # p-valeur = 0.03740115 < 0.05 





prop.test(c(69,70),c(150,200),alternative = c("two.sided"))
data:  c(69, 70) out of c(150, 200)
X-squared = 3.8846, df = 1, p-value = 0.04873
alternative hypothesis: two.sided
95 percent confidence interval:
 0.0005755704 0.2194244296
sample estimates:
prop 1 prop 2 
  0.46   0.35 





n1 = 150;  n2 = 200; n = n1+n2
somme1 = 69 ;somme2 = 70; somme = somme1+somme2
(piChapo1 = somme1/n1);(piChapo2 = somme2/n2)  ##  0.46;  0.35
(piChapo=somme/n)  ##  0.3971429

On souhaite tester
H0: pi1=pi2 contre H1: pi1<> pi2  au seuil alpha=5%

## On utilise la statistique  statistique Z 
(numerateur1=(piChapo1-piChapo2))  # 0.11

#(numerateur1=(piChapo1-piChapo2)/(1/n1+1/n2))   ##  6

(denominateur1=sqrt( piChapo*(1-piChapo)*(1/n1+1/n2)))##   0.05285109

#(denominateur2=sqrt( piChapo*(1-piChapo)/(1/n1+1/n2)));   ## la même chose

(z= numerateur1/denominateur1)  ##   2.081319
(Cq=qnorm(0.975))  ## 1.959964


## Sous H0, Z suit approximativement une loi normale centrée réduite
##  puisque n1 et n2 sont très grands
donc 
(qc=qnorm(0.975));qnorm(0.025)  # 1.959964
Région de rejet = ]-infini, -1,96[  U ] 1,96, infini[

# Comme z appartient  à la region de rejet, on rejette donc H0 :
# il y a une différence significative entre les deux publicités au seuil de 5%




###~~~~ Exo 6  comparaison de deux espérances
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(MASS)
?crabs
head(crabs)
  sp sex index   FL  RW   CL   CW  BD
1  B   M     1  8.1 6.7 16.1 19.0 7.0
2  B   M     2  8.8 7.7 18.1 20.8 7.4
3  B   M     3  9.2 7.8 19.0 22.4 7.7
4  B   M     4  9.6 7.9 20.1 23.1 8.2
5  B   M     5  9.8 8.0 20.3 23.0 8.2
6  B   M     6 10.8 9.0 23.0 26.5 9.8

Population1 = crabs$FL[ crabs$sp=="B"]
Population2 = crabs$FL[ crabs$sp=="O"] # O et 0 sont différents
length(Population1); # 100
length(Population2); # 100
n1= 28; n2=29
x=sample(Population1, n1, replace= T)
y=sample(Population2, n2, replace= T)

#premier test:   H0: population normale 
shapiro.test(x)
# W = 0.95587, p-value = 0.2772 
# p-valeur > 0,10 OK
# on peut considérer que Popu1 est normalement distribuée: x ~ N(mu1, sigma1^2)

#deuxième test:  H0: population normale 
shapiro.test(y)
# W = 0.95746, p-value = 0.2843
on peut considérer que Popu2 est normalement distribuée : y ~ N(mu2, sigma2^2)

# troisième test:  H0: sigma1^2 = sigma2^2 contre H1: sigma1^2 <> sigma2^2
# au seuil alpha = 10%
# On utilise F = Scx^2/ Scy^2 qui suit, lors que H0 est vraie, loi de Fisher à
# (n1-1, n2-1) de degrés de liberté
(C1= qf(0.10/2, n1-1, n2-1)) # 0.5270029
(C2= qf(1- 0.10/2, n1-1, n2-1)) #  1.889424
# Région de rejet [0, C1[ union ]C2, infini[
# conclusion du 3ème test préliminaire :
(F= var(x)/var(y)) # les deux variances corrigées d'échantillon)
#  0.8479801
# conclusion du 3ème test préliminaire :  la différence entre les deux variances th 
# n'est pas significative 

#test principal : H0: mu1>=mu2 contre H1: mu1 <mu2  au seuil de 5%
#région de rejet  ] -infini, C[ où C=
qt( 0.05, 28+29-2); - qt(1- 0.05, 28+29-2); #  -1.673034

Ts= (Xbar-Ybar)/sqrt(Sc^2*(1/n1+1/n2))
(num=(mean(x) - mean(y))); # -2.507882
( den= sqrt( (27*var(x)+28*var(y))/(28+29-2)* (1/28+1/29))) #   0.809105
(z=num/den)  #" -3.099575
# comme z appartient à la région de rejet, on rejette H0, 
# on confirme que # la taille des crabes bleus est significativement plus petite que celle des crabes oranges


###~~~~ Exo 7  comparaison de deux espérances
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Une agence environnementale soupçonne que le poisson présent dans un cours d'eau 
# proche d'une usine a une concentration de mercure élevée. Pour confirmer cette 
# suspicion, 37 poissons ont été capturés dans ce cours d'eau et leurs tissus 
# ont été mesurés pour le mercure. Un autre échantillon de 43 poissons d'un autre cours 
# d'eau loin de toute usine ont également été capturés et mesurés. Les concentrations 
# de mercure dans les tissus du poisson en mg/kg sont indiquées ci-dessous 
# (vous pouvez télécharger les données sur le site Moodle "Etude de Cas")~: 

n1=37; n2=43

x=c(0.19,0.63,0.92,0.80,0.47,1.19,0.66,0.88,0.59,0.08,0.98,1.01,0.45,0.93,1.06,
0.79,0.50,0.71,0.89,0.61,0.91,0.76,1.30,0.15,1.02,0.59,0.79,0.90,0.69,0.50,
0.13,0.11,0.80,0.82,1.01,1.09,0.68)

# Test de la normalité du premier échantillon au seuil de 10% : 
H0 : échant est issu d'une population normalement distribuée contre H1
shapiro.test(x)
# W = 0.95125, p-value = 0.1059
# p-valeur > 0,10, on conserve H0.

y=c(0.24,0.65,0.51,0.66,0.48,0.17,0.55,0.31,0.05,1.16,0.98,0.73,0.02,0.45,0.07
,0.34,0.39,0.34,0.54,0.33,1.04,0.09,0.47,0.77,0.47,0.97,0.17,0.10,0.15,0.96
,0.65,0.30,0.23,0.58,0.43,0.51,0.45,0.22,0.42,0.66,0.71,0.28,0.10)

# Test de la normalité du  second échantillon au seuil de 10%  : 
H0 : échant est issu d'une population normalement distribuée contre H1
shapiro.test(y)
# W = 0.95637, p-value = 0.102
# p-valeur > 0,10, on conserve H0.

# On suppose 2 lois normales : N(mu1, sigma_1^2) et N(mu2, sigma_2^2)

# le 3ème test préliminaire au seuil de alpha = 10% : 
# H0 : sigma1^2 = sigma2^2
# La loi de F, lorsque sigma1^2 = sigma2^2, est une loi de Fisher

qf(0.05, n1-1, n2-1); qf(0.95, n1-1, n2-1)
##  0.5820107 ;  1.696465
# zone de rejet [0,0.582 [ et ]1.696,infini[
var(x);var(y) ;mean(x);mean(y)
var(x)/var(y) #   1.137593
# Concl : il n'y a pas de diff significative entre deux variances théoriques

# On souhaite tester H0: mu1<= mu2 contre H1: mu1 > mu2 au seuil de 0.05
# on utilise la statistique de Student 
# Ts= (Xbar-Ybar) / (sigma_hat*sqrt(1/n1+1/n2))
# avec sigma_hat = sqrt( ( (n1-1)*S_cx^2+(n2-1)*S_cy^2 )/(n1+n2-2) )
# qui suit une loi de Student à (n1+n2-2) d.d.l.
qt(0.95, df=n1+n2-2) ##  1.664625

# zone de rejet ]1.64, infini[ 
n1=length(x); n2=length(y)
xbar=mean(x); ybar=mean(y); scx2=var(x); scy2=var(y)
sigma.hat= sqrt( ((n1-1)*scx2+(n2-1)*scy2)/(n1+n2-2) )

0.09457868
[1] 0.08313931
[1] 0.7186486
[1] 0.4581395
37; n2=43

sqrt((35*0.0945 + 41*0.0831)*(1/37+1/43)/78)#  0.06579194

(0.7186-0.4581)/0.0658  #  3.958967

(xbar-ybar)/(sigma.hat*sqrt(1/n1+1/n2))  ## 3.906971
 
## Comme 3.906971  > valeur critique =>
## on rejette H0 : la crainte de l'agence est confirmée au seuil de 0.05 !!
## si on se trompe, la proba de se tromber est majorée par 0.05 !






###~~~~ Exo 8  comparaison de deux espérances
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Pour étudier l'évolution des heures passées à regarder la télévision dans 
#un ménage, un institut de sondage a interrogé 250 ménages,   a obtenu une moyenne 
#de  7,6 heures par jour avec un écart-type 2,6 heures.   Alors que les résultats 
#d'un an auparavant on avait une moyenne de 7,1 heures par jour avec un écart-type de    
#2.15 heures sur 200 ménages. 

#Que peut on conclure au seuil de 5% ?


##Sans données, on peut pas tester la normalité. En vertu de Théoreème Central Limite
## n1 n2 sont grands => on suppose des lois N(mu1, sigma1^2) et N(mu2, sigma2^2)

## Test de la comparaison de deux espérances 
## On souhaite tester H0: mu1 = mu2 contre H1: mu1 <> mu2 au seuil alpha=5%

n1=250;    n2=200
xbar=7.6;  ybar=7.1 ##  
scx=2.6;    scy=2.15 # 
# La statistique utilisée dépend de la conclusion du test suivant :

## On doit d'abord effectuer un TEST PRELIMINAIRE

#  H0 : (sigma1)^2=(sigma2)^2 contre  H1: (sigma1)^2 <> (sigma2)^2 
# au seuil alpha=5%
# On utilise F= scx^2/scy^2   qui suit, sous H0, une loi de Fisher
# à (n1-1, n2-1) d.d.l.

(q1=qf(0.05,n1-1,n2-1)); (q2=qf(0.95,n1-1,n2-1))  ## bilatéral 

##  0.8024572 ;[1] 1.250138

# region de rejet [0; 0,80[ U ]1,25,  infini[

(F=2.6^2/2.15^2)  ##  1.462412
## F= 1.462412 appartient à la région de rejet on rejette H0,
## Cl : il y a une diff significative entre les deux variances théoriques. 

## Par conséquent on utilise la statistique de Welch  
## pour tester H0 : mu1 = mu2 contre H1 : mu1<> mu2

# T = (xbar-ybar)/sqrt( scx^2/n1 + scy^2/n2)  qui suit approximativement, sous Ho, 
# une loi de Student avec les d.d.l. calculés ci-dessous :
##  On détermine les degrés de liberté  

(num=(scx^2/n1 + scy^2/n2)^2); ##    0.002334136
(den=(scx^4/(n1-1)/n1^2 + scy^4/(n2-1)/n2^2));## 5.234802e-06
num/den  ##     447.4976
(dl=round(num /den))  ##  447   (round : arondir)

qt(0.975,dl)  ## attention: non  qt(0.975,dl-1) 
##  1.965285

## région de rejet ]-infini, -1,97[ U ]1,97, infini[

(T = (xbar-ybar)/sqrt( scx^2/n1 + scy^2/n2) )
 ##    2.232666
# Le test étant significatif, on rejette H0 (mu1=mu2): on conclut 
# qu'il y a une diff significative entre les deux espérances.











#########################################
##~~~~~~~~~~~~~~~~~~~~
# Exercice 9  (Fiche 5)
##~~~~~~~~~~~~~~~~~~~~



##  2. Comparaison d'une variance
## ~~~~

# (X1, Xn) échantillon de loi N(mu, sigma^2)
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

## risque de deuxième espèce
compteur/M  ##  0.88982
(beta1=round(pchisq(4*32.85233/sigma12 , df=19)-pchisq(4*8.906516/sigma12 , df=19),3)) 
##  0.89


###  loi K2  non calibrée
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

## risque de deuxième espèce
compteur/M  ##   0.7097
(beta2=round(pchisq(4*32.85233/sigma22 , df=19)-pchisq(4*8.906516/sigma22 , df=19),3)) 
##  0.709

###  loi K2  non centrée 
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

## Risques de  2ème espèce

(beta1=round(pchisq(4*32.85233/sigma12 , df=19)-pchisq(4*8.906516/sigma12 , df=19),3)) 
##  0.89
(beta2=round(pchisq(4*32.85233/sigma22 , df=19)-pchisq(4*8.906516/sigma22 , df=19),3)) 
##  0.709


legtxt=c(  paste("Sous Ho sigma2=", 4), paste("Valeurs critiques=", 8.906516),
paste("Valeurs critiques=", 32.85233), paste("Sous H1 sigma2=", sigma12),paste("Risque  2e espèce=", beta1),
paste("Sous H1 sigma2=", sigma22),paste("Risque  2e espèce=",beta2) )
legend("topright",legtxt, lty=c(2,2,2,1,1,1,1), lwd=1, col=c("green","black", "black",  "blue","blue",  "brown", "brown"),bty="n")




