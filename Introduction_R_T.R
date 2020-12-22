#################################################################
############      Introduction au logiciel  R      ##############
#################################################################



#
############################################
###        § 1  Notions de base         ####
############################################

## ~~~~~ 1.1 Généralités


## Ressources


# Ligne de commande
# R > Prompt en attente de commande
# + indique une commande non terminée : exemple


3*4;1+5;5^6 

3*
+(-4)



3*
(-4)

# Pas de " clic-bouton " (on évite l’impression de facilité et donc des bêtises)
# Appel à une fonction avec ses paramètres entre parenthèses
# L’absence de parenthèses provoque l’affichage du code de la fonction
# le caractère ; sépare deux commandes qui seront exécutées séquentiellement
# Le caractère # permet d’insérer un commentaire


## Aide en ligne

help(plot)
  ?plot  # (idem)
  help.search("plot")
  ??plot # (idem)
  help(help.search)
  help(help)


## Espace de travail

## Aides pratiques

# Un éditeur basique pour préparer son script et copier-coller dans 
# la console R

# (1)  Editeur Tinn-R : 

# éditeur gratuit permettant notamment une coloration syntaxique et l’interaction
#  avec la console R

# (2) Rstudio :

# environnement de développement intégré

# (3) Package Rcmdr :  ## R Commander
#  interface graphique avec menus déroulants et zones "script" et "sortie"


## ~~~~~ 1.2  Structures de données


## 1 Scalaire

# entier, réel, logique, chaîne de caractères ;

# affectation <- ou =

x<-2 # (x=2)  il ne faut pas confondre avec x <  -2


x=2; (x < -2)
x+3; x=6
x+3

ls()
rm(x)  # remove x : effacer la valeur de x
# ls() liste les variables de l’environnement de travail
# rm() efface une ou plusieurs variables

x+3  # Erreur : objet 'x' introuvable


  2+2
  exp(10)
  a = log(2.7);a

  (b <- cos(10))
  a+b
  a<b
  2==3  # on s'interroge si 2 est égal à 3
  b = 2 ; b < 3

  ls()
  rm(a)
  a
  a="Bonjour ! Un peu de retenu SVP"
  a
3*
+(-4)


## 2. Vecteur

 d = c(2,3,5,8,4,6)
 (g = d+10);


?vector

  is.vector(list(3,4))

  is.vector(d)

  is.vector(1:10)

  5.2:1.2

  seq(from=1,to=20,by=2.5); seq(1,20,2.5)

  rep("bleu",times=10)

  
  rep(4:6, times=5) ##  4 5 6 4 5 6 4 5 6 4 5 6 4 5 6

  rep(4:6, each=5)  ## 4 4 4 4 4 5 5 5 5 5 6 6 6 6 6

  rep(4:6,c(3,2,1)) ##  4 4 4 5 5 6

rep(LETTERS[c(4,21, 1,19)], times=5)# "D" "U" "A" "S" "D" "U" "A" "S" "D" "U" "A" "S" "D" "U" "A" "S" "D" "U" "A"
[20] "S"
 

  d = c(2,3,5,8,4,6)

  d[2]; #  3 ;
  d[2:5]; #  3 5 8 4

  d[-3]  ## 2 3 8 4 6

  d[-c(1,3)]  ##  3 8 4 6


  (f = c(a=12,b=26,c=32,d=41))

  names(f); #  "a" "b" "c" "d"

  f["a"]  ## donne la valeur de a

  d[2] <- "toto";  ## modifier la 2ème composante
  d;
  d+1

seq(0,1,length=3) #  0.0 0.5 1.0
seq(0,1,length=20)


 (g = seq(0,1,length=20))  ## i/19, i=0..19

 (f = c(a=12,b=26,c=32,d=41))
  f>30; 
#     a     b     c     d 
# FALSE FALSE  TRUE  TRUE 

  f[f>30];

  f[(f>20)&(f<=35)]; 
#  b  c 
# 26 32 


  which(f>30)

  (d[2]="toto")

  f[2] = 22;f+100;

  cos(f);

  length(f); ## 4 : la longueur de f

  d # "2"    "toto" "5"    "8"    "4"    "6" 

  sort(d)     ##  trier dans l'ordre croissant
#  "2"    "4"    "5"    "6"    "8"    "toto"

  rev(sort(d)) ##  l'ordre décroissant
#  "toto" "8"    "6"    "5"    "4"    "2"  



## 3. Matrice

# Produit matriciel %*%

  (A = matrix(1:15,ncol=5)) ## nb de column
  (B = matrix(1:15,nc=5,byrow=T)) # par ligne 

  A*B  ## multip composante par composante
  A%*%B # multip matricielle

  cbind(A,B);  # c : collage par column
  rbind(A,B)   # r : collage par row

 (x=c(1:5))  # vecteur colonne par défaut
 t(x) # son transposé, donc vecteur ligne

  x%*%t(x) # => une matrice
  t(x)%*%x # =sum (x_i^2) = 55

# sélectionner une partie de la matrice

  A[1,3]; # 1ère ligne, 3 ème colonne
  A[ ,2]; # la 2ème colonne
  A[ ,-2];#
 
  A[1:2,c(4,1,2)]


  A+B;
  A*B
  t(B)## B transposée

  A%*%t(B); t(B)%*%A

  cos(A); cos(A[1:2,1:2])

## fonction apply (et tapply)

  apply(A,2,sum) ##   6 15 24 33 42

  B
  apply(B,1,max)  ##  5 10 15
  ?apply

  
 (g = seq(0,1,length=20))  ## i/19, i=0..19
 (C = matrix(g,nrow=4))  ## nb de row
#          [,1]      [,2]      [,3]      [,4]      [,5]
# [1,] 0.00000000 0.2105263 0.4210526 0.6315789 0.8421053
# [2,] 0.05263158 0.2631579 0.4736842 0.6842105 0.8947368
# [3,] 0.10526316 0.3157895 0.5263158 0.7368421 0.9473684
# [4,] 0.15789474 0.3684211 0.5789474 0.7894737 1.0000000



  C[C[,1]>0.1,] 
#           [,1]      [,2]      [,3]      [,4]      [,5]
# [1,] 0.1052632 0.3157895 0.5263158 0.7368421 0.9473684
# [2,] 0.1578947 0.3684211 0.5789474 0.7894737 1.0000000


# Pouquoi ?

# C[,1]>0.1 permet de sélectionner les composante de la 
# 1ère colonne de C qui sont > 0.1

(C[,1]>0.1) # <=>c(F,F,T,T) car seules les deux dernières composantes sont >0.1

C[C[,1]>0.1,] # contient donc seulement les deux dernières lignes :
# C[C[,1]>0.1,]  <=> C[c(3,4),1:5]

(C[C[,1]>0.1,]==C[c(3,4),1:5])


A
#     [,1] [,2] [,3] [,4] [,5]
# [1,] 1     4    7    10   13
# [2,] 2     5    8    11   14
# [3,] 3     6    9    12   15
apply(A,2,sum) 

## Arguments de la fonction apply() :
# A : matrice de travail
# 2 : on s’intéresse aux colonnes (1 pour les lignes)
# sum : fonction à appliquer sur les colonnes de la matrice de travail

# => apply(A,2,sum) : calcule la somme en colonne de la matrice A
 
#
  apply(A,2,sum)
# [1] 6 15 24 33 42



## 4. Liste - list

  (x = list("bidon",1:8))
x[1];  x[2]

  is.vector(x[2])
  a=c(1:8)
  a==x[2]
# Erreur : l'objet (list) ne peut être converti automatiquement en un type 'integer'
  (x[2])[2]  # ???

  x[[2]]+10*3

  y = list(matrice=A,vecteur=f,texte="bidon",scalaire=8)

  names(y);
  y[[1]]
  y$matrice;  ## récupérer la matrice de y
  y$vec
  y[c("texte","scal")]
  y[c("texte","scalaire")]
  length(y)  ## 4
  length(y$vecteur)
  cos(y$scal)+y[[2]][1]

## 5. Data frame

# Structure spéciale pour les jeux de données de type 
# Individus × Variables

# Analogies avec les matrices et les listes pour 
# l’accès aux colonnes (composants)
# Les colonnes peuvent être de natures différentes 
# (variables quantitatives et qualitatives)

  (taille = runif(12,160,180)) # r : simulation
# 163.0188 162.1548 176.5070 176.3021 162.9400 177.1090 160.0455 167.5959 177.7024 176.1494 169.2600 174.9056

  (masse = runif(12,50,90))

   sexe = rep(c("M","F","F","M"),3)

  (H = data.frame(taille,masse,sexe))
     taille    masse sexe
1  163.0188 86.93037    M
2  162.1548 65.87063    F
3  176.5070 71.43076    F
4  176.3021 56.53900    M
5  162.9400 65.40654    M
6  177.1090 86.93708    F
7  160.0455 82.05015    F
8  167.5959 64.29631    M
9  177.7024 81.97110    M
10 176.1494 88.36121    F
11 169.2600 77.53683    F
12 174.9056 80.28695    M

  summary(H)


  H[1,];  ##  1er individu
  H$taille # la variable "taille" de H :12 valeurs
  H$sexe   # la variable "sexe" de H :12 valeurs

## ~~~~~ 1.4 Extensions (packages) 

# Où trouver des extensions (packages) ?

# Rubrique Packages sur cran.r-project.org 
# ou un site mirroir plus proche.
# Consulter la rubrique Task Views pour un regroupement 
# thématique de packages (Finance, Genetics, Medical Imaging...)

data(package="base")
data()

CO2; dim(CO2)
help(CO2)

road # erreur : ce jeu de données n'est pas dans le package de base

# Utiliser un package

install.packages("MASS")      ##     avec les double "  "

library("MASS")  ##     avec ou sans "  "

data(package="MASS")  ## ceci donne la liste des fichiers.data

## Visualiser (afficher) le jeu de données en  tapant

road  ## il s’agit du nom du fichier contenant le jeu de données
        ## road = fichier " Road Accident Deaths in US States"

help(road)

dim(road)

# Installation (en ligne) : Menu Packages (sous Windows),
# choix d’un site miroir puis choix du package
# Installation (en local) : Menu Packages (sous Windows), à
# partir d’un fichier Zip
# Pour gérer les packages en ligne de commande, utiliser
# l’ensemble des fonctions install.packages(),
# update.packages()...
# Chargement soit par menu soit par la fonction library()



############################################
###      § 2 Fonctions graphiques       ####
############################################

## ~~~~~ 2.1  Construction de graphiques
## ~~~~~ 2.2  Sauvegarde et/ou exportation
## ~~~~~ 2.3  Évolutions

  help.search("plot")

x=seq(-10,10, length=500)
y=exp(-x^2)
plot(x, y, type="l", col="blue")


donnes=c(12,10,7,13,26,16,4,12)
pie(donnes);
?pie ; help(pie)

barplot(donnes);
 ?barplot

# Une variable quantitative

# Ex : Tirage aléatoire d’un échantillon de taille 100
# issu d’une loi uniforme sur l’intervalle [0,1]

  runif(20)

  x=runif(1000)
  hist(x);

  ?hist
  boxplot(x); # Boîte à moustaches
  ?boxplot

# Deux variables quantitatives

# Ex :Représentation de la fonction sinus sur
# l’intervalle [-10,10]

  x=seq(-10,10,l=100) # l=length
  plot(x,sin(x),type="l",col="red") # l : linear

  abline(h=0,v=pi/2,lty=2) 
  points(pi/2,1, pch="+",cex=3,col="red") 
  lines(x,cos(x),col="blue") # superposition d'une courbe



## Trois variables

## Ex : Représentation de la fonction sinus 
# cardinal sur [-10,10]^2
  persp # Construction de x, y, et z
  image(x,y,z) 
  persp(x,y,z) 
  contour(x,y,z) 
  filled.contour(x,y,z) ?
  ?image;?contour;?filled.contour

# Créer un graphique :
plot(),image()...
# Ajouter à un graphique existant :
lines(), abline(), points(), text(), rect()...
# Récupérer les coordonnées d’un point en cliquant :
locator(1), text(locator(1),"ici")
# Ouvrir une nouvelle fenètre graphique :
windows(), X11()
# Découper une fenètre graphique :
par(mfrow=c(lig,col)); layout()
par(mfrow=c(2,3))  ## par ligne

 stripchart(x);?stripchart

## ~~~~~ 2.2  Sauvegarde et/ou exportation


## ~~~~~ 2.3  Évolutions

3D (rgl), interactivité (iplots), facilité de création de
graphiques complexes (ggplot2), représentation de
réseaux (igraph) ...


############################################
###      § 3 Un peu de statistique      ####
############################################

## ~~~~~ 3.1  Distribution de probabilité
## ~~~~~ 3.2  Tests statistiques
## ~~~~~ 3.3  Statistique descriptive unidimensionnelle
## ~~~~~ 3.4  Régression
## ~~~~~ 3.5  Statistique descriptive multidimensionnelle

Les distributions courantes sont programmées : Beta,
Binomiale, Cauchy, Normale, Uniforme, Weibull...
Plusieurs fonctions pour chaque distribution. Par exemple,
pour la loi normale :
dnorm() : fonction densité (density)
pnorm() : fonction de répartition (probability)
qnorm() : fonction quantile (quantile)
rnorm() : générateur aléatoire (random)

## Si X suit une loi N(0,1), alors

## f(x=0) 
dnorm(0); 1/sqrt(2*pi)  ##  0.3989423


# P(X<=1.96) =0.9750021
pnorm(1.96)  # 0.9750021

qnorm(0.9750021) # 1.96
rnorm(5, mean=-10, sd=1.5)
##  -11.184939  -9.516371 -10.690544 -10.421780  -8.122653

rbinom(10, 5, 0.3)
#  3 1 1 1 1 2 2 2 2 0


u=seq(-3,3, l=100) ## abscisses
v=dnorm(u)         ## ordonnées
plot(u,v, type="l", col="red", main="Densité N(0,1)")

help.search("Distribution")

## ~~~~~ 3.2  Tests statistiques

# La plupart des tests statistiques courants (et bien d’autres) 
# sont programmés dans R.
# Test de Student pour la comparaison de moyennes.
# Test de Fisher pour la comparaison de variances.
# Test de nullité du coefficient de corrélation.
# Test de Kolmogorov-Smirnov ...

  x=rnorm(100)
  y=rnorm(100,mean=1)
  t.test(x,y)
  var.test(x,y)
  t.test(x,y,var.equal=T)
  cor.test(x,y)
  ks.test(x,y)
  ks.test(x,"pnorm")
  ks.test(y,"pnorm")
  ks.test(y,"pnorm",1)
  help.search("test",package="stats")


## ~~~~~ 3.3  Statistique descriptive unidimensionnelle

# Les fonctions boxplot() et hist() peuvent ne pas produire de
graphique (option plot=FALSE).
# La fonction stem() produit une diagramme stem-and-leaf (tige et
# feuille) qui donne un aperçu de la répartition des données de façon
# plus « rustique » qu’un histogramme 
# La fonction summary() est une fonction générique (comme plot()
# par exemple) qui s’adapte à la classe (fonction class()) de l’objet passé
# en paramètre (vecteur, matrice, data frame, résultat d’une fonction...) ?

  x=runif(100)
  y=runif(100)
  mean(x);var(x);sd(x)  ## version observée
  min(x);max(x)
  quantile(x);
  median(x)
  quantile(x,0.9)  ## 90% en dessous, 10 au dessus

  boxplot(x,plot=FALSE)
  boxplot(x)
  cov(x,y);
  cor(x,y)
  summary(x) 
  stem(x);stem(y)
  hist(x,plot=F)


############################################
###          § 4  Programmation         ####
############################################



## ~~~~~ 4.1  Structures de contrôle
## ~~~~~ 4.2  Fonctions
## ~~~~~ 4.3  Pour aller plus loin

## Répétition

Formes classiques de la répétition :
Nombre de répétitions pré-défini : 

for
## Répétition jusqu’à obtention d’un critère :

while

repeat, break, next
  help("for")
renvoie une aide en ligne
commune pour les
structures de contrôle
(répétition et condition)
  for (i in 1:10) print(i)
  som=0
  for (j in -5:5){
+ som=som+j
+ print(som)}
  for (i in c(2,4,5,8)) print(i)
  i=0
  while (i<10){
+ print(i)
+ i=i+1}

Condition


# Structure classique :
if ... else
# Structure particulière

ifelse(test,oui,non)
 
# Renvoie un objet de la même forme que
test.
  y=z=0;
  for (i in 1:10) {
+ x=runif(1)
+ if (x>0.5) y=y+1
+ else z=z+1 }
  y;z

  x = rnorm(10) 
x
  y = ifelse(x>0, 1, -1) 
y

## ~~~~~ 4.2  Fonctions


## Création de fonctions : 

function(arg1,...){corps}
  f1=function(x){x+2}
  f1(3)

  x = f1(4)
  f2 = function(a,b=0){a+b} ?
  f2(a=2,b=3)
  f2(5)

 calc.rayon=function(r){
    p=2*pi*r;s=pi*r*r;
    list(ray=r,perim=p,surf=s) }


  (resultat=calc.rayon(3))

  resultat$r

  

## ~~~~~ 4.3  Pour aller plus loin 
