

#####################################################
#######  CC 1          Octobre 2019           #######
#####################################################

#~~          Exercice  1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1)
# la vrasemblance L(p) = (1-p)^(s-n) * p^n
n = 5; s = sum(c(1,1,3,1,2)) # = 8
# L = (1-p)^3*p^5, càd 
L = function(p){ (1-p)^3*p^5}

# 2)
 L(0.5); L(0.3);L(0.1);
#  0.00390625; 0.00083349; 7.29e-06

# 3)
# les deux expressions ne contient pas de paramètre inconnu, donc sont 
# des statistiques. Elles auraient pu etre indép s'il s'agaissait de loi 
# gaussienne. Ce n'est pas le cas, donc elles ne le sont pas.

# Remarque : il ne suffit pas de dire que Sc2 dépend de Xbar !!

##  4) Comparaison de trois estimateurs de theta de Geom(p)


############  si   p = 2/3;  theta = 1/p^2
M = 10000; n = 5; p = 2/3; 
(3/2)^2##  c'est à dire theta = 2.25

esti1=c();esti2=c();esti3=c();
for (i in 1:M) {
echant = rgeom(n,p)+1
esti1[i] = var(echant) + mean(echant)
esti2[i] = ( sum(echant)-1 )^2/n/(n-1)  # sum() = n*mean()
esti3[i] = ( n*mean(echant)^2+ mean(echant) )/(n+1)
}

## Basé sur les réalisations, on évalue les performances des estimateurs
round(mean(esti1), 4) ; round(var(esti1),4)
##  2.2763 ;  1.8593 

round(mean(esti2), 4) ; round(var(esti2),4)
##  2.3377 ;  2.3102

round(mean(esti3), 4) ; round(var(esti3),4)
##  2.28 ;    1.4554

# Remarque : si vous avez obtenu des résultats numériques très diff, 
# alors, il y a sûrement des erreurs quelques parts. 
# Conclusion : Le deuxième est biaisé, les deux atres sont sans biais. 
# Le 3ème est le meilleur car de variance minimale.


############  si   p = 3/4;  theta = 1/p^2
M = 10000; n = 5; p = 3/4; 
(4/3)^2##  theta = 1.777778

esti1=c();esti2=c(); esti3=c()
for (i in 1:M) {echant = rgeom(n,p)+1
esti1[i] = var(echant) + mean(echant)
esti2[i] = ( sum(echant)-1 )^2/n/(n-1)  # sum() = n*mean()
esti3[i] = ( n*mean(echant)^2+ mean(echant) )/(n+1)
}

## Basé sur les réalisations, on évalue les performances des estimateurs
round(mean(esti1), 4) ; round(var(esti1),4)
##  1.7729 ;   0.793


round(mean(esti2), 4) ; round(var(esti2),4)
##  1.7151 ;  0.9575


round(mean(esti3), 4) ; round(var(esti3),4)
## 1.7765 ;   0.6388


# Remarque : si vous avez obtenu des résultats numériques très diff, 
# alors, il y a sûrement des erreurs quelques parts. 

# Conclusion : Le deuxième est biaisé, les deux atres sont sans biais. 
# Le 3ème est le meilleur car de variance minimale.


#~~          Exercice  2  Loi exponetielle (sauf Science Economique)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

N=1000; n= 50; lambda =1/4


Xbar=c(); Sc2=c(); Z=c(); 
N=1000; n=50; lambda = 1/4
for (i in 1:N){
   Echant = - log( 1- runif(n, 0,1))/lambda
   Xbar[i] = mean(Echant)
   Sc2[i] = var(Echant)
   Z[i] = sqrt(n)*(Xbar[i] - 4)/sqrt(16)
}

mean(Xbar); #    4.006301
mean(Sc2);  #   16.01903


# 1-a)
# Si on définit X = -log(1-U)/lambda, alors pour tout x > 0
# {-log(1-U)/lambda <= x } <==> {-log(1-U)  <= lambda *x } car lambda >0
#  <==> {log(1-U) >= - lambda*x }  <==> {(1-U) >= exp(-lambda*x) }
#  <==> {U <= 1-  exp(-lambda*x)}
#  lambda >0 et x > 0 ==> 0< 1- exp(-lambda*x) <1  
# Or P( U < t) = t  si 0 < t <1
# Ainsi P( X <= x) = P( U<= 1- exp(-lambda*x) ) = 1- exp(-lambda*x)
# F(x) =  1- exp(-lambda*x) *1( x > 0)

# 1-b) Sa dérivée par rapport à x =
# lambda* exp(-lambda*x) *1( x > lambda)


# 2-a) 
 N=1000; n = 50; lambda = 1/4
 Echant = -log(1- runif(n, 0,1)) /lambda

# 2-b) + 2-c)

N=1000; n=50; lambda = 1/4
Xbar=c(); Sc2=c(); Z=c(); 
for (i in 1:N){
   Echant = -log(1- runif(n, 0,1)) /lambda
   Xbar[i] = mean(Echant)
   Sc2[i] = var(Echant)
   Z[i] = sqrt(n)*(Xbar[i] - 4)/4
}

# Remarque : il ne faut pas utiliser : rexp(n, rate=1/4) !!!

mean(Xbar);mean(Sc2)
# b-i) 
mean(Xbar); #   3.994777
# si N tend vers l'infini, la moyenne de N réalisations de Xbar tend 
# vers son espérence E(Xbar). Mais Xbar et X_1 ont la même espérance, donc 
# la limite E(Xbar) = E(X_1) = 1/lambda = 4 d'après la loi forte des grands nombres.

# b-ii)
mean(Sc2);  #    16.26985
# si N tend vers l'infini, la moyenne de N réalisations de Sc2 tend 
# vers son espérance E(Sc2). Mais Sc2 est sans biais pour sigma ^2, donc 
# la limite E(Sc2) = var(X_1) = sigma^2 = 1/lambda^2 = 16 d'après la loi forte des grands nombres.

# Remarque : si vous avez obtenu des résultats numériques très diff, 
# alors, il y a sûrement des erreurs quelques parts. 

# b-iii) T contient le paramètre sigma^2, donc ne peut pas être une statistique.
# La loi de T aurait pu etre loi de Khi-deux s'il s'agaissait de loi 
# gaussienne. Ce n'est pas le cas, donc la loi de T est inconnue.

# c-ii) Z[i] = sqrt(n)*(Xbar[i] - 3)/sqrt(3)
# puisque  lambda = 2 implique que  mu = 3*lambda/2 = 3 et sigma^2= 3*lambda^2/4 = 3

# 3-a)   la commande "probability = TRUE" normalise les données, donc 
# hist(Z, probability = TRUE) ou hist(Z/1000, probability = TRUE) sont correctes.

# 3-b) curve( dnorm(x, 0, 1), add = TRUE)

# 4) 
mean(X); #   3.999787


var(X) #   16.1298
#  E(X_1) = mu = 1/lambda = 4; Var(X_1) = 1/lambda^2 = 16
# les résultats sont évidemment cohérents



#~~          Exercice  2  Loi de Pareto  (Science Economique)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1-a)
# Si on définit X = lambda/(1-U)^(1/3), alors 
# {lambda/(1-U)^(1/3) <= x } <==> {lambda^3/(1-U) <= x^3}
#  <==> {(1-U)/ lambda^3 >= 1/x^3}  <==> {(1-U) >= lambda^3/x^3 }
# car lambda^3 >0
#  <==> {U <= 1- lambda^3/x^3 }
# x> lambda >0 ==> 0< 1- lambda^3/x^3 <1
# Or P( U < t) = t  si 0 < t <1
# Ainsi P( X <= x) = P( U<= 1- lambda^3/x^3 ) = 1- lambda^3/x^3 
# F(x) = 1- lambda^3/x^3  *1( x > lambda)

# 1-b) Sa dérivée par rapport à x =
# 3*lambda^3/x^4  *1( x > lambda)


# 2-a) 
 N=1000; n = 50; lambda = 2
 Echant = lambda/(1- runif(n, 0,1)) ^(1/3)

# 2-b) + 2-c)

N=1000; n=50; lambda = 2
Xbar=c(); Sc2=c(); Z=c(); 
for (i in 1:N){
   Echant = lambda/(1- runif(n, 0,1)) ^(1/3)
   Xbar[i] = mean(Echant)
   Sc2[i] = var(Echant)
   Z[i] = sqrt(n)*(Xbar[i] - 3)/sqrt(3)
}

mean(Xbar);var(Xbar)

# b-i) 
mean(Xbar); #    3.009582
# si N tend vers l'infini, la moyenne de N réalisations de Xbar tend 
# vers son espérence E(Xbar). Mais Xbar et X_1 ont la même espérance, donc 
# la limite E(Xbar) = E(X_1) = 3*lambda /2 = 3 d'après la loi forte des grands nombres.

# b-ii)
mean(Sc2);  #    3.072073
# si N tend vers l'infini, la moyenne de N réalisations de Sc2 tend 
# vers son espérance E(Sc2). Mais Sc2 est sans biais pour sigma ^2, donc 
# la limite E(Sc2) = var(X_1) = sigma^2 =  3*lambda^2 /4 = 3 d'après la loi forte des grands nombres.

# Remarque : si vous avez obtenu des résultats numériques très diff, 
# alors, il y a sûrement des erreurs quelques parts. 

# b-iii) T contient le paramètre sigma^2, donc ne peut pas être une statistique.
# La loi de T aurait pu etre loi de Khi-deux s'il s'agaissait de loi 
# gaussienne. Ce n'est pas le cas, donc la loi de T est inconnue.

# c-ii) Z[i] = sqrt(n)*(Xbar[i] - 3)/sqrt(3)
# puisque  lambda = 2 implique que  mu = 3*lambda/2 = 3 et sigma = sqrt(3*lambda^2/4) = sqrt(3)

# 3-a)   la commande "probability = TRUE" normalise les données, donc 
# hist(Z, probability = TRUE) ou hist(Z/1000, probability = TRUE) sont correctes.

# 3-b) curve( dnorm(x, 0, 1), add = TRUE)
# 3-c) omis

# 4) 
mean(Xbar); #  3.009599
var(Xbar) #  0.05890861
# E(Xbar)= E(X_1) = mu = 3; Var(Xbar) = Var(X_1)/n = 3/50= 0.06
# les résultats sont évidemment cohérents







