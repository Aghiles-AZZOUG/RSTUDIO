

#################################################################
#######        Fiche 2 Statistiques et estimations        #######
#################################################################

# ~~~~  Exo  1
#________________



###############################
# 1)  loi normale N(-10,1.5^2)   


N=5000; n=15; 
mu=-10; sigma=1.5

Echant=matrix(NA, nrow=N,ncol=n) # N réalisations  NA : sans valeurs
moy.echan=c(); var.echan=c();# N réalisations

for (j in 1:N) { 
   Echant[j,] = rnorm(n, mean=mu, sd=sigma) #mean : espérance
   moy.echan[j] = mean(Echant[j,])   # mean : Xbar = moyenne d'échantillon
   var.echan[j] =  var(Echant[j,]) } #  var : S_c^2= variance corrigée d'échan


Echant[2019,] ;
moy.echan[2019];var.echan[2019]

# a) 
summary(moy.echan);# càd résumé de Xbar

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-11.616 -10.266 -10.002 -10.003  -9.743  -8.743 

# Théoriquement L(Xbar)=N(mu  , sigma^2/n ), notamment

# E(Xbar) = mu,    Var(Xbar) = sigma^2/n 

round(mean(moy.echan),4); round(var(moy.echan), 4);# = 1.5^2/n=0.15

#   -9.9969 ;   0.1471

# variance et l'écart-type de Xbar
1.5^2/n ; sqrt(1.5^2/n) # 0.15;  
sum(moy.echan)/N; mean(moy.echan)  #  -10.00871


# a) Histogramme de Xbar

hist(moy.echan) # par défaut

mx=min(moy.echan);Mx=max(moy.echan)
brk1=c(seq(mx, Mx, length=100)) # découage en 99 intervalles 

hist(moy.echan, probab=TRUE,main="Histo de Xbar",col="darkblue", breaks=brk1)
curve(dnorm(x, mean=mu, sd=sqrt(sigma^2/n)),col="red",lwd = 2,add=T) # ajouter  
   

summary(var.echan);


# b-i) proportions observées des évènements A, B et C

# P(Xbar  in [-10.5, -9.5] ; sc2 in [2; 4])=

#x[(x>a) & ( x<=b)] 

# b-i) proportion observée de A  :#   0.8056

length(moy.echan[(moy.echan>=-10.5)&(moy.echan<=-9.5)]);#   4028

## proportion 
 4028/N #   0.8056

sum(ifelse((moy.echan>=-10.5)&(moy.echan<=-9.5),1,0))/N  #   0.8056

length(moy.echan[(moy.echan>=-10.5)&(moy.echan<=-9.5)])/N  # 0.8056



# explication :
(x=1:20) #  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
y=ifelse(x>9, 1,0); print(y); #  0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1
sum(ifelse(x>9, 1,0)) # 11
x[x>9]; #  10 11 12 13 14 15 16 17 18 19 20
length(x[x>9]) # 11

# probabilité correspondante en utilisant la loi de Xbar

pnorm(-9.5, mu, sqrt(sigma^2/n))- pnorm(-10.5, mu, sqrt(sigma^2/n))
#  0.8032944

# b-ii) proportion observée de B  :#   0.5374

sum(ifelse((var.echan>=2)&(var.echan<=4),1,0))/N    #  0.5374
length(var.echan[(var.echan>=2)&(var.echan<=4)])/N  #  0.5374




# b-iii) proportion observée de C  :#    0.426

sum(ifelse((moy.echan>=-10.5)&(moy.echan<=-9.5)
&(var.echan>=2)&(var.echan<=4),1,0))/N  #     0.4304


length(var.echan[(moy.echan>=-10.5)&(moy.echan<=-9.5)
&(var.echan>=2)&(var.echan<=4)])/N  #    0.4304


# Xbar et Sc^2 sont-elles indépendantes ? 
0.7976*0.5374 ;   0.4286302
#   0.4304    ;  0.4304



# c)  valeurs théoriques :

# Xbar suit N(-10, 1.5^2/15), 14*Sc2/1.5^2 suit chisq(14)

# Proba(A) = F(-9.5)- F(-10.5)  # fct de répartition de Xbar

pnorm(-9.5, mean=-10, sd= 1.5/sqrt(15))-
pnorm(-10.5,mean=-10, sd= 1.5/sqrt(15))
#  0.8032944


# Proba(B) = F(14*(4)/1.5^2) - F(14*(2)/1.5^2) 

# ou F est la fct de répartition de (n-1)*Sc^2/1.5^2

# 2<= Sc^2<=4  <=>   
14*(2)/1.5^2 <= (n-1)*Sc^2/1.5^2 <= 14*(4)/1.5^2

 pchisq(14*(4)/1.5^2, df=14)-pchisq(14*(2)/1.5^2, df=14)
#  0.5349765

# Prob(C)= Proba(A)*Proba(B)   grâce à l'indépendance
0.8032944*0.5349765  # 0.4297436



# d)  ## Indépendance entre Xbar et Sc2

# cor :  coefficient de corrélation observé,  basé sur N réalisation
# l'indépendance entre X et Y => cor(X, Y)=0. 
# la récirproque est fausse

cor(moy.echan, var.echan)  #  -0.00590949

plot(moy.echan, var.echan, pch=".") # pch : charactère utilisé du nuage des points

# e)

Ts= sqrt(15)*(moy.echan-(-10))/sqrt(var.echan)  ##!!! T=TRUE
mt=min(Ts);Mt=max(Ts)
brk2=c(seq(mt, Mt, length=100)) # découage
hist(Ts, proba=TRUE, main="Histo de Ts", breaks=brk2) # proportion
curve(dt(x, df=14),col="red",lwd = 2,add=TRUE)   # t: loi de Student
curve(dnorm(x),col="green",lwd = 2,add=TRUE) # pour comparer


# f) 
K2=14*var.echan/1.5^2
mk=min(K2); Mk=max(K2)
brk3=c(seq(mk, Mk, length=100)) # découage
hist(K2, proba=TRUE, main="Histo de K2", breaks=brk3)
curve(dchisq(x, df=14),col="red",lwd = 2,add=TRUE)
  # densité de khi-deux à 14 d.d.l 




# ~~~~  Exo  2    loi exponentielle E(1)
#________________
# 


N=5000; n=15; lambda =1
# a)

Echantillon=matrix(NA, nrow=N,ncol=n) # N réalisations
moy.echan=c(); var.echan=c()# N réalisations
for (j in 1:N) { 
  Echantillon[j,]=rexp(n, rate=lambda)
   moy.echan[j]=mean(Echantillon[j,])
   var.echan[j] = var(Echantillon[j,]) }
 
cor(moy.echan, var.echan)  #  0.7123837

summary(moy.echan);
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.3280  0.8131  0.9822  1.0005  1.1627  2.2138 


summary(var.echan);
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.06598 0.51292 0.80127 1.00081 1.27961 6.61508 

# b)
#  P(Xbar  in [0.8, 1.5] ; sc2 in [1; 3])=

length(moy.echan[(moy.echan>=0.8)&(moy.echan<=1.5)])*
length(var.echan[(var.echan>=1)&(var.echan<=3)])/N^2; 

#  0.2621632


length(var.echan[(moy.echan>=0.8)&(moy.echan<=1.5)]);
length(var.echan[(var.echan>=1)&(var.echan<=3)])
# 3986

length(moyenne[(moy.echan>=0.8)&(moy.echane<=1.5)&(var.echan>=1)&(var.echan<=3)])/N
#  0.3254


# valeur théorique 
# n*Xbar suit norm(-10, 1.5^2/15), 14*Sc2/1.5^2 suit chisq(14);



# e)
Ts= sqrt(15)*(moy.echan-(-10))/sqrt(var.echan)  ##!!! T=TRUE
mt=min(Ts);Mt=max(Ts)
brk2=c(seq(mt, Mt, length=100)) # découage
hist(Ts, proba=TRUE, main="Histo de T", breaks=brk2)
curve(dt(x, df=14),col="red",lwd = 2,add=TRUE)   # t: loi de Student
curve(dnorm(x),col="green",lwd = 2,add=TRUE)


# f) 
K2=14*var.echan/1.5^2
mk=min(K2); Mk=max(K2)
brk3=c(seq(mk, Mk, length=100)) # découage
hist(K2, proba=TRUE, main="Histo de T", breaks=brk3)
curve(dchisq(x, df=14),col="red",lwd = 2,add=TRUE)
  # densité de khi-deux à 14 d.d.l 


(pnorm(-9.5,mean=-10, sd= 1.5/sqrt(15))-pnorm(-10.5,mean=-10, sd= 1.5/sqrt(15)))*( pchisq(14*4/1.5^2, df=14)-pchisq(14*2/1.5^2, df=14))
 0.4297436

  
# g) P(X > 1,5+3 | X > 1,5) = P(X> 1,5+3)/P(X> 1,5)
X = rbind(Echantillon[,1:15]); length(X)
length(X[(X> 4.5)])/length(X[(X>1.5)])
#   0.0485873

length(X[(X> 3)])/N/15 #  0.04990667


(1-pexp(4.5, rate=1))/(1-pexp(1.5, rate=1))  #  0.04978707
(1-pexp(3, rate=1))  #  0.04978707


# ~~~~  Exo  3  Loi des grands nombres
#________________


n=5000; theta=1 #  E(X)= theta  =15 ou 1 ou une autre valeur >0
#  x contient une réalisation de 5000 v.a. de loi uniforme U[0,2*theta]

x=runif(n, min=0,max=2*theta)  # ou rbinom(n, 1,1/3) ou d'autres lois
# cumsum(x) définit un vecteur avec les coordonnées 
( x1, (x1+x2), ... (x1+...xn) ...) # où xi=x[i]
# (1:n) définit (1,2,...n)
# cumsum(x)/(1:n)  définit  un vecteur avec les coordonnées 
# (x1, (x1+x2)/2, ... (x1+...xk)/k ...)

plot(cumsum(x)/(1:n),col="blue",cex=0.2) # cex : grosseur 
lines(1:n,rep(theta,n),col="red") # superposer la valeur de E(X)= theta

## loi de Bernoulli
n=5000; p=.6 #  E(X)=p
#  x contient une réalisation de 5000 v.a. de loi Bernoulli B(1,p)
x=rbinom(n, 1,p)  # 
plot(cumsum(x)/(1:n),col="blue",cex=0.2)
lines(1:n,rep(p,n),col="red")

## loi de Poisson

n=5000; lambda=3 #  E(X)=lambda
#  x contient une réalisation de 5000 v.a. de loi de Poisson 
x=rpois(n, lambda)  # de paramètre lambda
plot(cumsum(x)/(1:n),col="blue",cex=0.2)
lines(1:n,rep(lambda,n),col="red")

u=seq(0,1,l=100)
v=(1-u)^2*u^2
plot(u,v, type="l", col="red")



# ~~~~  Exo  4 La fammille des lois de Poisson, Comparaison des estimateurs
#________________

##   P_{lambda}(X=x) = (lambda^x/x!)* exp(-lambda)  où theta = lambda

# a) soit (x1,... xn) une réalisation d'un échnatillon de 
# loi de poisson P(lambda)
# la fct de vraisemblance = produit de probabilité du genre
# lambda^xi/xi! exp(-lambda), i=1...n

# L(lambda) = Produit (lambda^xi/xi!)*exp(-lambda) =...
# l(lambda) = log(L(lambda)) =...

# l'estimateur du Maximum de vraisemblance =...


# b)

M=10000;n=15;lambda=5
esti1=c();esti2=c(); # c(): 1 vecteur vide avec dim à définir
esti3=c();esti4=c()

for (i in 1:M) {
   echant=rpois(n,lambda) # v.a. de Poisson (non sauvegardé)
   esti1[i]=mean(echant)   # xbar  
   esti2[i]= var(echant)   # Sc^2
   esti3[i]=median(echant)
   esti4[i]=0.4*mean(echant)+0.6*var(echant) # combinaison convexe
}

# Basé sur M=3000 réalisations, on évalue les performances des 
# estimateurs

# 1) Sont-ils sans biais ?
mean(esti1);mean(esti2);mean(esti3);mean(esti4);

# 2) Lequel est le plus stable ?
var(esti1);var(esti2);var(esti4)

 5.0008   0.3320508  # Xbar est sans biais de var minimale : le meilleur

mean(esti2);var(esti2)  
#  5.010683 ;  4.074591  
# aussi sans biais, mais de variance très grande

mean(esti3);var(esti3)  
# 4.857 ;  0.6000844

mean(esti4);var(esti4) 
# [1] 5.001788  ; 0.3707718  # aussi sans biais



# ~~~~  Exo  5   La fammille des lois exponenentielles, l'estimateur du MV
#________________


M=5000;n=15;theta=4

esti1=c();esti2=c()
esti3=c();esti4=c()
#echant=matrix(NA, nrow=M, ncol=n)
for (i in 1:M) {
   echant = rexp(n,rate =1/theta ) # loi exp avec 1/theta = 1/4
   esti1[i]= mean(echant)   # xbar  
   esti2[i]= (1+1/n)*sqrt(sum(echant^2)/n/2)  # estim des moments
   esti3[i]= (1+1/n)*sqrt(var(echant))  # l'ecart-type
   esti4[i]= median(echant)/log(2)/(1+ 1/n) 
}

# 1) Sont-ils sans biais ?
mean(esti1);mean(esti2);mean(esti3);mean(esti4);

# 2) Lequel est le plus stable ?
var(esti1);var(esti2);var(esti3); var(esti4)


# Basé sur M=5000 réalisations, on évalue les performances des 
# estimateurs

mean(esti1);var(esti1)
# 4.007881  1.090835
 
mean(esti2);var(esti2)  
#   4.076379 ;[1] 1.309872
# aussi sans biais, mais de variance très grande

mean(esti3);var(esti3)  
##[1] 4.032302 ;[1] 1.925606

mean(esti4);var(esti4) 
# 3.015941 ;[1] 1.565571




# ~~~~  Exo  6
#________________


M=100000;n=15;#  
lambda=3

esti1=numeric(M);esti2=numeric(M);
esti3=numeric(M);esti4=numeric(M)
# ?dexp  # f(x) = lambda {e}^{- lambda  x} où lambda  = rate 

for (i in 1:M) {
  echant=rexp(n,rate=3)
   esti1[i]=1/mean(echant)
   esti2[i]=(n-1)/(n*mean(echant) )
   esti3[i]=4*n/sd(echant)/(4*n+11)
   esti4[i]=log(2)*(n-4)/median(echant)/(n-3) # log(2) =   0.6931472
}

mean(esti1);var(esti1) # ! E(1/Xbar) et 1/E(Xbar) ne sont pas égaux !
#   3.227854 ;  0.8002606   # maxi vraisemblance, mais biaisé

mean(esti2);var(esti2)
#    2.999713;  0.6929787 ;  # sans biais (le meilleur)

mean(esti3);var(esti3)  
#   3.004976;  1.153894     #  biaisé

mean(esti4);var(esti4)   
#  3.014811 ; 1.545834        #  biaisé

3.049633*35/45/2 ; (2*n+5.5)/n/2



# ~~~~  Exo  7  uniforme
#________________



u=seq(0.5,5, length=100)
v=1/u^5
plot(u, v,xlim=c(0,5), type="l")
# Comparison of quatre estimators of theta of U(0,theta)


M=10000; n=10;
esti1=numeric(M);esti2=numeric(M);esti3=numeric(M); esti4=numeric(M)

theta=2
for (i in 1:M) {
  echant=runif(n,min=0,max=theta)
   esti1[i]=max(echant)
   esti2[i]=max(echant)*n/(n+1) 
   esti3[i]=max(echant)*(n+1)/n
   esti4[i]=2*mean(echant)}

mean(esti1);var(esti1)# Max Vraisemblance, mais très biaisé
#  1.813798;  0.02894366

mean(esti2);var(esti2)# très biaisé
#  1.648907;  0.02392038

mean(esti3);var(esti3)# sans biais, le meilleur
#  1.995177;  0.03502183

mean(esti4);var(esti4)# sans biais, mais grande variance
#  1.991138 ; 0.1314043



# ~~~~  Exo  8   La fammille des lois gaussiennes, l'estimateur du MV de sigma^2
#________________


#  1) Comparaison de trois estimateurs de theta de Norm(mu,sigma^2)
#########    theta = sigma^2

M=5000; n=15

esti1=numeric(M);  esti2=numeric(M);
esti3=numeric(M); esti4=numeric(M); 

for (j in 1:M) { 
  echant=rnorm(n, mean=8, sd=2)  
   esti1[j]=var(echant)*(n-1)/n
   esti2[j]=var(echant)
   esti3[j]=sqrt(sum((echant-mean(echant))^4)/3/n)
  # esti4[j]=sqrt(sum((echant-8)^4)/3/n)
}


mean(esti1);var(esti1)  # MV , mais biaisé
mean(esti2);var(esti2)  # sans biais, le meilleur
mean(esti3);var(esti3)  # biaisé
#mean(esti4);var(esti4)  # sans biais ??
# [1] 3.761866 [1] 2.19334


# 2)  Comparaison de trois estimateurs de theta de Norm(mu,sigma^2)
#########    theta = mu^2

n=15;M=3000;#  mu=2, sigma^2=10^2
esti1=numeric(M);esti2=numeric(M);esti3=numeric(M)

for (i in 1:M) {
echant=rnorm(n,2,10)
esti1[i]=mean(echant)^2
esti2[i]=mean(echant)^2 -var(sample)/n
esti3[i]=max( mean(echant)^2 -var(sample)/n, 0)
}

mean(esti1);var(esti1)
#   10.90926
[1] 223.0433


mean(esti2);var(esti2)
#  4.221686
[1] 230.0781

esti2[esti2<0]; length(esti2[esti2<0]) #  1654

mean(esti3);var(esti3)
#   6.903888
[1] 182.6502

