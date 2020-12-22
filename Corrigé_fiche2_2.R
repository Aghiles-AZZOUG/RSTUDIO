


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





# ~~~~  Exo  9   La fammille des lois gaussiennes, l'estimateur du MV de sigma^2
#________________


# Soit (X1...Xn) un échantillon de loi N(mu, sigma^2) avec sigma^2=4mu^2 
# (curved normal model).

# Comparer 5 estimateurs suivants :

# T1= Xbar ; T2=median; T3= Midrange;
# T4 = S/2C et T5=a* T1+(1-a)*T4
# où S est l'écart-type corrigé, C la constante pour corriger le biais :
C=sqrt(2/n-1)*Gamma(n/2)/Gamma((n-1)/2), a est une constante astucieusement choisi.

# Ce sont tous des estimateurs non biaisés, mais lequel a la variance minimale ? C'est T5 
# et non T1 .

# On peut montrer ceci mathématiquement ou par simulation en R.
# Quand mu=5, n=50, Var(T1)=2 mais a= 0,11 et Var(T5) =0.228 ~~ 2/9.

# n= 50!!!!

# 5)   Comparison de trois estimateurs du paramètre inconnu mu de loi  Norm(mu,4*mu^2)
# avec sigma^2=4*mu^2

(const=sqrt(2/49)*gamma(25)/gamma(24.5));  # 0.9949113
# Pour T3  
100/50 # 2
# Pour T4
(v1=(1/const^2-1)*100/4)  # 0.2563901
(v=(1/const^2-1)/4)  #  0.002563901


(alpha=v/(1/50+v));(1/const^2-1)/4/(1/50+(1/const^2-1)/4) ## 0.1136285
1/9
 

sqrt(2/pi)*(gamma(25)^2)*2^(48)/gamma(49)/sqrt(49); # 0.9949113

sqrt(50^2/100)# 5

alpha  #  0.1136285
# 2 + 3

M=10000;n=50   # n=20;
esti1=c();esti2=c();esti3=c();
esti4=c();esti5=c();esti6=c()

(const=sqrt(2/49)*gamma(25)/gamma(24.5)); # = 0.9949113
 
for (i in 1:M) {
echant=rnorm(n,5,10)  # qui se renouvelle à chaque valeur de i !

esti1[i] =(echant[1]+echant[n])/2
esti2[i] =median(echant)
esti3[i] =mean(echant)  
esti4[i] =sd(echant)/(2*const)   
esti5[i] =alpha*esti3[i] + (1-alpha)*esti4[i]
esti6[i] =0.11*esti3[i] + (1-0.11)*esti4[i] # aproximaton du 5ème
}
sigma^2 = 4*(5^2) = 100
mean(esti1);var(esti1) #   4.921718; 50.79252 # sigma^2/2=50

mean(esti2);var(esti2) #   4.983539; 3.07826

mean(esti3);var(esti3) #   4.97592;  2.038247 # sigma^2/50=2

mean(esti4);var(esti4) #   4.963163;  0.2563559 

mean(esti5);var(esti5) #   5.005269; 0.224599

mean(esti6);var(esti6) #   5.001341; 0.225585


par(mfrow=c(2,1))
hist(esti3);hist(esti4)


# e) On définit T_alpha = alpha*T3 + (1-alpha)*T4. 
# T3 et T4 sont sans biais => T_alpha l'est aussi
# T3 et T4 sont indépendants => 
# var(T_aplha) = var(alpha*T3) +var((1-alpha)*T4)
# = alpha^2*var(T3) + (1-alpha)^2*var(T4)
# = 2*alpha^2 + 0.25*(1-alpha)^2

# Déterminer la constante alpha pour avoir la viriance la plus petite 
# => alpha = 2*0.25/(4+2*0.25) = 0.1111111

1-const^2)*25/const^2  # = 0.2563901

2.038247/0.224599     # =  9.075049
2*0.25/(4+2*0.25)     #  0.1111111
2*0.2564/(4+2*0.2564) #  0.1136323

     # T5 a une variance presque 10 fois plus petite que T3 !!



_


# Exercice  10
##### ~~~~~~~~~~~~~~~~~~~~


#   n=5  B(1,theta)   0.15<=theta <=0.85


# Qn 1 : le 1 terme représente le biais au carré de l'estimateur T,
# et le second la variance de T.

# Qn 2 : Pour T_1, son biais est nul : car
# E(T_1)= E(sum(X_i,i=1..5)*(1/5))=(1/5)* E(sum(X_i,i=1..5))
# =E(X_1)=theta
# Par conséquent, son EQR(T_1) = Var(T_1) et 

# Var(T_1)= Var(sum(X_i,i=1..5)*(1/5))=(1/5)^2* Var(sum(X_i,i=1..5))
# =(1/5)^2* sum(Var(X_i),i=1..5)= Var(X_1)/5= theta*(1-theta)/5.

# Qn 3 : 
(x=seq(0.15,0.85, le=100))
f1=function(x){ x*(1-x)/5}
y=f1(x)
plot(x,y,type="l", col="blue",ylim=c(0.01, 0.085))# l : linéaire

# Qn 4 : Pour T_2 : E(T_2) = 0.7*E(T_1)+0.3*E(1/2) = 0.7*theta+0.3/2
# Son biais B = 0.7*theta+0.3/2 -theta = 0.3*(1/2-theta)
# B^2 = 0.3^2*(1/2-theta)^2 alors que sa variance 
# Var(T_2) = Var(0.7*T_1+0.3*(1/2)) = Var(0.7*T_1)= 0.7^2*Var(T_1)
# = 0.7^2*theta*(1-theta)/5.

# Qn 5 :
f2=function(x){((1-0.7)^2*(x-0.5)^2+0.7^2*x*(1-x)/5)}
y2=f2(x)
lines(x,y2,  col="red", lwd = 2)

# Qn 6 :
f1(0.85)-f2(0.85) #  0.00198

# On note C1= (1-0.7^2)/5, C2 = 0.3^2
# D(theta) = C1* (theta-theta^2) - C2*(theta^2-theta+1/4)
# D'(theta) = (C1+C2)(1-2*theta) 
# D'(theta) positive sur [0.15, 0.5[ puis negative sur ]0.5, 0.85]
# D(theta) croit sur [0.15, 0.5[ puis décroit sur ]0.5, 0.85]
# Comme D(0.15)=D(0.85)>0 => 
# D(theta) >= D(D(0.15) >0 pour tout theta appartenant [0.15,0.85].

 
# Qn 8 : T2 est meilleur puisque EQR(T2) < EQR(T1) sur  [0.15,0.85]




#########################################################################

# Conditions suffisantes :

# 1)  (n-1)/(n+1) < lambda <1 

n=10; (n-1)/(n+1)  #  0.8181818

# 2) theta >  (1-sqrt(C2)/(C1+C2))/2#   (1-(2-sqrt(2))/4

# 2-a) lambda petit
(lambda=0.82)
(C1=(1-lambda^2)/n) ; (C2= (1-lambda)^2)

((1- sqrt( 1- C2/(C1+C2)))/2)  #  0.1454713

# B) lambda grand

(lambda=0.99)
(C1=(1-lambda^2)/n) ; (C2= (1-lambda)^2)

# ii) theta >  (1-sqrt(C2)/(C1+C2))/2#   (1-(2-sqrt(2))/4

((1- sqrt( 1- C2/(C1+C2)))/2)  #   0.01210833



n=5; (n-1)/(n+1)  #  0. 0.6666667

# A) lambda petit
(lambda=0.67)
(C1=(1-lambda^2)/n) ; (C2= (1-lambda)^2)

# ii) theta >  (1-sqrt(C2)/(C1+C2))/2#   (1-(2-sqrt(2))/4

(2-sqrt(2))/4   #  0.1464466
((1- sqrt( 1- C2/(C1+C2)))/2)  #  0.1453833

# B) lambda grand
(lambda=0.99)
(C1=(1-lambda^2)/n) ; (C2= (1-lambda)^2)

# ii) theta >  (1-sqrt(C2)/(C1+C2))/2#   (1-(2-sqrt(2))/4

(2-sqrt(2))/4   #  0.1464466
((1- sqrt( 1- C2/(C1+C2)))/2)  #   0.006165464





s=0.65
f4=function(x){((1-s)^2*(x-0.5)^2+s^2*x*(1-x)/5)}
(y4=f4(x))
lines(x,y4,  col="blue", lwd = 2)#, add="T")




curve(f2(x),  col="blue", lwd = 2)#,add=T)


0.25/5

EQM1= 



#########################################
## Etude de lois


##  Indépendance
cor(Moyenne.echantillon, var.echantillon)
#  0.00417283 ;   -0.005938432

plot(Moyenne.echantillon, var.echantillon, pch=".")



##lois de Xbar

mx=min(Moyenne.echantillon)
Mx=max(Moyenne.echantillon)
brkx=c(seq(mx,Mx,length=100))
hist(Moyenne.echantillon,proba=T, breaks=brkx)
curve(dnorm(x, mean=-10, sd=1.5), col="blue", add=T)
curve(dnorm(x, mean=-10, sd=1.5/sqrt(10)), col="red", add=T)


# Loi de Student 

Stud=sqrt(10)*(Moyenne.echantillon+10)/sqrt(var.echantillon)

ms=min(Stud)
Ms=max(Stud)
brks=c( seq(ms,Ms,length=100))
hist(Stud,proba=T,breaks=brks)
curve(dt(x, df=9), col="red", add=T)



# Loi de Khi-deux 


K2=9*var.echantillon/1.5^2
mk=min(K2)
Mk=max(K2)
brkk=c( seq(mk,Mk,length=100))
hist(K2,proba=T,breaks=brkk)
curve(dchisq(x, df=9), col="red", add=T)




u=seq(-15,-7, length=100)
v=dnorm(u,mean=-10, sd=1.5) 
plot(u,v, type="l")
(x=rnorm(10, mean=640, sd=10))
# (x1=round(x))
x1=c(634,634,628,644,644,633,647,625,643,647)
mean(x1)  #  637.9


N=500
moyenne=rep(NA,N)
for (i in 1:N){
r=sample(10, 2, replace=T)
r2=sample(10, replace=T)
moyenne=mean(c(x1[r[1]],x1[r[2]]))}

mean(moyenne)
#  637.5



## 1) loi binomiale B(5, 0.3)





## 2) Loi hypergéométrique  hyper(m = 10, n = 30, k = 9). 
Il y a dans un étag $N=30$  poissons qui y vivent. 


Dans un bassin où il y a N=30 poissons, dont M=9 sont marqués.  Quelle est
la probabilité quand on en pêche (simultanément) k=10 d'en trouver k=3 qui sont marqués ? 

?dhyper # le nombre total = (m+n) 
 dhyper(0:9, m = 9, n = 21, k = 10)

[1] 1.173958e-02 8.804689e-02 2.438221e-01 3.250962e-01 2.275673e-01
 [6] 8.533775e-02 1.673289e-02 1.593609e-03 6.290561e-05 6.989512e-07



for (k in 0:9){ 
print (  (choose(9, k)*choose(21, (10-k))/choose(30, 10)) )
 }





## 3) loi géométrique G(1/3)
sample(6,  replace = TRUE) 

p=1/3

N=1000
Geo=rep(NA, N)
for (i in 1:N){
    k=0
    r=sample(6,1, replace = TRUE)
    while (r<5)  
    {k=k+1
    r=sample(6,1, replace = TRUE)}    
    Geo[i]= k
}
Geo


table(Geo)

  0   1   2   3   4   5   6   7   8   9  10  11  12  14 
329 223 159  78  73  54  27  17  13  10   6   3   5   3 
lehgth(

(empir= table(Geo)/N)
    0     1     2     3     4     5     6     7     8     9    10    11    12    14 
0.329 0.223 0.159 0.078 0.073 0.054 0.027 0.017 0.013 0.010 0.006 0.003 0.005 0.003

length(empir)


summary(empir)

(x0=as.data.frame(empir))
length(x$Freq)
(y=c(x$Freq[-14], 0, 0.03))
(theo= dgeom(0:14, prob = 0.333333))

 [1] 0.333333000 0.222222111 0.148148148 0.098765481 0.065843687 0.043895813 0.029263890
 [8] 0.019509270 0.013006186 0.008670795 0.005780533 0.003853691 0.002569128 0.001712753
[15] 0.001141836
y

(counts = t(matrix(c(y, theo), ncol=2)))

barplot(counts, main="Fréquence empirique et théorique",
  xlab="valeur de X", col=c("darkblue","red"), 	 beside=T)

x=sample(6,6000, replace = TRUE)
table(x)



rbinom(20, 1, 1/3)
dbinom(4,5,0.3);
dbinom(5,5,0.3);
rnorm(10000, mean=-10, sd=1.5)


# maximum vrasemblance
# Loi Bernoulli

f=function(p){ p^7*(1-p)^13}
(x=seq(0.1,0.9, length=9))
(y=f(x))
round(y*10^7,2)
plot(x,y, type="l")

# Loi uniforme U[0,theta]

N=1000; n=8; lambda=2
Echantillon=matrix(NA, nrow=N,ncol=n) # N réalisations
est=rep(NA,N); # N réalisations
for (j in 1:N) { 
  Echantillon[j,]=runif(n, 0,2)
 est[j]=max(Echantillon[j,]) }
 

mean(est)   #  1.779216

mean(est)*9/8 #  2.001618

N=1000; n=20
x=runif(N,0,2)

?dgamma

# Loi exponentielle


N=1000; n=8; lambda=2
Echantillon=matrix(NA, nrow=N,ncol=n) # N réalisations
est=rep(NA,N); # N réalisations
for (j in 1:N) { 
  Echantillon[j,]=rexp(n, rate=2)
 est[j]=1/mean(Echantillon[j,]) }
 

mean(est)   
mean(est)*7/8


xbar*(n-1)/n  sans bias 

# loi Gamma(3, 1/lambda)
?dgamma

f(x)= 1/(s^a Gamma(a)) x^(a-1) e^-(x/s)

N=1000; n=8; lambda=4
Echantillon=matrix(NA, nrow=N,ncol=n) # N réalisations
 est.1=rep(NA,N);  est.2=rep(NA,N);# N réalisations
for (j in 1:N) { 
  Echantillon[j,]=rgamma(n, shape =3, rate=4)
  est.1[j]=3/mean(Echantillon[j,])
  est.2[j]=sqrt(3)/sd(Echantillon[j,]) }
 

mean(est.1) ; mean(est.1)*23/24  
mean(est.2)*21/24


xbar*(n-1)/n  sans bias 



A un standard,  entend " votre temps d'attente est estimé à 5 m". 
Ce temps d'attente, noté T, est une v.a. qu suit une loi exponentielle
et l'estimation annocée correspond à l'espérence de T. Vous avez déjà attendu d'une
m. Quelle est la prob plus de 10 mi au total ?  