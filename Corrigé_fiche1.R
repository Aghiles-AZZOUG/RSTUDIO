#######################################################
##         Eléments de corrigé de la Fiche 1         ##
#######################################################

# ~~ Exercice 1
# a) 
Popul= paste("P", 1:10)
combn(Popul, 3)

# le nb de possibilités de choisir 3 parmis 10
choose(10,3) #  120

# Choisir au hasard <=> 
# tous les triplets dans "Popul" ont la même chance d'être choisi

# b)
K=4;N=10;k=3
choose(K,2)*choose(N-K,1)/choose(N,3) #  0.3
dhyper(2,K,N-K, k) #  0.3

# loi hypergéométrique 
M=500
M.realis = rhyper(M, K, N-K, k)

# c) 
# Fréquences observés
(Freq.obs =table(M.realis)/M)
# Fréquences  théoriques
(Freq.theo = dhyper(0:3,K,N-K,k))

# d) 

Effectifs = matrix(c(table(M.realis), Freq.theo*M),nrow=2,byrow=TRUE)
colnames(Effectifs) = 0:3
row.names(Effectifs) = c("Estimé","Théorique")
barplot(
  Effectifs,beside=TRUE,
  legend.text=rownames(Values)
  ,args.legend = list(x=20,y=500),
  ylim=c(0,300),col=c("darkblue","red")
  )

  
# e)  
# la moyenne théorique mu= m/(m+n)*k
k*K/N ; sum((0:3)*dhyper(0:3,K,N-K,k)) # 1.2

sum(M.realis)/M; mean(M.realis) #  1.212
[1] 1.212


##  ~~ Exercice 2
# a) 
# pour modéliser S :
# i) code fidèle
ifelse( (sample(1:6, 1) >4), 1,0) 

S=0
for (i in 1:5){
   S=S+ifelse( (sample(1:6, 1) >4), 1,0)}
print(S)

# ii) code simplifié : en réalité, S suit une loi binomiale B(5,1/3)
S = rbinom(1, 5, 1/3)

# b)  

M = 1000; n = 5
p = 1/3
M.realis = rbinom(M, n, p)

Eff.obs= table(M.realis)
proba = dbinom(0:5, n, p)
Eff.theo= proba*M


# c)
Effectifs= matrix(c(Eff.obs, Eff.theo),nrow=2,byrow=TRUE)
colnames(Effectifs) = 0:n
row.names(Effectifs) = c("Estimé","Théorique")
barplot(
   Effectifs,beside=TRUE,
  legend.text=rownames(Values)
  ,args.legend = list(x=20,y=500),
  ylim=c(0,400),col=c("darkblue","red")
  )


# d)
5/3
sum((0:5)*dbinom(0:5, n, p)); n*p  # 1.666667

mean(M.realis) #   1.695



# e) 
# Différence : tirage " avec remise" contre " sans remise"

# Si on avait fait un tirage avec remise
# dans l'exercice 1, on aurait aussi eu
# une binomiale, mais B(3, 0.4)

# ~~ Exercice 3
# mot clé : sample
treering[1:20]
?treering
# a) 
Population = treering
length(Population) # 7980
n=65; M=500
echant=matrix(NA, nrow=M, ncol=n)
moy.echant=c()

# b) Simulation
for (i in 1:M){
   echant[i,]=sample(Population, n, replace = T)
   moy.echant[i]=mean(echant[i,])}

moy.echant[50:60]
#  0.9278308 0.9707846 0.9905538 1.0557385 0.9841846 0.9056000 0.9752000 0.9366615 1.0168923 0.9532154
# [11] 0.9631846
hist(moy.echant)

# c) la moyenne d'échantillon -> variable aléatoire
# on a obtenu 500 réalisations de cette moyenne

#  la moyenne théorique = la moyenne de la population =>
sum(Population) /length(Population) # 0.9968362
mean(Population)  # 0.9968362
# la moyenne des 500 réalisations
mean(moy.echant) #  0.9960326


# ~~ Exercice 4
rm(list=ls())
# a) loi géométrique
# p= P("succès") = 1/3, P(Y =k)= (1-p)^(k-1)*p
dgeom(0:19, 1/3)
barplot(dgeom(0:19, 1/3), col="darkblue")

# b) 
M=1000; 
M.realis=rgeom(M,1/3)+1
(Eff=table(M.realis))
#   1   2   3   4   5   6   7   8   9  10  11  12  13  14  16  18  20 
# 297 245 168 114  55  38  29  19  10   5   6   8   2   1   1   1   1 

(m= length(M.realis[M.realis>=15])) # 3
(Eff.empi= c(Eff[1:14], m))
names(Eff.empi)=c(1:14,">=15")
Eff.empi
barplot(Eff.empi)
#
(Eff.theo=c(dgeom(0:13,1/3)*M, (1-pgeom(13,1/3))*M))
barplot(Eff.theo)

# la moyenne théorique 1/p = 1/(1/3)= 3
# car 
sum((0:55 + 1)*dgeom(0:55, 1/3))  # 3

# la moyenne observée basée sur M réalisations
mean(y) #  2.942



# ~~ Exercice 5
rm(list=ls())
M=5000; 
# réalisations de la loi uniforme
u=runif(M)
hist(u, col= "green")

# pour obtenir les réalisations de loi exp lambda=1/2
lambda = 1/2
x = -log(1-u)/lambda  # car p^(-1)(u) = q(u) = -log(1-u)/lambda

m= floor(min(x)); Ma= ceiling(max(x))  # bornes
brk= seq(m, Ma, 0.3) # définir un découpage sur [m,Ma]

hist(x, probability=T, col="darkblue", breaks= brk)
# superposer la densité de la loi exponentielle
s=seq(0,15, length=100);
t=dexp(s, rate=1/2)
lines(s, t, type="l", col="red",lwd=2, add=T)

# c)  P( 8 < X <=12)
# valeur approchée :
length(x[( x>8) & (x <=12)])/M #    0.0162

# d) 
# valeur exacte = F(12)- F(8)
pexp(12 ,rate=1/2) - pexp(8,rate=1/2)  # 0.01583689

# intégrale de la densité

?integrate
f=function(x){ dexp(x,rate=1/2)}
integrate( f, 8, 12)  # 0.01583689 with absolute error < 1.8e-16

# e) 


x=seq(8,12,length=200) ; y=dexp(x,rate=1/2)
plot(x,y,type="l", lwd=2, col="red", ylim=c(0,max(y)))
polygon(c(8,x,12),c(0,y,0),col="gray")


# ~~ Exercice 6
# a )
M=5000
xn=rnorm(M, -10, 1.5)

# b) 
xn[(xn > -12)&(xn <=-10)]

# c)
length(xn[(xn > -12)&(xn <=-10)])/M
#  0.4262
# la proba corespondante 
pnorm(-10, -10, 1.5)- pnorm(-12, -10, 1.5) #  0.4087888
x=seq(-16, -4, length=100)
y =dnorm(x, -10, 1.5)
plot(x, y, type="l", col="red")
u=seq(-12,-10, length=100); v=dnorm(u, -10, 1.5)
polygon(c(-12, u,-10), c(0, v, 0), col="grey")

# ~~ Exercice 7


# Exercice 7

####  Triangle aléatoire
s

## trois points (1,1), (100,1), (50,50*sqrt(3))
P=c(1,50,100)  ## abscisse
Q=c(1,50*sqrt(3),1)  ## ordonnée

N=5000
x=rep(NA,N); y=rep(NA,N); 

q= sample(100, 2, replace=T)  ## choisir le 1 point
x[1]=q[1];y[1]=q[2]
for (i in 1:(N-1)){
   r=sample(3, 1, replace=T)
   x[i+1]=(x[i]+P[r])/2;
   y[i+1]=(y[i]+Q[r])/2;}
plot(x,y, pch=".",cex=1)











