
enumerate <- function(X, FUN, ...) {
  result <- vector("list", length(X))
  for (i in seq_along(result)) {
    tmp <- FUN(X[[i]], i, ...)
    if (is.null(tmp))
      result[i] <- list(NULL)
    else
      result[[i]] <- tmp
  }
  result
}

###############


groupe1 = scan("./echantillons/groupe1.txt", what=integer(), sep="\n")
groupe2 = scan("./echantillons/groupe2.txt", what=integer(), sep="\n")

####################
# Partie 2
####################

# Question 2.1

groupe1_ordonne = sort(groupe1)
groupe2_ordonne = sort(groupe2)

# minimum, 1er quartile, médianne, 
#moyenne, 3ème quartile et max de groupe1
summary(groupe1)

# minimum, 1er quartile, médianne, 
#moyenne, 3ème quartile et max de groupe1
summary(groupe2)

# Histogramme à classes de même largeur groupe1
n1 = length(groupe1_ordonne)
k = round(1 + log2(n1))
a0 <- min(groupe1_ordonne) - 0.025*(max(groupe1_ordonne)-min(groupe1_ordonne))
ak<-max(groupe1_ordonne)+0.025*(max(groupe1_ordonne)-min(groupe1_ordonne))
bornes <- seq(a0, ak, (ak - a0)/k)
hist(groupe1_ordonne, prob=T, breaks=bornes)


# Histogramme à classes de même effectif

histoeff <- function(x, xlim=NULL, ...)
{
  sx <- sort(x)
  n <- length(x)
  k <- round(log(n)/log(2)+1)
  rangex <- max(x)-min(x)
  breaks <- c(min(x)-0.025*rangex, quantile(x, seq(1, k-1)/k), max(x)+0.025*rangex)
  col <- 0
  if (is.null(xlim)) xlim<-c(breaks[1], breaks[k+1])
  hist(x, breaks=breaks, col=col, xlim=xlim, probability=T, ...)
}


# Histogramme à classe de même largeur groupe2
n2 = length(groupe2_ordonne)
k = round(1+log2(n2))
a0 <- min(groupe2_ordonne) - 0.025*(max(groupe2_ordonne)-min(groupe2_ordonne))
ak<-max(groupe2_ordonne)+0.025*(max(groupe2_ordonne)-min(groupe2_ordonne))
bornes <- seq(a0, ak, (ak - a0)/k)
hist(groupe2_ordonne, prob=T, breaks=bornes)



# Question 2.2



r = 1
p = 0.5 #Â TODO
q = 1 - p
F<-function(k)
{
  # return (p / q * (1 - q ** (k + 1)) / (1 - q))
  return ((1 - (1 - p) ** (k + 1)) / (1 - p))
}

n = 10
m = 5
k = seq(1,5,1)
plot(k, F(k))

lines(k, F(k), type="s")

h<-function(x)
{
  return (log(1 - x))
}

x = sort(groupe1)
y = x
for (i in 1:length(x))
{
  y[i] = h(i / length(x))
}
plot(x, y)
x = head(x, -1)
y = head(y, -1)
reg<-lm(y~x)
lines(x, fitted.values(reg))

# approximation de p 
coeff = coef(reg)
p = 1 - exp(coeff[2])

# calcul de E[X] pour le groupe 1
E <- 1/p

# calcul de l'intervalle de confiance au seuil de 5% d'après la 
# formule de la question 1.2 pour le groupe 1
alpha<- 0.05
xn = mean(groupe1)
n = length(groupe1)
u_alpha = qnorm(1-alpha/2)
borne_inf = 1/xn - u_alpha*sqrt((xn-1)/(n*xn**3))
borne_sup = 1/xn + u_alpha*sqrt((xn -1)/(n*xn**3))

# calcul de l'intervalle de confiance au seuil de 5% d'après la 
# formule de la question 1.2 pour le groupe 2
alpha<- 0.05
xn = mean(groupe2)
n = length(groupe2)
u_alpha = qnorm(1-alpha/2)
borne_inf = 5/xn - u_alpha*sqrt((xn-1)/(n*xn**3))
borne_sup = 5/xn + u_alpha*sqrt((xn -1)/(n*xn**3))


# groupe 2

# Fonction permettant de calculer la probabilité empirique 
# Pour le groupe 2

probabilite_empirique <- function(groupe){
  
  n <- max(groupe)
  liste <-c()
  for (i in 1:n){
    liste[i] = sum(groupe == i)
  }
  return(liste/length(groupe))
}

probabilite_empirique(groupe1)

graphe_proba_empirique <- function(groupe){
  proba_empirique <- probabilite_empirique(groupe)
  x <- c()
  y <- c()
  for (i in 1: length(proba_empirique)-1){
    if (proba_empirique[i+1] != 0 && proba_empirique[i] != 0){
      x = append(x, i)
      y = append(y, i*proba_empirique[i]/proba_empirique[i+1])
    }
  }
  plot(x, y)
  reg<-lm(y~x)
  lines(x, fitted.values(reg))
  return(coef(reg))
  
}




# Fonction permettant de calculer un p à partir du maximum de vraisemblance et 
# d'un r donné

calcul_maximum_vraisemblance_pour_p <- function(r, data){
  return(r/mean(data))
}

# Fonction permettant de calculer la vraisemblance pour un r et un p donnés
calcul_vraisemblance <- function(p, r, data){
  vraisemblance <- 1
  for (i in 1:length(data)){
    vraisemblance <- vraisemblance*(choose(data[i]-1, r-1)*(1-p)**(data[i]-r)*p**r)
  }
  return(vraisemblance)
}

# Fonction permettant de déterminer r
# pour le groupe 2

estimer_p_r<-function(data)
{
  meilleur_r = 1
  p_associe = calcul_maximum_vraisemblance_pour_p(meilleur_r, data)
  vraisemblance = calcul_vraisemblance(p_associe, meilleur_r, data)
  for (i in 1:min(data)) {
    nouveau_p = calcul_maximum_vraisemblance_pour_p(i, data)
    n_vraisemblance = calcul_vraisemblance(nouveau_p, i, data)
    if(vraisemblance < n_vraisemblance) {
      meilleur_r = i
      p_associe = nouveau_p
      vraisemblance = n_vraisemblance
      print(i)
      print(n_vraisemblance)
    }
  }
  return (c(meilleur_r, p_associe))
}

estimer_p_r(groupe1)

# calcul de p pour le groupe 2
# suivant la loi binomiale négative

p <- 1 - 1/graphe_proba_empirique(groupe2)[2]

# Calcul de E[X] pour le groupe 2
E <- 5/p

# Probabilité que le nombre de fixations soit
# supérieur à 10 pour groupe 1
p_10 <- length(groupe1[groupe1>=10])/length(groupe1)


# Probabilité que le nombre de fixations soit
# supérieur à 10 pour groupe 2
p_10 <- length(groupe2[groupe2>=10])/length(groupe2)


############
# Partie 3 #
############


# Fonction permettant de calculer l'intervalle de confiance de seuil alpha
# pour une loi géométrique sur un échantillon de taille n
intervalle_confiance <- function(alpha, n, echantillon){
  xn = mean(echantillon)
  u_alpha = qnorm(1-alpha/2)
  if (xn >= 1){
    borne_inf = 1/xn - u_alpha*sqrt((xn-1)/(n*xn**3))
    borne_sup = 1/xn + u_alpha*sqrt((xn-1)/(n*xn**3))
    intervalle <- c(borne_inf, borne_sup)
    return(intervalle)
  }
  return(-1)
}

echantillon <- rgeom(10, 0.15)
intervalle <- intervalle_confiance(0.05, 10, echantillon )


# Question 3.1
# On simule une 100 échantillons de taille 200 pour la loi géometrique 
# de paramètre p = 0.15
simulation_loi_geometrique <- function(n, m, p, alpha){
  compteur <- 0
  for (i in 1:m){
    echantillon <- rgeom(n,p)+1
    intervalle <- intervalle_confiance(alpha, n, echantillon)
    if (intervalle[1] <= p && p <= intervalle[2]){
      compteur <- compteur + 1
    }
  }
  return(compteur/m)
}

compteur <- simulation_loi_geometrique(100000, 100, 0.1, 0.05)
print(compteur)

# Simulation à n variable
graphe_simulation_n <- function(m, p, alpha){
  x <- c()
  y <- c()
  for (i in 10:3000) {
    x<- append(x, i)
    y<- append(y, simulation_loi_geometrique(i, m, p, alpha))
  }
  plot(x, y, xlab = "nombre d'échantillons", ylab="couverture de p", main = "Couverture de p en fonction du nombre d'échantillons")
}

# Simulation à p variable
graphe_simulation_p <- function(n, m, alpha){
  x <- seq(0.01, 0.5, 0.01)
  y <- c()
  for (i in 1: length(x)){
    y <- append(y, simulation_loi_geometrique(n, m, x[i], alpha))
  }
  plot(x, y, xlab="probabilité", ylim=c(0, 1), ylab="couverture de p", main ="Couverture de p en fonction de la probabilité")
}

#simulation a m variable
graphe_simulation_m<-function(n,p,alpha) {
  x<-c()
  y<-c()
  for (i in 1:1000) {
    x <- append(x, i)
    y <- append(y, simulation_loi_geometrique(n, i, p, alpha))
  }
  plot(x, y, xlab ="nombre de répétitions", ylab="couverture de p", main = "Couverture de p en fonction du nombre de répétitions")
}

#simulation à alpha variable
graphe_simulation_alpha <- function(n, m, p){
  x <- seq(0.01, 0.3, 0.01)
  y <- c()
  for (i in 1: length(x)){
    y <- append(y, simulation_loi_geometrique(n, m, p, x[i]))
  }
  plot(x, y, xlab="probabilité", ylab="couverture de p", main ="Couverture de p en fonction de alpha")
}

#graphe 1
graphe_simulation_n(1000, 0.15, 0.05)

#graphe 2
graphe_simulation_p(1000, 1000, 0.05)

#graphe 3 
graphe_simulation_m(1000, 0.15, 0.05)

#graphe 4
graphe_simulation_alpha(10000, 1000, 0.15)

simu = simulation_loi_geometrique(100, 100, 0.15, 0.05) 
print(simu)

#m = 1000000
#n = 1000000
#p = 0.1
#l = numeric(m)
#for (i in 1:m)
#{
#  l[i] = sum(rgeom(n, p))
#}
#hist(l)



#Question 3.4
question3_4<-function(r,p,n,m){
  tab_estim_p<-c()
  tab_estim_r<-c()
  
  differentiel_moyenne_p<-c()
  differentiel_moyenne_r<-c()
  
  quadra_p<-c()
  quadra_r<-c()
  
  for (i in (1:m)){
    simulation<-rnbinom(n,r,p)
    moyenne<-mean(simulation)
    variance<-var(simulation)
    
    tab_estim_r[i]<-moyenne**2/(moyenne+variance)
    tab_estim_p[i]<-moyenne/(moyenne+variance)
    
    differentiel_moyenne_p[i]<-tab_estim_p[i]-p
    differentiel_moyenne_r[i]<-tab_estim_r[i]-r
    
    quadra_p[i]<-differentiel_moyenne_p[i]**2
    quadra_r[i]<-differentiel_moyenne_r[i]**2
  }
  biais_p<-mean(differentiel_moyenne_p)
  biais_r<-mean(differentiel_moyenne_r)
  
  err_quadra_p<-mean(quadra_p)
  err_quadra_r<-mean(quadra_r)
  return(c(biais_p,biais_r,err_quadra_p,err_quadra_r))
}

tableau_p<-c()
tableau_r<-c()
tableau_errp<-c()
tableau_errr<- c()
#attach(mtcars)
#par(mfrow=c(4,1))
tab_indice<-seq(1,100,1)
tab_indice<-append(tab_indice,seq(100,200,2))

for (i in tab_indice){
  calcul <- question3_4(2,0.3,i,100)
  tableau_p<-append(tableau_p,calcul[1])
  tableau_r<-append(tableau_r,calcul[2])
  tableau_errp<- append(tableau_errp,calcul[3])
  tableau_errr<- append(tableau_errr,calcul[4])
}

plot(tab_indice, tableau_p, main = "biais de p",xlab="",ylab = "")
plot(tab_indice, tableau_r, main = "biais de r",xlab="",ylab = "")
plot(tab_indice, tableau_errp, main = "erreure quadratique de p",xlab="",ylab = "")
plot(tab_indice, tableau_errr, main = "erreure quadratique de r",xlab="",ylab = "")

