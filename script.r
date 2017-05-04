
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




####################
# Partie 2
####################

# Question 2.1

groupe1_ordonne = sort(groupe1)
groupe2_ordonne = sort(groupe2)

# minimum, 1er quartile, m�dianne, 
#moyenne, 3�me quartile et max de groupe1
summary(groupe1)

# minimum, 1er quartile, m�dianne, 
#moyenne, 3�me quartile et max de groupe1
summary(groupe2)

# Histogramme � classes de m�me largeur groupe1
n1 = length(groupe1_ordonne)
k = round(1 + log2(n1))
a0 <- min(groupe1_ordonne) - 0.025*(max(groupe1_ordonne)-min(groupe1_ordonne))
ak<-max(groupe1_ordonne)+0.025*(max(groupe1_ordonne)-min(groupe1_ordonne))
bornes <- seq(a0, ak, (ak - a0)/k)
hist(groupe1_ordonne, prob=T, breaks=bornes)


# Histogramme � classes de m�me effectif

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

#Histogramme � classe de m�me effectif groupe1
histoeff(groupe1)

# Histogramme � classe de m�me largeur groupe2
n2 = length(groupe2_ordonne)
k = round(1+log2(n2))
a0 <- min(groupe2_ordonne) - 0.025*(max(groupe2_ordonne)-min(groupe2_ordonne))
ak<-max(groupe2_ordonne)+0.025*(max(groupe2_ordonne)-min(groupe2_ordonne))
bornes <- seq(a0, ak, (ak - a0)/k)
hist(groupe2_ordonne, prob=T, breaks=bornes)

#Histogramme � classe de m�me effectif groupe2
histoeff(groupe2)

# Question 2.2

groupe1 = scan("./echantillons/groupe1.txt", what=integer(), sep="\n")
groupe2 = scan("./echantillons/groupe2.txt", what=integer(), sep="\n")

r = 1
p = 0.5 # TODO
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

# calcul de l'intervalle de confiance au seuil de 5% d'apr�s la 
# formule de la question 1.2
alpha<- 0.05
xn = mean(groupe1)
n = length(groupe1)
u_alpha = qnorm(1-alpha/2)
borne_inf = 1/xn - u_alpha*sqrt((xn-1)/(n*xn**3))
borne_sup = 1/xn + u_alpha*sqrt((xn -1)/(n*xn**3))

############
# Partie 3 #
############

m = 1000000
n = 1000000
p = 0.1
l = numeric(m)
for (i in 1:m)
{
  l[i] = sum(rgeom(n, p))
}
hist(l)

