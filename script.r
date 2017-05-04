
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

####################
# Question 2.1
####################
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

#Histogramme à classe de même effectif groupe1
histoeff(groupe1)

# Histogramme à classe de même largeur groupe2
n2 = length(groupe2_ordonne)
k = round(1+log2(n2))
a0 <- min(groupe2_ordonne) - 0.025*(max(groupe2_ordonne)-min(groupe2_ordonne))
ak<-max(groupe2_ordonne)+0.025*(max(groupe2_ordonne)-min(groupe2_ordonne))
bornes <- seq(a0, ak, (ak - a0)/k)
hist(groupe2_ordonne, prob=T, breaks=bornes)

#Histogramme à classe de même effectif groupe2
histoeff(groupe2)
