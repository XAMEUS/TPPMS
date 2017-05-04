
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
plot(k, F(k))mi

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
