############
# Partie 3 #
############

# q3.2

q3 <- function(n, e) {
  m = 1000
  p = 0.1789
  l = numeric(m)
  for (i in 1:m)
  {
    l[i] = sum(rgeom(n, p)) + n
  }
  c = 0
  for (i in 1:m)
  {
    if ((abs(l[i] / n - 1 / p) >= e))
    {
      c = c + 1
    }
  }
  return (c/m)
}

x = seq(1, 100)
y = x
e = 1.333333
for (i in seq(1, 100))
{
  y[i] = q3(x[i], e)
}
plot(x, y)
title(main = expression(epsilon ~ " = " ~ 1.333333))
# q3.3
simu <- function(n)
{
  m = 1000
  p = 0.1789
  l = numeric(m)
  for (i in 1:m)
  {
    l[i] = mean(rgeom(n, p))+1
  }
  return (l)
}

data = simu(1000)
m<-mean(data)
std<-sqrt(var(data))
hist(data, breaks=20, prob=TRUE, main=expression(n ~ " = " ~ 1000))
curve(dnorm(x, mean=m, sd=std), col="blue", lwd=2, add=TRUE)

