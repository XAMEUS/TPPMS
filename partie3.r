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
    l[i] = sum(rgeom(n, p))
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
    l[i] = mean(rgeom(n, p))
  }
  return (l)
}
res = simu(100)
xx <- seq(1,1000)
hist(res, proba=T)
normali = dnorm(xx)
for(i in 1:length(normali)) {
    normali[i] = normali[i] + 5
}
lines(xx,normali,col="red")
