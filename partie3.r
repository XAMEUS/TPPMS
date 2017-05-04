############
# Partie 3 #
############

# q3.2

q3 <- function(n) {
  m = 1000
  p = 0.1789
  l = numeric(m)
  for (i in 1:m)
  {
    l[i] = sum(rgeom(n, p))
  }
  e = 10
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
x = seq(10, 200, 10)
y = x
for (i in seq(1, 20))
{
  y[i] = q3(i)
}
plot(x, y)
