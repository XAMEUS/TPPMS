n = 1000
r = 1
p = 0.1789
echantillon = sort(rnbinom(n,r,p))
for (i in 1:n) {
    echantillon[i] = echantillon[i] + r
}

#Estimons p (1.1)
print("Estimation de p selon la méthode des moments, et de maximum de vraisemblance")
print(r / mean(echantillon))

# Question 1.3
n = 1000
p = 0.1789
x = rgeom(n, p)
for(i in 1:n) {
    x[i] = x[i] + 1
}
x = sort(x)
y = x
for (i in 1:length(x)-1) {
    y[i] = log(1 - i/n)
}
x = head(x, -1)
y = head(y, -1)
plot(x, y)
reg<-lm(y~x)
lines(x, fitted.values(reg))
print(reg)
print(1 - exp(reg$coefficients[2]))


#Estimons p (1.4)
print("Estimation de p graphique")
h<-function(x)
{
  return (log(1 - x))
}

y = echantillon
for (i in 1:n)
{
  y[i] = h(i / n)
}
plot(echantillon, y)
x = head(echantillon, -1)
y = head(y, -1)
reg<-lm(y~x)
lines(x, fitted.values(reg))
print(reg)
print(1 - exp(reg$coefficients[2]))

# Second cas

n = 100
r = 20
p = 0.1
simul_second <-function(n, r, p) {
    echantillon = sort(rnbinom(n,r,p))
    for (i in 1:n) {
        echantillon[i] = echantillon[i] + r
    }
    liste_x = c()
    liste_y = c()

    for (i in 1:length(echantillon)-1) {
        if (length(echantillon[echantillon==echantillon[i]+1])) {
            liste_x = c(liste_x, echantillon[i])
            liste_y = c(liste_y, c(echantillon[i] * length(echantillon[echantillon==echantillon[i]])  / length(echantillon[echantillon==echantillon[i]+1])))
        }
    }
    return(list(x=liste_x, y=liste_y))
}
res = simul_second(n, r, p)
liste_x = res$x
liste_y = res$y

plot(liste_x, liste_y)
n_reg<-lm(liste_y~liste_x)
print(n_reg)
lines(liste_x, fitted.values(n_reg))
print("pg2")
print(1 - 1/ n_reg$coefficients[2])
print("pg3")
print(1 + (r-1) / n_reg$coefficients[1])

# Test de pg2 et pg3
listpg2 = c()
listpg3 = c()
for(r in seq(10, 10000, 500)) {
    pg2 = 0
    pg3 = 0
    y = 10
    for(j in 1:y) {
        res = simul_second(n, r, 0.5)
        reg<-lm(res$y~res$x)
        pg2 = pg2 + 1 - 1/ reg$coefficients[2]
        pg3 = pg3 + 1 + (r-1) / reg$coefficients[1]
    }
    pg2 = pg2 / y
    pg3 = pg3 / y
    listpg2 = c(listpg2, pg2)
    listpg3 = c(listpg3, pg3)
}
plot(seq(10, 1000, 50), listpg2)
plot(seq(10, 1000, 50), listpg3)
