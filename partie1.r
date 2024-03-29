n = 100
r = 1
p = 0.1789
echantillon = sort(rnbinom(n,r,p))
for (i in 1:n) {
    echantillon[i] = echantillon[i] + r
}

#Estimons p (1.1)
print("Estimation de p selon la méthode des moments, et de maximum de vraisemblance")
print(r / mean(echantillon))



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

n = 10000
r = 10
p = 0.5
echantillon = sort(rnbinom(n,r,p))
for (i in 1:n) {
    echantillon[i] = echantillon[i] + r
}
liste_x = c()
liste_y = c()

for (i in 1:length(echantillon)-1) {
    if(length(liste_x[liste_x==echantillon[i]]) != 0) {
        next
    }
    liste_x = c(liste_x, echantillon[i])
    liste_y = c(liste_y, c(echantillon[i] * length(echantillon[echantillon==echantillon[i]])  / length(echantillon[echantillon==echantillon[i+1]])))
}
plot(liste_x, liste_y)
n_reg<-lm(liste_y~liste_x)
print(n_reg)

print("pg2")
print(1 - 1/ n_reg$coefficients[2])
print("pg3")
print(1 + (r-1) / n_reg$coefficients[1])
