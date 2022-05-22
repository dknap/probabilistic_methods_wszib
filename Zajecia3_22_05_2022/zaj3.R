# Dawid Knap

#zadanie 

punkty<-c(26, 36, 23, 18, 24, 35, 32, 21, 38, 29, 29, 23, 28, 28, 21, 34, 30, 25, 22, 30, 12, 10)
m0<-30

t_obl = (mean(punkty) - m0)/sd(punkty)*sqrt(length(punkty))
print(t_obl)
alfa<-0.05
t_kryt<-qt(1-alfa/2, length(punkty)-1)

print(t_kryt)

# wniosek
# ponieważ |t_obl = -2.547526| > t_kryt = 2.079614
# to hipotezę zerową należy odrzucić


# zadanie 2
# wczytanie danycho

kwoty<-read.table("hipotezy1.txt")
str(kwoty)
summary(kwoty)

mean(kwoty$V1)

# H0: sr = 310
# H1: sr != 310

m0<-310
u_obl = (mean(kwoty$V1) - m0)/sd(kwoty$V1)*sqrt(length(kwoty$V1))
print(u_obl)
alfa<-0.05
u_kryt<-qnorm(1-alfa/2)
print(u_kryt)

# wnioski
# ponieważ |u_obl = 2.274148| > u_kryt = 1.959964 to
# hipotezę zerową należy odrzucić


##### funckja t.test #####
# H0: sr = 310
# H1: sr < 310

m0<-310
alfa<-0.05
t.test(kwoty$V1,mu=m0,sd=sd(kwoty$V1),conf.level = 0.95, alternative = "less")


# zadanie 3

dane<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00292/Wholesale%20customers%20data.csv")
head(dane)

x<-dane$Milk
mean(x)
sd(x)

# H0: sr = 5.025
# H1: sr < 5.025

m0<-5025
u_obl = (mean(x) - m0)/sd(x)*sqrt(length(x))
print(u_obl)
alfa<-0.05
u_kryt<-qnorm(1-alfa)
print(u_kryt)

# Wnioski
# Ponieważ |u_obl = 2.192057| > u_kryt = 1.644854 to
# hipotezy zerowej nie należy odrzucać


# zadanie 4

table(dane$Channel)

x<-dane$Milk[dane$Channel==2]
length(x)
y<-dane$Milk[dane$Channel==1]
length(y)

# H0: sr1 = sr2
# H1: sr1 != sr2

u_obl<-(mean(x)-mean(y))/ sqrt(var(x)/length(x) + var(y)/length(y) )
print(u_obl)

alfa =.05
u_kryt<-qnorm(1-alfa/2)
print(u_kryt)

# Wnioski
# Ponieważ |u_obl = 8.541551| > u_kryt = 1.959964 to
# H0 należy odrzucić na korzyść H1

?t.test
t.text(x,y)
t.test(Milk~Channel,data=dane)

# testowanie normalności rozkładu

reszty<-c(3.9, -0.3, -3.1, -5.5, -6, -2.6, -2.3, 3.4, 4.7, 2.6, 3.1, 0.8, 9.7, 5.4, 1.6, 1.4, 4.2, -0.3, -7.4,
          -3.4, 1, 4.6, -2.7, -5.2)

# H0: reszty z rozkładu normalnego
# H1: reszty nie są z rozkładu normalnego

shapiro.test(reszty)

# wniosek 
# Ponieważ obliczone p-value = 0.7858 > alfa = 0.05 to
# nie ma podstaw do odrzucenia hipotezy H0


# POTRZEBNE DO PROJEKTU:
shapiro.test(dane$Milk)
# wniosek
# Ponieważ obliczone p-value < 2.2e-16 = 2.2 * 10^-16 < alfa = 0.05
# H0 należy odrzucić

