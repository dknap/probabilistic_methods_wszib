#typy danych
x<-NULL
class(x)

a<-2.33
class(a)
print(a)

s<-15
class(s)
str(s)

d<-TRUE
class(d)

imie<-"Dawid"
class(imie)

wzrost<-c("wysoki","sredni","niski", "sredni", "wysoki", "sredni", "wysoki",
          "wysoki","sredni","niski",
          "wysoki","sredni","niski")
class(wzrost)
table(wzrost)

wz<-as.factor(wzrost)
class(wz)
table(wz)
str(wz)
mode(wz)

data<-as.Date("2022-03-13")
print(data)
class(data)

data2<-data+7       # data za 7 dni
print(data2)


# obiekty

#wektor
x<-seq(25, 45, 3)
class(x)
str(x)
y<-c(x,"ala")       # wektor
class(y)
str(y)

lista<-list(x=x,a="ala")
class(lista)					# list
str(lista)

xx<-sample(x, 150, replace = T)
str(xx)
y<-sample(c("Anna", "Marek", "Arek", "Marta", "Sonia", "Adam", "Kamil"), 150, replace = T)
str(y)

zz<-seq(as.Date("2022-03-13"), by=1, length.out=150)
str(zz)

dane<-data.frame(wiek=xx, imie=y, data=zz)
str(dane)
View(dane)

getwd()         # sprawdzanie ścieżki dostępu do projektu

mieszkania<-read.csv(file="mieszkania-pow.csv", sep=";")
str(mieszkania)
 
mieszkania$d<-as.Date(mieszkania$data, origin="1900-01-01")
str(mieszkania)


# Podstawowe statystyki opisowe

# średnia
mean(mieszkania$cena)
# mediana
median(mieszkania$cena)
# kwartyle
quantile(mieszkania$cena)
# minimum
min(mieszkania$cena)
# maksimum
max(mieszkania$cena)
# podsumowanie
summary(mieszkania$cena)

# przedziały częstości, histogram
h<-hist(mieszkania$cena,plot = F)
print(h)

# wczytywanie danych z linku
dane<-read.csv(file="http://artemis.wszib.edu.pl/~basiura/szeregi_d.csv", sep=";",dec=".", head=T)
str(dane)
# attach(dane)
plot(dane$szereg10, type="l", col="darkblue")
hist(dane$szereg10, breaks=25, col="lightblue", border="red", probability=T)
grid()

rug(dane$szereg10, col="red")
lines(density(dane$szereg10),lty=2,lwd =3, col="red")
curve(dnorm(x,mean = mean(dane$szereg10), sd = sd(dane$szereg10)),add=T,col="darkgreen",lty=3,lwd=3)


# miary rozproszenia dla mieszkań

# rozstęp
rozstep<-(max(mieszkania$cena))-(min(mieszkania$cena))
print(rozstep)

# rozstęp międzykwartylny
rozstep_miedzykwartylny<-quantile(mieszkania$cena)[4]-quantile(mieszkania$cena)[2]
print(rozstep_miedzykwartylny)

# wariancja
wariancja<-var(mieszkania$cena)
print(wariancja)

# odchylenie standardowe = pierwiastek z wariancji
odchylenie_standardowe<-sd(mieszkania$cena)
print(odchylenie_standardowe)
# print(odchylenie_standardowe - mean(mieszkania$cena))

# błąd standardowy z próbki losowej to odchylenie standardowe / pierwiastek z liczności próby
blad_standardowy<-sd(mieszkania$cena)/sqrt(length(mieszkania$cena))
print(blad_standardowy)

# odchylenie przecietne od mediany
odchylenie_przecietne_od_mediany<-mad(mieszkania$cena)
print(odchylenie_przecietne_od_mediany)

# wspolczynnik zmiennosci = odchylenie standardowe / średnią arytmetyczną
wspolczynnik_zmiennosci<-sd(mieszkania$cena)/mean(mieszkania$cena)
print(wspolczynnik_zmiennosci)

boxplot(mieszkania$cena, horizontal = T, col="lightblue", border="red")
