#### cvicenia 2023-09-28


# z minula sme uz mali
# ak uz mame nainstalovane, tak znova netreba
# install.packages("ggplot2movies")
library(ggplot2movies)
movies
head(movies)

#### cvicenie 1.3 ####

?movies

### ktore filmy dominuju na trhu
summary(movies)

# drama, komedia, short

### najstarsi komedialny film
movies$Comedy == 1

# ulozim indexy comedy filmov a pozriem sa na ich roky
ind_comedy <- (movies$Comedy == 1)
year_comedy <- movies[ind_comedy, ]$year

# pozriem, kolko mam rokov pri komediach
length(year_comedy)

# ak chcem len zistit rok - zoradim, alebo pozriem minimum
sort(year_comedy)
min(year_comedy)
# [1] 1896

# keby som chcela teraz hladat, na akom indexe to je
# musim manualne najst, na akych indexoch sa nachazaju komedie s takym rokom
which(movies[ind_comedy, ]$year == min(year_comedy))
# [1] 16054
# len na tomto jednom riadku
# zobrazim si prislusny riadok
# (pozor!, len z casti komedialnych filmov)
movies[ind_comedy, ][16054,]

# vidime, ze je to comedy z daneho roku

# ina, priamociarejsia moznost pomocou order
order(year_comedy)
# vypise nam indexy, na ktore sa mame pozerat,
# ak to chceme zoradene od najmensieho

# pozriem sa na prvy index, na ktory mam pozerat (teda na najstarsi film)
com_old <- order(year_comedy)[1]
year_comedy[com_old]
# ak chcem vediet nazov filmu
movies[ind_comedy, ][com_old,]

# keby som teraz chcela 4 najstarsie
com_old_4 <- order(year_comedy)[1:4]
movies[ind_comedy, ][com_old_4,]

# pozor, tych z roku 1898 moze byt aj viac, vypise prvy 
# lepsie pomocou order, ak to chcem potom aj pozerat

### KTORY animovany, neskor ako 1980, dlhsi ako 30 minut -
### najlepsi a najhorsi rating

#vsetky animovane
movies[movies$Animation==1,]
#vsetky LEN starsie ako 1980 (nemusia byt animovane)
movies[movies$year>1980,]
#vsetky LEN dlhsie ako 30 minut
movies[movies$length>30,]

# chcem vsetky tieto podmienky naraz
ind_a_80_30 <- (movies$Animation==1) & (movies$year>1980) & (movies$length>30)

# pozriem si ratingy na riadkoch, kde platia vsetky tie 3 podmienky naraz
movies[ind_a_80_30,]$rating

# keby som chcela len rating
sort(movies[ind_a_80_30,]$rating)

# ale ja chcem aj film - lepsie takto
order(movies[ind_a_80_30,]$rating)
# staci mi jedno najhorsie hodnotenie
order(movies[ind_a_80_30,]$rating)[1]
# [1] 89
# a jedno najlepsie hodnotenie - zoradim v klesajucom poradi
order(movies[ind_a_80_30,]$rating,decreasing=T)[1]
# [1] 195

# pozriem si tie filmy
movies[ind_a_80_30,][89,]
movies[ind_a_80_30,][195,]
# keby som chcela len nazov
movies[ind_a_80_30,][195,]$title

# lepsie by bolo si indexy ulozit, a nie takto priamo cislo pisat
ind_rat_low = order(movies[ind_a_80_30,]$rating)[1]
movies[ind_a_80_30,][ind_rat_low,]

# alebo rovno naraz v jednom riadku
movies[ind_a_80_30,][order(movies[ind_a_80_30,]$rating)[1],]

# potom keby som chcela 3 najhorsie, staci prepisat, kolko z order chcem zobrat
movies[ind_a_80_30,][order(movies[ind_a_80_30,]$rating)[1:3],]

# vsimli sme si, ze ma sice zle hodnotenie, ale malo hlasov
### dalsia otazka - este taky, co ma aspon 1000 hlasov

ind_v_a_80_30 <- (movies$votes>=1000) & (movies$Animation==1) & (movies$year>1980) & (movies$length>30)

movies[ind_v_a_80_30,]$rating
sort(movies[ind_v_a_80_30,]$rating)
order(movies[ind_v_a_80_30,]$rating)
order(movies[ind_v_a_80_30,]$rating)[1]
order(movies[ind_v_a_80_30,]$rating,decreasing=T)[1]

movies[ind_v_a_80_30,][20,]
movies[ind_v_a_80_30,][73,]


#### DU vyskusat ####
## ako sa vola najhorsia romanticka komedia minuleho tisicrocia - DU 
## zoznam 10 najhorsich filmov s rozpoctom vacsim ako 20 milionov - DU 
## ktory zaner je najuspesnejsi po 2000? DU


### stupa pocet recenzii v case pre akcne filmy?

# vytriedim si akcne filmy
ind_action <- (movies$Action == 1)
# zobrazim si na grafe vzdy rok a pocet recenzii
plot(movies[ind_action, ]$year, movies[ind_action, ]$votes)

# stupa

# ktorych filmov je viacej, Terminator, Batman alebo Star Wars

install.packages("stringr")
library(stringr)
?str_detect
sum(str_detect(movies$title,"Batman"))
sum(str_detect(movies$title,"Terminator"))
sum(str_detect(movies$title,"Star Wars"))

# o Batmanovi
# berieme do uvahy len take, ktore to maju priamo v nazve

# pomocou pairs zobrazte year, length, rating
head(movies)
pairs(movies[,c(2,3,5)])

# zopar filmov je podozrivo dlhych, zistite, ze ktore to su

order(movies$length, decreasing=T)
ind_long <- order(movies$length, decreasing=T)[1:3]
movies[ind_long, ]

# ak by sme chceli, aby nam ten graf viac ukazal, 
# tak mozeme napr. 6 najdlhsich vyhodit
ind_long_6 <- order(movies$length, decreasing=T)[1:6]
movies[ind_long_6, ]
pairs(movies[-ind_long_6,c(2,3,5)])
# teraz je to uz krajsie na tom grafe


#### cvicenie 2.1. ####

y = TRUE
is.numeric(y)
z = as.numeric(y)
z
is.numeric(z)


height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
ind1 <- (height > 1.75)
ind2 <- 1*ind1

is.logical(ind1)
is.logical(ind2)

# akonahle logicku premennu pouzijem s nejakou numerickou operaciou,
# tak sa z nej stava numericka premenna

height[ind1]
height[ind2]

# mozeme si pozriet, ako to funguje
height[c(1,2,3)]
# zavolala som prvy az treti udaj
height[c(1,1,3)]
# zavolala som dva krat prvy a potom treti
height[c(1,0,1)]
# zavolala som prvy, ziadny, prvy

# POZOR, R-ko indexuje od 1 !!!



#### cvicenie 2.2 ####
(0.1 + 0.2) == 0.3

# ale 
0.1+0.2

# skusime
(0.1 + 0.2) - 0.3

# vidime, ze je tam nejaka drobna chyba
# ak chcem vediet, ci sa nieco rovna
# je lepsie pouzit to, ze v absolutnej hodnote
# ma rozdiel byt mensi ako nieco
# to nieco je super vediet, ake ma byt podla toho,
# aka chyba moze vzniknut

abs((0.1 + 0.2) - 0.3) <= 1e-10

abs((0.1 + 0.2) - 0.3) <= 1e-15

abs((0.1 + 0.2) - 0.3) <= 1e-18



#### cvicenie 2.3 filmy ####

library(dplyr)

## zo zadania 

#option 1
ind <- movies$year > 1995 & movies$Action==1
#look at column called "budget"
movies[ind,"budget"]
#or 
# "budget" is in the 4th column
names(movies)
#look at the fourth column
movies[ind,4]


#option 2 directly (less readable)
movies[movies$year > 1995 & movies$Action==1,"budget"]
movies[movies$year > 1995 & movies$Action==1,4]


#option 3 nicer
# use function with, takes dataset as the first argument
with(movies, budget[year > 1995 & Action==1])


#option 4 much nicer (use dplyr package)
movies %>% 
  filter(year > 1995, Action==1) %>%
  select(budget)


# chceme zoradit animovane filmy s aspon 1000 hlasmi vydane po roku 1980
# dlhsie ako 30 minut
# podla ich hodnotenia od najlepsieho po najhorsi
?filter
?arrange
?desc

# ako by sme to robili predtym
ind_v_a_80_30 <- (movies$votes>=1000) & (movies$Animation==1) & (movies$year>1980) & (movies$length>30)
order_movies <- order(movies[ind_v_a_80_30,]$rating,decreasing=T)
movies[ind_v_a_80_30,][order_movies,]

# ked pouzijeme kniznicu dplyr
movies %>% 
  filter(votes>=1000, Animation==1, year>1980, length>30) %>%
  arrange(desc(rating))

# elegrantnejsie, citatelnejsie

# chcem dramy, rating vacsi ako 8, dlhsie ako 2hodiny
# aky je priemerny budget 20 najdrahsich filmov?

# viem si tento vystup aj ukladat ako dataframe a krajsie don pozerat


dd <- movies %>% 
  filter(Drama==1, rating > 8, length > 120) %>% 
  arrange(desc(budget))

dd[,"budget"]
mean(dd$budget[1:20])



#### Cvicenie 2.4 ####

bmi <- function(height, weight){
  return(weight/((height/100)^2))
}

bmi(180, 70)
bmi(70, 180)
bmi(weight = 70, height = 180)

# da sa zadat aj parameter nejaky, ktory bude defaultne prednastaveny
# vtedy ak to nezadam, tak zoberie ten prednastaveny
# dava sa to pri zadavani funkcie
bmi_def <- function(height, weight=70){
  return(weight/((height/100)^2))
}
# uzitocne, ak chcem mat moznost vykreslit aj obrazok, ale nie vzdy ho chcem kreslit

?mean
# mean(x, trim = 0, na.rm = FALSE, ...)
# vidime, ze trim je defaultne nastavene na 0, atd

#### Cvicenie 2.5 ####

# suma
# musim nejako defaulne nastavit x
# aby mi to nic nezmenilo na sume, tak to bude 1
x <- 0
for (i in 1:350){
  x <- x + (i*(i-1)/(i+2))
}
x

# pozor pri pustani znova, najprv musim vynulovat to x zase
# inak mi to bude pripocitavat znova celu sumu uz k tomuto x, co mam teraz

# sucin
# tu pozor, keby sme nastavili y = 0, tak vynulujeme cely sucin
y <- 0
# preto tu potrebujeme neutralny prvok nasobenie, tj. 1
y <- 1

for (i in 1:350){
  y <- y*(i+1)/(i+2)
}
y

# mozem to aj naraz, kedze idem s i cez rovnake hodnoty

x <- 0
y <- 1

for (iNumber in 1:350){
  x <- x + (iNumber*(iNumber-1)/(iNumber+2))
  y <- y*(iNumber+1)/(iNumber+2)
}


#### DU ####
# stiahnut a pozriet data k cviceniu 2.6 a 2.7





