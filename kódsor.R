setwd("C:/Users/User/OneDrive/Asztali gép/CORVINUS/1. félév/R Több változósadatelemzés/BEADNDÓ")
salaries <- read.csv("ds_salaries.csv")

#beolvastam az adattáblámat
#megnézem, hogyan épül fel az adattáblám

str(salaries)
typeof(salaries)
# ez egy lista, ami azonos hosszú vektorokból áll - minden oszlpoba 607 megfigyelést találunk

#-----------------------------------------------------------

# 2. gyakorisági táblázat kilistázza a változó összes lehetséges értékét és megadja, hogy melyik érték hányszor fordul elő a vizsgált adattáblában
# megadja a változó értékeinek gyakoriságát

#NUMERIC GYAKORISÁG - remote cégek gyakoriságának megoszlása
table(salaries$remote_ratio)
plot(table(salaries$remote_ratio))
# különböző állások gyakorisága
table(salaries$job_title)


# cég méretek gyakorisága
table(salaries$company_size)


# tapasztalatok gyakorisága
table(salaries$experience_level)

table(salaries$salary_in_usd)


# employeek lakhelyének gyakorisága
table(salaries$employee_residence)

# hist fügvényben megvizgsáljuk, hogy milyen a gyakorisági eloszlása  remote állásoknak
hist(salaries$remote_ratio)
# az eredményben láthatjuk, hogy a 607 állásból több mint a fele, 100% os remota állást ajánl.


#-------------------------------------------------
#2 Helyzetmutatók (módusz, medián, átlag)


# módusz: egy változónak a legtöbbször előforduló értéke - ezt a gyakorisági táblázatból a legegyszerűbb leolvasni

plot(table(salaries$salary_in_usd))

table(salaries$job_title)
# legtöbbször előforduló érték - módusz - 143 Data Scientist 

# tapasztaloatok gyakoriságának ábrlzolása plottal
plot(table(salaries$experience_level))
# láthatjuk hogy a senior pozik a legtöbbzsör előforudló értékek módusza,
#a keresendő állások legtöbbször előfrorduló esetben a senioroknak szólnak 



#medián: egy változó mediánja az az érték, aminél a változó értékeinek 50%-a kisebb, másik fele pedig nagyobb

median(salaries$salary_in_usd)
# 101570

hist(median(salaries$salary_in_usd))

median(salaries$experience_level)
# MI
median(salaries$company_size)
# M



#átlag:  változó elemeinek összege osztva az adatok elemszámával.
mean(salaries$salary_in_usd)

hist(mean(salaries$salary_in_usd))
# 112297.9   összehasonlíthatjuk a mediánt, és az átlag eredményét, kifejthetjük a különbésget hasonlóságot, beszélhetünk a  kilógó értékről







#-----------------------------------
#2. Kvantilisek : öröklik a medián tulajdonságait
# Negyedelő KVARTILIS Q1-25% (alsókvartilis), Q2-Me, Q3-75% (felső kvartilis)
# Ötödölő KVINTILIS - 4 db van belőle 
# TizedelŐ DECILISEK - 9 db van belőle
# Századoló PERCENTILISEK - 99 db van belőle

#nézzük meg, hogy a fizetéseknek USD-ben hová esik a Q1-es értéke, Q2,Q3-as értéke.
quantile(salaries$salary_in_usd, probs = 0.25) #  25% 62726  , tehát a fizetések elő negyedéne eléri a 60 ezer dollárt 
quantile(salaries$salary_in_usd, probs = 0.50) #  50% 101570 megegyzeik az általunk régebben kiszámolt medián érétkkel
quantile(salaries$salary_in_usd, probs = 0.75) #  75% 150000 ezt csak úgy érdekeségből lett kiszáámolva


#summary
summary(salaries)


salaries$experience_level<- as.factor(salaries$experience_level)
summary(salaries)
#ebben azonosítottuk a factorral, hogy a summary függvénybe a caharcter alapú expereince level oszlopot is vegye bele, így láthatjuk, hogy végreredményül azonosítani tudta lefutatta rajt a függvnyt is.








#Szóródási Mutatók:
#Szórás: Egy véletlenszerűen kiválasztott érték az átlaghoz képest várhatóan mennyivel tér el. +-

N <- nrow(salaries) # az elemszám

#szórás
atlag_fizetes <- mean(salaries$salary_in_usd)
sqrt(sum((salaries$salary_in_usd - atlag_fizetes)^2) / N)
# véletlenszerűen kiválasztott érték az áltaghoz képest várhatóan 70898.79 fog eltérni.


szoras_salaries <- sqrt(sum((salaries$salary_in_usd - atlag_fizetes)^2) / N)

#Relatív szoras : szórás hányszorosa az átlagnak : 0.6313458 szóval, a relatív szórás az 63% 
szoras_salaries/atlag_fizetes



# eloszlások alakja

hist(salaries$salary_currency)
hist(mean(salaries$salary_in_usd))
hist(median(salaries$salary_in_usd))



describe(salaries)
# n: Az elemszáma a változónak. Mint látjuk, ez itt nem egységes, hiszen nem azonos számú érettségiző volt a háromtárgyból.A hiányzó értékeket tehát nem számolta bele a változó elemszámába.
#mean: átlag értéke
#sd: A szórás értéke (itt is az N − 1-gyel osztós verziót számolja)
#median: A medián értéke
#trimmed: A nyesett átlag értéke úgy, hogy a változó értékeinek alsó és felső 10%-át kihagyja az átlagszámolás során.
#mad: Mean Absolut Deviation –> a szórás abszolútértékkel (és nem négyzet + gyökvonással) számolt értéke.
#min, max és range: A változó értékeinek minimuma és maximuma, valamint ezek különbsége, mint a változó teljesterjedelme: 
#range = max - min
#skew: Az α3 mutató értéke
#kurtosis: Az α4 mutató értéke
#se: Az átlag standard hibája (standard error) –> egyelőre nem foglalkozunk vele, következő anyagban tanulunk róla.


#Doboz ábra, kilógó értékek

boxplot(salaries$salary_in_usd)
summary(salaries$salary_in_usd)

# Tukey féle kerítések:  Tukey-féle kerítések segítségével jelzi az adatsor kilógó értékeit. A Tukey-féle kerítések alapján egy változó azon értékei 
#minősülnek kilógónak, amelyek a változó középső 50%-hoz, tehát az IKT-hez képest túl messze helyezkednek el 
#(akár felfelé, akár lefelé). 

#*-----------------------------------------------------------------------------------
#*
#*
#*
#*
#*
#*
# 3. numerikus változó átlagának és mediánjának intervallumbecslését  90%-os megbízhatósági szinten
# a minőségi változó kategóriái szerint külön-külön!
  

# numerikus válotzó átlag medián, intervallumbecslés

summary(salaries$salary_in_usd)

#szórás
sd(salaries$salary_in_usd)

#arányosítani
prop.table(table(salaries$job_title))*100

# Megfigyelt adatom = Minta
# Mintából számolok stat mutatókat - átl, szórás, medián, arányok minden
# De én ezen stat mutatók értékét a 
#  megifgyelt adatokon túli világban (sokaság) 
#  akarom tudni
#válasz: INTERVALLUMBECSLÉS (konfidencia-intervallum)

#intervallumbecslés előfeltételei
# 1. a mintakiválasztási módja legyen véletlen
#  ha véletlen a minta (és elég nagy) --> reprezentetívak -> egy változó értékeinek arányai kb. ugyan azok a mintába mint a sokaságba.
#   pl ha a sokaságban kb 40% férfi akkor a mintába is 40% férfi legyen
#
# 2. statisztikai mutató legyen torzítatlanul becsülhető - szarvasos példa - átlagosan eltaláltaák a szarvast de maga a lövések nem
#  ha van sok-sok (több ezer különböző minta) mintavételem 
#  akkor a mintákból szűmított stat. mutatók értékei átlagosan visszadják a mutató valós sokasági értékét
#  Átlag, Arány Medián --> alapból torzítatlanok, VISZONT  a szórás az torzít
#       az a szórás torzít ahol szórás: gyök(szum((Érték-Átlag)^2)/N) --> lefelé torzít
#    TOZÍTATLAN szórás = gyök(szum((Érték-Átlag)^2)/N-1)

#standard hiba átlagra = korr. szór / gyök (N)

#átlagos KOnf. Intervallum = mintaátlag +- SH
# olyan intervallum határt kell tudnia amibe benne van jó esélyjel az a valós érték amit keresek
#megbízhatósága = 68%

#(1-alfa) megbízhatóságú konfidencia inetrv = mintaátlag +- SH * k 
# k= megbízhatósági korrekció szor = qnorm(1-alfa/2)
# alfa = megengedett becslési hibavalószínüség a becslésben
# sima qnorm: átlag=0 és szórás=1
# (érték - átlag) / szórás --> átlag=0 és szórás=1    - NORMALIZÁLÁS / STANDARDIZÁLÁS

stand_net <- (salaries$salary_in_usd - 
    mean(salaries$salary_in_usd, na.rm =TRUE)) /
    sd(salaries$salary_in_usd, na.rm =TRUE)

mean(stand_net, na.rm=TRUE)  #5.71

#átlag - normális eloszlás átlaga 0
round(mean(stand_net, na.rm=TRUE),3)

#szórás - normális eloszlás szórása 1 
round(sd(stand_net, na.rm=TRUE),3)

# hangsuly a korrekciós szorzóba a normális eloszláson van 


# fizetésekre nézzük a 98% os konfidencia intervallimot

summary(salaries$salary_in_usd)
mintaatlag <- mean(salaries$salary_in_usd, na.ram=TRUE)
korr_szoras <- sd(salaries$salary_in_usd)
N <- 607
alfa <- 0.1 #10% om van hibára 

SH <- korr_szoras/sqrt(N)
k <- qnorm(1-alfa/2)

# Alsó határ AH
mintaatlag-SH*k
# Felső határ FH 
mintaatlag+SH*k

# teljes intervallum  107560.6 - 117035.2  - 90% os pontosággal ennyi fizetés várható egy data sciencel foglalkozó munkakaeresőnek ezen az oldalon

# prudens becslés elve --> mondd a roszabbat


install.packages("rcompanion")
library(rcompanion)

# ez kiszámolj az előbbi manuális számolásainkat a aminta átlag -+ standard hibás számolásunkat
# adott táblából nézzen egy átlag intervallum becslést
groupwiseMean(salary_in_usd ~ 1,
              data = salaries,
              conf = 0.90,
              na.rm=TRUE)




# azért group wise mert csoportonként is lehet nézni, nominális változók esteén
groupwiseMean(salary_in_usd ~ experience_level,
              data = salaries,
              conf = 0.90,
              na.rm=TRUE)

groupwiseMean(salary_in_usd ~ job_title,
              data = salaries,
              conf = 0.90,
              na.rm=TRUE)

groupwiseMean(salary_in_usd ~ company_location,
              data = salaries,
              conf = 0.90,
              na.rm=TRUE)

groupwiseMean(salary_in_usd ~ remote_ratio,
              data = salaries,
              conf = 0.90,
              na.rm=TRUE)

groupwiseMean(salary_in_usd ~ company_size,
              data = salaries,
              conf = 0.90,
              na.rm=TRUE)



# 90% OS MEGBIZHATÓSÁGÚ becslés arány
# 0/1 értékű változó átlaga = 1-sek aránya
prop.table(table(salaries$job_title))*100
prop.table(table(salaries$company_location))*100

salaries$Amerika <- 0
salaries$Amerika [salaries$company_location=="US"] <- 1
mean(salaries$Amerika)

#arányosított állások amerikában átlagosan 58%  felső és alsó határa= -- ITT VAN VALAMI PROBLÉMA MERT NEM MUTATAJA AZ ELEMSZÁMOT
groupwiseMean(Amerika ~ "1",
              data = salaries,
              conf = 0.90,
              na.rm=TRUE)


#arányosítva a pozikat US-ben lévő pozikat? kimondottan a preferenciára kérdez rá nem azt  hogy konkrétan hol van - ez nem releván számunkra
groupwiseMean(Amerika ~ job_title,
              data = salaries,
              conf = 0.90,
              na.rm=TRUE)


#mintaarány +- SH * K
# SH = korr.szórás / gyök(N)   - ha nagy az N akkor megesi a szórást
# ez a bebizonyítás hogy a valós mintaarány a  vett mintaaránytól nem eltérő
# ha 0/1 az adatsor (azaz 0 vagy egy értéket vehet fel) akkor a szórás az gyök(p(1-p))
#    ahol p= 1-esek aránya 

#ha az 'n' nő azaz több elemes mintánk van akkor a konf. intervallum hossza csökkne
#  mi az a minimális elemszám ahol a konfidencia intervallum becslés csökken
----------------------------------------------------------------------------------------
 # Nme jó ez a fejlezet , de ez arról szól, hhogy kiderítsem mekkora elemszámú minta kell ahoz hogy pl 3% os standarsd hibám legyen 
  #- nekem ez nem működött mert 0 ra jött ki minden, nem mutatja az elemszámot leve

#konfindencia intervallum hossza amerikába
# az a szám ami az konfidencie intervallum hisszát jellemzi
  # mért arány +- KI_Hossz (KONFIDENCIAINTERVALLUM)
  
groupwiseMean(Amerika ~ 1,
                data = salaries,
                conf = 0.90,
                na.rm=TRUE)  

KI_Hossz_Amerika <-0.618-0.585
# +- 3,3% pont

# +- 3% pontot akarok elérni  --> n = ?
#kérdés hogy ha ilyen pontosággal akrok becsülni hány elemű mintát kéne vennem

# konfidencia intervallum hossz hogyan jött ki
# Hossz = SH * k

#korrigált szórás osztva az elemszámmal
SH <- sd(salaries$Amerika[salaries$company_location=="US"])/sqrt(355)

k <- qnorm(1-alfa/2)

SH*k ## nekem 0 ra jön ki 

# 0.03 = szórás / gyökr(n) * 1.6
# 0.03/1.6 = szor / gyök (n)
# gyök (n) = szór * 1,6/0,03
# n = (szór*1,6 / 0,03)^2
#

#sd(salaries$Amerika[salaries$company_location=="US"])



----------------------------------------------------------------------------------------------------
#medián konfidencia értéke - robsoztus  akilógó értékek,re ha az adtahalmaz kicsi akkor nem lesz változatos az adatszórás
groupwiseMedian(salary_in_usd ~ experience_level,
                data = salaries, conf =  0.90)

groupwiseMedian(salary_in_usd ~ 1,
                data = salaries, conf =  0.90)

#groupwiseMedian(salary_in_usd ~ company_location, # nem lehet ezt számolni
               # data = salaries, conf =  0.90)





--------------------------------------------------------------------------------------------
# Hipotézis
# az oldalon található állások 58% a mind Amerikában van , felső határa 61 % alsó haára 55% minden második állás amerikában lesz
# konfidencia intervallumok -> van egy mintavételem a teljes sokaságból, --> ebból kiszámolok valamilyen statisztikau mutatót (átlag, medián ...) 
  #és megnézem hogy mit mond el nekem a statisztikai mutató, statisztikai mutató sokasági értékéről 
  
#hipotézis vizsgálatnál másfajta logikai rendszer lesz - van egy előfeltevésem egy stat. mutató soksasági értékéről, 
  # pl a data analyst poziciók több int 60 % a amerikában található
  # miután megvan a feltételem, azután veszekegy mintát, azért hogy az előfeltételemet ez igazolja vagy cáfolja 
  # mint eldönti hogy igaz vagy hamis-e az előfeltétel
#előfeltétel = hipotézis
  
#feltétel: Átlag(salary_in_usd) >60000

 #minta az átlagban
mean(salaries$salary_in_usd)  # ez igaz mmert 112 ezer dollár az átlag

# egy hipotézis vizsgálat feladat eldönthető konfeidencia intervallummal is pl: ha kijönn ehogy 20 ezerrel eltérő lehet az érték akkor is benne avgyunk az általunk felálított feltételezésben

# hipotézis vizgsálat 4 lépése - lényege hogy a mintavételi hiba nagyságába betudhatóe az érték amit kiszmolunk


# 3 ra egyszerűsthető
# 1. Állítás --> nullhipotézist csinálunk, illetve alternatív hipotézist csinálunk (2 al állítás)
# 2. p-érték nevű mutatószám számítása (számítunk egy p értéket eldönti hogy melyik hipotézis jó)
# 3. p-érték alapján el leehet dönteni, hogy a null vagy az alternatív hipotézis igaz, ebből meglehet mondani hogy az alap állítás igaz vagy hamis (valós állításba is tudjuk helyezni)

# Állítás: átlag fizetés > mint 90 ezer,  
# M(sokasági átlag MÖ) 
# H0 - null hipotzéis  - ha törik ha szakad, azt mondja, hogy az átlag egyenlő azza la számmal ami a hipotézisben szerepel
# H1 - alternatív hipotézis - 
#  
#     M>90000     M=90000     M!=90000      M<=90000    M<90000     M>=90000 
# H0  M=90000     M=90000     M=90000       M=90000     M=90000     M=90000    CSAK EGYENLŐSÉG LEHET
# H1  M>90000     M!=90000    M!=90000      M>90000     M<90000     M<90000    Ellentétje kell az alapnak nem pedig a H0-nak

# az első lépés minidg arról fog szólni, hogy az előfeltevésemet átfogalmazom null és alternatív hipotézis párrá

# 1. H0: Átlag (salary_in_usd) = 90000 || H1: Átlag (salary_in_usd) > 90000  --> H1 szeretem
# 2. p-érték számolás lesz ahol figyelembe veszem a megfigyelt adatotkat, mintékat

# számolása attól függ milyen statisztikai mutatót számolok
# p-érték (R függvénye attól függ, mi a stat. mutató MOST: átlag)


t.test(salaries$salary_in_usd, alternative = "greater", mu = 90000)


# p érték p-value = 2.059e-14  --> 0.0000000002059
# p érték megmutatja hogy mi a valószinüsége hogy  a h0-t elutasítani hibás döntés
   # ha azt mondom a null hipotézisre hogy hamis akkor mennyi a valószinüsége hogy hibás
   # mi a valószinüsége hogy a H0-t elutasítani hibás döntés? 
   # ha a p érték kisebb, mint a kritikus szint (általában 0,05), akkor a kutatók elutasítják a nullhipotézist, 
   # és elfogadják az alternatív hipotézist.
# döntés: p-érték kicsi --> H0 diszlájk --> H1 lájk  --> eredeti állításunk a H1 ben volt --> állításunk igaznak vehetjük
# A fizetések átlaga szignifikánsan nagyobb mint 90000.



###*#######################################################################################################################
#Utolsó feladat

library(ggplot2)

install.packages("ggplot2")


#Kapcsolatvizgsálat, 2 változó . x , y --> Mennyire befolyásolja Y-t az X?
# például mennyire befolyásolja az árakat a company location?
# mennyire befolyásolja a fizetéseket az experience?
# egyik oszlop mennyire befolyásolja a másikat

# amiért ez eltud bonyolódnia, a mérési skálák esete
# a kapcsolatvizsgálat esete mennyire befolyásolja az egyik változó a másikat - 
# 3 féle statisztikai mutatóval tudjuk megadni,attól függően hogy x,y nak milyen a mérési skálája



# 1. eset vegyes kapcsolat: egyik változó Nominális ( ordinálist is ide értem) + intervallum vagy arány mérési skálájú
#árakat település típus: arány + nominális ---> vegyes kapcsolat

# 2. eset mindkét nominális vagy ordinális - Asszociációs kapcsolat:  nomin(ord) + nomin (ord)

# 3. este Korrelációs kapcsolat: interv/arány +interv/arány 


# ezek az esetek döntik el melyik statisztikai mutatókat használhatom majd a akapcsolat vizgsálatakor

#Folyamata:
#3 fő lépés
# 1. mi a kapcsolat jellege --> mely országokba nagyobb a fizetés , hol jellemzőek entry level pozik --> Ábrákkal válaszoljuk meg
 # mindegyikhez kaocsolódik egy ábra típus 
# 2. magyarázó erő két változó között - statisztikai mutatóval ami leírja hogy a két váloztozó között x,y között milyen erős a kapcsolat 
 # a megfigyelt adatok körében értelmezhető
# 3. Megnézzük, hogy a akpacsolat létezik-e szebb szóval Szignifikáns e a nem megyfigyelt adatok körében - azaz a sokaság 
 # --> hipotézis vizsgálat

#######################################################################################################

# Vegyes kapcsolat :Fizetés és Országok kapcsolata
#1. Ábra kapcsolat jellegéről (átlag, medián,)  - a teljes ár eloszlás hogyan különbözik országonként --> doboz ábra, vagy csoportosított hisztrogra

# doboz ábra külön mindkettőre

ggplot(salaries, aes(y=salary_in_usd, x=company_location, fill=company_location)) +
  geom_boxplot()
# egy szem kilógó érték a 600.000 Usd fizetést jelzi, torzítani fogja az értékeket - ki kell dobni, további elemzéseknél túl zavaró

salaries <- salaries[salaries$salary_in_usd < 500000,]  # egy megfigyeléssel rövidült a táblám

# arra akarom rávenni az ábrát, hogy csinálja meg a különböző országokr

# arra akarom rávenni hogy csinálja meg külön tapasztalati szintekre

ggplot(salaries, aes(y=salary_in_usd, x=experience_level, fill=experience_level)) +
  geom_boxplot()
# eredménykétn láthatjuk hogy a dobozok nagyjából hasonló m agasságuak kivéve a expert tapasztalatal rendelkezőket, ami érdekes lehet
# a mid senirnál van olyan kilógó érték is ami magasabb egy expert nél is. Ezen kívül ami még érdekes lehet hoyg az entry lvl poziknál van olyan kilógó érték ami eléri az expert szint fizetését is.
# az eloszlások jobbra elnyúloak mindegyik felfele nyúlik, legmagasabb értékek az expert résznél vannak
# ki értékekek nkívül szépen növekednek a fizetések ahogyan tapasztalatottal gyarapszik az ember, nagy növekedés a senior és az expertnél található

# 2. mennyire szorosak a kapcsolatok  - a mutatószám amivel megtaláljuk a kacssolat szorosságát - variancia - h2 magyarul, eta 2 angolul
# variancia - hányados = h^2

aov(salary_in_usd~experience_level , data = salaries)  # visszaad egy táblázatot hanem csak a sum of squares experience-lvl 6.714309e+11
# ssb = ss between = 6.714309e+11
# ssr = ss residuals = 2.141492e+12
# sst = ss totals = 6.714309e+11 + 2.141492e+12 = ssb +ssr = 2.8129229e+12

# variancia(Ár) = SST/(N-1)
var(salaries$salary_in_usd)*(606-1) # eredménye pont az mikor az ssb és az ssr -t összeadnám  
# 2.812923e+12  --> szórás négyzet teteje - minden elem eltérése az átlagtól a négyzeten összeadva

#SST = SZUM((Érték-Átlag)^2)  - az fizetések az összesített szórása - ennyivel térnek el az átlagtól egy négyzetes skálán
# infósok úgy hívják hogy teljes információ az árakban, másnéven fisher információ

# residuál az a rész ami a maradvényérték , az össz információ a  residuálja az a rész ami nem magyraázható megy a csoportok közti eltéréssel, másnévnen ami a csportokon belüli eltérérssel jön

# H^2 = ssb/sst   --> 0-1 közt arányszám érték % ban

6.714309e+1/(6.714309e+1+2.141492e+12) # 3.135342e-11  --> 0.000000003135342  % - összes információ 

# az eredmény azt jelzi, hogy az SSB közel van egymáshzo nagyon és így SSB kicsi így az SSR-nek nagynak nkell lennie, 
#azaz egy mefgiyelés a csoport átlagához képest elég messez van, a csoport átlagok közelesnek

# H^2 < 10% gyenge  - ez a mi esetünkbe gyengének számít
# 10% <= H^2 <= 50% --> KÖZEPES
# 50% < H^2 --> ERŐS/SZOROS

# 3. szignifikáns-e a sokaság?

# H0: H^2 = 0 - A kapcsoalt Nem szignifikáns a sokaságban
# H1: H^2 > 0 - A kapcsolat SZIGNIFIKÁNS marad

# p-érték -> welch-korrekciózott F-próba

oneway.test(salary_in_usd~experience_level , data = salaries)

# p-value < 2.2e-16 --> H0 elutasítható minden szokásos alfa szignifikancia szinten
# a kapcsolat szignifikáns a sokaságban

# megfelelő elemszám - minden csportbanlegyen legalább 100 megfigyelés 

summary(salaries$experience_level) # sajnos a mi esetünkben a feltétel nem telejsült, mert az expert lvl-ben 25 db van és az entry lvl is csak 88

#más esetben a ritka kategóriákat össze lehetne vonni de a beadandó esetében megengedett, hogy ne teljesüljön





#########################################################################################################
# Asszociáció eset: experience lvl + jpb title megnázzük hogyan oszlanak el a meghirdetett állások tapasztaaltok alapján, milyen akpcsolatba vannak egymással

# 1. jelleg: ábra --> halmozott oszlop diagaram - egyik nominális változón belül megnézem a másik változó arányait




ggplot(salaries, aes(x=company_location, fill=job_title)) + 
  geom_bar()


ggplot(salaries, aes(x=company_location[salaries$company_location!="US"], fill=experience_level)) + 
  geom_bar()

ggplot(salaries, aes(x=company_location, fill=experience_level)) + 
  geom_bar()



ggplot(salaries, aes(x=experience_level, fill=job_title)) +    #ez jo
  geom_bar()


ggplot(salaries, aes(x=experience_level, fill=company_location)) +   # ez is jó
  geom_bar()







ggplot(salaries, aes(x=experience_level, fill=job_title)) +    #ez lesz  # látjuk hogy az állások hogyan oszlanak meg tapsztalati szint szerint, 
  geom_bar()
#láthatjuk, hogy legkevesebb állás az expertnél található, és experten belül a többihez képest kevesebb principal dataval foglalkozó állás található, míg arányosítva a hed of data itt a legnagyobb
#arányokat tekintva mind az entry, mid senior, és senior között hasonló az eloszlás az állások akapcsán, többségük, data analyst, science, enginer állások 70 % ba körülbelül többi pedig a machine learningel foglakozó állások és Ai területen dolgozók nyitott pozicióját mondanám 
# ami a lejgobban szembetűnik tapasztalat szinten az állások darabszáma, ha nem vesszük figyelembe a közel 25 db expert állást, láthatjuk hogy harmadik helyen a négyből az entry level áll olyan 80 db al míg mid seniorként körülbelül 210 állás elérhető,és akiknek a legjobban kedvez a piac a senioork akiknek kb 280 db nyitott állás áll rendelkezésükre.

# 2. erősség: mutató

#cramér együttható:
install.packages("questionr")
library("questionr")

(table(salaries[,c("experience_level","job_title")]))   #együttes gyakorisági táblázat

cramer.v(table(salaries[,c("experience_level","job_title")]))
 # c = 0.4655748 közepes a kapcsolat erőssége -Krámér együttható 0-1 közötti mutatószám -> de ez sem százalékos! - a végén olyan logika van , gyakorlatilag a H-nak a szórásnhányadosnak a rokona - ennyi a haszna, össz elehet mérni más mutatókkal
 # c < 0.3 --> gyenge 
 # 0.3 <= c <= 0.7 --> közepes
 # 0.7 < C --> erős vagy szoros

# közepes erősségű kapcsolatban vannak a pozik és a tapasztalati szintű állások


# 3. szignif.: hipotézisvizsgálat

#H0: C = 0 - A Kapcsolat nem szignifikáns 
#H1: C > 0 - a kapcsolat szignifikáns 

#p-érték --> khi-négyzet próbából jön

#előfeltétel: együttes gyakorisági táblában minden elem >= 10  - a mi esteünkben ez nem fog teljesülni

chisq.test(table(salaries[,c("experience_level","job_title")]))

#p-value < 2.2e-16  --> minden szokásos szignifikancia szinten H0 elvethető
# kapcsolat szignif(H1)


###########################################################################################################

#3. arányskáálás, intervallumskálás változók : évek és a fizetések 

 #1. ábrázoljuk a kapcsolatot - ivel két numerikus van ezért az ábrázolás pont diagrammon fog történni
 # x:  work_year  y: salary_in_usd


ggplot(salaries, aes(x=remote_ratio, y = salary_in_usd)) +
  geom_point()

ggplot(salaries, aes(x=work_year, y = salary_in_usd)) +
  geom_point()


ggplot(salaries, aes(x=salary_in_usd, y = work_year)) +
  geom_point()


ggplot(salaries, aes(x=work_year, y = salary_in_usd)) +   # ez lesz az 
  geom_point() + geom_smooth(method =  lm)

# jellemzés: egyirányú pozitív emelkedésű tengely , látható egy átlagos fizetés emelkedés 2020-2022 között viszont azt is láthatjuk, hogy 2021 ben a kiugró értékekből volt akár 450 ezer dolláros fizetés is
# viszont ez 2022 ben már csak a max 400 ezres volt. Elmondható hogy ezek alapján a legjobb évet 2021 zárta mert a kiugró értékekek 2020 as hoz hasonló volt, míg a stabil fizetési szakasz az a 2022-re hasonlít.
# összességébe véve felfeé stagnáló fizetést vehetünk észre az évek haladtával. Szürke sáv: egyenesnek a 95 % os konfidencia intervalluma - a nem megfigyelt fizetések értéóelmében, szük a konfidencie intervallum - ha kevés megfigyelés van akkor tág, ha sok akkor szűk - ezek alapján látszik, hogy a 2020 as állásokból kevesebb volt mint a 2022-ből
# egyirányú pozitív emelkedésű tengely 

#2. mutatószám az erősségre # ezekre a trendvonalakra, mmennyire jól illeszkednek rajta a pontok
# korrreláció mutató - egyenesre való illeszkedés mérérse: (Pearson-féle korreláció) = r
# -1 és +1 közötti mutató szám    -1 <= r <= +1
cor(salaries$work_year, salaries$salary_in_usd) # r=0.1845471  - az előjjel az irányt jelöli - azaz pozitív korreláció egy irányú kapcsolat - pozitív meredekségú egyenwes -> pozitív kkorreláció
# nem százalékos!!


# korreláció abszolút értékét a kapcsolat szorosságának a leírására használjuk
# |r| --> 0-1 de nem százalékos
# határok: 0.3   > közepes
#          0.7   > erős
# gyenge a kapcsolatunk ? 


#korreláció négyzetre emeelése # az évek mulásáa hény százalékba magyarázza  afizetés emelkedését,?
#R-négyzet = r^2 --> % ban -->  határok 10% , 50%
# 
0.1845471^2 # eredmény 0.03405763  -> az évek múlása 3 % ban magyaráza a fizetés emelkedést
cor(salaries$work_year, salaries$salary_in_usd)^2

#3. kapcsolat szignifikanciája a sokaságban

#Pontokra egjobban illeszkedő egyenesek = regresszió egyenesek - lineálris legresszió
lm(salary_in_usd ~ work_year, data = salaries)

# work year: ez a meredekség
# tengely metszet: -366

# becsült salaries = 18171* work year + (-36619860)


# -36619860 : Modell becslés ha nem telne az idő akkor -36619860 lenne a fizetés?
# 18717 : Ha egy % al telnek az évek akkor 18717 egységgel növeli a a mortalitásra adott egységet.

#H0: B1 = 0  | kapcsolat nem szignifikáns
#H1: B1 <> 0 | kapcsolat szignifikáns 

#p-érték # regressziós egyenes értékét nézzük summary fügványben

summary(lm(salary_in_usd ~ work_year, data = salaries))

# p-value: 4.809e-06  - H0 elvethető , H1: kapcsolat szignifikáns

# a residual standard error: 67070 on 604 # átlagos becslési hiba 







###########################################################################################
#Javítás


setwd("C:/Users/User/OneDrive/Asztali gép/CORVINUS/1. félév/R Több változósadatelemzés/BEADNDÓ")
salaries <- read.csv("ds_salaries.csv")


table(salaries$salary_in_usd)
hist(salaries$salary_in_usd)
summary(salaries)
boxplot(salaries$salary_in_usd)

sd(salaries$salary_in_usd)

boxplot(salaries$salary_in_usd, main="Salary Distribution", ylab="Salary (USD) ")
