<<<<<<< HEAD
X<-mydata[,c("q4_1", "q4_2","q4_3","q4_4","q4_5","q4_6","q4_7","q4_8","q4_9")]
library(GGally)
ggpairs(X)

# Gjør til faktor
mydata$q4_1 <- as.factor(mydata$q4_1)
mydata$q4_2 <- as.factor(mydata$q4_2)
mydata$q4_3 <- as.factor(mydata$q4_3)
mydata$q4_4 <- as.factor(mydata$q4_4)
mydata$q4_5 <- as.factor(mydata$q4_5)
mydata$q4_6 <- as.factor(mydata$q4_6)
mydata$q4_7 <- as.factor(mydata$q4_7)
mydata$q4_8 <- as.factor(mydata$q4_8) # ikke med
mydata$q4_9 <- as.factor(mydata$q4_9) # ikke med 
mydata$q4_10 <- as.factor(mydata$q4_10) 

#### A: Test "Synsing" ####
### Test tillit forskning - forklare tillit til rovviltforskning relatert til tillt forskere og forskning
# Forklaring på variablene
# 1= forskning er viktig
# 2=alvorlig mister tro på forskning
# 3=tillit generell forskning
# 4 = tillit medisinsk forsk
# 5 = Tillit klimaforskning
# 6 = Forskere har høy ekspertise
# 7 = forskere har høyr troverdighet
m1 <- polr(q4_10~ 1,mydata, Hess =T)  
m1a <- polr(q4_10~ q4_3,mydata, Hess =T) # bedre 
m1b <- polr(q4_10~ q4_4,mydata, Hess =T)
m1c <- polr(q4_10~ q4_5,mydata, Hess =T)
bbmle::ICtab(m1,m1a,m1b,m1c, type="AICc", logLik = T) # 
m1d <- polr(q4_10~ q4_3 + q4_1, mydata, Hess =T)
m1e <- polr(q4_10~ q4_3 + q4_2, mydata, Hess =T) # Bedre 
bbmle::ICtab(m1a,m1d,m1e, type="AICc", logLik = T) 
m1f <- polr(q4_10~ q4_3 + q4_2 + q4_6, mydata, Hess =T)
m1g <- polr(q4_10~ q4_3 + q4_2 + q4_7, mydata, Hess =T) # Bedre
bbmle::ICtab(m1e,m1f,m1g, type="AICc", logLik = T) 
# best model: m1g <- polr(q4_10~ q4_3 + q4_2 + q4_7, mydata, Hess =T)
## Holdninger
# Holdning: (mydata$q3_1average) - TODO Kom tilbake til denne - DEN MÅ FIKSES OM DEN SKAL BRUKES
# m1gHoldning <- polr(q4_10~ q4_3 + q4_2 + q4_7 + mydata$q3_1average, mydata, Hess =T)



#### B: Test #### 
### rovdyr tilstedeværelse
#"RzoneTilstede"
#"ArtTilstede" 
mydata$qArtTilstede <- as.factor(mydata$ArtTilstede)
mydata$RzoneTilstede <- as.factor(mydata$RzoneTilstede)
m2 <- polr(q4_10~ 1, mydata, Hess = T)
m2a <- polr(q4_10~  ArtTilstede, mydata, Hess =T) # bedre
m2b <- polr(q4_10~ RzoneTilstede, mydata, Hess =T)
bbmle::ICtab(m2,m2a,m2b, type="AICc", logLik = T) 

## Sos Demografi
mydata$Kjønn <- as.factor(mydata$Kjønn)
mydata$q8_1Utdanning <- as.factor(mydata$q8_1Utdanning)
m4a <- polr(q4_10~ ArtTilstede + Alder, mydata, Hess =T) 
m4b <- polr(q4_10~ ArtTilstede + Kjønn, mydata, Hess =T)
m4c <- polr(q4_10~ ArtTilstede + q8_1Utdanning, mydata, Hess =T) # Bedre 
bbmle::ICtab(m2a,m4a,m4b,m4c, type="AICc", logLik = T) 
m4d <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder, mydata, Hess =T)
m4e <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Kjønn, mydata, Hess =T)
m4f <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn, mydata, Hess =T) # Bedre
# QUESTION: Skal vi ta velge m4f her? Det er vel riktig siden den er best og 2.5 bedre enn enklere modell. 
bbmle::ICtab(m4c,m4d,m4e,m4f, type="AICc", logLik = T)
m4g <- polr(q4_10~ q8_1Utdanning + Alder + Kjønn, mydata, Hess =T)
bbmle::ICtab(m4f,m4g,type="AICc", logLik = T)
# QUESTION: Her er m3c bare 1.6 "dårligere" enn m4a. og har én df mindre. Da tar velger vi å beholde m3c, ikke sant? 
#m4d <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q8_1Utdanning + Alder, mydata, Hess =T) # Bedre
 

### Jakttradisjoner  
m5a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_10, mydata, Hess =T)
m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T) # Bedre
bbmle::ICtab(m4d,m5a,m5b, type="AICc", logLik = T) 
# QUESTION: modellen blir bedre, men kan vi inkludere q2_11.1 som den er?


### Beitedyr
names(mydata)

mydata$SausluppetFylke <- as.numeric(mydata$SausluppetFylke)
hist(mydata$SausluppetFylke)
hist(log(mydata$SausluppetFylke))

mydata$Sautetthet <- as.numeric(mydata$Sautetthet)
hist(mydata$Sautetthet)
hist(log(mydata$Sautetthet))

mydata$Tapt.saulam.fylke <- as.numeric(mydata$Tapt.saulam.fylke)
hist(mydata$Tapt.saulam.fylke)
hist(log(mydata$Tapt.saulam.fylke))

mydata$Saulamtapprosent.F <- as.numeric(mydata$Saulamtapprosent.F)
hist(mydata$Saulamtapprosent.F)
hist(log(mydata$Saulamtapprosent.F))

m6a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + SausluppetFylke , mydata, Hess =T)
m6b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Sautetthet, mydata, Hess =T) 
m6c <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Tapt.saulam.fylke, mydata, Hess =T) 
m6d <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Saulamtapprosent.F, mydata, Hess =T) 

bbmle::ICtab(m5b,m6a,m6b,m6c,m6d, type="AICc", logLik = T) # ingen bedre slik det er naa
# QUESTION: Kan vi bruke variablene som over? Dårlig fordeling og muligens rart å gjøre dem numeriske når det bare er 14 forskjellige numre. 
# må eventuelt prøve å finne data per kommune/beitelag? 

m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T)
### Kan vi fjerne noe? 
m7a <- polr(q4_10~ q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T)
m7b <- polr(q4_10~ ArtTilstede + Alder + Kjønn + q2_11.1, mydata, Hess =T)
m7c <- polr(q4_10~ ArtTilstede + q8_1Utdanning  + Kjønn + q2_11.1, mydata, Hess =T)
m7d <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + q2_11.1, mydata, Hess =T)
m7e <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn, mydata, Hess =T)
#m7f <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn, mydata, Hess =T)

bbmle::ICtab(m5b,m7a,m7b,m7c,m7d,m7e, type="AICc", logLik = T) 
# m7d er bedre- Fjern derfor Kjønn
m7d2 <- polr(q4_10~ q8_1Utdanning + Alder + q2_11.1, mydata, Hess =T)
bbmle::ICtab(m5b,m7d,m7d2, type="AICc", logLik = T)
# kan ikke fjerne flere. m7d er beste model
# QUESTION: Kan vi gjøre slik? 
# QUESTION: Jeg regner med at jeg må gjoere alle variabler som skal vaere faktor til faktorvariabel manuelt? I saa fall, kan du se raskt over om jeg har gjort dette, og riktig, for alle som benyttes? 
# QUESTION: Her har fulgt den metoden hvor alle modeller innenfor 2 AIC-verdier fra den beste vurderes, og jeg tar den med færrest df av disse. Hva synes du om denne metoden? Kan det gjøres slik her selv om det heter dAICc (jeg vet ikke hva d'en staar for)? 
# QUESTION: Eller kan man ikke begynne å fjerne variabler igjen? Må jeg da evt. gå tilbake å ta de bort tidligere i prosessen? (men blir ikke det også rart?!)
# for om jeg har gjort riktig, så skal q8_1Utdanning være med i modellen da den enklere modellen uten denne er 2.1 AIC dårligere. Men så, om man sjekker modellene nederst i proseessen så
# er modellen uten q8_1Utdanning bedre (innenfor 2 AIC)
# QUESTION: Vi kunne ha testet for effekten av q2_9 også (frykt), men i så fall får vi kanskje samme problem som med q3_1average?
# QUESTION: Det er nå flere variabler jeg har tatt ut som er "tett på" å være med i den beste modellen, slik som "q8_1Utdanning", "q2_11.1", "Alder", "ArtTilstede" 
# og dette er variabler jeg er sterkt interessert i, som det er viktig å diskutere. Hvordan forholder man seg til disse? for de er vel signifikante men akkurat ikke med i den beste modellen,
=======
X<-mydata[,c("q4_1", "q4_2","q4_3","q4_4","q4_5","q4_6","q4_7","q4_8","q4_9")]
library(GGally)
ggpairs(X)

# Gjør til faktor
mydata$q4_1 <- as.factor(mydata$q4_1)
mydata$q4_2 <- as.factor(mydata$q4_2)
mydata$q4_3 <- as.factor(mydata$q4_3)
mydata$q4_4 <- as.factor(mydata$q4_4)
mydata$q4_5 <- as.factor(mydata$q4_5)
mydata$q4_6 <- as.factor(mydata$q4_6)
mydata$q4_7 <- as.factor(mydata$q4_7)
mydata$q4_8 <- as.factor(mydata$q4_8) # ikke med
mydata$q4_9 <- as.factor(mydata$q4_9) # ikke med 
mydata$q4_10 <- as.factor(mydata$q4_10) 

#### A: Test "Synsing" ####
### Test tillit forskning - forklare tillit til rovviltforskning relatert til tillt forskere og forskning
# Forklaring på variablene
# 1= forskning er viktig
# 2=alvorlig mister tro på forskning
# 3=tillit generell forskning
# 4 = tillit medisinsk forsk
# 5 = Tillit klimaforskning
# 6 = Forskere har høy ekspertise
# 7 = forskere har høyr troverdighet
m1 <- polr(q4_10~ 1,mydata, Hess =T)  
m1a <- polr(q4_10~ q4_3,mydata, Hess =T) # bedre 
m1b <- polr(q4_10~ q4_4,mydata, Hess =T)
m1c <- polr(q4_10~ q4_5,mydata, Hess =T)
bbmle::ICtab(m1,m1a,m1b,m1c, type="AICc", logLik = T) # 
m1d <- polr(q4_10~ q4_3 + q4_1, mydata, Hess =T)
m1e <- polr(q4_10~ q4_3 + q4_2, mydata, Hess =T) # Bedre 
bbmle::ICtab(m1a,m1d,m1e, type="AICc", logLik = T) 
m1f <- polr(q4_10~ q4_3 + q4_2 + q4_6, mydata, Hess =T)
m1g <- polr(q4_10~ q4_3 + q4_2 + q4_7, mydata, Hess =T) # Bedre
bbmle::ICtab(m1e,m1f,m1g, type="AICc", logLik = T) 
# best model: m1g <- polr(q4_10~ q4_3 + q4_2 + q4_7, mydata, Hess =T)
## Holdninger
# Holdning: (mydata$q3_1average) - TODO Kom tilbake til denne - DEN MÅ FIKSES OM DEN SKAL BRUKES
# m1gHoldning <- polr(q4_10~ q4_3 + q4_2 + q4_7 + mydata$q3_1average, mydata, Hess =T)



#### B: Test #### 
### rovdyr tilstedeværelse
#"RzoneTilstede"
#"ArtTilstede" 
mydata$qArtTilstede <- as.factor(mydata$ArtTilstede)
mydata$RzoneTilstede <- as.factor(mydata$RzoneTilstede)
m2 <- polr(q4_10~ 1, mydata, Hess = T)
m2a <- polr(q4_10~  ArtTilstede, mydata, Hess =T) # bedre
m2b <- polr(q4_10~ RzoneTilstede, mydata, Hess =T)
bbmle::ICtab(m2,m2a,m2b, type="AICc", logLik = T) 

## Sos Demografi
mydata$Kjønn <- as.factor(mydata$Kjønn)
mydata$q8_1Utdanning <- as.factor(mydata$q8_1Utdanning)
m4a <- polr(q4_10~ ArtTilstede + Alder, mydata, Hess =T) 
m4b <- polr(q4_10~ ArtTilstede + Kjønn, mydata, Hess =T)
m4c <- polr(q4_10~ ArtTilstede + q8_1Utdanning, mydata, Hess =T) # Bedre 
bbmle::ICtab(m2a,m4a,m4b,m4c, type="AICc", logLik = T) 
m4d <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder, mydata, Hess =T)
m4e <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Kjønn, mydata, Hess =T)
m4f <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn, mydata, Hess =T) # Bedre
# QUESTION: Skal vi ta velge m4f her? Det er vel riktig siden den er best og 2.5 bedre enn enklere modell. 
bbmle::ICtab(m4c,m4d,m4e,m4f, type="AICc", logLik = T)
m4g <- polr(q4_10~ q8_1Utdanning + Alder + Kjønn, mydata, Hess =T)
bbmle::ICtab(m4f,m4g,type="AICc", logLik = T)
# QUESTION: Her er m3c bare 1.6 "dårligere" enn m4a. og har én df mindre. Da tar velger vi å beholde m3c, ikke sant? 
#m4d <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q8_1Utdanning + Alder, mydata, Hess =T) # Bedre
 


### Jakttradisjoner  
m5a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_10, mydata, Hess =T)
m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T) # Bedre
bbmle::ICtab(m4d,m5a,m5b, type="AICc", logLik = T) 
# QUESTION: modellen blir bedre, men kan vi inkludere q2_11.1 som den er?


### Beitedyr
names(mydata)

mydata$SausluppetFylke <- as.numeric(mydata$SausluppetFylke)
hist(mydata$SausluppetFylke)
hist(log(mydata$SausluppetFylke))

mydata$Sautetthet <- as.numeric(mydata$Sautetthet)
hist(mydata$Sautetthet)
hist(log(mydata$Sautetthet))

mydata$Tapt.saulam.fylke <- as.numeric(mydata$Tapt.saulam.fylke)
hist(mydata$Tapt.saulam.fylke)
hist(log(mydata$Tapt.saulam.fylke))

mydata$Saulamtapprosent.F <- as.numeric(mydata$Saulamtapprosent.F)
hist(mydata$Saulamtapprosent.F)
hist(log(mydata$Saulamtapprosent.F))

m6a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + SausluppetFylke , mydata, Hess =T)
m6b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Sautetthet, mydata, Hess =T) 
m6c <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Tapt.saulam.fylke, mydata, Hess =T) 
m6d <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Saulamtapprosent.F, mydata, Hess =T) 

bbmle::ICtab(m5b,m6a,m6b,m6c,m6d, type="AICc", logLik = T) 
# QUESTION: Kan vi bruke variablene som over? Dårlig fordeling og muligens rart å gjøre dem numeriske når det bare er 14 forskjellige numre. 
# må eventuelt prøve å finne data per kommune/beitelag? 


### Kan vi fjerne noe? 
m7a <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_3_5 + q8_1Utdanning + q2_11.1 + q2_7.5, mydata, Hess =T)
m7b <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q2_11.1 + q2_7.5, mydata, Hess =T)
m7c <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_3_5 + q2_11.1 + q2_7.5, mydata, Hess =T)
m7d <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_3_5 + q2_7.5, mydata, Hess =T)
names(mydata)
bbmle::ICtab(m6b,m7a,m7b,m7c,m7d, type="AICc", logLik = T) 
# QUESTION: Jeg regner med at jeg må gjoere alle variabler som skal vaere faktor til faktorvariabel manuelt? I saa fall, kan du se raskt over om jeg har gjort dette, og riktig, for alle som benyttes? 
# QUESTION: m7d er innenfor 2 dAICc-verdier. Vil det si at vi kan bruke m7c som best model?
# QUESTION: Eller kan man ikke begynne å fjerne variabler igjen? Må jeg da evt. gå tilbake å ta de bort tidligere i prosessen? (men blir ikke det også rart?!)
# for om jeg har gjort riktig, så skal q8_1Utdanning være med i modellen da den enklere modellen uten denne er 2.1 AIC dårligere. Men så, om man sjekker modellene nederst i proseessen så
# er modellen uten q8_1Utdanning bedre (innenfor 2 AIC)
# QUESTION: Vi kunne ha testet for effekten av q2_9 også (frykt), men i så fall får vi kanskje samme problem som med q3_1average?
# QUESTION: Det er nå flere variabler jeg har tatt ut som er "tett på" å være med i den beste modellen, slik som "q8_1Utdanning", "q2_11.1", "Alder", "ArtTilstede" 
# og dette er variabler jeg er sterkt interessert i, som det er viktig å diskutere. Hvordan forholder man seg til disse? for de er vel signifikante men akkurat ikke med i den beste modellen,
>>>>>>> afb3496acb8465bf9bddb21538c8180036800a89
# men så nærme at de bedre men innenfor 2 AIC (gjelder ikke alle disse)