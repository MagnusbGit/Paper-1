#### LOADING DATA ####
setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
#mydata <-read.csv("~/DIV NINA/DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";",dec = "," ,na.strings = "")
mydata <- read.csv(file="DatasetSpørreundersøkelseOkt2019b.csv",header=T,sep=";")


#### PACKAGES #### 
library(ggplot2)
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(tidyverse)
library(lattice)
library(bbmle)


#### FACTORS ++ ####
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

# Regn ut q3_sum
mydata$q3_1a[mydata$q3_1a==4] <- 0
mydata$q3_1b[mydata$q3_1b==4] <- 0
mydata$q3_1c[mydata$q3_1c==4] <- 0
mydata$q3_1d[mydata$q3_1d==4] <- 0
mydata$q3_2a[mydata$q3_2a==4] <- 0
mydata$q3_2b[mydata$q3_2b==4] <- 0
mydata$q3_2c[mydata$q3_2c==4] <- 0
mydata$q3_2d[mydata$q3_2d==4] <- 0
RovviltsituasjonN<-mydata[,c("RESPID",c("q3_1a","q3_1b", "q3_1c", "q3_1d") )]
mydata$q3_1sums <- rowSums(RovviltsituasjonN[2:5], na.rm=FALSE)
head(mydata)



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
m1gHoldning <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums, mydata, Hess =T)
bbmle::ICtab(m1g,m1gHoldning, type="AICc", logLik = T) 
# Endelig best model: 
# m1gHoldning <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums, mydata, Hess =T)


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
# Beslutning: Tar med både m2a og ikke m2b siden de er veldig like. Enig?  


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
bbmle::ICtab(m4c,m4d,m4e,m4f, type="AICc", logLik = T)
#m4f <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn, mydata, Hess =T) # Best av disse
 


### Jakttradisjoner  
m5a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_10, mydata, Hess =T)
m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T) # Bedre
bbmle::ICtab(m4d,m5a,m5b, type="AICc", logLik = T) 
#m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11, mydata, Hess =T) # Best av disse
# QUESTION: modellen blir bedre, men kan vi inkludere q2_11.1 som den er?


### Beitedyr
names(mydata)
str(mydata$Sauperkm2)

hist(mydata$Sauperkm2)
hist(log(mydata$Sauperkm2))
mydata$Sauperkm2Log <- log(mydata$Sauperkm2) 

hist(mydata$SauLamGeit)
hist(log(mydata$SauLamGeit))
mydata$SauLamGeit <- log(mydata$SauLamGeit)

hist(mydata$TapSauLamGeit)
hist(log(mydata$TapSauLamGeit))
mydata$TapSauLamGeit <- log(mydata$TapSauLamGeit)


m6a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Sauperkm2Log, mydata, Hess =T)
m6b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Sautetthet, mydata, Hess =T) 
m6c <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Tapt.saulam.fylke, mydata, Hess =T) 
m6d <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + Saulamtapprosent.F, mydata, Hess =T) 

bbmle::ICtab(m5b,m6a,m6b,m6c,m6d, type="AICc", logLik = T) 
# Question: Se egen mail - får feilmelding 


### Kan vi fjerne noe? 
m7a <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_3_5 + q8_1Utdanning + q2_11.1 + q2_7.5, mydata, Hess =T)
m7b <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q2_11.1 + q2_7.5, mydata, Hess =T)
m7c <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_3_5 + q2_11.1 + q2_7.5, mydata, Hess =T) # Bedre
m7d <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_3_5 + q2_7.5, mydata, Hess =T)
names(mydata)
bbmle::ICtab(m6b,m7a,m7b,m7c,m7d, type="AICc", logLik = T) 


# - Til vurdering: Vi kunne ha testet for effekten av q2_9 også (frykt). Kan gjøre dette ved å summere, slik som vi gjorde for q3_1?
