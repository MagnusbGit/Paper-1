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

### i) Test tillit forskning - forklare tillit til rovviltforskning relatert til tillt forskere og forskning
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

### ii) Holdninger
m1gHoldning <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums, mydata, Hess =T)
bbmle::ICtab(m1g,m1gHoldning, type="AICc", logLik = T) 
# Endelig best model: 
# m1gHoldning <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums, mydata, Hess =T)



#### B: Test #### 

### i) rovdyr tilstedeværelse
#"RzoneTilstede"
#"ArtTilstede" 
mydata$qArtTilstede <- as.factor(mydata$ArtTilstede)
mydata$RzoneTilstede <- as.factor(mydata$RzoneTilstede)
m2 <- polr(q4_10~ 1, mydata, Hess = T)
m2a <- polr(q4_10~  ArtTilstede, mydata, Hess =T) # bedre
m2b <- polr(q4_10~ RzoneTilstede, mydata, Hess =T)
bbmle::ICtab(m2,m2a,m2b, type="AICc", logLik = T) 
# Beslutning: Tar med både m2a og ikke m2b siden de er veldig like. Enig?  


### ii) Sos Demografi
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
 

### iii) Jakttradisjoner  
mydata$q2_11.1 <- as.factor(mydata$q2_11.1)
m5a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_10, mydata, Hess =T)
m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T) # Bedre
bbmle::ICtab(m4d,m5a,m5b, type="AICc", logLik = T) 
#m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11, mydata, Hess =T) # Best av disse
# QUESTION: modellen blir bedre, men kan vi inkludere q2_11.1 som den er?


### iv) Beitedyr
names(mydata)

hist(mydata$SauLamGeit)
hist(log(mydata$SauLamGeit))
mydata$SauLamGeitLog <- log(mydata$SauLamGeit+1)

hist(mydata$TapSauLamGeit)
hist(log(mydata$TapSauLamGeit))
mydata$TapSauLamGeitLog <- log(mydata$TapSauLamGeit+1)

m6a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + SauLamGeitLog, mydata, Hess =T) 
m6b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + TapSauLamGeitLog, mydata, Hess =T) 
bbmle::ICtab(m5b,m6a,m6b, type="AICc", logLik = T) 
# Beslutning: Behold m5b. 





#### Interaksjoner? ####
# ArtTilstede ~ Alder
plot(ArtTilstede~Alder, data=mydata)
plot(Alder~ArtTilstede, data=mydata)

# ArtTilstede ~ Kjønn
plot(ArtTilstede~Kjønn, data=mydata)

# test for interaksjon ArtTilstede ~ Alder og  ArtTilstede ~ Kjønn
m7a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + ArtTilstede:Alder, mydata, Hess =T)
m7b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + ArtTilstede:Kjønn, mydata, Hess =T)
bbmle::ICtab(m5b,m7a,m7b, type="AICc", logLik = T) 

# ArtTilstede ~ Alder
plot(Kjønn~Alder, data=mydata)
plot(Alder~ArtTilstede, data=mydata)

# ArtTilstede ~ q2_11.1 
plot(ArtTilstede~q2_11.1, data=mydata)
m7e <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + ArtTilstede:q2_11.1, mydata, Hess =T)
bbmle::ICtab(m5b,m7e, type="AICc", logLik = T) 



#### A og B ####
mKomb1 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums + ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T)
mKomb2 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T)
mKomb3 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums + ArtTilstede + Alder + Kjønn + q2_11.1, mydata, Hess =T)
mKomb4 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums + ArtTilstede + q8_1Utdanning + Kjønn + q2_11.1, mydata, Hess =T)
mKomb5 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums + ArtTilstede + q8_1Utdanning + Alder + q2_11.1, mydata, Hess =T)
mKomb6 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums + ArtTilstede + q8_1Utdanning + Alder + Kjønn, mydata, Hess =T)
mKomb7 <- polr(q4_10~ q4_3 + q4_2 + q3_1sums + ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T)
mKomb8 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T)
mKomb9 <- polr(q4_10~ q4_3 + q4_7 + q3_1sums + ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T)
mKomb10 <- polr(q4_10~ q4_2 + q4_7 + q3_1sums + ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T)
mKomb11 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums + ArtTilstede + q8_1Utdanning + q2_11.1, mydata, Hess =T)
mKomb12 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums + q8_1Utdanning + Alder + q2_11.1, mydata, Hess =T)
# QUESTION: Blir ikke bedre om jeg fjerner en variabel. Så da er vel den mest beskrivende modellen som kan lages for både A og B den modellen som
# inkluderer alle variablene her?

bbmle::ICtab(m1gHoldning,m5b,mKomb1,mKomb2,mKomb3,mKomb4,mKomb5,mKomb6,mKomb7,mKomb8,mKomb9,mKomb10,mKomb11,mKomb12, type="AICc", logLik = T) 


# Interaksjoner i kombinasjon A og B?
# ArtTilstede ~ Alder
plot(q3_1sums~Alder, data=mydata)
plot(Alder~q3_1sums, data=mydata)
m7c <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 +q3_1sums + Alder:q3_1sums, mydata, Hess =T)
m7d <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1 + q3_1sums, mydata, Hess =T)
bbmle::ICtab(m5b,m7c,m7d, type="AICc", logLik = T) 



# - Til vurdering: Vi kunne ha testet for effekten av q2_9 også (frykt). Kan gjøre dette ved å summere, slik som vi gjorde for q3_1?
