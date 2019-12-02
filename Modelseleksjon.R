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

# Test tillit forskning - forklare tillit til rovviltforskning relatert til tillt forskere og forskning
# Forklaring på variablene
# 1= forskning er viktig
# 2=alvorlig mister tro på forskning
# 3=tillit generell forskning
# 4 = tillit medisinsk forsk
# 5 = Tillit klimaforskning
# 6 = Forskere har høy ekspertise
# 7 = forskere har høyr troverdighet
m9 <- polr(q4_10~ 1,mydata, Hess =T)  # Best
m9a <- polr(q4_10~ q4_3,mydata, Hess =T) 
m9b <- polr(q4_10~ q4_4,mydata, Hess =T)
m9c <- polr(q4_10~ q4_5,mydata, Hess =T)
bbmle::ICtab(m9,m9a,m9b,m9c, type="AICc", logLik = T) # 

m9d <- polr(q4_10~ q4_3 + q4_1, mydata, Hess =T)
m9e <- polr(q4_10~ q4_3 + q4_2, mydata, Hess =T) # Bedre 
bbmle::ICtab(m9a,m9d,m9e, type="AICc", logLik = T) 

m9f <- polr(q4_10~ q4_3 + q4_2 + q4_6, mydata, Hess =T)
m9g <- polr(q4_10~ q4_3 + q4_2 + q4_7, mydata, Hess =T) # Bedre
bbmle::ICtab(m9e,m9f,m9g, type="AICc", logLik = T) 

### tilstedeværelse
#"RzoneTilstede"
#"ArtTilstede" 
m2a <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede, mydata, Hess =T) # bedre
m2b <- polr(q4_10~ q4_3 + q4_2 + q4_7 + RzoneTilstede, mydata, Hess =T)
bbmle::ICtab(m9e,m2a,m2b, type="AICc", logLik = T) 

### attitude
# Holdning: (mydata$q3_1average) - TODO Kom tilbake til denne - DEN MÅ FIKSES OM DEN SKAL BRUKES
# m3 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede +mydata$q3_1average , mydata, Hess =T) # best

#m3 <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_3 , mydata, Hess =T) 
Z<-mydata[,c("q3_3_3", "q3_3_4","q3_3_5","q3_3_6","q3_3_7","q3_3_8","q3_3_9")]
ggpairs(Z)
m3a <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_3 , mydata, Hess =T) 
m3b <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_4 , mydata, Hess =T) 
m3c <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 , mydata, Hess =T) # bedre
m3d <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_6 , mydata, Hess =T) 
m3e <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_7 , mydata, Hess =T) 
m3f <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_8 , mydata, Hess =T) 
m3g <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_9 , mydata, Hess =T) 
bbmle::ICtab(m9g,m3a,m3b,m3c, m3d,m3e,m3f,m3g, type="AICc", logLik = T) 


### Sos Demografi
m4a <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + Alder, mydata, Hess =T) 
m4b <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + Kjønn, mydata, Hess =T)
m4c <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q8_1Utdanning, mydata, Hess =T) # bedre
bbmle::ICtab(m3c,m4a,m4b,m4c, type="AICc", logLik = T) 
# QUESTION: Her er m3c bare 1.7 "dårligere" enn m4a. og har én df mindre. Da tar velger vi m3c, ikke sant? 

m4d <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q8_1Utdanning + Alder, mydata, Hess =T) # Bedre
bbmle::ICtab(m4c,m4d, type="AICc", logLik = T) 

### Jakttradisjoner  
m5a <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q8_1Utdanning + q2_10 , mydata, Hess =T)
m5b <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q8_1Utdanning + q2_11.1 , mydata, Hess =T) # Bedre
bbmle::ICtab(m4d,m5a,m5b, type="AICc", logLik = T) 


# QUESTION: modellen blir bedre, men kan vi inkludere q2_11.1 som den er?

### attitude and experience damage + hunting
m6a <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q8_1Utdanning + q2_11.1 + q2_6.5, mydata, Hess =T)
m6b <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q8_1Utdanning + q2_11.1 + q2_7.5, mydata, Hess =T) # Bedre
bbmle::ICtab(m5b,m6a,m6b, type="AICc", logLik = T) 
# QUESTION: modellen blir bedre, men kan vi inkludere q2_7.5?

# Kan vi fjerne noe? 
m7a <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_3_5 + q8_1Utdanning + q2_11.1 + q2_7.5, mydata, Hess =T)
m7b <- polr(q4_10~ q4_3 + q4_2 + q4_7 + ArtTilstede + q3_3_5 + q2_11.1 + q2_7.5, mydata, Hess =T)
m7c <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_3_5 + q2_11.1 + q2_7.5, mydata, Hess =T)
names(mydata)
bbmle::ICtab(m6b,m7a,m7b,m7c, type="AICc", logLik = T) 
# QUESTION: m7c er innenfor 2 dAICc-verdier. Vil det si at vi kan bruke m7c som best model?
# QUESTION: Eller kan man ikke begynne å fjerne variabler igjen? Må jeg da evt. gå tilbake å ta de bort tidligere i prosessen? (men blir ikke det også rart?!)

# QUESTION: Vi kunne ha testet for effekten av q2_9 også (frykt), men i så fall får vi kanskje samme problem som med q3_1average?