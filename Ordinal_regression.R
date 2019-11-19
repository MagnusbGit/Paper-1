
# Ordinal regression (proportional odds logistic regression - for multi-class ordered variables)

#setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
#mydata <-read.csv("~/DIV NINA/DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";",dec = "," ,na.strings = "")
#mydata <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")

library(ggplot2)
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(tidyverse)
library(lattice)
#library(Rtools)
library(bbmle)

#str(mydata)
#table(mydata$q4_10)
#lapply(mydata[,c("q4_10","Kjønn","q3_1b")], table)
#str(mydata$q4_10)

#ggplot(mydata[1:100,], aes(x = q4_10, y = Folkemengde)) +
#  geom_boxplot(size = .75) +
#  geom_jitter(alpha = .5) +
#  facet_grid(Kjønn ~ ArtTilstede , margins = TRUE) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) # boxplot er vel uaktuelt naar man har 2110 resodenter (her tok jeg bare 100 av de for at det skulle bli leselig, og det kan man jo ikke gjore)

#### As faktor ####
#mydata<- mydata %>% 
#  rename(sex = Kjønn)
# making factors and ordered factors
mydata$q4_10 <- as.ordered(mydata$q4_10)
mydata$Kjønn <- as.factor(mydata$Kjønn)
mydata$q3_1b <- as.ordered(mydata$q3_1b) # obs! 4 betyr vet ikke. Skal den fjernes/endres? hvordan? OG kan vi bruke rank-deficient variables? før warning message. 
mydata$q2_6.5 <- as.factor(mydata$q2_6.5)
mydata$q2_7.5 <- as.factor(mydata$q2_7.5)
mydata$q2_10 <- as.ordered(mydata$q2_10)
mydata$AntallRartZone <- as.factor(mydata$AntallRartZone)
mydata$AntallRArterF <- as.factor(mydata$AntallRArterF) # Endret til hva som så ut til å være en gruppert versjon av variablen?
#mydata$Antallarter <- as.factor(mydata$Antallarter)

# TO DO: Grab q3_1- and q3_2averanges from OrdinalPaper1-script.  
names(mydata)

#### Train data ####

#Hente ut to rader fra samme kommuner: 

#for(i in levels(mydata$Kommune))
#{                                          
test.data <- mydata %>% group_by(Kommune) %>% sample_n(size = 2)
#}

## TO DO: lag modellen med teningsdata, og så bruke den modellen for å predikere for den andre delen (testdata). På den måten kan man se feks hvor generell modellen er,
##dvs hvor godt treffer den predikerte verdien for testdata i forhold til faktisk/målte verdier i testdata?

#for(i in levels(mydata$Kommune))
#{                                          
#  test.data <- mydata %>% group_by(Kommune) %>% sample_n(size = 2)
#  cr<-with(test.data, polr(q4_10,1:max(test.data$q4_10)))
#  keep.Z<-cbind(keep.Z,cr$est)
#}
#keep.Z
# MB: ser ogsaa at det er mange forskjellige maater aa gjoere det paa.
# MB: .. som LOOCV og k-fold. Er det den sistnevte vi gjoer bare at vi passer paa aa alltid ha med f.eks. 3 av samme kommune i trainingsettet og 2 i test?
## KM: Jepp, alltid mange måter å gjøre alt på ;-), men poenget er at man på en elelr annen måte får testet modellen sin. Om det da er et poeng at den skal være generell.
## Om den er ment for å være en mer beskrivende modell, så bruker man bare all data man har inn i å trene opp modellen, og beskriver resultatene utifra det.
str(test.data)
str(mydata)


#### Modeller ####
#legger til alle variabler vi �nsker � teste

#Dataene trengs nok å skaleres eller sentres, se hvordan det ser ut med og uten en log-transformasjon feks
#m1 <- polr(q4_10~ Alder + Kjønn  + Folkemengde + BefTetthetKommune + Sum.felt.hjortedyr + SausluppetFylke + AntallRartZone + Wolfzone + AntallRArterF + ArtTilstede + q3_1b + q2_6.5 + q2_7.5 + q2_10 , mydata, Hess=TRUE) 
hist(mydata$Folkemengde)
hist(log(mydata$Folkemengde))
#mydata$Sum.felt.hjortedy <- 
hist(mydata$Sum.felt.hjortedyr)
hist(log(mydata$Sum.felt.hjortedyr)) # er det mulig aa log-transforemre her, eller maa noe annet gjoeres?   

m1 <- polr(q4_10~ Alder + Kjønn +log(Folkemengde) +BefTetthetKommune + log(Sum.felt.hjortedyr) + SausluppetFylke + AntallRartZone + Wolfzone + AntallRArterF + ArtTilstede + q3_1b + q2_6.5 + q2_7.5 + q2_10 , mydata, Hess=TRUE) 
summary(m1)
#Du får rank-deficient modell her fordi vi putter for mange variable inn for å forklare variasjonen. 

## Svar: Det er litt forskjellige tilnærminger til å redusere variabler, en måte er å sette opp alle mulige modeller for så å gjøre en model_average basert på disse
#så slutt-modellen er altså en sammensettning av alle modellene, men vekte basert på AIC-verdier, og hva disse modellene inneholder. 
#En anne måte er å sette opp en modell med alle variable vi tror gir mening, og bare oppgi denne. og en annen er igjen å finne den "enkleste" modellen basert på noen seleksjonsmetode.
#Det man kan gjøre er å sette opp feks 10 kandidatmodeller, som vi har trua på med tanke på vraiable-kombinasjoner, og så sette disse opp mot hverandre og sjekke forskjeller i AIC-verdier
#MB: Jeg tenkte foerst vi burde proeve aa selektere basert paa kandidatmodeller. Men er det ikke en del kritikk ift bruk av stepwise selection ogsaa? 
## KM: mener her å sette opp modeller basert på antagelser fra litteratur/eller annen vitenskap. Så ikke selektere per variabel egentlig, men sette opp hele modeller man har trua på, og så teste disse i mellom seg.
## Har man feks kunnskap, eller en ide om at Kjønn og Alder uansett er viktig her, så er det ikke noe vits å teste ut dette på nytt
# kan gjøre begge varianter, men bare presenter en av dem. 



#######################
# Modellseleksjon AIC #
#######################
#setter opp et eksempel med bakgrunn  i variablene som er med i modellene under (og med en grunnløs (?) antagelse om at kjønn og alder har alltid noe å si)
names(mydata)

# Kunn effekt av demografi (dvs ingen effekt av variable)

m0 <- polr(q4_10~ Alder + Kjønn, mydata, Hess=TRUE)

### Effekt av tilsetdeværelse av rovdyrarter 

m1 <- polr(q4_10~ Alder + Kjønn + ArtTilstede, mydata, Hess=TRUE)
 
m1a <- polr(q4_10~ Alder + Kjønn + ArtTilstede, mydata, Hess=TRUE)
m1b <- polr(q4_10~ Alder + Kjønn + q3_1average, mydata, Hess=TRUE)
m1c <- polr(q4_10~ Alder + Kjønn + q3_2average, mydata, Hess=TRUE)
# Effekt av tilsetdeværelse av rovdyrzone
m1d <- polr(q4_10~ Alder + Kjønn + ArtTilstede + RzoneTilstede, mydata, Hess=TRUE)
m1e <- polr(q4_10~ Alder + Kjønn+ ArtTilstede + Wolfzone, mydata, Hess=TRUE)

# Antagligvis så vil effekten av artilsetdeværelse variere med alder (bare ment hypotetisk)

m2 <- polr(q4_10~ Alder*ArtTilstede + Kjønn , mydata, Hess=TRUE)
m2b <- polr(q4_10~ Alder*q3_1average + kjønn, mydata, Hess=TRUE)
### Sosioøkonomiske effekter

m3 <- polr(q4_10~ Alder + Kjønn + ArtTilstede+q8_1Utdanning+q8_3Inntekt, mydata, Hess=TRUE)

### Sosioøkonomiske og demografi
m4 <- polr(q4_10~ Alder + Kjønn + log(Folkemengde) + BefTetthetKommune + q8_1Utdanning + q8_4Parti, mydata, Hess =T)

### Rovdyr tilstede

m5 <- polr(q4_10~ Alder + Kjønn + ArtTilstede+q8_1Utdanning+q8_3Inntekt, mydata, Hess=TRUE)

### Attitude rovdyr

m6 <- polr(q4_10~ Alder + Kjønn + ArtTilstede + q8_1Utdanning+q8_3Inntekt, mydata, Hess=TRUE)

### Jakttradisjoner 

m7a <- polr(q4_10~ Alder + Kjønn + ArtTilstede + q2_10 + q2_11.1 + q2_11.3 , mydata, Hess=TRUE)

m7b <- polr(q4_10~ log(Sum.felt.hjortedyr) + q2_10 + q2_11.1 + q2_6.5 + q2_7.5 + q2_11.1, mydata, Hess =T) # Kan kanskje droppe denne som bare er en kombinasjon av to? For det dekkes uansett, eller?  

### attitude and experience damage + hunting

m8 <- polr(q4_10~ Alder + Kjønn + ArtTilstede + q2_1.5 + q2_2.5 + q2_3.5 + q2_6.5 + q2_7.5 + log(Sum.felt.hjortedyr), mydata, Hess=TRUE)

### Tillit 
m9 <- polr(q4_10~ q4_1 + q4_2+q4_3+q4_5+q4_6+q4_7+q4_8+q4_9, mydata, Hess =T) # SPM:hvis vi inkluderer alle tillitsspørsmålene saa faar vi veldig lav AICc. Men gir vel ikke mening aa gjoere saant? Basert paa faktoranalysen kan vi vel ikke slaa sammen noen av disse variablene, evt. velge ut noen (i saa fall maa vel valget baseres paa hvilkne vi har mest tro paa basert på oekologi eller lignende? 
m9b <- polr(q4_10~ q4_1 + q4_2+q4_3+q4_5+q4_6+q4_7+q4_8+q4_9, mydata, Hess =T) # 
### Modell jeg har god tro paa paa tvers av temaer
m10 <- polr(q4_10~Alder+Kjønn+ArtTilstede+q2_6.5+q3_1average+q6_average,mydata,Hess=T) # 
#tilpasning ved å slette de man har minst tro på (ved aa jukse og se paa) - det skal man sikkert ikke gjoere, men det eneste jeg fikk ut at det var at kjoenn kan fjernes fra denne modellen
m10b <- polr(q4_10~Alder+ArtTilstede+q2_6.5+q3_1average+q6_average,mydata,Hess=T) # Kjoenn boer bort
m10c <- polr(q4_10~Alder+q2_6.5+q3_1average+q6_average,mydata,Hess=T)
m10d <- polr(q4_10~Alder+ArtTilstede+q2_6.5+q3_1average,mydata,Hess=T)

m10b2 <- polr(q4_10~Alder+ArtTilstede+q2_6.5+q3_1average+q6_average + q4_3,mydata,Hess=T) 
m10b3 <- polr(q4_10~Alder+ArtTilstede+q2_6.5+q3_1average+q4_3,mydata,Hess=T) 
m10b4 <- polr(q4_10~Alder+ArtTilstede+q2_6.5+q3_1average+q4_3+q4_8+q4_9,mydata,Hess=T) #hvis vi isteden legger til variabler vi vet korrelerer faar vi bedre modell, men gir kanskje ikke mye mening aa teste. 
m10b5 <- polr(q4_10~Alder+q3_1average+q4_3+q4_8+q4_9,mydata,Hess=T) 

m11 <- polr(q4_10~Alder+ArtTilstede+q2_6.5+q3_1average + q4_3,mydata,Hess=T)
m11 <- polr(q4_10~Alder+ArtTilstede+q2_6.5+q3_1average,mydata,Hess=T)
m11 <- polr(q4_10~Alder+ArtTilstede+q2_6.5 + q4_3,mydata,Hess=T)
m11 <- polr(q4_10~Alder+ArtTilstede+q3_1average + q4_3,mydata,Hess=T)

##OSV, osv
# try to reduce this particular model, but only get higher dAICC:


# Se på forskjeller mellom modeller
bbmle::ICtab(m0,m1,m2,m3, type="AICc", logLik = T)
bbmle::ICtab(m0,m1,m2,m3,m4,m5,m6,m7a,m7b,m8,m9,m10, type="AICc", logLik = T) # 
bbmle::ICtab(m0,m1,m2,m3,m10,m10b,m10c,m10d,m10b2,m10b3,m10b4,m10b5,m9,m11, type="AICc", logLik = T) # 

# m9 og m10b5 scorer veldig "bra", men er ikke gode modeller! 

## Lager tabeller
stargazer::stargazer(m9, m10b2,m10b3,type="html",
                     title="Regression Results",
                     intercept.bottom = F,
                     intercept.top = T,
                     ci = F, digits=2,
                     notes = "This is a caption.",
                     model.names = T,
                     single.row = T,
                     out="C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1/modeller.doc")##mange flere muligheter på layout her, sjekk ?stargazer

stargazer::stargazer(m2,m10b3,m10b4,m10b5,type="html",
                     title="Regression Results",
                     intercept.bottom = F,
                     intercept.top = T,
                     ci = F, digits=2,
                     notes = "This is a caption.",
                     model.names = T,
                     single.row = T,
                     out="C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1/modeller.doc")##mange flere muligheter på layout her, sjekk ?stargazer


#### Alternative modeller ####                             
# SPM: Boer vi gruppere dem inn i temaer? Eller boer jeg velge ut den modellen jeg har best tro paa paa tvers av "tema"? 
#m1 <- polr(q4_10~ Alder + Kjønn + q3_1b + q2_6.5 + q2_7.5 + q2_10 + BefTetthetKommune + Sum.felt.hjortedyr+ AntallRartZone+ Wolfzone + Antallarter + ArtTilstede, mydata, Hess=TRUE) # Inkludering av Sum.felt.hjortedyr gir warning message  + f�r warning message design appears to be rank-deficient, In sqrt(diag(vc)) : NaNs produced
# hva skjer om jeg gjoer q4
#mSE <-polr(q4_10~ Alder + Kjønn + log(Folkemengde) + BefTetthetKommune + q8_1Utdanning + q8_2Arbeid + q8_3Inntekt + q8_4Parti, mydata, Hess =T)
#mR <- polr(q4_10~ Bearzone + Wolfzone + Lynxzone + Wolverinezone + AntallRartZone +RzoneTilstede+bjørn+gaupe+jerv+ulv+Antallarter+ArtTilstede, mydata, Hess =T) # DROPP VARIABLER
#mByland <- polr(q4_10~ Alder + Kjønn + log(Folkemengde) + BefTetthetKommune + q8_1Utdanning + q8_2Arbeid + q8_3Inntekt + q8_4Parti, mydata, Hess =T)
#mJ <- polr(q4_10~ log(Sum.felt.hjortedyr) + q2_10 + q2_11.1, mydata, Hess =T) # Jegertradisjoner
#mD <- polr(q4_10~+ q2_6.5 + q2_7.5 + q2_11.1, mydata, Hess =T) # J# Damage . OBS- SPM: Hva blir riktig å inkludere for q2_6 og q2_7, se spoerreundersoekelse
#mJandD <- polr(q4_10~ log(Sum.felt.hjortedyr) + q2_10 + q2_11.1,q2_6.5 + q2_7.5 + q2_11.1, mydata, Hess =T) # Kan kanskje droppe denne som bare er en kombinasjon av to? For det dekkes uansett, eller?  
#mAtt <- polr(factor(q4_10) ~ q3_1median + q3_2median + q6_average, mydata,Hess = T) # NB need to run OversiktPaper1 q3_2median before running this
#mExp #model experience 
#mTrust <- polr(q4_10~ q4_1 + q4_2+q4_3+q4_5+q4_6+q4_7+q4_8+q4_9, mydata, Hess =T) # SPM: Basert paa faktoranalysen kan vi vel ikke slaa sammen noen av disse variablene, evt. velge ut noen (i saa fall maa vel valget baseres paa hvilkne vi har mest tro paa basert på oekologi eller lignende? 

# div jeg ikke har tatt stilling til ennaa 
#ma <- polr(q4_10~ Alder + Kjønn + BefTetthetKommune, mydata, Hess=TRUE) # Folkemengde fungerer ikke - får feilmelding på summary. 
#mb <- polr(q4_10~ Alder + Kjønn + BefTetthetKommune + q8_1Utdanning + q8_3Inntekt + q8_4Parti, mydata, Hess=TRUE)
#mc <- polr(q4_10~ log(Folkemengde)+Alder + Kjønn + BefTetthetKommune + AntallRartZone+ Wolfzone + Antallarter + ArtTilstede, mydata, Hess=TRUE) # Inkludering av Sum.felt.hjortedyr gir warning message . In sqrt(diag(vc)) : NaNs produced
#md <- polr(q4_10~ Alder + Kjønn + q3_1b + q2_6.5 + q2_7.5 + q2_10 + BefTetthetKommune + Sum.felt.hjortedyr+ AntallRartZone+ Wolfzone + Antallarter + ArtTilstede, mydata, Hess=TRUE) # Inkludering av Sum.felt.hjortedyr gir warning message  + f�r warning message design appears to be rank-deficient, In sqrt(diag(vc)) : NaNs produced
#me <- polr(q4_10~ Alder + Kjønn + q3_1b + q2_6.5, mydata, Hess = TRUE)  # rank-deficient. 
#mf <- polr(q4_10~ Alder + Kjønn + q2_6.5, mydata, Hess = TRUE) 
#mg <- polr(q4_10~ Alder + Kjønn + Folkemengde, mydata, Hess = TRUE)
#range(mydata$Folkemengde)

# Ser paa en av modellene
summary(mSE)
(ctable <- coef(summary(mSE)))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
ctable
(ci <- confint(mc))
confint.default(mc)
# SPM: Forstaar ikke helt utskriftene her. Er ikke p-verdiene signifikante for alle men enkelte har har CI som inkluderer 0? 


#### averaging ####


#### selection ####





 

#### EKSTRA UNDER - ikke byr oss enda ####

# bruker mc videre for å sette opp prosessen bare. 
#summary(ma)
#summary(mb)
summary(mc)
#summary(mb)
#summary(md) 
#summary(me)
#summary(m1)

# eksempel: 


#### get coeficients ####
(ctable <- coef(summary(mc)))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
ctable
(ci <- confint(mc))
confint.default(mc)


#### AIC #### 
AIC(mc)
drop1(mc, type=F)  
mc2 <- polr(q4_10~ AntallRartZone+ Wolfzone + ArtTilstede, mydata, Hess=TRUE)# remove variables that are more than 2 AIC-values larger than the smallest one. 
drop1(mc2, type=F)
# Kommentar: Dette kan ikke være riktig måte å gjøre det på. Hva gjør egt. drop1? 
## Nei, ikke helt stuerent, men se kommentar over. Sett opp 10-15 modeller som gir mening, og gjerne en 0modell uten variable, og test mellom disse



