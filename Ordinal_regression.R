
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
mydata$q3_1b[mydata$q3_1b==4] <- NA  
is.na(mydata$q3_1b)

# Question: do this with all variables of interest. Maybe extracting interesting variables first? 
# running model with more variables 
#variable <- c("Folkemengde", "BefTetthetKommune","q2_6","q2_7","q2_10","Sum.felt.hjortedyr","SausluppetFylke","Tapt.saulam.fylke","Saulamtapprosent.F","Felt.HjortElg.K", "Bearzone","Wolfzone","Wolverinezone", "Lynxzone","AntallRartZone", "bj�rn","gaupe","jerv","ulv","Antallarter","ArtTilstede")
# sporsmaal om en rekke variabler. 


#### Train data ####
#Det er et poeng for å validere modellen, men tror vi kjører all data inn i modelleringen her siden vi ikke har s� mye data i utgangspunktet. Vi m�tte is�fall ha gjort et utvalg innenfor hver kommune ja (om vi tenker at respondentene har en n�stet i kommune). 
# Det kan i s�fall enkelt gj�res ved � kj�re funksjonen i linje 37 i en loop som indekserer p� kommune.
# Svar: Ok, kunne gjerne trengt hjelp til å sette opp dette? Aldri satt opp loop selv før. 
# Oppgave jeg maa gjoere: Sett opp loop

##KM: HMMM, tror ikke jeg husker helt hva vi skulle sette opp loop på, er det blitt borte?
#loop kan settes opp på flere måter, men den enkleste (og tregeste hvis det er mye data) er en for-loop. Er det mye data så pleier jeg å bruke data.table-pakka, men den tar det ofte litt lengere tid å bli dus på
#generelt kan en for-loop for å indeksere på en faktor settes opp sånn
for(i in levels(faktor))
{                                          
  Shit that needs too be done
  
  eks: 
  test.data<-subset(data, subset=Value==i)
  cr<-with(test.data,chapmanRobson(Alder_ny_slope,lnN,1:max(test.data$Alder_ny_slope)))
  keep.Z<-cbind(keep.Z,cr$est)
}
keep.Z

#MB: Man kan vel gjoere slik for aa hente ut to rader fra samme kommuner: 
for(i in levels(mydata$Kommune))
{                                          
  test.data <- mydata %>% group_by(Kommune) %>% sample_n(size = 2)
}
#MB: skal man ha inn en test inn i denne, slik som under? (bortsett fra at jeg ikke helt forstod hvordan dette skulle gjoeres så den er helt feil saa langt)
#MB: men tror jeg trenger hjelp til aa forstaa hva vi oensker aa oppnaa
for(i in levels(mydata$Kommune))
{                                          
  test.data <- mydata %>% group_by(Kommune) %>% sample_n(size = 2)
  cr<-with(test.data, polr(q4_10,1:max(test.data$q4_10)))
  keep.Z<-cbind(keep.Z,cr$est)
}
keep.Z
# MB: ser ogsaa at det er mange forskjellige maater aa gjoere det paa.
# MB: .. som LOOCV og k-fold. Er det den sistnevte vi gjoer bare at vi passer paa aa alltid ha med f.eks. 3 av samme kommune i trainingsettet og 2 i test?

mydata2
test.data
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

#MB: Spørsmål Hva velger man som vurderingsgrunnlag til å fjerne variabler? p-verdien er jo veldig liten for samtlige?
## Svar: Det er litt forskjellige tilnærminger her, en måte er å sette opp alle mulige modeller for så å gjøre en model_average basert på disse
#så slutt-modellen er altså en sammensettning av alle modellene, men vekte basert på AIC-verdier, og hva disse modellene inneholder. 
#En anne måte er å sette opp en modell med alle variable vi tror gir mening, og bare oppgi denne. og en annen er igjen å finne den "enkleste" modellen basert på noen seleksjonsmetode.
#Det man kan gjøre er å sette opp feks 10 kandidatmodeller, som vi har trua på med tanke på vraiable-kombinasjoner, og så sette disse opp mot hverandre og sjekke forskjeller i AIC-verdier
#MB: Jeg tenkte foerst vi burde proeve aa selektere basert paa kandidatmodeller. Men er det ikke en del kritikk ift bruk av stepwise selection ogsaa? 
# MB: er det noe problem aa gjoere begge varianter som du nevner?

#### Alternative modeller ####                             
# SPM: Boer vi gruppere dem inn i temaer? Eller boer jeg velge ut den modellen jeg har best tro paa paa tvers av "tema"? 
#m1 <- polr(q4_10~ Alder + Kjønn + q3_1b + q2_6.5 + q2_7.5 + q2_10 + BefTetthetKommune + Sum.felt.hjortedyr+ AntallRartZone+ Wolfzone + Antallarter + ArtTilstede, mydata, Hess=TRUE) # Inkludering av Sum.felt.hjortedyr gir warning message  + f�r warning message design appears to be rank-deficient, In sqrt(diag(vc)) : NaNs produced
names(mydata)
# hva skjer om jeg gjoer q4
mSE <-polr(q4_10~ Alder + Kjønn + log(Folkemengde) + BefTetthetKommune + q8_1Utdanning + q8_2Arbeid + q8_3Inntekt + q8_4Parti, mydata, Hess =T)
mR <- polr(q4_10~ Bearzone + Wolfzone + Lynxzone + Wolverinezone + AntallRartZone +RzoneTilstede+bjørn+gaupe+jerv+ulv+Antallarter+ArtTilstede, mydata, Hess =T) # DROPP VARIABLER
mByland <- polr(q4_10~ Alder + Kjønn + log(Folkemengde) + BefTetthetKommune + q8_1Utdanning + q8_2Arbeid + q8_3Inntekt + q8_4Parti, mydata, Hess =T)
mJ <- polr(q4_10~ log(Sum.felt.hjortedyr) + q2_10 + q2_11.1, mydata, Hess =T) # Jegertradisjoner
mD <- polr(q4_10~+ q2_6.5 + q2_7.5 + q2_11.1, mydata, Hess =T) # J# Damage . OBS- SPM: Hva blir riktig å inkludere for q2_6 og q2_7, se spoerreundersoekelse
mJandD <- polr(q4_10~ log(Sum.felt.hjortedyr) + q2_10 + q2_11.1,q2_6.5 + q2_7.5 + q2_11.1, mydata, Hess =T) # Kan kanskje droppe denne som bare er en kombinasjon av to? For det dekkes uansett, eller?  
mAtt <- polr(factor(q4_10) ~ q3_1median + q3_2median + q6_average, mydata,Hess = T) # NB need to run OversiktPaper1 q3_2median before running this
mExp #model experience 
mTrust <- polr(q4_10~ q4_1 + q4_2+q4_3+q4_5+q4_6+q4_7+q4_8+q4_9, mydata, Hess =T) # SPM: Basert paa faktoranalysen kan vi vel ikke slaa sammen noen av disse variablene, evt. velge ut noen (i saa fall maa vel valget baseres paa hvilkne vi har mest tro paa basert på oekologi eller lignende? 

mkomb <- # 

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


