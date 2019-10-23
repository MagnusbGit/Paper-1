
# Ordinal regression (proportional odds logistic regression - for multi-class ordered variables)

#setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
mydata <-read.csv("~/DIV NINA/DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";",dec = "," ,na.strings = "")
#mydata <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")

library(ggplot2)
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(tidyverse)

#str(mydata)
#table(mydata$q4_10)
#lapply(mydata[,c("q4_10","Kjønn","q3_1b")], table)
#str(mydata$q4_10)

#ggplot(mydata[1:100,], aes(x = q4_10, y = Folkemengde)) +
#  geom_boxplot(size = .75) +
#  geom_jitter(alpha = .5) +
#  facet_grid(Kjønn ~ ArtTilstede , margins = TRUE) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) # boxplot er vel uaktuelt naar man har 2110 resodenter (her tok jeg bare 100 av de for at det skulle bli leselig, og det kan man jo ikke gjore)


names(mydata)
# spørs om man bør endre navn på variabler som har norske bokstaver? Så ikke git lager kvalm med døm? i så fall må det endres i scriptet under. 
#Kan ofte vaere en fordel aa ungaa egene norske bokstaver ja, saan generelt
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
mydata$q3_1b[mydata$q3_1b==4] <- NA # does this work? 
is.na(mydata$q3_1b)

# Question: do this with all variables of interest. Maybe extracting interesting variables first? 
# running model with more variables 
#variable <- c("Folkemengde", "BefTetthetKommune","q2_6","q2_7","q2_10","Sum.felt.hjortedyr","SausluppetFylke","Tapt.saulam.fylke","Saulamtapprosent.F","Felt.HjortElg.K", "Bearzone","Wolfzone","Wolverinezone", "Lynxzone","AntallRartZone", "bj�rn","gaupe","jerv","ulv","Antallarter","ArtTilstede")
# sporsmaal om en rekke variabler. 

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
  test.data<-subset(data, subset=variable==i)
  cr<-with(test.data,chapmanRobson(Alder_ny_slope,lnN,1:max(test.data$Alder_ny_slope)))
  keep.Z<-cbind(keep.Z,cr$est)
}
keep.Z

#### The model - MASS ####
#legger til alle variabler vi �nsker � teste

# Tenker det er greit å bruke AIC som modelseleksjonskriterie her, kjenner du til gangen i det, eller skal jeg sette opp for deg?
# Svar: Jeg har gjort et slags forsøk, men trenger nok hjelp til dette videre. 

# Full model: - obs: får noen warnings, som jeg trenger hjelp til å ordne. "Folkemengde" fungerer f.eks. ikke.  
#Dataene trengs nok å skaleres eller sentres, se hvordan det ser ut med og uten en log-transformasjon feks

#m1 <- polr(q4_10~ Alder + Kjønn  + Folkemengde + BefTetthetKommune + Sum.felt.hjortedyr + SausluppetFylke + AntallRartZone + Wolfzone + AntallRArterF + ArtTilstede + q3_1b + q2_6.5 + q2_7.5 + q2_10 , mydata, Hess=TRUE) 
hist(mydata$Folkemengde)
hist(log(mydata$Folkemengde))
#mydata$Sum.felt.hjortedy <- 
  
  
m1 <- polr(q4_10~ Alder + Kjønn +log(Folkemengde) +BefTetthetKommune + log(Sum.felt.hjortedyr) + SausluppetFylke + AntallRartZone + Wolfzone + AntallRArterF + ArtTilstede + q3_1b + q2_6.5 + q2_7.5 + q2_10 , mydata, Hess=TRUE) 
summary(m1)
#Du får rank-deficient modell her fordi vi putter for mange variable inn for å forklare variasjonen. 

# Spørsmål Hva velger man som vurderingsgrunnlag til å fjerne variabler? p-verdien er jo veldig liten for samtlige?
## Svar: Det er litt forskjellige tilnærminger her, en måte er å sette opp alle mulige modeller for så å gjøre en model_average basert på disse
#så slutt-modellen er altså en sammensettning av alle modellene, men vekte basert på AIC-verdier, og hva disse modellene inneholder. 
#En anne måte er å sette opp en modell med alle variable vi tror gir mening, og bare oppgi denne. og en annen er igjen å finne den "enkleste" modellen basert på noen seleksjonsmetode.
#Det man kan gjøre er å sette opp feks 10 kandidatmodeller, som vi har trua på med tanke på vraiable-kombinasjoner, og så sette disse opp mot hverandre og sjekke forskjeller i AIC-verdier

# Tester variabler for å se hvilke som ikke fungerer                               
#ma <- polr(q4_10~ Alder + Kjønn + BefTetthetKommune, mydata, Hess=TRUE) # Folkemengde fungerer ikke - får feilmelding på summary. 
#mb <- polr(q4_10~ Alder + Kjønn + BefTetthetKommune + q8_1Utdanning + q8_3Inntekt + q8_4Parti, mydata, Hess=TRUE)
mc <- polr(q4_10~ log(Folkemengde)+Alder + Kjønn + BefTetthetKommune + AntallRartZone+ Wolfzone + Antallarter + ArtTilstede, mydata, Hess=TRUE) # Inkludering av Sum.felt.hjortedyr gir warning message . In sqrt(diag(vc)) : NaNs produced
#md <- polr(q4_10~ Alder + Kjønn + q3_1b + q2_6.5 + q2_7.5 + q2_10 + BefTetthetKommune + Sum.felt.hjortedyr+ AntallRartZone+ Wolfzone + Antallarter + ArtTilstede, mydata, Hess=TRUE) # Inkludering av Sum.felt.hjortedyr gir warning message  + f�r warning message design appears to be rank-deficient, In sqrt(diag(vc)) : NaNs produced
#me <- polr(q4_10~ Alder + Kjønn + q3_1b + q2_6.5, mydata, Hess = TRUE)  # rank-deficient. 
#mf <- polr(q4_10~ Alder + Kjønn + q2_6.5, mydata, Hess = TRUE) 
#mg <- polr(q4_10~ Alder + Kjønn + Folkemengde, mydata, Hess = TRUE)
#range(mydata$Folkemengde)

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


