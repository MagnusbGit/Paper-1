
# Ordinal regression (proportional odds logistic regression - for multi-class ordered variables)

setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
#mydata <-read.csv("~/DIV NINA/DatasetSp�rreunders�kelseSept2019.csv",header=T,sep=";",dec = "," ,na.strings = "")
mydata <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")

library(ggplot2)
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(tidyverse)


str(mydata)
table(mydata$q4_10)
lapply(mydata[,c("q4_10","Kjønn","q3_1b")], table)
str(mydata$q4_10)

ggplot(mydata[1:100,], aes(x = q4_10, y = Folkemengde)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(Kjønn ~ ArtTilstede , margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) # boxplot er vel uaktuelt naar man har 2110 resodenter (her tok jeg bare 100 av de for at det skulle bli leselig, og det kan man jo ikke gjore)


names(mydata)
# spørs om man bør endre navn på variabler som har norske bokstaver? Så ikke git lager kvalm med døm? i så fall må det endres i scriptet under. 
#mydata<- mydata %>% 
#  rename(
#    sex = Kjønn)
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

#Det er et poeng for � validere modellen, men tror vi kj�rer all data inn i modelleringen her siden vi ikke har s� mye data i utgangspunktet. Vi m�tte is�fall ha gjort et utvalg innenfor hver kommune ja (om vi tenker at respondentene har en n�stet i kommune). 
# Det kan i s�fall enkelt gj�res ved � kj�re funksjonen i linje 37 i en loop som indekserer p� kommune.
# Svar: Ok, kunne gjerne trengt hjelp til å sette opp dette? Aldri satt opp loop selv før. 


# The model - MASS
#legger til alle variabler vi �nsker � teste

# Tenker det er greit å bruke AIC som modelseleksjonskriterie her, kjenner du til gangen i det, eller skal jeg sette opp for deg?
# Svar: Jeg har gjort et slags forsøk, men trenger nok hjelp til dette, ja. 

# Full model: - obs: får noen warnings, som jeg trenger hjelp til å ordne. "Folkemengde" fungerer f.eks. ikke.  
m1 <- polr(q4_10~ Alder + Kjønn  + Folkemengde + BefTetthetKommune + Sum.felt.hjortedyr + SausluppetFylke + AntallRartZone + Wolfzone + AntallRArterF + ArtTilstede + q3_1b + q2_6.5 + q2_7.5 + q2_10 , mydata, Hess=TRUE) 

# Hva velger man som vurderingsgrunnlag til å fjerne variabler? p-verdien er jo veldig liten for samtlige?  

# Tester variabler for å se hvilke som ikke fungerer                               
#ma <- polr(q4_10~ Alder + Kjønn + BefTetthetKommune, mydata, Hess=TRUE) # Folkemengde fungerer ikke - får feilmelding på summary. 
#mb <- polr(q4_10~ Alder + Kjønn + BefTetthetKommune + q8_1Utdanning + q8_3Inntekt + q8_4Parti, mydata, Hess=TRUE)
#mc <- polr(q4_10~ Alder + Kjønn + BefTetthetKommune + Sum.felt.hjortedyr+ AntallRartZone+ Wolfzone + Antallarter + ArtTilstede, mydata, Hess=TRUE) # Inkludering av Sum.felt.hjortedyr gir warning message . In sqrt(diag(vc)) : NaNs produced
#md <- polr(q4_10~ Alder + Kjønn + q3_1b + q2_6.5 + q2_7.5 + q2_10 + BefTetthetKommune + Sum.felt.hjortedyr+ AntallRartZone+ Wolfzone + Antallarter + ArtTilstede, mydata, Hess=TRUE) # Inkludering av Sum.felt.hjortedyr gir warning message  + f�r warning message design appears to be rank-deficient, In sqrt(diag(vc)) : NaNs produced
#me <- polr(q4_10~ Alder + Kjønn + q3_1b + q2_6.5, mydata, Hess = TRUE)  # rank-deficient. 
#mf <- polr(q4_10~ Alder + Kjønn + q2_6.5, mydata, Hess = TRUE) 
#mg <- polr(q4_10~ Alder + Kjønn + Folkemengde, mydata, Hess = TRUE)
#range(mydata$Folkemengde)

#summary(ma)
#summary(mb)
#summary(mc)
#summary(mb)
#summary(md) 
#summary(me)

summary(m1)
summary(m1c)

# get coeficients 
(ctable <- coef(summary(mb)))
p <- pnorm(abs(ctable[,"t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
ctable
(ci <- confint(ma))
confint.default(ma)

# AIC 
AIC (ma)
AIC (mb)
AIC (mc)

# eksempel: 
m1a <- polr(q4_10~ Alder + Kjønn + Folkemengde+  BefTetthetKommune, train, Hess=TRUE) # Folkemengde fungerer ikke - får feilmelding på summary.  
AIC(m1a)
drop1(m1a) # naar jeg bruker type="F" saa faar jeg feilmelding. Hva gjoer jeg med dette? Har droppet denne her. 





# next steps - har ikke sett p� dette enn�. 
# p-value
#m1.coef <- data.frame(coef(summary(m1)))
#m1.coef$pval = round((pnorm(abs(m1.coef$t.value), lower.tail = FALSE) * 2),2)
#m1.coef

# prediciton 
#pred <- predict(m1, train[1:5,], type="prob")
#print(pred, digits = 3)

## Andre ting jeg ikke har sett p� enn� 
#install.packages("stargazer") ? 
#library(stargazer)
#stargazer(m1, type="html", out="m1.htm")

#a way to use the command "factor" 
#tab_q8_3Inntekt$q8_3Inntekt<- factor(tab_q8_3Inntekt$q8_3Inntekt, levels =c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","98","99"))

# visualising # 

# Hvilke visulariseringsprogrammer pleier du å bruke? ggplot?

# bruke Hmsic for aa sjekke proportion odds assumption? 


