# Ordinal regression (proportional odds logistic regression - for multi-class ordered variables)

setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
mydata <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")
str(mydata)
table(mydata$q4_10)
str(mydata$q4_10)
mydata$q4_10 <- as.ordered(mydata$q4_10)
mydata$Kjønn <- as.factor(mydata$Kjønn)
mydata$q3_1b <- as.ordered(mydata$q3_1b) # obs! 4 betyr vet ikke. Skal den fjernes/endres? hvordan? OG kan vi bruke rank-deficient variables? får warning message. 
str(mydata$q3_1b)
mydata$q3_1b[mydata$q3_1b==4] <- NA # does this work? 
is.na(mydata$q3_1b)
str(mydata$q2_6)

table(mydata$q2_6.5)
table(mydata$q2_7.5)  
str(mydata$q2_6.5)
str(mydata$q2_7.5)
mydata$q2_6.5 <- as.factor(mydata$q2_6.5)
mydata$q2_7.5 <- as.factor(mydata$q2_7.5)
mydata$q2_10 <- as.ordered(mydata$q2_10)



# do this with all variables of interest. Maybe extracting interesting variables first? 
# running model with more variables 
#variable <- c("Folkemengde", "BefTetthetKommune","q2_6","q2_7","q2_10","Sum.felt.hjortedyr","SausluppetFylke","Tapt.saulam.fylke","Saulamtapprosent.F","Felt.HjortElg.K", "Bearzone","Wolfzone","Wolverinezone", "Lynxzone","AntallRartZone", "bjørn","gaupe","jerv","ulv","Antallarter","ArtTilstede")
# spørsmål om en rekke variabler. 


# Partition data - ser at man gjør dette, og forstår hvorfor, men kan vi gjøre det her? 
# hvordan blir det f.eks. når man har 5 fra hver kommune og tar bare 80 % ut uten å ta hensy til dette? 
ind <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.8,0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# The model - MASS
library(MASS)
m1 <- polr(q4_10~ Alder + Kjønn + q3_1b + q2_6.5 + q2_7.5 + q2_10 + Folkemengde + BefTetthetKommune + Sum.felt.hjortedyr + SausluppetFylke + AntallRartZone + Wolfzone + Antallarter + ArtTilstede, train, Hess=TRUE)
str(mydata$Wolfzone)
mydata$Wolfzone <- as.factor (mydata$Wolfzone
tab_q8_3Inntekt$q8_3Inntekt<- factor(tab_q8_3Inntekt$q8_3Inntekt, levels =c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","98","99"))
                              
m1 <- polr(q4_10~ Alder + Kjønn  + Folkemengde + BefTetthetKommune, train, Hess=TRUE)
summary(m1)


# next steps
# p-value
m1.coef <- data.frame(coef(summary(m1)))
m1.coef$pval = round((pnorm(abs(m1.coef$t.value), lower.tail = FALSE) * 2),2)
m1.coef

# prediciton 
pred <- predict(m1, train[1:5,], type="prob")
print(pred, digits = 3)

## Andre ting jeg ikke har sett på ennå 
#install.packages("stargazer") ? 
#library(stargazer)
#stargazer(m1, type="html", out="m1.htm")

