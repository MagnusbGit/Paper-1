<<<<<<< HEAD
# Ordinal regression (proportional odds logistic regression - for multi-class ordered variables)

setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
mydata <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")
str(mydata)
table(mydata$q4_10)
str(mydata$q4_10)
mydata$q4_10 <- as.ordered(mydata$q4_10)
mydata$Kjønn <- as.factor(mydata$Kjønn)
mydata$q3_1b <- as.ordered(mydata$q3_1b) # obs! 4 betyr vet ikke. Skal den fjernes/endres? hvordan?

str(mydata$q3_1b)

# do this with all variables of interest. Maybe extracting interesting variables first? 

# Partition data - ser at man gjør dette, og forstår hvorfor, men kan vi gjøre det her? 
# hvordan blir det f.eks. når man har 5 fra hver kommune og tar bare 80 % ut uten å ta hensy til dette? 
ind <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.8,0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# The model - MASS
library(MASS)
m1 <- polr(q4_10~Alder+Kjønn, train, Hess=TRUE)
summary(m1)

# running model with more variables 
#variable <- c("Folkemengde", "BefTetthetKommune","q2_6","q2_7","q2_10","Sum.felt.hjortedyr","SausluppetFylke","Tapt.saulam.fylke","Saulamtapprosent.F","Felt.HjortElg.K", "Bearzone","Wolfzone","Wolverinezone", "Lynxzone","AntallRartZone", "bjørn","gaupe","jerv","ulv","Antallarter","ArtTilstede")
# spørsmål om en rekke variabler. 

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

=======
# Ordinal regression (proportional odds logistic regression - for multi-class ordered variables)

setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
mydata <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")
str(mydata)
table(mydata$q4_10)
str(mydata$q4_10)
mydata$q4_10 <- as.ordered(mydata$q4_10)
mydata$Kjønn <- as.factor(mydata$Kjønn)
mydata$q3_1b <- as.ordered(mydata$q3_1b) # obs! 4 betyr vet ikke. Skal den fjernes/endres? hvordan?

str(mydata$q3_1b)

# do this with all variables of interest. Maybe extracting interesting variables first? 

# Partition data - ser at man gjør dette, og forstår hvorfor, men kan vi gjøre det her? 
# hvordan blir det f.eks. når man har 5 fra hver kommune og tar bare 80 % ut uten å ta hensy til dette? 
ind <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.8,0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# The model - MASS
library(MASS)
m1 <- polr(q4_10~Alder+Kjønn, train, Hess=TRUE)
summary(m1)

# running model with more variables 
#variable <- c("Folkemengde", "BefTetthetKommune","q2_6","q2_7","q2_10","Sum.felt.hjortedyr","SausluppetFylke","Tapt.saulam.fylke","Saulamtapprosent.F","Felt.HjortElg.K", "Bearzone","Wolfzone","Wolverinezone", "Lynxzone","AntallRartZone", "bjørn","gaupe","jerv","ulv","Antallarter","ArtTilstede")
# spørsmål om en rekke variabler. 

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

>>>>>>> 342c8f2739d8f7f905339fe6e8e1c8091eedb16a
