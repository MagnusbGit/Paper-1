# Course in analyzing survey data in R - DATACAMP
detach(package:dplyr)
detach(package:plyr)
library(plyr)
library(dplyr)
library(grid)
library(nnet)
library(coin)
library(boot)
#library(simpleboot) # NO
library(knitr)
library(ggplot2)
library(dplyr)
library(AICcmodavg)
#library(polycor) #NO
library(likert)
library(MASS)
#library(ordinal) # NO
library(likert)
library(simpleboot)
library(Hmisc)
library(reshape2)
library(foreign)
library(psych)
library(FSA)
library(lattice)
library(plyr)
library(rcompanion)
library(car)
library(tidyverse)
library(sqldf)

# Load data
setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
Arbeidsfil1 <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")
head(Arbeidsfil1)
#attach(Arbeidsfil1)
names(Arbeidsfil1)
detach(package:Hmisc)


######                                    ######
# OVersikt over demografi og økonomi #
######                                    ######
# Oversikt innbyggertallgruppe
tab_innbyggertallgruppe <- Arbeidsfil1 %>%
  group_by(Innbyggertallgruppe) %>%
  summarize(Freq = n()) %>%
  mutate(Prop = Freq/sum(Freq)) %>%
  arrange(desc(Prop))
tab_innbyggertallgruppe

ggplot(data = tab_innbyggertallgruppe, mapping = aes(x = Innbyggertallgruppe, y = Prop)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits = tab_innbyggertallgruppe$Innbyggertallgruppe) # Labels layer omitted


# Oversikt yrke
tab_q8_2Arbeid <- Arbeidsfil1 %>%
  group_by(q8_2Arbeid) %>%
  summarize(Freq = n()) %>%
  mutate(Prop = Freq/sum(Freq)) %>%
  arrange(desc(Prop))
tab_q8_2Arbeid

ggplot(data = tab_q8_2Arbeid, mapping = aes(x = q8_2Arbeid, y = Prop)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits = tab_q8_2Arbeid$q8_2Arbeid) # Labels layer omitted

# Oversikt kj?nn
names(Arbeidsfil1)
tab_Kjønn <- Arbeidsfil1 %>%
  group_by(Kjønn) %>%
  summarize(Freq = n()) %>%
  mutate(Prop = Freq/sum(Freq)) %>%
  arrange(desc(Prop))
tab_Kjønn

ggplot(data = tab_Kjønn, mapping = aes(x = Kjønn, y = Prop)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits = tab_Kjønn$Kjønn) # Labels layer omitted

# Oversikt L?nn
#Arbeidsfil1$q8_3Inntekt <- factor(Arbeidsfil1$q8_3Inntekt, levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","98","99"))
tab_q8_3Inntekt <- Arbeidsfil1 %>%
  group_by(q8_3Inntekt) %>%
  summarize(Freq = n()) %>%
  mutate(Prop = Freq/sum(Freq)) %>%
  arrange(desc(Prop))
tab_q8_3Inntekt

str(Arbeidsfil1)  
str(Arbeidsfil1$q8_3Inntekt)
unique(Arbeidsfil1$q8_3Inntekt)
tab_q8_3Inntekt$q8_3Inntekt<- factor(tab_q8_3Inntekt$q8_3Inntekt, levels =c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","98","99"))

ggplot(data = tab_q8_3Inntekt, mapping = aes(x = tab_q8_3Inntekt$q8_3Inntekt, y = Prop)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","98","99")) # Labels layer omitted


# Mulig det ikke er helt det du var ute etter her, men legger det inn som et ekempel på hvordan man kan forenkle litt (og jobbe på samme datasett):
ggplot(Arbeidsfil1, aes(Innbyggertallgruppe)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Svarprosent?")+
  scale_x_discrete(limits = c("-2499","2500-4900","5000-9999","10000-24999","25000-49999","50000-99999","10000+")) #10000+ mangler en null, og bør kanskje stå 1-2499 på første kattegori?

#Eventuelt hvis du også vil se fordeling mtp feks kjønn med det samme
ggplot(Arbeidsfil1, aes(Innbyggertallgruppe, group = Kjønn)) + 
  geom_bar(aes(y =  (..count..)/sum(..count..), fill = factor(Kjønn)), stat="count") + 
  scale_y_continuous(labels=scales::percent,expand = c(0, 0)) +
  scale_x_discrete(limits = c("-2499","2500-4900","5000-9999","10000-24999","25000-49999","50000-99999","10000+"))+ #10000+ mangler en null, og bør kanskje stå 1-2499 på første kattegori?
  scale_fill_discrete(name = "Kjønn", labels = c("Menn", "Kvinner"))+#sjekk om det ble riktig
  labs(y = "Prosent")+
  theme( #ksempler på å "pynte" litt
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,legend.title = element_text(size=14, face="bold")
    ,legend.text = element_text(size=14)
    ,axis.title.x=element_text(size=14) #kan gjøre det samme for y også, samt for tallene på aksen
  )+
  theme(axis.line = element_line(color = 'black'))

#Samme for lønnstrinn
ggplot(Arbeidsfil1, aes(Innbyggertallgruppe, group = q8_3Inntekt)) + 
  geom_bar(aes(y =  (..count..)/sum(..count..), fill = factor(q8_3Inntekt)), stat="count") + 
  scale_y_continuous(labels=scales::percent,expand = c(0, 0)) +
  scale_x_discrete(limits = c("-2499","2500-4900","5000-9999","10000-24999","25000-49999","50000-99999","10000+"))+ #10000+ mangler en null, og bør kanskje stå 1-2499 på første kattegori?
  scale_fill_discrete(name = "Inntekt")+
  labs(y = "Prosent")+
  theme( #ksempler på å "pynte" litt
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,legend.title = element_text(size=14, face="bold")
    ,legend.text = element_text(size=14)
    ,axis.title.x=element_text(size=14) #kan gjøre det samme for y også, samt for tallene på aksen
  )+
  theme(axis.line = element_line(color = 'black'))

#eller forenklet
ggplot(Arbeidsfil1, aes(factor(q8_3Inntekt))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Prosent")+
  theme( #ksempler på å "pynte" litt
    panel.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_rect(fill=NA,linetype = "dashed", colour = "black")
    ,axis.title.y=element_text(size=14) #kan gjøre det samme for x også, samt for tallene på aksen
  )+
  theme(axis.line = element_line(color = 'black'))



#str(Arbeidsfil1$q8_3Inntekt)
#unique(Arbeidsfil1$q8_3Inntekt)
#Arbeidsfil1$q8_3Inntekt <- factor(Arbeidsfil1$q8_3Inntekt, levels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","98","99"))



#positionsI <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","98","99")
# Oversikt Aldersgrupe
#names(Arbeidsfil1)
#tab_Aldersgruppe <- Arbeidsfil1 %>%
#  group_by(Aldersgruppe) %>%
#  summarize(Freq = n()) %>%
#  mutate(Prop = Freq/sum(Freq)) %>%
#  arrange(desc(Prop))
#tab_Aldersgruppe

#positions <- c("80+", "67-79", "50-66","30-49","15-29") # to specify the order of the bars in the plot
#p <- ggplot(theTable, aes(x = Position)) + scale_x_discrete(limits = positions)
#ggplot(data = tab_Aldersgruppe, mapping = aes(x = Aldersgruppe, y = Prop)) + 
#  geom_col() + 
#  coord_flip() + 
#  scale_x_discrete(limits = positions) # Labels layer omitted


##### 
# Oversikt over svar 
#####


# boxplot - Tillit sp?rsm?l q4_1, q4_4, q4_5 og q4_10 - ikke veldig informativ - forslag til endring? bare droppe boxplot kanskje. 
b_q4_1 <- Arbeidsfil1$q4_1
b_q4_4 <- Arbeidsfil1$q4_4
b_q4_5 <- Arbeidsfil1$q4_5
b_q4_10 <- Arbeidsfil1$q4_10
boxplot(b_q4_1,b_q4_4,b_q4_5,b_q4_10,
main="Tillit sp?rsm?l q4_1, q4_4, q4_5 og q4_10",
at = c(1,2,3,4), names = c("Generell","Medisin","Klima","Rovvilt"),las = 1,
col = c("grey"), horizontal = T,notch = TRUE)

# Make centered likert charts
Ptestdata<- Arbeidsfil1[,c("q4_1","q4_2","q4_3","q4_6","q4_7")]
Ptestdata[1:5] <- lapply(Ptestdata[1:5], factor, levels = 1:5)

# Create a likert object
Ptestdata_likert<-likert(Ptestdata[1:5])
plot(Ptestdata_likert, ordered = FALSE, group.order = names(Ptestdata[1:5]))
plot(Ptestdata_likert, ordered = FALSE, centered = FALSE, group.order = names(Ptestdata[1:5]))
plot(Ptestdata_likert, type = "heat",group.order = names(Ptestdata[1:5]))



#Forskning generelt
PtestdataGenerelt <- Arbeidsfil1[,c("q4_1","q4_2","q4_3")]

PtestdataGenerelt<- PtestdataGenerelt %>% 
  rename(
    Forskning_er_viktig = q4_1,
    Troen_pa_forskning = q4_2,
    Tillit_til_forkning.generelt = q4_3,
  )
PtestdataGenerelt[1:3] <- lapply(PtestdataGenerelt[1:3], factor, levels = 1:5)
PtestdataGenerelt_likert<-likert(PtestdataGenerelt[1:3])
plot(PtestdataGenerelt_likert, ordered = FALSE, centered = FALSE, group.order = names(PtestdataGenerelt[1:3]))


#spesifikk forskning
PtestdataSpesifikk <- Arbeidsfil1[,c("q4_3","q4_4","q4_5","q4_10")]
PtestdataSpesifikk<- PtestdataSpesifikk %>% 
  rename(
    Tillit_til_forskning_generelt = q4_3,
    Tillit_til_medisinsk_forskning = q4_4,
    Tillit_til_klimaforskning = q4_5,
    Tillit_til_rovviltforskning = q4_10,
  )
PtestdataSpesifikk[1:4] <- lapply(PtestdataSpesifikk[1:4], factor, levels = 1:5)
PtestdataSpesifikk_likert<-likert(PtestdataSpesifikk[1:4])
plot(PtestdataSpesifikk_likert, ordered = FALSE, centered = FALSE, group.order = names(PtestdataSpesifikk[1:4]))


# Generell forskning og rovviltforskning
PtestdataGenRov <- Arbeidsfil1[,c("q4_3","q4_6","q4_7","q4_10","q4_8","q4_9","q4_11")]
PtestdataGenRov<- PtestdataGenRov %>% 
  rename(
    Tillit_til_forskning_generelt = q4_3,
    Gen.forsk_hoy_ekspertise = q4_6,
    Gen.forsk_hoy_troverdighet = q4_7,
    Rov.forsk_hoy_ekspertise = q4_8,
    Rov.forsk_hoy_troverdighet = q4_9,
    Tillit_til_rovviltforskning = q4_10,
    Tillit_til_rovviltforskning_som_gen.forsk = q4_11,
  )
PtestdataGenRov[1:7] <- lapply(PtestdataGenRov[1:7], factor, levels = 1:5)
PtestdataGenRov_likert<-likert(PtestdataGenRov[1:7])
plot(PtestdataGenRov_likert, ordered = FALSE, centered = FALSE, group.order = names(PtestdataGenRov[1:7]))


## Example of type of plotting
# plot with grouping
# Create likert object with groupings included
Ptestdata2 <- Arbeidsfil1[,c("Innbyggertallgruppe","q4_1","q4_2","q4_3","q4_6","q4_7")]
Ptestdata2[2:6] <- lapply(Ptestdata2[2:6], factor, levels = 1:5)
Ptestdata2$Innbyggertallgruppe<-factor(Ptestdata2$Innbyggertallgruppe, levels=c("-2499","2500-4900","5000-9999","10000-24999","25000-49999","50000-99999","10000+"))
str(Ptestdata2)

both_Ptestdata2_likert = likert(Ptestdata2[, c(2:6), drop = FALSE], grouping = Ptestdata2$Innbyggertallgruppe)
plot(both_Ptestdata2_likert, include.histogram = TRUE)
plot(both_Ptestdata2_likert, type = "density") # gir denne noe info? 

# sex
PtestdataKjønn <- Arbeidsfil1[,c("Kjønn","q4_1","q4_2","q4_3","q4_6","q4_7","q4_10","q4_11")]
PtestdataKjønn[2:7] <- lapply(PtestdataKjønn[2:7], factor, levels = 1:5)
PtestdataKjønn$Sex<-factor(PtestdataKjønn$Kjønn, levels=c("1","2"))
str(PtestdataKjønn)
both_PtestdataKjønn_likert = likert(PtestdataKjønn[, c(2:7), drop = FALSE], grouping = PtestdataKjønn$Kjønn)
plot(both_PtestdataKjønn_likert, include.histogram = TRUE) # SPM: hvordan endrer jeg rekkefølgen på spørsmålene? nå kommer de helt rart. 

# alder
#names(Arbeidsfil1)
#levels(Arbeidsfil1$Aldersgruppe)
#PtestdataAldersgruppe <- Arbeidsfil1[,c("Aldersgruppe","q4_1","q4_2","q4_3","q4_6","q4_7","q4_10","q4_11")]
#PtestdataAldersgruppe[2:8] <- lapply(PtestdataAldersgruppe[2:8], factor, levels = 1:5)
#PtestdataSex$Aldersgruppe<-factor(PtestdataSex$Aldersgruppe)
#str(PtestdataAldersgruppe)
## levels=c("15-29","30-49","50-66","67-79","80+")
#both_PtestdataAldersgruppe_likert = likert(PtestdataAldersgruppe[, c(2:8), drop = FALSE], grouping = PtestdataAldersgruppe$Aldersgruppe)
#plot(both_PtestdataAldersgruppe_likert, include.histogram = TRUE)

# Related to number of carnivore species zones
#PtestdataSex <- Arbeidsfil1[,c("Sex","q4_1","q4_2","q4_3","q4_4","q4_5","q4_6","q4_7","q4_8","q4_9","q4_10","q4_11")]
PtestdataRdyr <- Arbeidsfil1[,c("AntallRartZone","q4_3","q4_4","q4_5","q4_10","q4_11")]
ggplot(PtestdataRdyr, aes())
str(PtestdataRdyr)
PtestdataRdyr[2:6] <- lapply(PtestdataRdyr[2:6], factor, levels = 1:5)
PtestdataRdyr$AntallRartZone<-factor(PtestdataRdyr$AntallRartZone, levels=c("0","1","2","3","4"))
str(PtestdataRdyr$AntallRartZone)
both_PtestdataRdyr_likert = likert(PtestdataRdyr[, c(2:6), drop = FALSE], grouping = PtestdataRdyr$AntallRartZone)
plot(both_PtestdataRdyr_likert, include.histogram = TRUE)



agg_table <- sqldf::sqldf("select question, category, SUM(responses) as total from survey group by question, category")
summarized_table <- agg_table %>%
  group_by(question) %>%
  mutate(countT= sum(total)) %>%
  group_by(category, add=TRUE) %>%
  mutate(per=round(100*total/countT,2))


# With or without carnivore zones
PtestdataRdyrYN <- Arbeidsfil1[,c("Rzone","q4_3","q4_4","q4_5","q4_10","q4_11")]
PtestdataRdyrYN[2:6] <- lapply(PtestdataRdyrYN[2:6], factor, levels = 1:5)
PtestdataRdyrYN$Rzone<-factor(PtestdataRdyrYN$Rzone, levels=c("Ja","Nei"))
str(PtestdataRdyrYN$Rzone)
both_PtestdataRdyrYN_likert = likert(PtestdataRdyrYN[, c(2:6), drop = FALSE], grouping = PtestdataRdyrYN$Rzone)
plot(both_PtestdataRdyrYN_likert, include.histogram = TRUE)

# Attitude toward carnivores compared to With or withour carnivore zones - MUST IMORT ALSO Q_3-attitudes questions
names(Arbeidsfil1)
PtestdataRdyrYN <- Arbeidsfil1[,c("Rzone","q4_3","q4_4","q4_5","q4_10","q4_11")]
PtestdataRdyrYN[2:6] <- lapply(PtestdataRdyrYN[2:6], factor, levels = 1:5)
PtestdataRdyrYN$Rzone<-factor(PtestdataRdyrYN$Rzone, levels=c("Ja","Nei"))
str(PtestdataRdyrYN$Rzone)
both_PtestdataRdyrYN_likert = likert(PtestdataRdyrYN[, c(2:6), drop = FALSE], grouping = PtestdataRdyrYN$Rzone)
plot(both_PtestdataRdyrYN_likert, include.histogram = TRUE)

# Attitude toward wolves compared to With or withour carnivore zones - MUST IMORT ALSO Q_3-attitudes questions
names(Arbeidsfil1)
PtestdataRdyrWolf <- Arbeidsfil1[,c("Wolfzone","q4_3","q4_4","q4_5","q4_10","q4_11")]
PtestdataRdyrWolf[2:6] <- lapply(PtestdataRdyrWolf[2:6], factor, levels = 1:5)
PtestdataRdyrWolf$Wolfzone<-factor(PtestdataRdyrWolf$Wolfzone, levels=c("Ja","Nei"))
str(PtestdataRdyrYN$Rzone)
both_PtestdataRdyrWolf_likert = likert(PtestdataRdyrWolf[, c(2:6), drop = FALSE], grouping = PtestdataRdyrWolf$Wolfzone)
plot(both_PtestdataRdyrWolf_likert, include.histogram = TRUE)

# q3_1
setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data sp?rreunders?kelse")
Arbeidsfil2 <- read.csv(file="KEG_Arbeidsfil126juli.csv",header=T,sep = ";")
names(Arbeidsfil2)
PtestdataRdyrYNQ3 <- Arbeidsfil2[,c("q3_1b","q4_3_tillit.genereltF","q4_4_tillit.medisinskF","q4_5_tillit.klimaF","q4_10_Tillit.Rovviltforskningen.i.N","q4_11_tillit.til.at.Rovviltforskerne.legger.frem.objektive.resultater")]

tab_q3_1b <- Arbeidsfil2 %>%
  group_by(q3_1b) %>%
  summarize(Freq = n()) %>%
  mutate(Prop = Freq/sum(Freq)) %>%
  arrange(desc(Prop))
tab_q3_1b
sum(tab_q3_1b$Prop)
ggplot(data = tab_q3_1b, mapping = aes(x = q3_1b, y = Prop)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits = tab_q8_2Arbeid$q8_2Arbeid) # Labels layer omitted

PtestdataRdyrYNQ3[2:6] <- lapply(PtestdataRdyrYNQ3[2:6], factor, levels = 1:5)
PtestdataRdyrYNQ3$q3_1b<-factor(PtestdataRdyrYNQ3$q3_1b, levels=c("1","2","3","4"))
both_PtestdataRdyrYNQ3_likert = likert(PtestdataRdyrYNQ3[, c(2:6), drop = FALSE], grouping = PtestdataRdyrYNQ3$q3_1b)
plot(both_PtestdataRdyrYNQ3_likert, include.histogram = TRUE)
str(PtestdataRdyrYNQ3$q3_1b)

# I egen kommune
Arbeidsfil2 <- read.csv(file="KEG_Arbeidsfil126juli.csv",header=T,sep = ";")
tab_q3_2b <- Arbeidsfil2 %>%
  group_by(q3_2b) %>%
  summarize(Freq = n()) %>%
  mutate(Prop = Freq/sum(Freq)) %>%
  arrange(desc(Prop))
tab_q3_2b
sum(tab_q3_2b$Prop)
ggplot(data = tab_q3_2b, mapping = aes(x = q3_2b, y = Prop)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_discrete(limits = tab_q8_2Arbeid$q8_2Arbeid) # Labels layer omitted
names(Arbeidsfil2)

PtestdataRdyrYNQ3_2 <- Arbeidsfil2[,c("q3_2b","q4_3_tillit.genereltF","q4_4_tillit.medisinskF","q4_5_tillit.klimaF","q4_10_Tillit.Rovviltforskningen.i.N","q4_11_tillit.til.at.Rovviltforskerne.legger.frem.objektive.resultater")]
str(PtestdataRdyrYNQ3_2)
str(PtestdataRdyrYNQ3_2$q3_2b)
PtestdataRdyrYNQ3_2[2:6] <- lapply(PtestdataRdyrYNQ3_2[2:6], factor, levels = 1:5)
PtestdataRdyrYNQ3$q3_2b<-factor(PtestdataRdyrYNQ3_2$q3_2b, levels=c("1","2","3","4"))
both_PtestdataRdyrYNQ3_2_likert = likert(PtestdataRdyrYNQ3_2[, c(2:6), drop = FALSE], grouping = PtestdataRdyrYNQ3$q3_2b)
both_PtestdataRdyrYNQ3_2_likert = likert(PtestdataRdyrYNQ3_2[, c(2:6), drop = FALSE], grouping = PtestdataRdyrYNQ3$q3_2b)
plot(both_PtestdataRdyrYNQ3_2_likert, include.histogram = TRUE)
str(PtestdataRdyrYNQ3$q3_2b)
PtestdataRdyrYNQ3$q3_2b

#plot_likert(PtestdataRdyrYNQ3_2)
# Examine  
str(Arbeidsfil1)
unique(Arbeidsfil1$q4_1)
Arbeidsfil1$q4_1.f = factor(Arbeidsfil1$q4_1,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_2.f = factor(Arbeidsfil1$q4_2,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_3.f = factor(Arbeidsfil1$q4_3,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_4.f = factor(Arbeidsfil1$q4_4,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_5.f = factor(Arbeidsfil1$q4_5,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_6.f = factor(Arbeidsfil1$q4_6,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_7.f = factor(Arbeidsfil1$q4_7,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_8.f = factor(Arbeidsfil1$q4_8,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_9.f = factor(Arbeidsfil1$q4_9,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_10.f = factor(Arbeidsfil1$q4_10,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$q4_11.f = factor(Arbeidsfil1$q4_11,ordered=TRUE, levels = c("1", "2", "3", "4", "5"))
Arbeidsfil1$Sex.f = factor(Arbeidsfil1$Sex,ordered=TRUE, levels = c("Male", "Female"))
headtail(Arbeidsfil1)
levels(Arbeidsfil1$q4_1.f)
summary(Arbeidsfil1[,c(18:28,42:52)]) #

XTq4_1 <- xtabs(~ Sex + q4_1.f, data=Arbeidsfil1)
XTq4_4 <- xtabs(~ Sex + q4_4.f, data=Arbeidsfil1)
XTq4_5 <- xtabs(~ Sex + q4_5.f, data=Arbeidsfil1)
XTq4_10 <- xtabs(~ Sex + q4_10.f, data=Arbeidsfil1)
XTq4_1
prop.table(XTq4_1, margin = 1)
prop.table(XTq4_4, margin = 1)
prop.table(XTq4_5, margin = 1)
prop.table(XTq4_10, margin = 1)
barplot(XTq4_1, beside=TRUE,
        legend=TRUE, ylim=c(0, 900),xlab="Likert score", ylab="Frequency", args.legend = list(x="topleft"))
barplot(XTq4_4, beside=TRUE,
        legend=TRUE, ylim=c(0, 900),xlab="Likert score", ylab="Frequency", args.legend = list(x="topleft"))
barplot(XTq4_5, beside=TRUE,
        legend=TRUE, ylim=c(0, 900),xlab="Likert score", ylab="Frequency", args.legend = list(x="topleft"))
barplot(XTq4_10, beside=TRUE,
        legend=TRUE, ylim=c(0, 900),xlab="Likert score", ylab="Frequency", args.legend = list(x="topleft"))
# alle-i-ett-plot - husk: M?rk farge = mann, lys = kvinne
par(mfrow = c(2,2))
barplot(XTq4_1, beside=TRUE,
        legend=F, ylim=c(0, 900),xlab="Generell", ylab="Frequency")
barplot(XTq4_4, beside=TRUE,
        legend=F, ylim=c(0, 900),xlab="Medisinsk", ylab="Frequency")
barplot(XTq4_5, beside=TRUE,
        legend=F, ylim=c(0, 900),xlab="Klima", ylab="Frequency")
barplot(XTq4_10, beside=TRUE,
        legend=F, ylim=c(0, 900),xlab="Rovvilt", ylab="Frequency")
par(mfrow = c(1,1))

# Examine Yrke
names(Arbeidsfil1)
XTYq4_1 <- xtabs(~ q8_2Arbeid + q4_1.f, data=Arbeidsfil1)
XTq4_4 <- xtabs(~ Sex + q4_4.f, data=Arbeidsfil1)
XTq4_5 <- xtabs(~ Sex + q4_5.f, data=Arbeidsfil1)
XTq4_10 <- xtabs(~ Sex + q4_10.f, data=Arbeidsfil1)
XTYq4_1
prop.table(XTYq4_1, margin = 1)
prop.table(XTq4_4, margin = 1)
prop.table(XTq4_5, margin = 1)
prop.table(XTq4_10, margin = 1)

XTYq4_10 <- xtabs(~ q8_2Arbeid + q4_10.f, data=Arbeidsfil1)
XTYq4_10
prop.table(XTYq4_10, margin = 1)

barplot(XTYq4_10, beside=TRUE,
        legend=TRUE, ylim=c(0, 200),xlab="Likert score", ylab="Frequency", args.legend = list(x="topleft"))

# Examine Age
XTAq4_10 <- xtabs(~ Aldersgruppe + q4_10.f, data=Arbeidsfil1)
XTAq4_10
XTAq4_10prop<-prop.table(XTAq4_10, margin = 1)
barplot(XTAq4_10prop, beside=TRUE,
        legend=TRUE, ylim=c(0, 0.4),xlab="Likert score", ylab="Frequency", args.legend = list(x="topleft"))

# Examine Innbyggertallgruppe
XTIGq4_10 <- xtabs(~ Innbyggertallgruppe + q4_10.f, data=Arbeidsfil1)
XTIGq4_10
str(XTIGq4_10)
XTIGq4_10prop<-prop.table(XTIGq4_10, margin = 1)
barplot(XTIGq4_10prop, beside=TRUE,
        legend=TRUE, ylim=c(0, 0.4),xlab="Likert score", ylab="Frequency", args.legend = list(x="topleft"))

XTIGq4_3 <- xtabs(~ Innbyggertallgruppe + q4_3.f, data=Arbeidsfil1)
XTIGq4_3
XTIGq4_3prop<-prop.table(XTIGq4_3, margin = 1)
XTIGq4_1prop
barplot(XTIGq4_10prop, beside=TRUE,
        legend=TRUE, ylim=c(0, 0.4),xlab="Likert score", ylab="Frequency", args.legend = list(x="topleft"))



#### Let us test some stuff
library(Hmisc)
# Examine data
head(Arbeidsfil1)
str(Arbeidsfil1)
names(Arbeidsfil1)

# Wilcoxon test on Sex
DataTestSex <- Arbeidsfil1[,c("RESPID","Sex","q4_1","q4_2","q4_3","q4_6","q4_7")]
str(DataTestSex)
DataTestSex$Sex<- factor(DataTestSex$Sex, levels = c("1","2"))
str(DataTestSex)
ex_1_DataTestSex <- filter(DataTestSex, Sex == "1" | Sex == "2")
wilcox.test(ex_1_DataTestSex$q4_1 ~ ex_1_DataTestSex$Sex, data = ex_1_DataTestSex) # RESULT: p-value=0.0002716

# Effect sizes & CIs
median_diff = two.boot(DataTestSex=="1", DataTestSex=="2", median, R=1000)
cat(paste0("Difference in medians is ", abs(median_diff$t0), "."))
boot.ci(median_diff, type = "perc") # Did this work??

#Multinominal
#multnom_q4_1 = multinom(q4_1 ~ Innbyggertallgruppe, data = Arbeidsfil1, trace = FALSE)
#multnom_q4_q_1 = multinom(q4_1 ~ 1, data = Arbeidsfil1, trace = FALSE)
#names(Arbeidsfil1)
#multnom_q4_1 = multinom(q4_1 ~ Innbyggertallgruppe+Sex+Alder, data = Arbeidsfil1, trace = FALSE)
# HOWEVER - miltinominal is not the best test. We should perhaps go for Ordinal Logistic Regression?

## OLR
names(Arbeidsfil1)
Arbeidsfil1$q4_1<- factor(Arbeidsfil1$q4_1)
m <- polr(q4_1 ~ Innbyggertallgruppe+Sex+Alder+q8_4Parti, data = Arbeidsfil1, Hess = TRUE)
summary(m)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(m)
exp(coef(m))
exp(cbind(OR = coef(m), ci))
vif(m)
