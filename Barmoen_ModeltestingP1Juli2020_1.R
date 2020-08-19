#### LOADING DATA ####
setwd("C:/Users/magnusb/OneDrive - Høgskolen i Innlandet/Dokumenter/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
#mydata <-read.csv("~/DIV NINA/DatasetSpÃ¸rreundersÃ¸kelseOkt2019.csv",header=T,sep=";",dec = "," ,na.strings = "")
mydata <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")
#### PACKAGES #### 
library(ggplot2)
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
library(tidyverse)
library(lattice)
library(bbmle)
library(plyr)
library(GGally)
library(data.table)
#### Fix variables ####
#Response variable
mydata$q4_10 <- factor(mydata$q4_10)

#A
#age
str(mydata$Alder)
#gender
str(mydata$Kjønn)
mydata$Kjønn <- as.factor(mydata$Kjønn)
#educaton: merge phd and master
str(mydata$q8_1Utdanning)
mydata$q8_1Utdanning <- factor(mydata$q8_1Utdanning)
new.levels<-c(1,2,3,4,4)
mydata$q8_1Utdanning <- factor(new.levels[mydata$q8_1Utdanning])

# B
# Feeling about carnivore numbers Norway: q3_1a til q3_1e 
mydata$q3_1a # Test hver av de og ser hvem som gjør det best. Gjør bare om til faktor. samme gjelder frykt - remove 4. 
mydata$q3_1a <- factor(mydata$q3_1a)
#mydata$q3_1a[mydata$q3_1a==4] <- NA
#table(mydata$q3_1a)
mydata$q3_1b <- factor(mydata$q3_1b)
#mydata$q3_1b[mydata$q3_1b==4] <- NA
#table(mydata$q3_1b)
mydata$q3_1c <- factor(mydata$q3_1c)
#mydata$q3_1c[mydata$q3_1c==4] <- NA
#table(mydata$q3_1c)
mydata$q3_1d <- factor(mydata$q3_1d)
#mydata$q3_1d[mydata$q3_1d==4] <- NA
#table(mydata$q3_1d)


#Fear: q2_9a til q2_9d. 1=ikke redd, 2=litt redd, 3=ganske redd, 4=veldig redd
str(mydata$q2_9a)
mydata$q2_9a <- factor(mydata$q2_9a)
mydata$q2_9b <- factor(mydata$q2_9b)
mydata$q2_9c <- factor(mydata$q2_9c)
mydata$q2_9d <- factor(mydata$q2_9d)

# experienced loss: q2_6a-e : a=jerv, b=ulv, c=gaupe, d=bjørn, e=ingen
#str(mydata$q2_6.5) 
#mydata$q2_6.5<-factor(mydata$q2_6.5)
#table(mydata$q2_6.5) # 0=ghave experinced loss from at least one LC, 1=have not experienced any loss

# C
# think carnivores are present
str(mydata$q2_3.5) 
table(mydata$q2_3.5) # 0=minst en art finnes, 1=ingen av disse finnes 
mydata$q2_3.5<-factor(mydata$q2_3.5)
# big game traditions

#hunt big game yourself
str(mydata$q2_11.1)
table(mydata$q2_11.1) # 0=do not hunt big game. 1=yes hunt big game
mydata$q2_11.1<-factor(mydata$q2_11.1)

#D
#hunter ratio
mydata$hunterratio <- mydata$Antall.reg.jegere/mydata$Folkemengde
#Sheep density
#alt1
mydata$Sautetthet <- as.numeric(mydata$Sautetthet)
#alt2
mydata$SausluppetFylke<-as.numeric(mydata$SausluppetFylke)
mydata$ArealKommune<-as.numeric(mydata$ArealKommune)
mydata$Saudensity <- mydata$SausluppetFylke/mydata$ArealKommune

#Loss of sheep - IKKE MED I PAPERI

#E
#Carnivore present
str(mydata$ArtTilstede)
# wolf present
str(mydata$ulv)
#carnivore zone
str(mydata$RzoneTilstede)
#wolf zone
str(mydata$Wolfzone)


names(mydata)

#### extract all variables needed ####
mydata2 <- mydata[c("RESPID","Kjønn","Alder","q8_1Utdanning","","q2_10"),] 
testdata<-mydata %>% select(Alder,Kjønn,q8_1Utdanning,q3_1a,q3_1b,q3_1c,q3_1d,q2_9a,q2_9b,q2_9c,q2_9d,q2_6.5,q2_3.5,q2_11.1,hunterratio,Sautetthet,Saudensity,ArtTilstede,ulv,RzoneTilstede,Wolfzone)


names(mydata)
mydata$Saulamtapprosent.F
#### Model selection ####

#A - social demographics and education
# alder, kjønn og utdanning
mA0 <- polr(q4_10~ 1,mydata, Hess =T) # 0 model
mA1 <- polr(q4_10~ Alder,mydata, Hess =T) # Alder
mA2 <- polr(q4_10~ Kjønn,mydata, Hess =T) # Kjønn 
mA3 <- polr(q4_10~ q8_1Utdanning,mydata, Hess =T) # utdanning 
bbmle::ICtab(mA0,mA1,mA2,mA3, type="AICc", logLik = T) 
mA4 <- polr(q4_10~ q8_1Utdanning+Alder,mydata, Hess =T) # komb
mA5 <- polr(q4_10~ q8_1Utdanning+Kjønn,mydata, Hess =T) # komb
mA6 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn,mydata, Hess =T) # komb
bbmle::ICtab(mA3,mA4,mA5,mA6, type="AICc", logLik = T) # Alle variablene skal med
bbmle::ICtab(mA1,mA2,mA3,mA4,mA6, type="AICc", logLik = T) # Alle variablene skal med
# B Carnivore presence


mB1a <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ArtTilstede,mydata, Hess =T)
mB1b <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv,mydata, Hess =T)
mB1c <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+RzoneTilstede,mydata, Hess =T)
mB1d <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+Wolfzone,mydata, Hess =T)
mB1e <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+q2_3.5,mydata, Hess =T) # finnes store rovdyr i din kommune? 
#mB1f <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+q2_6.5,mydata, Hess =T) # har du selv opplevd skader fra rovdyr  - tas ikke med. 
bbmle::ICtab(mA6,mB1a,mB1b,mB1c,mB1d,mB1e, type="AICc", logLik = T)
# mB1b best. tar bare med den variablen videre. Å kombinere disse gir vel ingen mening

# C Rural values  
mC1 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1,mydata, Hess =T) # Er du storviltjeger? 
mC2 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+hunterratio,mydata, Hess =T) # hunter ratio 
mC3 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_10,mydata, Hess =T) # Er det sterke tradisjoner for storviltjakt i din kommune? 
mC4 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+Sautetthet,mydata, Hess =T) # sautetthet
mC5 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+Saudensity,mydata, Hess =T) # sautetthet 2
bbmle::ICtab(mB1b,mC1,mC2,mC3,mC4,mC5, type="AICc", logLik = T)# Keep mC1 
mC6 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_6.5,mydata, Hess =T)# experienced loss
bbmle::ICtab(mC1,mC6, type="AICc", logLik = T)


mC7a <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+Saudensity,mydata, Hess =T) # komb
mC7b <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5,mydata, Hess =T) # komb  = best, 
bbmle::ICtab(mC1,mC7a,mC7b,type="AICc", logLik = T)

bbmle::ICtab(mB1b,mC1,mC2,mC3,mC4,mC5,mC6,mC7a,mC7b, type="AICc", logLik = T)# Keep mC1 

# D - Attitude and fear
# D1 feeling about carnivores
mD1a <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5+q3_1a,mydata, Hess =T) # rovviltsituasjon bjørn
mD1b <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5+q3_1b,mydata, Hess =T) # rovviltsituasjon ulv
mD1c <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5+q3_1c,mydata, Hess =T) # rovviltsituasjon gaupe
mD1d <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5+q3_1d,mydata, Hess =T) # rovviltsituasjon jerv
bbmle::ICtab(mC1,mD1a,mD1b,mD1c,mD1d, type="AICc", logLik = T) 
# mD1b er best 
# all species improve the model, but feeling about wolf numbers improve the model the most. Thus we only keept this one, or else the model become complex

# D2 Fear
#mD2a <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q3_1b+q2_6.5+q2_9a,mydata, Hess =T) ###Skal q2_6.5 være med eller ikke (mtp kommentar over?) 
#mD2b <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q3_1b+q2_6.5+q2_9b,mydata, Hess =T) 
#mD2c <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q3_1b+q2_6.5+q2_9c,mydata, Hess =T) 
#mD2d <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q3_1b+q2_6.5+q2_9d,mydata, Hess =T) 
#bbmle::ICtab(mD1b,mD2a,mD2b,mD2c,mD2d, type="AICc", logLik = T)


# D alt 2
mD2aalt <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5+q2_9a,mydata, Hess =T) 
mD2balt <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5+q2_9b,mydata, Hess =T) 
mD2calt <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5+q2_9c,mydata, Hess =T) 
mD2dalt <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5+q2_9d,mydata, Hess =T) 

mD3 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q2_6.5+q3_1b+q2_9b,mydata, Hess =T) # rovviltsituasjon ulv
bbmle::ICtab(mD1a,mD1b,mD1c,mD1d,mD2aalt,mD2balt,mD2calt,mD2dalt,mD3, type="AICc", logLik = T)

# for alle

bbmle::ICtab(mA1,mA2,mA3,mA4,mA5,mA6,mB1a,mB1b,mB1c,mB1d,mB1e,mC1,mC2,mC3,mC4,mC5,mC6,mC7a,mC7b,mD1a,mD1b,mD1c,mD1d,mD2aalt,mD2balt,mD2calt,mD2dalt,mD3, type="AICc", logLik = T)

# mD3 er best

#ggplot(mydata,aes(x = q8_1Utdanning,fill = factor(q4_10))) + 
#  geom_bar(position = "fill")
#ggplot(mydata,aes(x = Kjønn,fill = factor(q4_10))) + 
#  geom_bar(position = "fill") # 1=mann, 2=kvinne
#plot(mydata$q4_10~mydata$Alder)
#table(mydata$q8_1Utdanning)
#table(mydata$q2_6.5)

# Summer den beste modellen: 
summary_table <- coef(summary(mD3))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

prosent<-table(mydata$q4_10)
install.packages("memisc")
library("memisc")
mean(mydata$Alder)
sd(mydata$Alder)

percentages(prosent)
prosentk<-table(mydata$Kjønn)
percentages(prosentk)
table(mydata$q2_6.5)
# Vizulating
#######
#summaryfunction (run as is)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#lager long format av datasettet
library(data.table)



pred<-predict(mD3,newdata=mydata, type="probs") # Her er bestmodel mD2b og data er datasettet (eventuelt så kan du også lage en ny data.frame med forskjellige kombinasjoner innenfor data du har)
df_pred<-as.data.frame(pred)

pred_frame<-cbind(mydata,df_pred)

df_fitted_long <- melt(setDT(pred_frame), id.vars = c("RESPID", "q8_1Utdanning","Alder","Kjønn","ulv","q2_11.1","q2_6.5","q3_1b","q2_9b"), measure.vars =  c("1","2", "3","4","5") ,
                       variable.name = 'Confidence', value.name = "Probability")

#Plot of fitted values

int_plot<-ggplot(df_fitted_long, aes(x=Confidence, y=Probability, fill=Confidence)) +
  geom_boxplot()+
  scale_x_discrete(breaks=c("1","2", "3","4","5"),
                   labels=c("Very low trust","Low trust", "Neither","High trust","Very high trust"),expand = c(0.05,0.05))+ #Perhaps add actual value instead of number?
  theme(legend.position = "none")+
  #facet_grid( .~gender)+
  ylab("Probability of perception")+
  xlab("Confidence in large carnivores")+ #change
  #labs(title = "A")+
  #theme_minimal()+
  theme(axis.text=element_text(size=15),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 



# A 
###Alder-plot
tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("Alder","Confidence"))

Alder_plot <- ggplot(tgc,aes(x=Alder, y=Probability,colour=Confidence)) +theme_bw()+#remove shading with show.legend=FALSE pluss some extra lines
  #geom_ribbon(aes(ymin=Probability-sd, ymax=Probability+sd),fill="grey",linetype=0 ,alpha=0.4)+
  stat_smooth(method =lm,size=2,show.legend=FALSE)+
  stat_smooth(method =lm,fill=NA)+
  theme(legend.background = element_rect(fill="white",colour="white"))+
  theme(legend.title = element_text(size=14, face="bold"))+
  theme(legend.text = element_text(size=14))+
  scale_colour_discrete(name="Perception of statement", labels = c("1", "2", "3","4","5"))+ #Change
  #facet_grid(.~Rov_Forsk_tillit)+
  # theme(legend.justification=c(1,0), legend.position=c(0.99,0.73))+
  #geom_hline(yintercept=0, linetype="dashed", color = "black")+
  theme(panel.margin = unit(0, "lines"))+
  ylab("Predicted probability")+
  xlab("Age")+
  #scale_linetype_manual(values = c("0+5+37" = 2, "2+3+4" = 1, "6+7" = 3,"28+8+9+41" = 4),
  #                      name="Area",
  #                      breaks=c("2+3+4","6+7","28+8+9+41","0+5+37"),
  #                      labels=c("North E","Mid",  "South","North W"))+
  theme(axis.text=element_text(size=18,colour="black"),axis.title=element_text(size=18,face="bold")) +
  theme(axis.text.x = element_text(hjust=0.8))+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  ) +
  
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

#Herifra og ned må det tilpasses ditt datasett

# A 
# Gender


#income_long_new <- income_long %>% group_by(ROS_cat) %>% sample_n(2000)
tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("Kjønn","Confidence"))

pd <- position_dodge(0.1)

genplot<-ggplot(tgc, aes(x=Kjønn, y=Probability,group=Confidence,colour=Confidence,)) +theme_bw()+
  geom_linerange(aes(ymin=Probability-sd, ymax=Probability+sd),size=1.5) +
  geom_line(aes(colour=Confidence), size=1.5) +
  scale_x_discrete(breaks=c("1","2"),
                   labels=c("Male","Female"),expand = c(0.05,0.05))+
  geom_point(size=2) +
  scale_colour_discrete(name="Confidence", labels = c("Highly disagree","Disagree","Not sure", "Agree", "Highly agree"))+
  #facet_grid( .~gender)+
  ylab("Predicted probability")+
  xlab("Gender")+
  #labs(title = "A")+
  #theme_minimal()+
  theme(legend.title = element_text(size=18, face="bold"))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(legend.text = element_text(size=18))+ 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(3.5,"cm"),
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 

#A 
###Utdanning-plot

tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("q8_1Utdanning","Confidence"))

pd <- position_dodge(0.1)
str(mydata$q8_1Utdanning)
eduplot<-ggplot(tgc, aes(x=q8_1Utdanning, y=Probability,group=Confidence,colour=Confidence,)) +theme_bw()+
  geom_linerange(aes(ymin=Probability-sd, ymax=Probability+sd),size=1.5,) +
  geom_line(aes(colour=Confidence), size=1.5) +
  scale_x_discrete(breaks=c("1","2","3","4"),
                   labels=c("1","2","3","4"),expand = c(0.05,0.05))+
  geom_point(size=2) +
  scale_colour_discrete(name="Confidence", labels = c("Highly disagree","Disagree","Not sure", "Agree", "Highly agree"))+
  #facet_grid( .~gender)+
  ylab("Predicted probability")+
  xlab("Education level")+
  #labs(title = "A")+
  #theme_minimal()+
  theme(legend.title = element_text(size=18, face="bold"))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(legend.text = element_text(size=18))+ 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(3.5,"cm"),
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 


#### B Carnivore presence
# Wolf presence
table(mydata$ulv)
tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("ulv","Confidence"))

pd <- position_dodge(0.1)
wolfplot<-ggplot(tgc, aes(x=ulv, y=Probability,group=Confidence,colour=Confidence,)) +theme_bw()+
  geom_linerange(aes(ymin=Probability-sd, ymax=Probability+sd),size=1.5) +
  geom_line(aes(colour=Confidence), size=1.5) +
  scale_x_discrete(breaks=c("Ja","Nei"),
                   labels=c("Wolf present","Not present"),expand = c(0.05,0.05))+
  geom_point(size=2) +
  scale_colour_discrete(name="Confidence", labels = c("Highly disagree","Disagree","Not sure", "Agree", "Highly agree"))+
  #facet_grid( .~gender)+
  ylab("Predicted probability")+
  xlab("Wolf presence")+
  #labs(title = "A")+
  #theme_minimal()+
  theme(legend.title = element_text(size=18, face="bold"))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(legend.text = element_text(size=18))+ 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(3.5,"cm"),
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 


#### C rural values
# big game hunter q2_11.1. 0 = ikke storviltjeger. 1 = storviltjeger
table(mydata$q2_11.1)
tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("q2_11.1","Confidence"))

pd <- position_dodge(0.1)
hunterplot<-ggplot(tgc, aes(x=q2_11.1, y=Probability,group=Confidence,colour=Confidence,)) +theme_bw()+
  geom_linerange(aes(ymin=Probability-sd, ymax=Probability+sd),size=1.5) +
  geom_line(aes(colour=Confidence), size=1.5) +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("Not big hunter","Big game hunter"),expand = c(0.05,0.05))+
  geom_point(size=2) +
  scale_colour_discrete(name="Confidence", labels = c("Highly disagree","Disagree","Not sure", "Agree", "Highly agree"))+
  #facet_grid( .~gender)+
  ylab("Predicted probability")+
  xlab("Big game hunter (no/yes)")+
  #labs(title = "A")+
  #theme_minimal()+
  theme(legend.title = element_text(size=18, face="bold"))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(legend.text = element_text(size=18))+ 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(3.5,"cm"),
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 

table(mydata$q2_6.5)
# q2_6.5 experienced loss 0=opplevd skade. 1 = ikke opplevd skade
tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("q2_6.5","Confidence"))

pd <- position_dodge(0.1)
lossplot<-ggplot(tgc, aes(x=q2_6.5, y=Probability,group=Confidence,colour=Confidence,)) +theme_bw()+
  geom_linerange(aes(ymin=Probability-sd, ymax=Probability+sd),size=1.5) +
  geom_line(aes(colour=Confidence), size=1.5) +
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("0","1"),expand = c(0.05,0.05))+
  geom_point(size=2) +
  scale_colour_discrete(name="Confidence", labels = c("Highly disagree","Disagree","Not sure", "Agree", "Highly agree"))+
  #facet_grid( .~gender)+
  ylab("Predicted probability")+
  xlab("Experienced loss to LC (yes/no)")+
  #labs(title = "A")+
  #theme_minimal()+
  theme(legend.title = element_text(size=18, face="bold"))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(legend.text = element_text(size=18))+ 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(3.5,"cm"),
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 


#### D 
# q3_1b
table(mydata$q3_1b)# 1 = for lite, 2 = passe, 3 = for mye, 4 = vet ikke
tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("q3_1b","Confidence"))

pd <- position_dodge(0.1)
rovsitplot<-ggplot(tgc, aes(x=q3_1b, y=Probability,group=Confidence,colour=Confidence,)) +theme_bw()+
  geom_linerange(aes(ymin=Probability-sd, ymax=Probability+sd),size=1.5) +
  geom_line(aes(colour=Confidence), size=1.5) +
  scale_x_discrete(breaks=c("1","2","3","4"),
                   labels=c("Too few","Appropriate","Too many","Do not know"),expand = c(0.05,0.05))+
  geom_point(size=2) +
  scale_colour_discrete(name="Confidence", labels = c("Highly disagree","Disagree","Not sure", "Agree", "Highly agree"))+
  #facet_grid( .~gender)+
  ylab("Predicted probability")+
  xlab("Thougts on wolf population size")+
  #labs(title = "A")+
  #theme_minimal()+
  theme(legend.title = element_text(size=18, face="bold"))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(legend.text = element_text(size=18))+ 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(3.5,"cm"),
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 




# q2_9b
table(mydata$q2_9b)# 1 = for lite, 2 = passe, 3 = for mye, 4 = vet ikke
tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("q2_9b","Confidence"))

pd <- position_dodge(0.1)
fearplot<-ggplot(tgc, aes(x=q2_9b, y=Probability,group=Confidence,colour=Confidence,)) +theme_bw()+
  geom_linerange(aes(ymin=Probability-sd, ymax=Probability+sd),size=1.5) +
  geom_line(aes(colour=Confidence), size=1.5) +
  scale_x_discrete(breaks=c("1","2","3","4"),
                   labels=c("Not scares","Little scared","Pretty scared","Very scared"),expand = c(0.05,0.05))+
  geom_point(size=2) +
  scale_colour_discrete(name="Confidence", labels = c("Highly disagree","Disagree","Not sure", "Agree", "Highly agree"))+
  #facet_grid( .~gender)+
  ylab("Predicted probability")+
  xlab("Level of fear for wolves")+
  #labs(title = "A")+
  #theme_minimal()+
  theme(legend.title = element_text(size=18, face="bold"))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(legend.text = element_text(size=18))+ 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(3.5,"cm"),
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 





###Hunter plot

tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("q2_11.1","Confidence"))


Hunter_plot <- ggplot(tgc,aes(x=q2_11.1, y=Probability,colour=Confidence)) +theme_bw()+#remove shading with show.legend=FALSE pluss some extra lines
  #geom_ribbon(aes(ymin=Probability-sd, ymax=Probability+sd),fill="grey",linetype=0 ,alpha=0.4)+
  stat_smooth(method =lm,size=2,show.legend=FALSE)+
  stat_smooth(method =lm,fill=NA)+
  theme(legend.background = element_rect(fill="white",colour="white"))+
  theme(legend.title = element_text(size=14, face="bold"))+
  theme(legend.text = element_text(size=14))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  scale_colour_discrete(name="Perception of statement", labels = c("1", "2", "3","4","5"))+
  #facet_grid(.~Rov_Forsk_tillit)+
  # theme(legend.justification=c(1,0), legend.position=c(0.99,0.73))+
  theme(panel.margin = unit(0, "lines"))+
  ylab("Predicted probability")+
  xlab("Hunter ratio")+
  #scale_linetype_manual(values = c("0+5+37" = 2, "2+3+4" = 1, "6+7" = 3,"28+8+9+41" = 4),
  #                      name="Area",
  #                      breaks=c("2+3+4","6+7","28+8+9+41","0+5+37"),
  #                      labels=c("North E","Mid",  "South","North W"))+
  theme(axis.text=element_text(size=18,colour="black"),axis.title=element_text(size=18,face="bold")) +
  theme(axis.text.x = element_text(hjust=0.8))+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  ) +
  
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

####### Age
tgc <- summarySE(pred_frame_long, measurevar="Probability", groupvars=c("Alder","Perception_of_statment"))


Age_plot <- ggplot(tgc,aes(x=Alder, y=Probability,colour=Perception_of_statment)) +theme_bw()+#remove shading with show.legend=FALSE pluss some extra lines
  #geom_ribbon(aes(ymin=Probability-sd, ymax=Probability+sd),fill="grey",linetype=0 ,alpha=0.4)+
  stat_smooth(method =lm,size=2,show.legend=FALSE)+
  stat_smooth(method =lm,fill=NA)+
  theme(legend.background = element_rect(fill="white",colour="white"))+
  theme(legend.title = element_text(size=14, face="bold"))+
  theme(legend.text = element_text(size=14))+
  scale_colour_discrete(name="Perception of statement", labels = c("Dont know", "Guessing", "Manipulation","Political","Research"))+
  #facet_grid(.~Rov_Forsk_tillit)+
  # theme(legend.justification=c(1,0), legend.position=c(0.99,0.73))+
  theme(panel.margin = unit(0, "lines"))+
  ylab("Predicted probability")+
  xlab("Age")+
  #scale_linetype_manual(values = c("0+5+37" = 2, "2+3+4" = 1, "6+7" = 3,"28+8+9+41" = 4),
  #                      name="Area",
  #                      breaks=c("2+3+4","6+7","28+8+9+41","0+5+37"),
  #                      labels=c("North E","Mid",  "South","North W"))+
  theme(axis.text=element_text(size=18,colour="black"),axis.title=element_text(size=18,face="bold")) +
  theme(axis.text.x = element_text(hjust=0.8))+
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  ) +
  
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))




##Trust

#income_long_new <- income_long %>% group_by(ROS_cat) %>% sample_n(2000)
tgc <- summarySE(df_fitted_long, measurevar="Probability", groupvars=c("q2_11.1","Confidence"))

pd <- position_dodge(0.1)

wpsplot<-ggplot(tgc, aes(x=q2_11.1, y=Probability,group=Confidence,colour=Confidence,)) +theme_bw()+
  geom_linerange(aes(ymin=Probability-sd, ymax=Probability+sd),size=1.5) +
  geom_line(aes(colour=Confidence), size=1.5) +
  scale_x_discrete(breaks=c("1","2"),
                   labels=c("Not a big game hunter","Big game hunter"),expand = c(0.05,0.05))+
  geom_point(size=2) +
  scale_colour_discrete(name="Confidence", labels = c("Highly disagree","Disagree", "Agree", "Highly agree"))+
  #facet_grid( .~gender)+
  ylab("Predicted probability")+
  xlab("Hunter or not")+
  #labs(title = "A")+
  #theme_minimal()+
  theme(legend.title = element_text(size=18, face="bold"))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(legend.text = element_text(size=18))+ 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(3.5,"cm"),
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 



##Trust

#income_long_new <- income_long %>% group_by(ROS_cat) %>% sample_n(2000)
tgc <- summarySE(pred_frame_long, measurevar="Probability", groupvars=c("Rov_Forsk_tillit","Perception_of_statment"))

pd <- position_dodge(0.1)

wpsplot<-ggplot(tgc, aes(x=Rov_Forsk_tillit, y=Probability,group=Perception_of_statment,colour=Perception_of_statment,)) +theme_bw()+
  geom_linerange(aes(ymin=Probability-sd, ymax=Probability+sd),size=1.5) +
  geom_line(aes(colour=Perception_of_statment), size=1.5) +
  scale_x_discrete(breaks=c("Uenig","Verken_eller","Enig"),
                   labels=c("Disagree", "Not sure", "Agree"),expand = c(0.05,0.05))+
  geom_point(size=2) +
  scale_colour_discrete(name="Perception of statement", labels = c("Dont know", "Guessing", "Manipulation","Political","Research"))+
  #facet_grid( .~gender)+
  ylab("Predicted probability")+
  xlab("Trust in carnivore science")+
  #labs(title = "A")+
  #theme_minimal()+
  theme(legend.title = element_text(size=18, face="bold"))+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))+
  #theme(legend.title = element_text(size=18, face="bold"),legend.position="top")+
  theme(legend.text = element_text(size=18))+ 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour="lightgrey"),
    panel.grid.minor = element_blank(),
    #  ,panel.border = element_blank()
    legend.background = element_blank(),
    #legend.key = element_rect(fill = "white", color = NA),
    # Change legend key size and key width
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(3.5,"cm"),
    #axis.title.y  = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
  ) 


# ALTERNATIVES - er detmulig å gjøre modellen bedre ved å fjerne variabler?
summary_table
mAlt1 <- polr(q4_10~ q8_1Utdanning+Alder+ulv+q2_11.1+q3_1b+q2_9b+q2_6.5,mydata) 
mAlt2 <- polr(q4_10~ q8_1Utdanning+ulv+q2_11.1+q3_1b+q2_9b+q2_6.5,mydata)
mAlt3 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+ulv+q2_11.1+q3_1b+q2_9b+q2_6.5,mydata)
bbmle::ICtab(mD2b,mAlt1,mAlt2,mAlt3,mD1d, type="AICc", logLik = T)
# Alt 1 er best - men regner med at vi ikke bruker denn, men bruker mD2, det blir vel mest ryddig? 
#summary_tableAlt1 <- coef(summary(mAlt1))
#pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
#summary_tableAlt1 <- cbind(summary_table, "p value" = round(pval,3))
#summary_tableAlt1






####################################
####### SLUTT
###########################################

# mE2 is the best model
#mE2 <- polr(q4_10~ q8_1Utdanning+Alder+Kjønn+q3_1b+q2_11.1+ulv,mydata, Hess =T)

summary(mE2)

summary_table <- coef(summary(mE2))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table
table(mydata$q4_10)


#### A: Test "Synsing" #### - vet ikke om denne delen ska brukes i artikkelen
### i) Test tillit forskning - forklare tillit til rovviltforskning relatert til tillt forskere og forskning

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

### ii) Holdninger
#m1gHoldning <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums, mydata, Hess =T) # tok ikke med konstruksjonen av q3_1sums, for den slÃ¥r ikke ut pÃ¥ modellen uansett 
#bbmle::ICtab(m1g,m1gHoldning, type="AICc", logLik = T) 

# RESULTAT MODELTESTING A: Endelig best model: 
# m1gHoldning <- polr(q4_10~ q4_3 + q4_2 + q4_7 + q3_1sums, mydata, Hess =T)


#### B: Test #### 
# Response variable: q4_10 (trust in carnivore science)
# Predictor variables tested: Age, Gender, Education, Loss to carnivores, Big game traditions, carnivore presence, wolf presence, sheep density. + should we include hunter ratio too? (as for paper III?)


### i) rovdyr tilstedevÃ¦relse
#"RzoneTilstede"
#"ArtTilstede" 
### QUESTION: Trenger vi Ã¥ kontrollere for fylke/kommune i testen? 
mydata$qArtTilstede <- as.factor(mydata$ArtTilstede)
mydata$RzoneTilstede <- as.factor(mydata$RzoneTilstede)
m2 <- polr(q4_10~ 1, mydata, Hess = T)
m2a <- polr(q4_10~  ArtTilstede, mydata, Hess =T) # bedre
m2b <- polr(q4_10~ RzoneTilstede, mydata, Hess =T)
m2c <- polr(q4_10~ Wolfzone, mydata, Hess =T)
bbmle::ICtab(m2,m2a,m2b,m2c, type="AICc", logLik = T) 
# Beslutning: Tar med bare m2a og ikke m2b siden de er veldig like.


### ii) Sos Demografi
mydata$Kjønn <- as.factor(mydata$Kjønn)
mydata$q8_1Utdanning[mydata$q8_1Utdanning==5]<-4
mydata$q8_1Utdanning <- as.factor(mydata$q8_1Utdanning)
m4a <- polr(q4_10~ ArtTilstede + Alder, mydata, Hess =T) 
m4b <- polr(q4_10~ ArtTilstede + Kjønn, mydata, Hess =T)
m4c <- polr(q4_10~ ArtTilstede + q8_1Utdanning, mydata, Hess =T) # Bedre 
bbmle::ICtab(m2a,m4a,m4b,m4c, type="AICc", logLik = T) 
m4d <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder, mydata, Hess =T)
m4e <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Kjønn, mydata, Hess =T)
m4f <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn, mydata, Hess =T) # Bedre
bbmle::ICtab(m4c,m4d,m4e,m4f, type="AICc", logLik = T)
#m4f <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + KjÃ¸nn, mydata, Hess =T) # Best av disse


### iii) Jakttradisjoner  
mydata$q2_11.1 <- as.factor(mydata$q2_11.1)
m5a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_10, mydata, Hess =T)
m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.1, mydata, Hess =T) # Bedre
m5b2 <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.2, mydata, Hess =T)
m5b3 <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.3, mydata, Hess =T)
m5b4 <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.4, mydata, Hess =T)
m5b5 <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.5, mydata, Hess =T)
m5b6 <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + Kjønn + q2_11.6, mydata, Hess =T)
bbmle::ICtab(m4d,m5a,m5b,m5b2,m5b3,m5b4,m5b5,m5b6, type="AICc", logLik = T) 
#m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + KjÃ¸nn + q2_11, mydata, Hess =T) # Best av disse
str(mydata$q2_11.1)
summary(m5b)
summary_table <- coef(summary(m5b))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table


### iv) Beitedyr 
names(mydata)
mydata$Sautetthet
mydata$SauLamGeitLog # 


mydata$Sautetthet <- as.numeric(mydata$Sautetthet)
hist(mydata$Sautetthet)
hist(log(mydata$Sautetthet))


hist(mydata$SauLamGeit)

  
str(mydata$SauLamGeit)  
hist(mydata$SauLamGeit)
hist(log(mydata$SauLamGeit))
mydata$SauLamGeitLog <- log(mydata$SauLamGeit+1)

hist(mydata$TapSauLamGeit)
hist(log(mydata$TapSauLamGeit))
mydata$TapSauLamGeitLog <- log(mydata$TapSauLamGeit+1)

m6a <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + KjÃ¸nn + q2_11.1 + SauLamGeitLog, mydata, Hess =T) 
m6b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + KjÃ¸nn + q2_11.1 + TapSauLamGeitLog, mydata, Hess =T)
m6b2 <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + KjÃ¸nn + q2_11.1 + Saulamtapprosent.F, mydata, Hess =T) 
m6b3 <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + KjÃ¸nn + q2_11.1 + Sautetthet, mydata, Hess =T) 
bbmle::ICtab(m5b,m6b,m6b2,m6b3, type="AICc", logLik = T) 
# Beslutning: Behold m5b. 

# RESULTAT MODELTESTING B: Endelig best model: 
# m5b <- polr(q4_10~ ArtTilstede + q8_1Utdanning + Alder + KjÃ¸nn + q2_11.1, mydata, Hess =T)


#### GjÃ¸re videre analyser av m5b? 
