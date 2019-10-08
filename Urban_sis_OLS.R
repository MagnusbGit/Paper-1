###################
# OLS-analyse: Urban sis
###################

#importing data
urban_data<-read.csv("//ninsrvlil/kim.magnus.barum/My Documents/DIV NINA/Levering del 2/Levering del 2/rawdata_labels.csv",header=T,sep=";",dec = "," ,na.strings = "")

setwd("C:/Users/magnusb/Filr/My Files/Oppgave/Data spørreundersøkelse/RETTredigert/Arbeidsfil Paper 1")
urban_data <- read.csv(file="DatasetSpørreundersøkelseOkt2019.csv",header=T,sep=";")

# Set "vet ikke/ikke relevant" to na
#install.packages("naniar")
library(naniar)
urban_data<-urban_data %>% replace_with_na_all(condition = ~.x == "vet ikke/ikke relevant")
urban_data<-as.data.frame(urban_data)

## Sjekker for kolin?ritet i uavhengige variable (f?rst noen funksjoner for ? visualisere i plott)

panel.cor <- function(x, y, digits=1, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}


panel.smooth2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                        cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}


panel.lines2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                       cex = 1, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)){
    tmp=lm(y[ok]~x[ok])
    abline(tmp)}
  
}



panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

#pairs(urban_data[62:72], lower.panel=panel.smooth2, upper.panel=panel.cor[>0.5]) 
pairs(urban_data[62:72], lower.panel=panel.smooth2, upper.panel=panel.cor) 
# Many variables are very much linked --> may be difficult to separate the effects

#Checking response variable
table(urban_data$q4_1)
table(urban_data$q4_10)


# Looking at single variables
library(ggplot2)
library(gridExtra)
library(ggpubr)

variable<-c("q9_1" , "q9_2", "q9_3", "q9_4","q9_5","q9_6","q9_7","q9_8","q9_9","q9_10","q9_11","q9_12","q9_13","q9_14","q9_15","q9x1_16","q9x1_17","q9x1_18","q9x1_19","q9x1_20","q9x1_21","q9x1_22","q9x1_23","q9x1_24","q9x1_25","q9x1_26","q9x1_27","q9x1_28","q9x1_29","q9x1_30","q9x1_31")
for (i in levels(factor(variable))){
i<-noquote(i)
  print(ggplot(urban_data, aes(x = factor(q10_1), y = i)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)) 
}
  

plot_exp <- 
  function(i){
    dat <- subset(gg2,Ei == i )
    if (nrow(dat) > 0)
      ggplot(dat,aes(x=hours, y=variable, fill = Mi)) + 
      geom_point(aes(color = Mi),size = 3)
  }

ll <- lapply(seq_len(EXP), plot_exp)




a<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_1)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
b<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_2)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
c<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_3)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) 
d<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_4)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) 
e<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_5)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) 
f<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_6)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) 
g<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_7)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) 
h<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_8)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) 
i<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_9)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
j<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_10)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
k<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_11)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
l<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_12)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
m<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_13)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
n<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_14)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
o<-ggplot(urban_data, aes(x = factor(q10_14), y = q9_15)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
p<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_16)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
q<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_17)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
r<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_18)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
s<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_19)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
t<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_20)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
v<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_21)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
w<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_22)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
x<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_23)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
y<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_24)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
z<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_25)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
aa<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_26)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
bb<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_27)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
cc<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_28)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
dd<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_29)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
ee<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_30)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)
ff<-ggplot(urban_data, aes(x = factor(q10_14), y = q9x1_31)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5)



ggarrange(a, b, c,d,e,f,g,h,i,j,k,l,m,o,p,q,r,s,t,v,w,x,y,z,aa,bb,cc,dd,ee,ff + rremove("x.text"), 
          ncol = 2, nrow = 2)%>%
  ggexport(filename = "10_14.pdf")


pdf("plots.pdf", onefile = TRUE)
for (i in seq(length(plot))) {
  do.call("grid.arrange", plot[[i]])  
}
dev.off()
     
              
              # Cheking different predictor variables 
table(urban_data$q9_1)
table(urban_data$q9_2)
table(urban_data$q9_3)
table(urban_data$q9_4)
table(urban_data$q9_5)
table(urban_data$q9_6)
table(urban_data$q9_7)
table(urban_data$q9_8)
table(urban_data$q9_9)
table(urban_data$q9_10)
table(urban_data$q9_11)
table(urban_data$q9_12)
table(urban_data$q9_13)
table(urban_data$q9_14)
table(urban_data$q9_15)
table(urban_data$q9x1_16)
table(urban_data$q9x1_17)
table(urban_data$q9x1_18)
table(urban_data$q9x1_19)
table(urban_data$q9x1_20)
table(urban_data$q9x1_21)
table(urban_data$q9x1_22)
table(urban_data$q9x1_23)
table(urban_data$q9x1_24)
table(urban_data$q9x1_25)
table(urban_data$q9x1_26)
table(urban_data$q9x1_27)
table(urban_data$q9x1_28)
table(urban_data$q9x1_29)
table(urban_data$q9x1_30)
table(urban_data$q9x1_31)
#Fit ordered logit model for q10_1
library(foreign)
require(MASS)
require(Hmisc)
require(reshape2)
library(MuMIn)

dat<-urban_data[complete.cases(urban_data[ , 42:58]),]

global_mod10_1 <- polr(factor(q10_1) ~ q9_1 + q9_2+ q9_3+ q9_4+q9_5+q9_6+q9_7+q9_8+q9_9+q9_10+q9_11+q9_12+q9_13+q9_14+q9_15+q9x1_16+q9x1_17+q9x1_18+q9x1_19+q9x1_20+q9x1_21+q9x1_22+q9x1_23+q9x1_24+q9x1_25+q9x1_26+q9x1_27+q9x1_28+q9x1_29+q9x1_30+q9x1_31, data = dat, Hess=TRUE,na.action=na.fail)

(ctable <- coef(summary(global_mod10_1)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

## Removing some variables based on p-values
mod10_1.1 <- polr(factor(q10_1) ~ q9_12+q9_13+q9_14+q9_15 + q9x1_16+q9x1_24, data = dat, Hess=TRUE,na.action=na.fail)

AIC(global_mod10_1,mod10_1.1)

mod10_1.2 <- polr(factor(q10_1) ~ q9_4+q9_15 , data = dat, Hess=TRUE,na.action=na.fail)
mod10_1.3 <- polr(factor(q10_1) ~ q9_4+q9_15 , data = dat, Hess=TRUE,na.action=na.fail)
mod10_1.4 <- polr(factor(q10_1) ~ q9_15 , data = dat, Hess=TRUE,na.action=na.fail)

AIC(mod10_1.3,mod10_1.2,mod10_1.4)



(ctable <- coef(summary(mod10_1.3)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))



options(na.action = "na.fail")
final_mod<-dredge(mod10_1.2, evaluate=T, rank="AIC")
mod10_1<-model.avg(final_mod, subset =  delta <= 2,fit = TRUE)


#pframe<-with(dat,expand.grid(q9_12=seq(min(q9_12),max(q9_12),by=1),q9_13=seq(min(q9_13),max(q9_13),by=1),q9_14=seq(min(q9_14),max(q9_14),by=1),q9_15=seq(min(q9_15),max(q9_15),by=1),q9x1_16=seq(min(q9x1_16),max(q9x1_16),by=1),q9x1_24=seq(min(q9x1_24),max(q9x1_24),by=1)))
pframe<-with(dat,expand.grid(q9_4=seq(min(q9_4),max(q9_4),by=1),q9_15=seq(min(q9_15),max(q9_15),by=1)))
predprob<- predict(mod10_1.3, pframe, type="probs")

newdat <- cbind(pframe, predprob)
lnewdat <- melt(newdat, id.vars = c("q9_4", "q9_15"),
                variable.name = "q10_1", value.name="Probability")
head(lnewdat)

ggplot(lnewdat, aes(x = q9_4, y = Probability, colour = q10_1)) +
  geom_smooth() 

ggplot(lnewdat, aes(x = q9_15, y = Probability, colour = q10_1)) +
  geom_smooth()

(ctable <- coef(summary(global_mod10_1)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

mod10_1.1<-polr(factor(q10_1) ~ 1,data = urban_data, Hess=TRUE)


AIC(global_mod10_1,mod10_1.1)

options(na.action = "na.fail")
final_mod<-dredge(global_mod10_1, evaluate=T, rank="AIC")
