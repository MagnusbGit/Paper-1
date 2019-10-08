###############################
# Cluster analysis Magnus-PHD #
###############################
##importerer pakker (bare eksempel på hvordan du kan instalere mange pakker på en gang ved behov, egentlig unødvendig her med bare 3 pakker...)
if (!require('devtools', character.only=T, quietly=T)) {
  # Requires OpenSSL on the system
  install.packages('devtools')
  require('devtools')
}

if (!require('easypackages', character.only=T, quietly=T)) {
  devtools::install_github("jakesherman/easypackages")
  require('easypackages')
}

library(easypackages)

req_packages <- c('tidyverse', 'cluster', 'factoextra')

for (p in req_packages) {
  if(!require(p, character.only=T, quietly=T)){
    install.packages(p)
    print(p)
  }
}
libraries(req_packages[1:length(req_packages)])


## Importerer data
df<-read.csv("~/DIV NINA/DatasetSpørreundersøkelseSept2019.csv",header=T,sep=";",dec = "," ,na.strings = "")

# Velger ut bare kolonner av interesse for dette (dvs spm 4 for enkelhet skyld)
spm4<-c("RESPID","q4_1","q4_2","q4_3","q4_4","q4_5","q4_6","q4_7","q4_8","q4_9","q4_10", "q4_11")
df_spm4<-df[,spm4]

# K-means clustering, en maskinlæringsprosess som prøver å tilegne objekter (her folk) i forskjellige grupper med bakgrunn i hva de har svart.
#Dette gjør den ved at den velger tilfeldig objekter som skal stå i senter i de forskjellige gruppene
k2 <- kmeans(df_spm4, centers = 2, nstart = 25)
k3 <- kmeans(df_spm4, centers = 3, nstart = 25)
k4 <- kmeans(df_spm4, centers = 4, nstart = 25)
k5 <- kmeans(df_spm4, centers = 5, nstart = 25)

# plots to compare
(p1 <- fviz_cluster(k2, geom = "point", data = df_spm4) + ggtitle("k = 2"))
(p2 <- fviz_cluster(k3, geom = "point",  data = df_spm4) + ggtitle("k = 3"))
(p3 <- fviz_cluster(k4, geom = "point",  data = df_spm4) + ggtitle("k = 4"))
(p4 <- fviz_cluster(k5, geom = "point",  data = df_spm4) + ggtitle("k = 5"))

#sjekker om det finnes noen optimal clustering med forskjellige funksjoner

set.seed(12)

fviz_nbclust(df_spm4, kmeans, method = "wss")

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df_spm4, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


fviz_nbclust(df_spm4, kmeans, method = "silhouette")

gap_stat <- clusGap(df_spm4, FUN = kmeans, nstart = 25,
                    K.max = 15, B = 50)
fviz_gap_stat(gap_stat)

# Som du ser så er det ikke noen klare clustere her, det ser ut som det er for stor variasjon innenfor gruppene til at det er tydelige forskjeller opp til 15 grupper. 




