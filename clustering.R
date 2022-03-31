## clustering
# codigo para revista
#

# cargamos los datos

file_trends<-c("all_trends_danceability.csv", 
               "all_trends_energy.csv", 
               "all_trends_valence.csv")

# ponemos los nombres de los paises de forma adecuada para los mapas

dt<-read.csv2(file=file_trends[1], header = FALSE, sep = ",")
paises<-NULL
for (i in 1:length(dt$V1)) {
  paises<-c(paises,dt$V1[i])
}

paises[which(paises=="Columbia")]<-"Colombia"
paises[which(paises=="CostaRica")]<-"Costa Rica"
paises[which(paises=="CzechRepublic")]<-"Czech Republic"
paises[which(paises=="DominicanRepublic")]<-"Dominican Republic"
paises[which(paises=="ElSalvador")]<-"El Salvador"
paises[which(paises=="NewZealand")]<-"New Zealand"
paises[which(paises=="UnitedKingdom")]<-"UK"

#mapa

library(mapdata)
library(ggplot2)
library(maps)
library(ggrepel)
library(dplyr)

mapa_mundo <- map_data("world")


######## almacena los datos en una lista


datos<-list()
for (i in 1:3) {
  dt<-read.csv2(file=file_trends[i], header = FALSE, sep = ",")
  df<-data.frame(as.numeric(dt[1,2:length(dt)]),row.names = NULL)
  for (j in 2:length(dt$V1)) {
    df<-cbind(df,as.numeric(dt[j,2:length(dt)]))
  }
  rownames(df)<-NULL
  colnames(df)<-paises
  datos<-c(datos,list(df))
}

rm(file_trends,dt,df)

#####################################################################
# comienzo de procesos

# creando objeto ts para modelo
time_s<-list()
for (i in 1:length(datos)) {
  d_ts <-list()
  for (j in 1:length(datos[[i]])) {
    d_ts<-c(d_ts, list(ts(datos[[i]][,j],start = c(2017,06),
                          frequency = 12,names =paises[j])))
  }
  time_s[[i]]<-d_ts
}
rm(d_ts)

# libreria para clustering

library("dtwclust")

# calculamos los indices para ver que partición es la más adecuada

# Crisp partitions
# "Sil"    Silhouette index to be maximized.
# "D"      Dunn index to be maximized.
# "COP"    COP index to be minimized.
# "DB"     Davies-Bouldin index to be minimized.
# "DBstar" Modified Davies-Bouldin index (DB*) to be minimized.
# "CH"     Calinski-Harabasz index to be maximized.
# "SF"     Score Function to be maximized.

for (j in 1:length(datos)) {
    cvi_eu<-data.frame(matrix(rep(0,7*9),nrow = 7))
    rownames(cvi_eu)<-c("Sil","SF","CH","DB","DBstar","D","COP")
    colnames(cvi_eu)<-c("K3","K4","K5","K6","K7","K8","K9","K10","nCluster")


    for(i in 1:8){
        eu_cluster = tsclust(t(datos[[j]]), k = i+2,
                            distance = "L2", centroid = "pam",
                            seed = sample(3254:5842,1), trace = TRUE,
                            control = partitional_control(nrep = 10L))
        # Cluster validity indices
        cvi_tmp<-sapply(eu_cluster, cvi)
        cvi_eu["Sil",i]<-max(cvi_tmp["Sil",])
        cvi_eu["SF",i]<-max(cvi_tmp["SF",])
        cvi_eu["CH",i]<-max(cvi_tmp["CH",])
        cvi_eu["DB",i]<-min(cvi_tmp["DB",])
        cvi_eu["DBstar",i]<-min(cvi_tmp["DBstar",])
        cvi_eu["D",i]<-min(cvi_tmp["D",])
        cvi_eu["COP",i]<-min(cvi_tmp["COP",])
    } 
    cvi_eu["Sil",9]<-which.max(cvi_eu["Sil",1:8])+2
    cvi_eu["SF",9]<-which.max(cvi_eu["SF",1:8])+2
    cvi_eu["CH",9]<-which.max(cvi_eu["CH",1:8])+2
    cvi_eu["DB",9]<-which.min(cvi_eu["DB",1:8])+2
    cvi_eu["DBstar",9]<-which.min(cvi_eu["DBstar",1:8])+2
    cvi_eu["D",9]<-which.min(cvi_eu["D",1:8])+2
    cvi_eu["COP",9]<-which.min(cvi_eu["COP",1:8])+2

    n_cluster<-as.numeric(names(a[which.max(a<-summary(factor(cvi_eu[,"nCluster"])))]))

    eu_cluster = tsclust(t(datos[[j]]), k = n_cluster,
                        distance = "L2", centroid = "pam",
                        seed = sample(3254:5842,1), trace = TRUE,
                        control = partitional_control(nrep = 10L))

    # la mejor iteración

    eu_cluster<-eu_cluster[[which.max(sapply(eu_cluster, cvi)[1,])]]
    
    # plot cluster series
    
    plot(eu_cluster)+labs(title = NULL)+xlab(NULL)+ylab(NULL)

    # plot map
    
    mapa_cluster<-list()

    for (i in 1:length(eu_cluster@clusinfo$size)) {
        mapa_cluster<-c(mapa_cluster, list(paises[which(eu_cluster@cluster==i)]))
    }

    mapa_mundo %>% 
    filter(region %in% mapa_cluster[[1]]) -> map1
    mapa_mundo %>% 
    filter(region %in% mapa_cluster[[2]]) -> map2
    mapa_mundo %>% 
    filter(region %in% mapa_cluster[[3]]) -> map3

    mapa_mundo %>%
    ggplot() +
    geom_polygon(aes(x=long, y=lat, group=group), fill="grey90", colour="grey90") +
    theme_minimal() +
    theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_rect(colour= "black", size= 1)) +
    geom_polygon(data=map1, aes(x=long, y=lat, group=group), fill="red",color="red")+
    geom_polygon(data=map2, aes(x=long, y=lat, group=group), fill="blue",color="blue")+
    geom_polygon(data=map3, aes(x=long, y=lat, group=group), fill="green",color="green")+
    theme(legend.position = 'none') +
    ggtitle( "")
}





