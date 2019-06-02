library(gtools) #mixedsort()
file="~/Documents/OPEN DATA/data RATP/data_metro.csv"
data_metro<-read.csv(file,sep=";")
colnames(data_metro)<-c("id","ligne","X","Y","nom","ville")
lignes<-NULL
for(i in mixedsort(levels(data_metro$ligne))){
  lignes[[i]]<-subset(data_metro,ligne==i)
}
lignes<-lignes[1:16]
dist<-function(ligne,i,j){
  return(sqrt((lignes[[ligne]][i,"X"]-lignes[[ligne]][j,"X"])^2+(lignes[[ligne]][i,"Y"]-lignes[[ligne]][j,"Y"])^2))
}
distVec<-function(ligne){
  temp<-NULL
  nb<-nrow(lignes[[ligne]])
  for(i in 1:nb){
    temp<-c(temp,dist(ligne,i,1:nb))
  }
  temp[temp==0]=NaN
  return(matrix(temp,nrow=nb))
}
id_terminus<-function(ligne){
  terminus<-NULL
  for(i in 1:nrow(lignes[[ligne]])){
    station<-as.character(lignes[[ligne]][i,"nom"])
    name<-mixedsort(levels(data_metro$ligne))[ligne]
    if(grepl(station,name,fixed=TRUE)){
      terminus<-i
    }
  }
  return(terminus)
}
nom_terminus<-function(ligne){
  return(as.character(lignes[[ligne]][id_terminus(ligne),"nom"]))
}
chemin<-function(ligne){
  id<-id_terminus(ligne)
  nb<-nrow(lignes[[ligne]])
  i<-0
  path<-NULL
  dist<-distVec(ligne)
  start=id
  path<-c(path,start)
  while(i<nb){
    end=which.min(dist[,start])
    path<-c(path,end)
    dist[start,]<-NaN
    start=end
    i<-i+1
  }
  return(path)
}
reorganiser_donnees<-function(ligne){
  path<-chemin(ligne)
  nb<-nrow(lignes[[ligne]])
  data<-NULL
  for(i in 1:nb){
    data<-rbind(data,lignes[[ligne]][path[i],])
  }
  return(data)
}
lignes_triees<-NULL
for(i in 1:16){
  lignes_triees[[i]]<-reorganiser_donnees(i)
}
for(i in 1:max){
  write.table(lignes_triees[[i]],paste('ligne',i,'.csv',sep=""),sep=";")
}
#####################################################
# 2ème partie
# on peut commencer à jouer avec des données propres
#####################################################
library(maps)
library(mapdata)
library(maptools)
setwd("~/Documents/PROJETS DEV/PROJETS R/metro paris")
lignes<-NULL
Xmins<-NULL
Ymins<-NULL
Xmaxs<-NULL
Ymaxs<-NULL
for(i in 1:14){
  lignes[[i]]<-read.csv(paste("ligne",i,".csv",sep=""),sep=";",header=TRUE)
  Xmins<-c(Xmins,min(lignes[[i]]$X))
  Ymins<-c(Ymins,min(lignes[[i]]$Y))
  Xmaxs<-c(Xmaxs,max(lignes[[i]]$X))
  Ymaxs<-c(Ymaxs,max(lignes[[i]]$Y))
}
ligne3b<-read.csv("ligne3bis.csv",sep=";",header=TRUE)
ligne7b<-read.csv("ligne7bis.csv",sep=";",header=TRUE)
ligne7.2<-read.csv("ligne7-2.csv",sep=";",header=TRUE)
ligne13.2<-read.csv("ligne13-2.csv",sep=";",header=TRUE)
Xmins<-c(Xmins,min(ligne3b$X,ligne7b$X,ligne7.2$X,ligne13.2$X))
Ymins<-c(Ymins,min(ligne3b$Y,ligne7b$Y,ligne7.2$Y,ligne13.2$Y))
Xmaxs<-c(Xmaxs,max(ligne3b$X,ligne7b$X,ligne7.2$X,ligne13.2$X))
Ymaxs<-c(Ymaxs,max(ligne3b$Y,ligne7b$Y,ligne7.2$Y,ligne13.2$Y))
#http://professionnels.ign.fr/sites/default/files/GEOFLADept_FR_Corse_AV_L93.zip
#shapes_target<-"~/Documents/OPEN DATA/IGN GEOFLA/FR_DOM_Mayotte_shp_WGS84/DEPARTEMENT.shp"
#france<-readShapeSpatial(shapes_target)
couleurs<-c(rgb(242/255,201/255,49/255),rgb(33/255,110/255,180/255),rgb(154/255,153/255,64/255),rgb(187/255,77/255,152/255),rgb(22/255,139/255,83/255),rgb(121/255,187/255,146/255),rgb(223/255,154/255,177/255),rgb(197/255,163/255,202/255),rgb(205/255,200/255,63/255),rgb(223/255,176/255,57/255),rgb(142/255,101/255,56/255),rgb(50/255,142/255,91/255),rgb(137/255,199/255,214/255),rgb(103/255,50/255,142/255))
par(oma=c(0,0,0,0),mar=c(0,0,0,0))
Xmin=round(min(Xmins),3)
Xmax=round(max(Xmaxs),3)
Ymin=round(min(Ymins),3)
Ymax=round(max(Ymaxs),3)
library(RgoogleMaps)
newMap<-GetMap(center=c((Ymin+Ymax)/2,(Xmin+Xmax)/2),zoom=11,destfile="newmap.png",maptype="satellite")
#plot(france,xlim=c(Xmin,Xmax),ylim=c(Ymin,Ymax))
max<-length(lignes)
PlotOnStaticMap(newMap,lat=c(Ymin,Ymax),lon=c(Xmin,Xmax),destfile="newmap.png",add=FALSE)
for(i in 1:max){
  PlotOnStaticMap(newMap,lignes[[i]]$Y,lignes[[i]]$X,pch=20,col=couleurs[i],FUN=points,add=TRUE)
  PlotOnStaticMap(newMap,lignes[[i]]$Y,lignes[[i]]$X,lwd=2,col=couleurs[i],FUN=lines,add=TRUE)
  #points(lignes[[i]]$X,lignes[[i]]$Y,pch=20,col=couleurs[i])
  #lines(lignes[[i]]$X,lignes[[i]]$Y,lwd=2,col=couleurs[i])
}
#3b
PlotOnStaticMap(newMap,ligne3b$Y,ligne3b$X,pch=20,col=rgb(154/255,153/255,64/255),FUN=points,add=TRUE)
PlotOnStaticMap(newMap,ligne3b$Y,ligne3b$X,lwd=2,col=rgb(154/255,153/255,64/255),FUN=lines,add=TRUE)
#7b
PlotOnStaticMap(newMap,ligne7b$Y,ligne7b$X,pch=20,col=rgb(223/255,154/255,177/255),FUN=points,add=TRUE)
PlotOnStaticMap(newMap,ligne7b$Y,ligne7b$X,lwd=2,col=rgb(223/255,154/255,177/255),FUN=lines,add=TRUE)
#7-2
PlotOnStaticMap(newMap,ligne7.2$Y,ligne7.2$X,pch=20,col=rgb(223/255,154/255,177/255),FUN=points,add=TRUE)
PlotOnStaticMap(newMap,ligne7.2$Y,ligne7.2$X,lwd=2,col=rgb(223/255,154/255,177/255),FUN=lines,add=TRUE)
#13-2
PlotOnStaticMap(newMap,ligne13.2$Y,ligne13.2$X,pch=20,col=rgb(137/255,199/255,214/255),FUN=points,add=TRUE)
PlotOnStaticMap(newMap,ligne13.2$Y,ligne13.2$X,lwd=2,col=rgb(137/255,199/255,214/255),FUN=lines,add=TRUE)
dev.print(png,file="map metro4.png",width=860,height=860)
dev.off()