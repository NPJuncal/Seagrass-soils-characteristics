setwd("C:/Users/npjun/Dropbox/Seagrasses/Review suelos/R")

File<-"coord.csv"

A<-read.csv(File, header=T, sep=";", dec=".")
A<-as.data.frame(A)

####Mapa####

library(ggplot2)
library(maps)
library(tidyr)

               
WM <- map_data("world")

WSD<-read.csv("WSDp.csv", header=T, sep=";", dec=".")
WSD<-as.data.frame(WSD)



CDBD<-A %>% drop_na(DBD)
CMud<-A %>% drop_na(Mud..)
CCaCO3<-A %>% drop_na(CaCO3)
CCorg<-A %>% drop_na(Corg)


Var<-CCorg
N<-nrow(Var)

ggplot()+ggtitle("Corg")+
  geom_polygon(data = WM, aes(x=long, y = lat, group = group))+
  geom_point(aes(WSD[,1],as.numeric(WSD[,2])), color="palegreen4",size=1)+
  geom_point(aes(Var[,4],Var[,3]), color="blue",size=1)+
  theme(plot.title = element_text(size=16))
  #+ylim(-50,75)
  #xlim(-50,)

ggsave("Corg_map.jpg", units="cm", width = 15, height = 9)


