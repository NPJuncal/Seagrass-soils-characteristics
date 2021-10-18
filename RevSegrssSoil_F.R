setwd("C:/Users/npjun/Dropbox/Seagrasses/Review suelos/R")

File<-"TablaG.csv"

A<-read.csv(File, header=T, sep=";", dec=".")
A<-as.data.frame(A)

#### variables description ####


Variables<-A[,c(9:14)]

summary(Variables)

X<-split(A, A$Bioregion)

for(i in 1:length(X)) {
  Data<-as.data.frame(X[i])
  print(summary(Data))
  
}

#SD
for(i in 1:length(X)) {
  Data<-as.data.frame(X[i])
  SD<-sd(na.omit(Data[,14]))
  print(SD)
  
}


library(ggplot2)

ggplot(A, aes(x=A$Core.length)) + 
  geom_histogram()

summary(A$Core.length)

### count number of observations per trait #####

Regions <-unique(A$Region)
Bioregion<-unique(A$Bioregion)
Species<-unique(A$Specie)
Genus<- unique(A$Genus)
LifeForm<- unique(A$LifeForm)

library(dplyr)

#Regiones
xDBD<-A %>% group_by(Region) %>% summarise(count = length(DBD[!is.na(DBD)]))
xMud<-A %>% group_by(Region) %>% summarise(count = length(Mud[!is.na(Mud)]))
xCaCO3<-A %>% group_by(Region) %>% summarise(count = length(CaCO3[!is.na(CaCO3)]))
xCorg<-A %>% group_by(Region) %>% summarise(count = length(Corg[!is.na(Corg)]))

ObsRegion <- data.frame(matrix(NA, nrow = length(Regions), ncol = 5))
colnames(ObsRegion)<-c("Region","DBD","Mud","CaCO3","Corg")

ObsRegion[,c(1,2)]<-xDBD
ObsRegion[,c(3)]<-xMud[,2]
ObsRegion[,c(4)]<-xCaCO3[,2]
ObsRegion[,c(5)]<-xCorg[,2]

#Bioregiones

xDBD<-A %>% group_by(Bioregion) %>% summarise(count = length(DBD[!is.na(DBD)]))
xMud<-A %>% group_by(Bioregion) %>% summarise(count = length(Mud[!is.na(Mud)]))
xCaCO3<-A %>% group_by(Bioregion) %>% summarise(count = length(CaCO3[!is.na(CaCO3)]))
xCorg<-A %>% group_by(Bioregion) %>% summarise(count = length(Corg[!is.na(Corg)]))

ObsBioregion <- data.frame(matrix(NA, nrow = length(Bioregion), ncol = 5))
colnames(ObsBioregion)<-c("Bioregion","DBD","Mud","CaCO3","Corg")

ObsBioregion[,c(1,2)]<-xDBD
ObsBioregion[,c(3)]<-xMud[,2]
ObsBioregion[,c(4)]<-xCaCO3[,2]
ObsBioregion[,c(5)]<-xCorg[,2]

#Species

xDBD<-A %>% group_by(Specie) %>% summarise(count = length(DBD[!is.na(DBD)]))
xMud<-A %>% group_by(Specie) %>% summarise(count = length(Mud[!is.na(Mud)]))
xCaCO3<-A %>% group_by(Specie) %>% summarise(count = length(CaCO3[!is.na(CaCO3)]))
xCorg<-A %>% group_by(Specie) %>% summarise(count = length(Corg[!is.na(Corg)]))

ObsSpecie <- data.frame(matrix(NA, nrow = length(Species), ncol = 5))
colnames(ObsSpecie)<-c("Specie","DBD","Mud","CaCO3","Corg")

ObsSpecie[,c(1,2)]<-xDBD
ObsSpecie[,c(3)]<-xMud[,2]
ObsSpecie[,c(4)]<-xCaCO3[,2]
ObsSpecie[,c(5)]<-xCorg[,2]

#Genus

xDBD<-A %>% group_by(Genus) %>% summarise(count = length(DBD[!is.na(DBD)]))
xMud<-A %>% group_by(Genus) %>% summarise(count = length(Mud[!is.na(Mud)]))
xCaCO3<-A %>% group_by(Genus) %>% summarise(count = length(CaCO3[!is.na(CaCO3)]))
xCorg<-A %>% group_by(Genus) %>% summarise(count = length(Corg[!is.na(Corg)]))

ObsGenus <- data.frame(matrix(NA, nrow = length(Genus), ncol = 5))
colnames(ObsGenus)<-c("Genus","DBD","Mud","CaCO3","Corg")

ObsGenus[,c(1,2)]<-xDBD
ObsGenus[,c(3)]<-xMud[,2]
ObsGenus[,c(4)]<-xCaCO3[,2]
ObsGenus[,c(5)]<-xCorg[,2]

#Life form


xDBD<-A %>% group_by(LifeForm) %>% summarise(count = length(DBD[!is.na(DBD)]))
xMud<-A %>% group_by(LifeForm) %>% summarise(count = length(Mud[!is.na(Mud)]))
xCaCO3<-A %>% group_by(LifeForm) %>% summarise(count = length(CaCO3[!is.na(CaCO3)]))
xCorg<-A %>% group_by(LifeForm) %>% summarise(count = length(Corg[!is.na(Corg)]))

ObsLifeForm <- data.frame(matrix(NA, nrow = length(LifeForm), ncol = 5))
colnames(ObsLifeForm)<-c("LifeForm","DBD","Mud","CaCO3","Corg")

ObsLifeForm[,c(1,2)]<-xDBD
ObsLifeForm[,c(3)]<-xMud[,2]
ObsLifeForm[,c(4)]<-xCaCO3[,2]
ObsLifeForm[,c(5)]<-xCorg[,2]




write.csv(ObsBioregion,"ObsBioregion.csv", sep=";", dec=".")
write.csv(ObsGenus,"ObsGenera.csv", sep=";", dec=".")
write.csv(ObsLifeForm,"ObsLifeForm.csv", sep=";", dec=".")
write.csv(ObsRegion,"ObsRegion.csv", sep=";", dec=".")


### normality ##### (>0.05 normal, <0.05 no normal) 

# global
shapiro.test(A$CaCO3)
plot(density(na.omit(A$Corg)))
hist(na.omit(A$CaCO3), breaks=15)


# by LifeForm

X<-split(A, A$LifeForm)

NormLifeForm <- data.frame(LifeForm=character(),
                 DBD.W=numeric(), 
                 DBD.p=numeric(),
                 Mud.W=numeric(), 
                 Mud.p=numeric(),
                 CaCo3.W=numeric(), 
                 CaCo3.p=numeric(),
                 Corg.W=numeric(), 
                 Corg.p=numeric()) 



for(i in 1:length(X)) {
  NormLifeForm[i,1]<-names(X[i])
  Data<-as.data.frame(X[i])
  colnames(Data)<-colnames(A)
  N<-shapiro.test(na.omit(Data$DBD))
  NormLifeForm[i,2]<-N$statistic
  NormLifeForm[i,3]<-N$p.value
  N<-shapiro.test(Data$Mud)
  NormLifeForm[i,4]<-N$statistic
  NormLifeForm[i,5]<-N$p.value
  N<-shapiro.test(Data$CaCO3)
  NormLifeForm[i,6]<-N$statistic
  NormLifeForm[i,7]<-N$p.value
  N<-shapiro.test(Data$Corg)
  NormLifeForm[i,8]<-N$statistic
  NormLifeForm[i,9]<-N$p.value

  }



###### Homocedasticity ########

library(dplyr)

bartlett.test(A$DBD ~ A$LifeForm) ## normal distribution

library(car)
leveneTest(y = A$Mud, group = A$LifeForm, center = "median")



#### differences #### are significantly different (p < 0.05)


pairwise.wilcox.test(A$Corg, A$Bioregion,
                     p.adjust.method = "BH")


### Figures #####


library(ggplot2)

#DBD

PData<-A[,c("LifeForm","DBD")]
PData<-na.omit(PData)

PData$Genus <- factor(PData$Genus, levels = c("Mixed","Ruppia","Halophila","Halodule","Zostera","Syringodium","Cymodocea","Amphibolis","Thalassodendron","Thalassia",'Posidonia','Enhalus'),ordered = TRUE)

PData = filter(PData, LifeForm != "Mixed")

theme_update(plot.title = element_text(hjust = 0.5))
ggplot(PData, aes(PData[,2],PData[,1]),fill=X)+
  ggtitle("DBD (g/cm3)")+
  geom_boxplot(fill="grey90")+
  geom_text(aes(label=..count..), x=-0.3, stat="count", colour="black", size=5)+
  xlim(-0.3,3)+
  theme(axis.text=element_text(size=16, color = "#000000"),
        #axis.title.x=element_text(size=20),
        axis.text.x = element_text(color="#000000", 
                                   size=16),
        #axis.text.y=element_text(face="italic"),
        plot.title = element_text(size=18))


ggsave("DBD_Genus.jpg", units="cm", width = 20, height = 10)

#Mud

PData<-A[,c("Bioregion","Mud")]
PData<-na.omit(PData)

PData$Genus <- factor(PData$Genus, levels = c("Mixed","Ruppia","Halophila","Halodule","Zostera","Syringodium","Cymodocea","Amphibolis","Thalassodendron","Thalassia",'Posidonia','Enhalus'),ordered = TRUE)

PData = filter(PData, LifeForm != "Mixed")

theme_update(plot.title = element_text(hjust = 0.5))  
ggplot(PData, aes(PData[,2],PData[,1]),fill=X)+
    ggtitle("Mud% (<0.063 mm)")+
    geom_boxplot(fill="grey90")+
    geom_text(aes(label=..count..), x=-3, stat="count", colour="black", size=5)+
    xlim(-2,100)+
    theme(axis.text=element_text(size=16, color = "#000000"),
        #axis.title.x=element_text(size=20),
        axis.text.x = element_text(color="#000000", 
                                   size=16),
        #axis.text.y=element_text(face="italic"),
        plot.title = element_text(size=18))  
  

ggsave("Mud_LifeForm.jpg", units="cm", width = 20, height = 10)

#CaCO3

PData<-A[,c("Genus","CaCO3")]
PData<-na.omit(PData)

PData$Genus <- factor(PData$Genus, levels = c("Mixed","Ruppia","Halophila","Halodule","Zostera","Syringodium","Cymodocea","Amphibolis","Thalassodendron","Thalassia",'Posidonia','Enhalus'),ordered = TRUE)


ggplot(PData, aes(PData[,2],PData[,1]),fill=X)+
  ggtitle("CaCO3%")+
  geom_boxplot(fill="grey90")+
  geom_text(aes(label=..count..), x=-1, stat="count", colour="black", size=4)+
  xlim(-1,100)+
  theme_minimal()

#Corg

theme_update(plot.title = element_text(hjust = 0.5))  
PData<-A[,c("Bioregion","Corg")]
PData<-na.omit(PData)

#Para genus#PData$Genus <- factor(PData$Genus, levels = c("Mixed","Ruppia","Halophila","Halodule","Zostera","Syringodium","Cymodocea","Amphibolis","Thalassodendron","Thalassia",'Posidonia','Enhalus'),ordered = TRUE)

# for LifeForm# PData = filter(PData, LifeForm != "Mixed")

ggplot(PData, aes(PData[,2],PData[,1]),fill=X)+
  ggtitle("Corg%")+
  geom_boxplot(fill="grey90")+
  geom_text(aes(label=..count..), x=-1, stat="count", colour="black", size=5)+
  xlim(-1,12.5)+
  theme(axis.text=element_text(size=16, color = "#000000"),
        #axis.title.x=element_text(size=20),
        axis.text.x = element_text(color="#000000", 
                                   size=16),
        #axis.text.y=element_text(face="italic"),
        plot.title = element_text(size=18)) 

ggsave("Corg_Genus.jpg", units="cm", width = 20, height = 10)



#### correlation corg-mud per genus


X<-split(A, A$Genus)

DT <- data.frame(Genus=character(),
                 C_rho=numeric(), 
                 C_p=numeric(),
                 n=numeric()) 



for(i in 1:length(X)) {
  DT[i,1]<-names(X[i])
  Data<-as.data.frame(X[i])
  cor<-cor.test(x=Data[,11],y=Data[,14], method = "spearman")
  DT[i,2]<-cor$estimate
  DT[i,3]<-cor$p.value
  plot(x=Data[,11],y=Data[,14])
  Data1<-Data[!is.na(Data[,14]),]
  Data1<-Data1[!is.na(Data1[,11]),]
  DT[i,4]<-nrow(Data1)
}

write.csv(DT, "Mud-org_Genus.csv", sep=";", dec=".")

#########################################################
#### Comparisson between Intertidal and subtidal values

## number of observations per genera

A %>%count(Tidal)
TpG<-A %>% group_by(Genus) %>% count(Tidal) 



#### Redox

File<-"Redox.csv"

R<-read.csv(File, header=T, sep=";", dec=".")
R<-as.data.frame(R)

R<-R[-51,]

library(dplyr)

R %>% group_by(Water.depth) %>% summarise(count = length(Average[!is.na(Average)]))

#Water.depth   count
#* <chr>         <int>
#1 ""                1
#2 "Intertaidal"    21
#3 "Subtidal"       30

R %>% group_by(Water.depth) %>% summarise(count = length(Average[!is.na(Mud)]))

# A tibble: 2 x 2
#Water.depth count
#* <chr>       <int>
#1 Intertaidal    19
#2 Subtidal        7

library(ggplot2)

#Redox between subtidal and intertidal
theme_update(plot.title = element_text(hjust = 0.5))  
PData<-R[,c("Water.depth","Average")]
PData<-na.omit(PData)

ggplot(PData, aes(PData[,2],PData[,1]),fill=X)+
  ggtitle("Redox potential (mV)")+
  geom_boxplot(fill="grey90")+
  geom_text(aes(label=..count..), x=-260, stat="count", colour="black", size=5)+
  #xlim(-1,12.5)+
  theme(axis.text=element_text(size=16, color = "#000000"),
        #axis.title.x=element_text(size=20),
        axis.text.x = element_text(color="#000000", 
                                   size=16),
        plot.title = element_text(size=18)) 

ggsave("Redox.jpg", units="cm", width = 15, height = 7.5)


pairwise.wilcox.test(R$Average, R$Water.depth, #are significantly different (p < 0.05)
                     p.adjust.method = "BH")


#Mud between subtidal and intertidal with redox dataset
theme_update(plot.title = element_text(hjust = 0.5))  
PData<-R[,c("Water.depth","Mud")]
PData<-na.omit(PData)

ggplot(PData, aes(PData[,2],PData[,1]),fill=X)+
  ggtitle("Mud % (<0.0063 mm)")+
  geom_boxplot(fill="grey90")+
  geom_text(aes(label=..count..), x=-2, stat="count", colour="black", size=5)+
  #xlim(-1,12.5)+
  theme(axis.text=element_text(size=16, color = "#000000"),
        #axis.title.x=element_text(size=20),
        axis.text.x = element_text(color="#000000", 
                                   size=16),
        plot.title = element_text(size=18)) 

ggsave("Mud_interSub.jpg", units="cm", width = 15, height = 7.5)

pairwise.wilcox.test(R$Mud, R$Water.depth, #are significantly different (p < 0.05)
                     p.adjust.method = "BH")

# comparisons between tidal and intertadil all the dataset

theme_update(plot.title = element_text(hjust = 0.5))  
PData<-A[,c("Tidal","DBD")]
PData<-na.omit(PData)
PData <- subset(PData, subset = PData[,1] %in% c('Intertidal',"Subtidal"))


ggplot(PData, aes(PData[,2],PData[,1]),fill=X)+
  ggtitle("Corg %")+
  geom_boxplot(fill="grey90")+
  geom_jitter() +
  geom_text(aes(label=..count..), x=0, stat="count", colour="black", size=5)+
  xlim(0,2.5)+
  theme(axis.text=element_text(size=16, color = "#000000"),
        #axis.title.x=element_text(size=20),
        axis.text.x = element_text(color="#000000", 
                                   size=16),
        plot.title = element_text(size=18)) 

ggsave("Corg_tidal.jpg", units="cm", width = 15, height = 7.5)


pairwise.wilcox.test(A$DBD, A$Tidal, #are significantly different (p < 0.05)
                     p.adjust.method = "BH")

#Without Posidonia

B = filter(A, Genus != "Posidonia")

theme_update(plot.title = element_text(hjust = 0.5))  
PData<-B[,c("Tidal","Corg")]
PData<-na.omit(PData)
PData <- subset(PData, subset = PData[,1] %in% c('Intertidal',"Subtidal"))


ggplot(PData, aes(PData[,2],PData[,1]),fill=X)+
  ggtitle("Corg%")+
  geom_boxplot(fill="grey90")+
  geom_text(aes(label=..count..), x=-2, stat="count", colour="black", size=5)+
  #xlim(-1,12.5)+
  theme(axis.text=element_text(size=16, color = "#000000"),
        #axis.title.x=element_text(size=20),
        axis.text.x = element_text(color="#000000", 
                                   size=16),
        plot.title = element_text(size=18)) 

pairwise.wilcox.test(B$Corg, A$Tidal, #are significantly different (p < 0.05)
                     p.adjust.method = "BH")

ggsave("Corg_tidal-P.jpg", units="cm", width = 15, height = 7.5)

### Int vs Sub for thos genus with intertidal data

theme_update(plot.title = element_text(hjust = 0.5))

PData<-subset(A,subset = A$Genus %in% c("Zostera","Cymodocea","Enhalus","Halodule","Halophila","Thalassia","Thalassodendron"))
PData<-PData[,c("Tidal","Mud")]
PData<-na.omit(PData)
PData <- subset(PData, subset = PData[,1] %in% c('Intertidal',"Subtidal"))

ggplot(PData, aes(PData[,2],PData[,1]),fill=X)+
  ggtitle("Mud % of intertidal genera")+
  geom_boxplot(fill="grey90")+
  geom_text(aes(label=..count..), x=-4, stat="count", colour="black", size=5)+
  xlim(-4,100)+
  theme(axis.text=element_text(size=16, color = "#000000"),
        #axis.title.x=element_text(size=20),
        axis.text.x = element_text(color="#000000", 
                                   size=16),
        plot.title = element_text(size=18))

pairwise.wilcox.test(PData$Mud, PData$Tidal, #are significantly different (p < 0.05)
                     p.adjust.method = "BH")

ggsave("Zos_Mud_Tidal.jpg", units="cm", width = 15, height = 7.5)


##########correlation Mud-redox

cor.test(x=R[,4],y=R[,5], method = "spearman")

#Spearman's rank correlation rho

#data:  R[, 4] and R[, 5]
#S = 3665.4, p-value = 0.04192
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#      rho 
#-0.409773

ggplot(R, aes(R[,4],R[,5]))+ ggtitle("Mud % (<0.0063 mm)")+
  geom_point()+ geom_smooth(method = "lm")

Sub = filter(R, R[,2] != "Intertaidal")
Int = filter(R, R[,2] != "Subtidal")

cor.test(x=Sub[,4],y=Sub[,5], method = "spearman")

#Spearman's rank correlation rho
#
#data:  Sub[, 4] and Sub[, 5]
#S = 34, p-value = 1
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#0.02857143

cor.test(x=Int[,4],y=Int[,5], method = "spearman")

#Spearman's rank correlation rho
#
#data:  Int[, 4] and Int[, 5]
#S = 1614.4, p-value = 0.07636
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#       rho 
#-0.4161545 



