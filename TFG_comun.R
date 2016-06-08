
library(dplyr)
library(ggplot2)
library(cluster)
library(rJava)
library(xlsx)
library(Rmixmod)

###############################################################

#Author:Pol Àlvarez Matllana
#TFG_completo
#Algortimo de Cross-Selling mediante Clústers

###############################################################


ptm <- proc.time()
#Programa para la lectura de datos csv en R

#Empezamos leyendo el documento del cual vamos a extraer los datos:
Datos<-read.csv("PublicUSefulBankDataFurtherReduced.txt",sep=",",header=TRUE)

#Para que país en concreto? EN MAYUS
Pais<-"UNITED STATES"

#seleccionamos aquellas variables que usaremos para juntar empresas según similitud
clients <- data.frame(Datos$Risk.Country,Datos$Customer.Code, Datos$Line.Of.Business, 
                      Datos$Industry, Datos$Customer.Type, Datos$Profit.Center.Area, 
                      Datos$Segment, Datos$Area, Datos$Product.Description, Datos$Spread.Rate.Nominal)

clients <- filter(clients, Datos.Risk.Country==Pais);

clients[is.na(clients)] <- 0

#creamos un fichero para saber que producto tiene cada valor
#productos asociados
clientse<-data.frame(clients$Datos.Product.Description, clients$Datos.Spread.Rate.Nominal,clients$Datos.Product.Description)

clientse$clients.Datos.Product.Description<-as.numeric(clientse$clients.Datos.Product.Description)
clients$Datos.Product.Description<-clientse$clients.Datos.Product.Description

excel<-aggregate( formula = clients.Datos.Spread.Rate.Nominal~clients.Datos.Product.Description+clients.Datos.Product.Description.1, 
                  data = clientse,
                  FUN = mean);

write.xlsx(excel, "Productos_Bancarios.xlsx", sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

#industria
clientsi<-data.frame(clients$Datos.Industry,clients$Datos.Industry)

clientsi$clients.Datos.Industry<-as.numeric(clients$Datos.Industry)
clients$Datos.Industry<-clientsi$clients.Datos.Industry


write.xlsx(clientsi, "Industria.xlsx", sheetName="Sheet1",
           col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)


#Pasamos a numerico
clients$Datos.Customer.Type<-as.numeric(clients$Datos.Customer.Type)
clients$Datos.Line.Of.Business<-as.numeric(clients$Datos.Line.Of.Business)

clients$Datos.Area<-as.numeric(clients$Datos.Area)
clients$Datos.Profit.Center.Area<-as.numeric(clients$Datos.Profit.Center.Area)
clients$Datos.Segment<-as.numeric(clients$Datos.Segment)


#hacemos los clusters
dat<-select(clients,Datos.Customer.Type, Datos.Line.Of.Business, 
            Datos.Industry, Datos.Segment, Datos.Profit.Center.Area,Datos.Area)

dat<-scale(dat)

set.seed(3)
numcenters = 5;
ClusterKmeans<-kmeans(dat,numcenters,iter.max=10,algorithm = "Forgy")


Cluster1 <- data.frame(clients[(ClusterKmeans$cluster==1),])
Cluster2 <- data.frame(clients[(ClusterKmeans$cluster==2),])
Cluster3 <- data.frame(clients[(ClusterKmeans$cluster==3),])
Cluster4 <- data.frame(clients[(ClusterKmeans$cluster==4),])
Cluster5 <- data.frame(clients[(ClusterKmeans$cluster==5),])




pab1 <- "Histograma "
pab2 <- ".png"
plot <- paste(pab1, Pais, pab2,sep="")



pab1 <- "Histograma "
pab2 <- ".png"
plot <- paste(pab1, Pais, pab2,sep="")

ggplot(Cluster1,aes(Datos.Industry)) +
  geom_freqpoly(data=Cluster1,color = "green", alpha = 1, binwidth = 0.5)+
  geom_freqpoly(data=Cluster2,color = "red", alpha = 1, binwidth = 0.5)+
  geom_freqpoly(data=Cluster3,color = "black", alpha = 1, binwidth = 0.5)+
  geom_freqpoly(data=Cluster4,color = "blue", alpha = 1, binwidth = 0.5)+
  geom_freqpoly(data=Cluster5,color = "yellow", alpha = 1, binwidth = 0.5)+
  scale_x_discrete(breaks=1:length(levels(Datos$Industry)),
                   labels=levels(Datos$Industry))+
  theme(axis.title.x = element_blank(),axis.title.x =
          element_blank(),axis.text.x = element_text(angle = 90, hjust =
                                                       1))+
  ggtitle(plot)
ggsave(plot)

carga<-proc.time() - ptm