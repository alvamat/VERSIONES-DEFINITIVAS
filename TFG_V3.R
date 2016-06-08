###############################################################

#Author:Pol Àlvarez Matllana
#TFGv3
#Algortimo de Cross-Selling mediante Clústers

###############################################################

ptm <- proc.time()


#Bucle para determinar el C-S para cada uno de los Cluster
for(i in 1:numcenters){
  normaplot<-matrix(data=NA, ncol = 2, nrow=50);
  k=1;
  
  file<-toString(i)
  
  Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==i),]));
  ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==i),]);
  
  
  #Comprovamos que valor tiene el más reptido
  Productos_cluster<-(sort((ClusterImportante$Datos.Product.Description)));
  Productos_cluster<-as.numeric(names(which.max(table(Productos_cluster))));
  Quitar=Productos_cluster;
  
  Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster));
  Freq=1/nrow(Rep_num);
  
  Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster);
  S=1/Spread_Rate$clients.Datos.Spread.Rate.Nominal;
  
  
  norma<-c(Freq,S);
  
  
  
  if(norma[2]==Inf){
    P=1000;
  }else{
    P=norm(norma, type="2");
    normaplot[k,1]<-norma[1];
    normaplot[k,2]<-norma[2];
    valorrep<-nrow(Rep_num)
    Producto=Quitar;
  }
  
  
  
  #Hacemos lo mismo para los demás productos del vector
  
  
  t=0;
  Productos_cluster1<-data.frame(sort((ClusterImportante$Datos.Product.Description)));
  Longitud<-(sort(table(ClusterImportante$Datos.Product.Description)));
  if((!is.null(length(Longitud)) && length(Longitud)>1)){
    Long=length(Longitud)-1;
  }else{
    Long=0;
  }
  
  
  if(Long==0){
    excel2<-data.frame(ClusterImportante,Spread_Rate)
    write.csv(excel2, file="No_es_valido_CS",na="NA", row.names=TRUE)
  }else{
    
    while(Long>=1){
      
      
      Productos_cluster1<-filter(Productos_cluster1,Productos_cluster1$sort..ClusterImportante.Datos.Product.Description..!=Quitar)
      Productos_cluster2<-as.numeric(names(which.max(table(Productos_cluster1))));
      Quitar=Productos_cluster2;
      
      Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster2));
      Freq=1/nrow(Rep_num);
      
      Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster2);
      S=1/Spread_Rate$clients.Datos.Spread.Rate.Nominal;
      
      norma<-c(Freq,S);
      
      
      if(norma[2]==Inf){
        P1=1000;
      }else{
        P1=norm(norma, type="2");
        normaplot[k,1]<-norma[1];
        normaplot[k,2]<-norma[2];
        k=k+1;
      }
      
      
      if(P1<=P){
        P=P1
        Producto=Quitar;
        pob_mej=t;
        valorrep<-nrow(Rep_num)
      }
      
      Long=Long-1;
      t=t+1;
      
      
    }
    
    
    pab1 <- "OptMultiobj"
    pab2 <- file
    pab3 <- ".png"
    plot <- paste(pab1, Pais, pab2, pab3,sep="_")
    
    pab1 <- "Optimización Multiobjeto"
    pab2 <- file
    pab4 <-"cluster"
    plottitu <- paste(pab1, Pais, pab4,pab2, pab3,sep="_")
    
    png(plot)
    plot(normaplot, xlab="1/(Frecuencia en el Cluster)", ylab="1/(Spread Rate)", col="blue" ,main=plottitu)
    dev.off();
    #Generamos fichero
    
    if(pob_mej==1){
      Segundo_producto<-(filter(ClusterImportante, Datos.Product.Description!=Productos_cluster));
      Segundo_producto<-as.numeric(names(which.max(table(sort(Segundo_producto$Datos.Product.Description)))));
      
      Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Producto & Datos.Product.Description==Segundo_producto);
      
      Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Producto)
      Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
      
      
      excel2<-data.frame(Clientes_Potenciales,Spread)
      
      colnames(excel2)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual', 'Spread Rate Nominal Futuro')
      
      pab1 <- "V3No es posible C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "V3Producto para"
      pab2 <- file
      union2 <- paste(pab1, Pais, pab2,sep="_")
      ProdMay<-data.frame(Producto_mayor,valorrep)
      write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
      
    }else{
      Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Producto & Datos.Product.Description==Productos_cluster);
      
      Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Producto)
      Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
      
      
      excel2<-data.frame(Clientes_Potenciales,Spread)
      
      colnames(excel2)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual', 'Spread Rate Nominal Futuro')
      
      pab1 <- "V3Clientes C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "V3Producto para"
      pab2 <- file
      union2 <- paste(pab1, Pais, pab2,sep="_")
      ProdMay<-data.frame(Producto_mayor,valorrep)
      write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
    }
  }
  
  
  
}

v3<-proc.time() - ptm

#Algoritmo para recomendar un producto a una nueva empresa introducida en el sistema.

#necesitamos que todo sea numérico
Cluster1n<-Cluster1[,-2]
Cluster1n$Datos.Risk.Country<-as.numeric(Cluster1n$Datos.Risk.Country)
Cluster2n<-Cluster2[,-2]
Cluster2n$Datos.Risk.Country<-as.numeric(Cluster2n$Datos.Risk.Country)
Cluster3n<-Cluster3[,-2]
Cluster3n$Datos.Risk.Country<-as.numeric(Cluster3n$Datos.Risk.Country)
Cluster4n<-Cluster4[,-2]
Cluster4n$Datos.Risk.Country<-as.numeric(Cluster4n$Datos.Risk.Country)
Cluster5n<-Cluster5[,-2]
Cluster5n$Datos.Risk.Country<-as.numeric(Cluster5n$Datos.Risk.Country)

train=rbind(Cluster1n,Cluster2n,Cluster3n, Cluster4n,Cluster5n)

#Valores frontera para cada cluster
cl=factor(c(rep(1,nrow(Cluster1n)),rep(2,nrow(Cluster2n)),rep(3,nrow(Cluster3n)),rep(4,nrow(Cluster4n)),rep(5,nrow(Cluster5n))))

#La nueva empresa en el sistema
test=c(1,
       3,
       12,
       1,
       5,
       10,
       10,
       43,
       0.03172,
       0.30)

# Load the class package that holds the knn() function
library(class)

#Algoritmo KNN que nos dirá a que cluster pertenence según la minima distancia entre los puntos
resultado<-(knn(train, test, cl, k = 1))
attributes(.Last.value)

#Generamos salidas validas
pab1 <- "producto para"
pab2 <- resultado
union3 <- paste(pab1, Pais, pab2,sep="_")
nuevo<-read.csv(file=union3,sep=",", header=TRUE)

pab1 <- "Al nuevo cliente se le recomienda el C-S del Cluster"
pab2 <- resultado
union4 <- paste(pab1, pab2,sep="_")
write.csv(nuevo, file=union4, na="NA", row.names=TRUE)