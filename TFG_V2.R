###############################################################

#Author:Pol Àlvarez Matllana
#TFGv2
#Algortimo de Cross-Selling mediante Clústers

###############################################################

#Programa a seleccionar:
#         a-> solo tendra en cuenta la frecuencia poblacional
#         b-> solo tendra en cuenta el spread rate del producto
#         las dos opciones a 1 dan un resultado ponderado de ambos campos

a<-1;
b<-1;
ptm <- proc.time()

for(i in 1:numcenters){
  file<-toString(i)
  
  Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==i),]));
  ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==i),]);
  
  
  #Comprovamos que valor tiene el más reptido
  Productos_cluster<-(sort((ClusterImportante$Datos.Product.Description)));
  Productos_cluster<-as.numeric(names(which.max(table(Productos_cluster))));
  Quitar=Productos_cluster;
  
  Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster));
  R=nrow(Rep_num)/nrow(ClusterImportante);
  
  Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster);
  S=Spread_Rate$clients.Datos.Spread.Rate.Nominal;
  
  
  P=(R*(0.65*a)+S*(0.35*b));
  Producto=Quitar;
  pob_mej=1;
  valorrep<-nrow(Rep_num)
  
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
    pab1 <- "V2No es valido C-S"
    pab2 <- file
    union1 <- paste(pab1, Pais, pab2,sep="_")
    excel2<-data.frame(ClusterImportante,Spread_Rate)
    write.csv(excel2, file=union1,na="NA", row.names=TRUE)
  }else{
    
    while(Long>=1){
      
      Productos_cluster1<-filter(Productos_cluster1,Productos_cluster1$sort..ClusterImportante.Datos.Product.Description..!=Quitar)
      Productos_cluster2<-as.numeric(names(which.max(table(Productos_cluster1))));
      Quitar=Productos_cluster2;
      
      Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster2));
      R=nrow(Rep_num)/nrow(ClusterImportante);
      
      Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster2);
      S=Spread_Rate$clients.Datos.Spread.Rate.Nominal;
      
      P1=(R*(0.65*a)+S*(0.35*b));
      
      if(P1>=P){
        P=P1
        Producto=Quitar;
        pob_mej=t;
        valorrep<-nrow(Rep_num)
      }
      
      Long=Long-1;
      t=t+1;
      
      
    }
    #Generamos fichero
    
    if(pob_mej==1){
      Segundo_producto<-(filter(ClusterImportante, Datos.Product.Description!=Productos_cluster));
      Segundo_producto<-as.numeric(names(which.max(table(sort(Segundo_producto$Datos.Product.Description)))));
      
      Clientes_Potenciales<-filter(ClusterImportante, Datos.Product.Description!=Producto & Datos.Product.Description==Segundo_producto);
      
      Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Producto)
      Spread=Clientes_Potenciales$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
      
      
      excel2<-data.frame(Clientes_Potenciales,Spread)
      
      colnames(excel2)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual', 'Spread Rate Nominal Futuro')
      
      pab1 <- "V2No es posible C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "V2Producto para"
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
      
      pab1 <- "V2Clientes C-S"
      pab2 <- file
      union1 <- paste(pab1, Pais, pab2,sep="_")
      write.csv(excel2, file=union1,na="NA", row.names=TRUE)
      pab1 <- "V2Producto para"
      pab2 <- file
      union2 <- paste(pab1, Pais, pab2,sep="_")
      ProdMay<-data.frame(Producto_mayor,valorrep)
      write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
    }
  }
  
}

v2<-proc.time() - ptm
