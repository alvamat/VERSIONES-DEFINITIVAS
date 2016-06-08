###############################################################

#Author:Pol Àlvarez Matllana
#TFGv1
#Algortimo de Cross-Selling mediante Clústers

###############################################################

ptm <- proc.time()

for(i in 1:numcenters){
  file<-toString(i)
  
  #Detectar cluster más poblado
  Vectormayor<-nrow(data.frame(clients[(ClusterKmeans$cluster==i),]))
  ClusterImportante<-data.frame(clients[(ClusterKmeans$cluster==i),])
  
  
  
  #UsuariosCluster<-ClusterImportante$Datos.Customer.Code;
  
  #Cluster con picos más poblados (Primero ordenamos y luego seleccionamos)
  Productos_cluster<-(sort((ClusterImportante$Datos.Product.Description)));
  Productos_cluster<-as.numeric(names(which.max(table(Productos_cluster))));
  Spread_Rate<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster);
  
  Segundo_producto<-(filter(ClusterImportante, Datos.Product.Description!=Productos_cluster));
  
  Rep_num<-(filter(ClusterImportante, Datos.Product.Description==Productos_cluster));
  valorrep <- nrow(Rep_num)
  
  if(length(Segundo_producto)==0){
    excel2<-data.frame(ClusterImportante,Spread_Rate)
    write.csv(excel2, file="No_es_valido_CS",na="NA", row.names=TRUE)
  }else{
    Segundo_producto<-as.numeric(names(which.max(table(sort(Segundo_producto$Datos.Product.Description)))));
    
    Tercero_producto<-(filter(ClusterImportante, Datos.Product.Description!=Segundo_producto & Datos.Product.Description!=Productos_cluster));
    Tercero_producto<-as.numeric(names(which.max(table(sort(Tercero_producto$Datos.Product.Description)))));
    
    
    
    #Cross-selling
    
    #comprobar que empresas del producto más poblado no tienen los dos siguientes
    Clientes_Potenciales_P2<-filter(ClusterImportante, Datos.Product.Description!=Productos_cluster & Datos.Product.Description==Segundo_producto);
    Clientes_Potenciales_P3<-filter(ClusterImportante, Datos.Product.Description!=Productos_cluster & Datos.Product.Description==Tercero_producto);
    #clientes que ya tienen el primero hay que quitarlos de la lista
    
    
    
    #mostrará los dos productos siguientes ha recomendar en un arxivo xlm
    #2
    Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster)
    Spread=Clientes_Potenciales_P2$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
    
    
    excel2<-data.frame(Clientes_Potenciales_P2,Spread)
    colnames(excel2)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual', 'Spread Rate Nominal Futuro')
    
    pab1 <- "V1Clientes C-S"
    pab2 <- file
    pab3 <- "Con producto2"
    union1 <- paste(pab1, Pais, pab2, pab3,sep="_")
    write.csv(excel2, file=union1,na="NA", row.names=TRUE)
    pab1 <- "V1Producto para"
    pab2 <- file
    pab3 <- "Con producto2"
    union2 <- paste(pab1, Pais, pab2, pab3,sep="_")
    ProdMay<-data.frame(Producto_mayor,valorrep)
    write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
    
    #3
    Producto_mayor<-filter(excel, excel$clients.Datos.Product.Description==Productos_cluster)
    Spread=Clientes_Potenciales_P3$Datos.Spread.Rate.Nominal+Producto_mayor$clients.Datos.Spread.Rate
    
    
    excel3<-data.frame(Clientes_Potenciales_P3,Spread)
    
    colnames(excel3)=c('País','Cliente','Business','Industria','Tipo de Cliente','Area de provecho','Segmento','Area','Descripcion del producto','Spread Rate Nominal Actual','Spread Rate Nominal Futuro')
    
    pab1 <- "V1Clientes C-S"
    pab2 <- file
    pab3 <- "Con producto3"
    union1 <- paste(pab1, Pais, pab2,pab3,sep="_")
    write.csv(excel3, file=union1,na="NA", row.names=TRUE)
    pab1 <- "V1Producto para"
    pab2 <- file
    pab3 <- "Con producto3"
    union2 <- paste(pab1, Pais, pab2, pab3,sep="_")
    ProdMay<-data.frame(Producto_mayor,valorrep)
    write.csv(ProdMay, file=union2,na="NA", row.names=TRUE)
  }
  
}

v1<-proc.time() - ptm
