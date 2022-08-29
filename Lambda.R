# Usamos cross validation para encontrar el mejor lambda

lambda = function(X) {
  vmin=1
  vmax=10
  LAMBDA=seq(1,20,.5)
  N=length(LAMBDA)
  error=numeric(N)
  
  ## me quedo con una submuestra de observados (no NA) 
  R=min(300,nrow(X)*ncol(X))
  B=5
  lambdas=numeric(B)
  for(k in 1:B) {
  filas=sample(1:nrow(X),R,replace=TRUE)
  columnas=sample(1:ncol(X),R,replace=TRUE)
  
  aux=X[cbind(filas,columnas)]
  eliminar=which(is.na(aux))
  subX=aux[-eliminar]
  
  filass=filas[-eliminar]
  columnass=columnas[-eliminar]
  
  
  Xmodif=X
  Xmodif[cbind(filass,columnass)]=NA
  
  for(i in 1:N) {
    fits = softImpute(Xmodif,trace=TRUE,rank.max=6,lambda=LAMBDA[i],type="svd")
     #Completar la matriz
    Y=complete(Xmodif,fits)
   
    
    #Devuelve la matriz solo con enteros
    Y=round(Y,1)
   
    #OPCIONAL, obligamos a que el menor valor de la matriz sea 1 y el mayor 10
    #Pero pero pero, revisar que no haya negativos pq eso no esta bueno
    #Como tampoco esta bueno si hay valores mayores a 10
    Y[Y<vmin]=vmin
    Y[Y>vmax]=vmax
    
    subY=diag(Y[filass,columnass])
    error[i]=sqrt(sum((subY-subX)^2))
  }
  
  index.lambda=(1:N)[error==min(error)]
  lambdas[k]=LAMBDA[index.lambda]
  }
  return(median(lambdas))
}


lambda2=function(X)
{
  
  LAMBDA=seq(1,20,.5)
  N=length(LAMBDA)
  error=numeric(N)
  
  ## me quedo con una submuestra de observados (no NA) 
  R=min(300,nrow(X)*ncol(X))
  B=5
  lambdas=numeric(B)
  for(k in 1:B)
  {
    filas=sample(1:nrow(X),R,replace=TRUE)
    columnas=sample(1:ncol(X),R,replace=TRUE)
    
    aux=X[cbind(filas,columnas)]
    eliminar=which(is.na(aux))
    subX=aux[-eliminar]
    
    filass=filas[-eliminar]
    columnass=columnas[-eliminar]
    
    
    Xmodif=X
    Xmodif[cbind(filass,columnass)]=NA
    errorv=1001
    errorn=1000
    i=0
    error=numeric(N)
    while(errorv>errorn)
    { 
      i=i+1
      errorv=errorn
      
      fits = softImpute(Xmodif,trace=TRUE,rank.max=6,lambda=LAMBDA[i],type="svd")
      #Completar la matriz
      Y=complete(Xmodif,fits)
      
      
      #Devuelve la matriz solo con enteros
      Y=round(Y,1)
      
      #OPCIONAL, obligamos a que el menor valor de la matriz sea 1 y el mayor 10
      #Pero pero pero, revisar que no haya negativos pq eso no esta bueno
      #Como tampoco esta bueno si hay valores mayores a 10
      vmin=1
      vmax=10
      Y[Y<vmin]=vmin
      Y[Y>vmax]=vmax
      
      subY=diag(Y[filass,columnass])
      errorn=sqrt(sum((subY-subX)^2))
      error[i]=errorn
      
    }
    index.lambda=(1:N)[error==min(error[error!=0])]
    lambdas[k]=LAMBDA[index.lambda]
  }
  
  return(median(lambdas))
}

