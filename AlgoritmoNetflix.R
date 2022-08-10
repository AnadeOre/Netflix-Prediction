source('Lambda.R')

netflix = function(DatosPelis) {
  #Segun datos colocamos las respuestas extremas
  vmin=1
  vmax=10
  ##--------------------##
  #renombramos la matriz con datos faltantes
  X=DatosPelis
  X=X[,1:dim(X)[2]]
  X = as.matrix(X)
  
  #Creamos la matriz proyecci?n(lo uso solo para calcular lambda)
  PX=data.matrix(X)
  PX[is.na(PX)==T]=0
  
  # LAMBDA=1
  # LAMBDA=norm(PX,"2")/1.5 #Este es el que considera en el paper
  # LAMBDA
  LAMBDA = lambda(X)
  
  #Hacemos el algoritmo iterativo que busca las matrices U,D,V
  #Obs: en sofImpute se puede modificar type="svd","als", segun que metodo queremos usar
  # agregar el valor de lambda (lambda=valor)
  # limitar el rango a utilzar (rank.max=r)
  
  fits=softImpute(X,trace=TRUE,rank.max=6,lambda=LAMBDA,type="svd")
  
  #Completar la matriz
  Y=complete(X,fits)
  # Y
  
  #Devuelve la matriz solo con enteros
  Y=round(Y)
  # Y
  
  #OPCIONAL, obligamos a que el menor valor de la matriz sea 1 y el mayor 5
  #Pero pero pero, revisar que no haya negativos pq eso no esta bueno
  #Como tampoco esta bueno si hay valores mayores a 5
  Y[Y<vmin]=vmin
  Y[Y>vmax]=vmax
  # Y
  
  #?CUIDADO!  N O   C O R R E R  o lo vas a pisar!
  guardar=Y
  Y
}