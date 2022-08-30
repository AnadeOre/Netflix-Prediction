library("aws.s3")
source('AlgoritmoNetflix.R')
source('Credentials.R')

# Cosas AWS
credentials = getCredentials()
bucketname <- credentials$bucketname
Sys.setenv("AWS_ACCESS_KEY_ID" = credentials$AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = credentials$AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = credentials$AWS_DEFAULT_REGION)
get_bucket_df(bucketname)[1]
# Fin cosas AWS


# Guardar nuevas entradas
saveData <- function(data) {
  names = colnames(loadData()$new)
  if (data[1] == '') {
      data[1] = paste('Usuario', round(runif(1, min=1, max = 1000)), sep = '')
   }
  if (data[1] %in% names) {
      data[1] = paste(data[1], round(runif(1, min=1, max = 1000)), sep = '')
      print(data[1])
  }
  for (i in 2:24) {
    if (!is.na(data[i]) & as.numeric(data[i]) > 10) {
      data[i] = '10'
    }
    if (!is.na(data[i]) & as.numeric(data[i]) < 0) {
      data[i] = '1'
    }
  }
   data2 = paste0(
      paste(names(data), collapse = ","), "\n",
      paste(unname(data), collapse = ",")
    )
   print(data2)
    file_name <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data2))
  
    put_object(file = charToRaw(data2), object = file_name, bucket = bucketname)
}

areEmpty = function(datos) {
  n = dim(datos)[1]
  m = dim(datos)[2]
  filas = NULL
  columnas = NULL
  for (i in 1:n) {
    for (j in 1:m) {
      if (is.na(datos[i,j])) {
        columnas = c(columnas, j)
        filas = c(filas, i)
      }
    }
  }
  return(list(rows = filas, cols = columnas))
}

# Cargar datos de la base
loadData <- function() {
  
  newPeople = NULL
  dbInicial = NULL
  
  file_names <- get_bucket_df(bucketname)[["Key"]]
  file_names = rev(file_names)
  
  for (file in file_names) {
    if (startsWith(file, 'persona')) {
      dbInicial = c(dbInicial, file)
    }
    else {
      newPeople = c(newPeople, file)
    }
  }
  # Establezco la base de datos inicial
  dataDBInicial <- lapply(dbInicial, function(x) {
    object <- get_object(x , bucketname)
    object_data <- readBin(object, "character")
    read.csv(text = object_data, stringsAsFactors = FALSE)
  })
  dataDBInicial <- do.call(rbind, dataDBInicial)
  colnames(dataDBInicial) = NULL
  rownames(dataDBInicial) = NULL
  dataDBInicial = t(dataDBInicial)
  dataDBInicial = dataDBInicial[-1,]
  
  # Ahora acomodo los que se van a ver
  dataNew <- lapply(newPeople, function(x) {
    object <- get_object(x , bucketname)
    object_data <- readBin(object, "character")
    read.csv(text = object_data, stringsAsFactors = FALSE)
  })
  dataNew <- do.call(rbind, dataNew)
  rownames(dataNew) = dataNew[,1]
  dataNew = dataNew[,-1]
  if (!is.null(dataNew)) {
    dataNew = t(dataNew)
    if (dim(dataNew)[2]>= 2) {
      dataNew = dataNew[1:23,]
      dataDBInicial = dataDBInicial[1:23,]
      return(list(new = dataNew, inicial = dataDBInicial))
    }
  }
}


loadPainted = function() {
  
  newPeople = NULL
  
  file_names <- get_bucket_df(bucketname)[["Key"]]
  file_names = rev(file_names)
  
  for (file in file_names) {
    if (!startsWith(file, 'persona')) {
      newPeople = c(newPeople, file)
    }
  }

  data <- lapply(newPeople, function(x) {
    object <- get_object(x , bucketname)
    object_data <- readBin(object, "character")
    read.csv(text = object_data, stringsAsFactors = FALSE)
  })
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  rownames(data) = data[,1]
  data = data[,-1]
  if (!is.null(data)) {
    data = t(data)
  }
  
  columnas = NULL
  filas = NULL
  if (!is.null(data)) {
    for (j in 1:dim(data)[2]) {
      persona = data[,j]
      solopintar = persona[-(1:23)]
      solopelis = persona[-(24:length(persona))]
      for (i in 1:length(solopintar)) {
        if (!is.na(solopintar[i]) & solopintar[i] != F) {
          filas = c(filas, i)
          columnas = c(columnas, j)
        }
      }
    }
    if (!is.null(columnas)) {
      columnas = matrix(columnas, length(columnas),1)
      filas = matrix(filas, length(filas),1)
      return(list(rows = filas, cols = columnas))
    }    
  }
}
  
  
# Cambiar valor
changeData = function(row, col, valor) {
  if(!is.na(row) & !is.na(col)) {
    if (!is.na(valor)) {
      if (as.numeric(valor) <0){
        valor = 1
      }
      if (as.numeric(valor)>10) {
        valor = 10
      }
    }
    
    
    newPeople = NULL
    
    file_names <- get_bucket_df(bucketname)[["Key"]]
    file_names = rev(file_names)
    
    for (file in file_names) {
      if (!startsWith(file, 'persona')) {
        newPeople = c(newPeople, file)
      }
    }
    
    data <- lapply(newPeople, function(x) {
      object <- get_object(x , bucketname)
      object_data <- readBin(object, "character")
      read.csv(text = object_data, stringsAsFactors = FALSE)
    })
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    rownames(data) = data[,1]
    data = data[,-1]
    if (!is.null(data)) {
      data = t(data)
    }
    resp = data
    
    nombre_select = colnames(resp)[col]
    resp[row, col] = valor
    rownames(resp) = NULL
    colnames(resp) = NULL
    columna = c(nombre_select,resp[,col])
    columna = data.frame(columna)
    fields2 = c("nombre", "Toy Story", "Kung Fu Panda", "Encanto", "Sonic", "Ralph el Demoledor", "Minions", "Coco", "Intensamente",
                "Cars", "Buscando a Nemo", "Avengers", "Hotel Transilvania", "Frozen", "Red", "Luca", "Eternals",
                "Los Increibles", "Unidos", "El Rey León", "Grandes Heroes", "Cruella", "Spider Man", "Soul",
                "Toy_Story_check", "Kung_Fu_Panda_check", "Encanto_check", "Sonic_check", "Ralph_el_Demoledor_check", "Minions_check", "Coco_check", "Intensamente_check",
                "Cars_check", "Buscando_a_Nemo_check", "Avengers_check", "Hotel_Transilvania_check", "Frozen_check", "Red_check", "Luca_check", "Eternals_check",
                "Los_Increibles_check", "Unidos_check", "El_Rey_León_check", "Grandes_Heroes_check", "Cruella_check", "Spider_Man_check", "Soul_check")
    
    rownames(columna) = fields2
    columna = t(columna)
    
    filename = newPeople[col]
    data2 = paste0(
      paste(colnames(columna), collapse = ","), "\n",
      paste(unname(columna), collapse = ",")
    )
    put_object(file = charToRaw(data2), object = filename, bucket = bucketname)
  }
  
}

mostrarPintados = function(filas, columnas) {
  Datos = loadData()
  resp = Datos$new
  completa = netflix(Datos$new, Datos$inicial)
  if(!is.null(filas)) {
    for (i in 1:dim(filas)[1]) {
      resp[filas[i,1], columnas[i,1]] = completa[filas[i,1], columnas[i,1]]
    }
  }
  resp
}
