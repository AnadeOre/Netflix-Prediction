library('mongolite')
source('AlgoritmoNetflix.R')
source('Credentials.R')

credentials = getCredentials()
DBInicial = mongo(collection = credentials$collectionName,db =  credentials$databaseName, url = credentials$url)
DBNew = mongo(collection = credentials$newColName, db =  credentials$databaseName, url = credentials$url)


loadIniciales = function () {
  matriz = DBInicial$find()
  cantidadPersonas = dim(matriz)[1]
  fields = c("nombre", "Stranger Things", "Elite", "Spider Man", "Cobra Kai", "Rebelde Way", "A Dos Metros de Ti",
             "The Umbrella Academy", "Anne With an e", "Estamos Muertos", "Riverdale", "Go! Vive a tu manera",
             "Yo Nunca", "Un Amor Tan Hermoso", "El Juego del Calamar", "Cielo Grande", "La Casa de Papel",
             "Avengers", "Eternals", "The Boys", "Wanda Vision",
             "Stranger_Things_check", "Elite_check", "Spider_Man_check", "Cobra_Kai_check", "Rebelde_Way_check",
             "A_Dos_Metros_de_Ti_check", "The_Umbrella_Academy_check", "Anne_With_an_e_check",
             "Estamos_Muertos_check", "Riverdale_check", "Go!_Vive_a_tu_manera_check", "Yo_Nunca_check",
             "Un_Amor_Tan_Hermoso_check", "El_Juego_del_Calamar_check", "Cielo_Grande_check", "La_Casa_de_Papel_check",
             "Avengers_check", "Eternals_check", "The_Boys_check", "Wanda_Vision_check")
  fields2 = c("nombre", "Stranger_Things", "Elite", "Spider_Man", "Cobra_Kai", "Rebelde_Way", "A_Dos_Metros_de_Ti",
              "The_Umbrella_Academy", "Anne_With_an_e", "Estamos_Muertos", "Riverdale", "Go!_Vive_a_tu_manera",
              "Yo_Nunca", "Un_Amor_Tan_Hermoso", "El_Juego_del_Calamar", "Cielo_Grande", "La_Casa_de_Papel",
              "Avengers", "Eternals", "The_Boys", "Wanda_Vision",
              "Stranger_Things_check", "Elite_check", "Spider_Man_check", "Cobra_Kai_check", "Rebelde_Way_check",
              "A_Dos_Metros_de_Ti_check", "The_Umbrella_Academy_check", "Anne_With_an_e_check",
              "Estamos_Muertos_check", "Riverdale_check", "Go!_Vive_a_tu_manera_check", "Yo_Nunca_check",
              "Un_Amor_Tan_Hermoso_check", "El_Juego_del_Calamar_check", "Cielo_Grande_check", "La_Casa_de_Papel_check",
              "Avengers_check", "Eternals_check", "The_Boys_check", "Wanda_Vision_check")
  
  
  nuevamatriz = matrix(NA, length(fields),cantidadPersonas)
  rownames(nuevamatriz) = fields
  colnames(nuevamatriz) = matriz[,1]
  # nuevamatriz = nuevamatriz[-1,]
  
  for (movie in 2:41) {
    for (persona in 1:cantidadPersonas) {
      if (!is.null(matriz[persona, (fields2[movie])])) {
        if (!is.na(matriz[persona, (fields2[movie])])) {
          nuevamatriz[fields[movie], persona] = matriz[persona, fields2[movie]]  
        }
      }
    }
  }
  
  dataNew = nuevamatriz
  if (!is.null(dataNew)) {
    if (dim(dataNew)[2]>= 2) {
      dataNew = dataNew[2:21,]
    }
  }
  dataNew
}



# Guardar nuevas entradas
saveData <- function(data) {
  print(data)
  names = colnames(loadData()$new)
  if (data[1] == '') {
      data[1] = paste('Usuario', round(runif(1, min=1, max = 1000)), sep = '')
   }
  if (data[1] %in% names) {
      data[1] = paste(data[1], round(runif(1, min=1, max = 1000)), sep = '')
  }
  for (i in 2:21) {
    if (!is.na(data[i]) & as.numeric(data[i]) > 10) {
      data[i] = '10'
    }
    if (!is.na(data[i]) & as.numeric(data[i]) < 0) {
      data[i] = '1'
    }
  }
  
  data <- t(data)
  # Create a unique file name
  data = data.frame(data)
  print(data)
  DBNew$insert(data)
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
  iniciales= loadIniciales()
  matriz = DBNew$find()
  matriz = matriz[nrow(matriz):1, ]
  cantidadPersonas = dim(matriz)[1]
  if (dim(matriz)[1]>0 & dim(matriz)[2]>0) {
    fields = c("nombre", "Stranger Things", "Elite", "Spider Man", "Cobra Kai", "Rebelde Way", "A Dos Metros de Ti",
                "The Umbrella Academy", "Anne With an e", "Estamos Muertos", "Riverdale", "Go! Vive a tu manera",
                "Yo Nunca", "Un Amor Tan Hermoso", "El Juego del Calamar", "Cielo Grande", "La Casa de Papel",
                "Avengers", "Eternals", "The Boys", "Wanda Vision",
                "Stranger_Things_check", "Elite_check", "Spider_Man_check", "Cobra_Kai_check", "Rebelde_Way_check",
                "A_Dos_Metros_de_Ti_check", "The_Umbrella_Academy_check", "Anne_With_an_e_check",
                "Estamos_Muertos_check", "Riverdale_check", "Go!_Vive_a_tu_manera_check", "Yo_Nunca_check",
                "Un_Amor_Tan_Hermoso_check", "El_Juego_del_Calamar_check", "Cielo_Grande_check", "La_Casa_de_Papel_check",
                "Avengers_check", "Eternals_check", "The_Boys_check", "Wanda_Vision_check")
    fields2 = c("nombre", "Stranger_Things", "Elite", "Spider_Man", "Cobra_Kai", "Rebelde_Way", "A_Dos_Metros_de_Ti",
                "The_Umbrella_Academy", "Anne_With_an_e", "Estamos_Muertos", "Riverdale", "Go!_Vive_a_tu_manera",
                "Yo_Nunca", "Un_Amor_Tan_Hermoso", "El_Juego_del_Calamar", "Cielo_Grande", "La_Casa_de_Papel",
                "Avengers", "Eternals", "The_Boys", "Wanda_Vision",
                "Stranger_Things_check", "Elite_check", "Spider_Man_check", "Cobra_Kai_check", "Rebelde_Way_check",
                "A_Dos_Metros_de_Ti_check", "The_Umbrella_Academy_check", "Anne_With_an_e_check",
                "Estamos_Muertos_check", "Riverdale_check", "Go!_Vive_a_tu_manera_check", "Yo_Nunca_check",
                "Un_Amor_Tan_Hermoso_check", "El_Juego_del_Calamar_check", "Cielo_Grande_check", "La_Casa_de_Papel_check",
                "Avengers_check", "Eternals_check", "The_Boys_check", "Wanda_Vision_check")
    
    
   nuevamatriz = matrix(NA, length(fields),cantidadPersonas)
    rownames(nuevamatriz) = fields
    colnames(nuevamatriz) = matriz[,1]
    # nuevamatriz = nuevamatriz[-1,]
    
    for (movie in 2:41) {
      for (persona in 1:cantidadPersonas) {
        if (!is.null(matriz[persona, (fields2[movie])])) {
          if (!is.na(matriz[persona, (fields2[movie])])) {
            nuevamatriz[fields[movie], persona] = matriz[persona, fields2[movie]]  
          }
        }
      }
    }
    
    dataNew = nuevamatriz
    if (!is.null(dataNew)) {
      if (dim(dataNew)[2]>= 2) {
        dataNew = dataNew[2:21,]
        return(list(new = dataNew, inicial = iniciales))
      }
    }  
  }
}


loadPainted = function() {
  
  matriz = DBNew$find()
  matriz = matriz[nrow(matriz):1, ]
  if (dim(matriz)[1]>0 & dim(matriz)[2]>0) {
    cantidadPersonas = dim(matriz)[1]
    
    fields = c("nombre", "Stranger Things", "Elite", "Spider Man", "Cobra Kai", "Rebelde Way", "A Dos Metros de Ti",
               "The Umbrella Academy", "Anne With an e", "Estamos Muertos", "Riverdale", "Go! Vive a tu manera",
               "Yo Nunca", "Un Amor Tan Hermoso", "El Juego del Calamar", "Cielo Grande", "La Casa de Papel",
               "Avengers", "Eternals", "The Boys", "Wanda Vision",
               "Stranger_Things_check", "Elite_check", "Spider_Man_check", "Cobra_Kai_check", "Rebelde_Way_check",
               "A_Dos_Metros_de_Ti_check", "The_Umbrella_Academy_check", "Anne_With_an_e_check",
               "Estamos_Muertos_check", "Riverdale_check", "Go!_Vive_a_tu_manera_check", "Yo_Nunca_check",
               "Un_Amor_Tan_Hermoso_check", "El_Juego_del_Calamar_check", "Cielo_Grande_check", "La_Casa_de_Papel_check",
               "Avengers_check", "Eternals_check", "The_Boys_check", "Wanda_Vision_check")
    fields2 = c("nombre", "Stranger_Things", "Elite", "Spider_Man", "Cobra_Kai", "Rebelde_Way", "A_Dos_Metros_de_Ti",
                "The_Umbrella_Academy", "Anne_With_an_e", "Estamos_Muertos", "Riverdale", "Go!_Vive_a_tu_manera",
                "Yo_Nunca", "Un_Amor_Tan_Hermoso", "El_Juego_del_Calamar", "Cielo_Grande", "La_Casa_de_Papel",
                "Avengers", "Eternals", "The_Boys", "Wanda_Vision",
                "Stranger_Things_check", "Elite_check", "Spider_Man_check", "Cobra_Kai_check", "Rebelde_Way_check",
                "A_Dos_Metros_de_Ti_check", "The_Umbrella_Academy_check", "Anne_With_an_e_check",
                "Estamos_Muertos_check", "Riverdale_check", "Go!_Vive_a_tu_manera_check", "Yo_Nunca_check",
                "Un_Amor_Tan_Hermoso_check", "El_Juego_del_Calamar_check", "Cielo_Grande_check", "La_Casa_de_Papel_check",
                "Avengers_check", "Eternals_check", "The_Boys_check", "Wanda_Vision_check")
    nuevamatriz = matrix(NA, length(fields),cantidadPersonas)
    rownames(nuevamatriz) = fields
    colnames(nuevamatriz) = matriz[,1]
    # nuevamatriz = nuevamatriz[-1,]
    
    for (movie in 2:41) {
      for (persona in 1:cantidadPersonas) {
        if (!is.null(matriz[persona, (fields2[movie])])) {
          if (!is.na(matriz[persona, (fields2[movie])])) {
            nuevamatriz[fields[movie], persona] = matriz[persona, fields2[movie]]  
          }
        }
      }
    }
    
    data = nuevamatriz[-1,]
    columnas = NULL
    filas = NULL
    if (!is.null(data)) {
      for (j in 1:dim(data)[2]) {
        persona = data[,j]
        solopintar = persona[-(1:20)]
        solopelis = persona[-(21:length(persona))]
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
    resp = loadData()$new
    nombre_select = colnames(resp)[col]
    pelis = c("Stranger_Things", "Elite", "Spider_Man", "Cobra_Kai", "Rebelde_Way", "A_Dos_Metros_de_Ti",
                "The_Umbrella_Academy", "Anne_With_an_e", "Estamos_Muertos", "Riverdale", "Go!_Vive_a_tu_manera",
                "Yo_Nunca", "Un_Amor_Tan_Hermoso", "El_Juego_del_Calamar", "Cielo_Grande", "La_Casa_de_Papel",
                "Avengers", "Eternals", "The_Boys", "Wanda_Vision")
    peli_select = pelis[row]
    cambiar = DBNew$find(paste0('{"nombre" : ','"', nombre_select,'"', '}') )
    DBNew$update(paste0('{"nombre" : ','"', nombre_select,'"', '}'),
                 paste0('{"$set":{"',peli_select,'": ','"',valor,'"','}}'))
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
