outputDir <- "responses"

# Guardar nuevas entradas
saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  print(data)
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

# Cargar datos de la base
loadData <- function() {
  # Leer archivos
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Acomodar nombres 
  dataFull <- do.call(rbind, data)
  rownames(dataFull) = dataFull[,1]
  dataFull = dataFull[,-1]
  if (!is.null(dataFull)) {
    dataFull = t(dataFull)
  }
  dataFull
}

# Cambiar valor
changeData = function(row, col, valor) {
  outputDir <- "~/Documents/ShinyR/Pelis-Netflix/responses"
  resp = loadData()
  nombre_select = colnames(resp)[col]
  resp[row, col] = valor
  rownames(resp) = NULL
  colnames(resp) = NULL
  columna = c(nombre_select,resp[,col])
  columna = data.frame(columna)
  titulo = c('nombre', 'peli1', 'peli2', 'peli3')
  rownames(columna) = titulo
  colnames(columna) = NULL
  columna = t(columna)
  archivo = list.files(outputDir, full.names = TRUE)[col]
  archivo_editar = strsplit(archivo, '/')[[1]][8]
  write.csv(
    x = columna,
    file = file.path(outputDir, archivo_editar),
    row.names = FALSE, quote = TRUE
  )
}
