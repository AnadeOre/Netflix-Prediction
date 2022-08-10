outputDir <- "responses"

# Guardar nuevas entradas
saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
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

