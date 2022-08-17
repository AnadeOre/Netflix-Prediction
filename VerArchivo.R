outputDir <- "~/Documents/ShinyR/Pelis-Netflix/responses"
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
  dataFull
}
resp = loadData()


