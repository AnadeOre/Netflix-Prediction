outputDir <- "/home/ana/Documents/ShinyR/Pelis-Netflixs/responses"
loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  dataFull <- do.call(rbind, data)
  rownames(dataFull) = dataFull[,1]
  dataFull = dataFull[,-1]
  if (!is.null(dataFull)) {
    dataFull = t(dataFull)
  }
  dataFull
}
resp = loadData()

names = NULL
for (file in data) {
  names = c(names, file$nombre)
}
