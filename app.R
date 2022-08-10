library(shiny)
library(Matrix)
library(softImpute)
library(DT)

# Esto para probar con los datos del excel
DatosPelis = read.csv('/home/ana/Dropbox/Netflix/DatosPelis.csv',header = TRUE,sep =',')
row.names(DatosPelis) = DatosPelis[,1]
DatosPelis = DatosPelis[,-1]


fields <- c("nombre", "peli1", "peli2", "peli3")

# Cargar archivos con los scripts
source('ScriptsDatos.R')
source('AlgoritmoNetflix')
source('ScriptsVisuales.R')

# Para borrar celdas específicas de la tabla
submatriz = loadData()
indexes = reactiveValues()
indexes$rows = NULL
indexes$cols = NULL


borrarIndex = function(indexes) {
  indexes$rows = NULL
  indexes$cols = NULL
}

shinyApp(
  ui = bootstrapPage(
    
  sidebarPanel( width = 3,
      textInput(
        inputId = "nombre",
        label = 'Nombre',
        value = '',
        width = '200px'
      ),
      numericInput(
        inputId = "peli1", 
        label = "Película 1",
        value = NA,
        width = '70px',
        min = 1,
        max = 10),
      numericInput(
        inputId = "peli2", 
        label = "Película 2",
        value = NA,
        width = '70px'),
      numericInput(
        inputId = "peli3", 
        label = "Película 3",
        value = NA,
        width = '70px'),
      
      div(style="display:flex; justify-content:left",
        div(style='padding:2px', actionButton("submit", "Submit")),
        div(style='padding:2px', actionButton("completar", "Completar")),
      ),
      
      div(style="display:flex; justify-content:left",
        div(style='padding:2px',
        numericInput(
        inputId = 'elegir_row',
        label = 'Fila',
        value = NA,
        width = '60px')),
      div(style='padding:2px',
        numericInput(
        inputId = 'elegir_col',
        label = 'Columna',
        value = NA,
        width = '60px'))
      ),
      
      div(style="display:flex; justify-content:left",
          div(style='padding:2px', actionButton('elegir', 'Elegir')),
          div(style='padding:2px', actionButton('completarSubMatriz', 'Completar'))
      )),
  
  mainPanel(
    dataTableOutput("responses", width = 500),
    dataTableOutput('submatriz', width = 500),
    dataTableOutput("completo", width = 500), 
  )
  ),
  
  
  server = function(input, output, session) {
    
    # Se completa una persona
    formData <- reactive({
      data <- sapply(fields, function(x) ifelse(input[[x]]!=0, input[[x]], NA))
      data
    })
    
    # Al apretar el botón se guarda la persona
    observeEvent(input$submit, {
      saveData(formData())
      submatriz = loadData()
    })
    
    # Mostrar y actualizar
    output$responses <- renderDataTable({
      input$submit
      datatable(loadData(),
        options = list(autoWidth = TRUE,
          columnDefs = list(list(width = '50px', targets = 1))
      ))
      # datatable(DatosPelis)
    })
    # Botón de completar
    observeEvent(input$completar, {
      output$completo <- renderDataTable({
        datatable(netflix(DatosPelis),
          options = list(autoWidth = TRUE,
             columnDefs = list(list(width = '50px', targets = 1))
          ))
        # datatable(netflix(DatosPelis))
      }) 
    })
    
    # Elegir celda para eliminar
    observeEvent(input$elegir, {
      submatriz[input$elegir_row, input$elegir_col] <<- NA
      indexes$rows = rbind(indexes$rows, input$elegir_row)
      indexes$cols = rbind(indexes$cols, input$elegir_col)
      output$submatriz = renderDataTable({
         datatable(submatriz, options = list(
           dom = "t",
           rowCallback = JS(changeCellColorRed(indexes$rows, indexes$cols))
         ))
      })
    })
    # Completar la matriz con celdas eliminadas
    observeEvent(input$completarSubMatriz, {
      output$submatriz = renderDataTable({
        datatable(netflix(submatriz), options = list(
          dom = "t",
          rowCallback = JS(changeCellColorGreen(indexes$rows, indexes$cols))
        ))
      })
    })
  }
)
