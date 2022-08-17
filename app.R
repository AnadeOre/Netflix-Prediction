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
source('AlgoritmoNetflix.R')
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

headerCallback <- c(
  "function(thead, data, start, end, display){",
  "  var $ths = $(thead).find('th');",
  "  $ths.css({'vertical-align': 'bottom', 'white-space': 'nowrap'});",
  "  var betterCells = [];",
  "  $ths.each(function(){",
  "    var cell = $(this);",
  "    var newDiv = $('<div>', {height: 'auto', width: '8px'});",
  "    var newInnerDiv = $('<div>', {text: cell.text()});",
  "    newDiv.css({margin: 'auto'});",
  "    newInnerDiv.css({",
  "      transform: 'rotate(180deg)',",
  "      'writing-mode': 'tb-rl',",
  "      'white-space': 'nowrap'",
  "    });",
  "    newDiv.append(newInnerDiv);",
  "    betterCells.push(newDiv);",
  "  });",
  "  $ths.each(function(i){",
  "    $(this).html(betterCells[i]);",
  "  });",
  "}"
)

shinyApp(
  ui = bootstrapPage(
    
  sidebarPanel( width = 3,
                
    tags$head(
    tags$style(type="text/css", "
    #loadmessage {
     position: fixed;
     top: 0px;
     left: 0px;
     width: 100%;
     padding: 5px 0px 5px 0px;
     text-align: center;
     font-weight: bold;
     font-size: 100%;
     color: #000000;
     background-color: #CCFF66;
     z-index: 105;
    }
   
   @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');

body {
  background-color:  white;
  color: rgb(54, 31, 31); /* text color */
}

/* Change header text to imported font */
h2 {
    font-family: 'Bebas Neue';
    text-transform: uppercase;
    font-weight: 900;
    color: #E50914;
}

.td-text-center {
  width = '10px';
}

/* Make text visible on inputs */
.shiny-input-container {
  color: #564d4d;
}
"),
  ),
  titlePanel("Netflix"),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")),         
    h3("Agregar Datos"),
    helpText("Complete todos los campos para agregar una nueva persona a la base de datos."),  
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
      
      # CAMBIAR VALOR
      h3("Cambiar Dato"),
      helpText("Elija la columna y fila del dato mal ingresado para cambiarlo."),
      div(style="display:flex; justify-content:left",
        div(style='padding:2px',
        numericInput(
        inputId = 'elegir_row_cambiar',
        label = 'Fila',
        value = NA,
        width = '60px')),
      div(style='padding:2px',
        numericInput(
        inputId = 'elegir_col_cambiar',
        label = 'Columna',
        value = NA,
        width = '60px')),
      div(style='padding:2px',
          numericInput(
            inputId = 'cambiar_num',
            label = 'Calificación',
            value = NA,
            width = '60px')),
      div(style='margin-top:27px', actionButton('cambiar_valor_boton', 'Cambiar'))
      ),
  
  h3("Pintar celda"),
  helpText("Elija la columna y fila de la celda que desea pintar."),

    div(style="display:flex; justify-content:left",
        div(style='padding:2px',
            numericInput(
              inputId = 'pintar_row',
              label = 'Fila',
              value = NA,
              width = '60px')),
        div(style='padding:2px',
            numericInput(
              inputId = 'pintar_col',
              label = 'Columna',
              value = NA,
              width = '60px')),
      div(style='margin-top:27px', actionButton('pintar', 'Pintar')),
    ),
),
  
  mainPanel(
    div(style = 'padding-top: 35px;', dataTableOutput("responses")),
    div(style = 'padding-top: 35px;', dataTableOutput('submatriz')),
    # div(style = 'margin-top: 35px;', dataTableOutput("completo")),
  )
  ),
  
  
  server = function(input, output, session) {
    
    # Se completa una persona
    formData <- reactive({
      data <- sapply(fields, function(x) ifelse(input[[x]]!=0, input[[x]], NA))
      data
    })
    
    # GUARDAR NUEVA PERSONA
    observeEvent(input$submit, {
      saveData(formData())
      submatriz = loadData()
    })
    
    # TABLA 1
    output$responses <- renderDataTable({
      input$submit
      datatable(loadData(), options = list(
        headerCallback = JS(headerCallback),
        autoWidth = TRUE,
        searching = FALSE,
        pageLength = 25,
        columnDefs = list(list(width = '8px', targets = "_all"))
      ))
    })
    # TABLA 1 COMPLETA
    observeEvent(input$completar, {
      output$responses <- renderDataTable({
        datatable(netflix(loadData()), options = list(
          rowCallback = JS(changeCellColorGreen(indexes$rows, indexes$cols)),
          headerCallback = JS(headerCallback),
          autoWidth = TRUE,
          searching = FALSE,
          pageLength = 25,
          columnDefs = list(list(width = '8px', targets = "_all"))
        ))
      }) 
    })
  
    # CAMBIAR VALOR DE CELDA
    observeEvent(input$cambiar_valor_boton, {
      changeData(input$elegir_row_cambiar, input$elegir_col_cambiar, input$cambiar_num)
      output$responses = renderDataTable({
        datatable(loadData(), options = list(
          headerCallback = JS(headerCallback), 
          autoWidth = TRUE,
          searching = FALSE,
          pageLength = 25,
          columnDefs = list(list(width = '8px', targets = "_all"))
        ))
      })
    })

    observeEvent(input$pintar, {
      indexes$rows = rbind(indexes$rows, input$pintar_row)
      indexes$cols = rbind(indexes$cols, input$pintar_col)
      output$responses = renderDataTable({
        datatable(loadData(), options = list(
          rowCallback = JS(changeCellColorRed(indexes$rows, indexes$cols)),
          headerCallback = JS(headerCallback),
          autoWidth = TRUE,
          searching = FALSE,
          pageLength = 25,
          columnDefs = list(list(width = '8px', targets = "_all"))
        ))
      })
    })
  }
)
