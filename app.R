library(Matrix)
library(softImpute)
library(DT)

fields2 = c("nombre", "Stranger Things", "Elite", "Spider Man", "Cobra Kai", "Rebelde Way", "A Dos Metros de Ti",
            "The Umbrella Academy", "Anne With an e", "Estamos Muertos", "Riverdale", "Go Vive a tu Manera",
            "Yo Nunca", "Un Amor Tan Hermoso", "El Juego del Calamar", "Cielo Grande", "La Casa de Papel",
            "Avengers", "Eternals", "The Boys", "Wanda Vision",
            "Stranger_Things_check", "Elite_check", "Spider_Man_check", "Cobra_Kai_check", "Rebelde_Way_check",
            "A_Dos_Metros_de_Ti_check", "The_Umbrella_Academy_check", "Anne_With_an_e_check",
            "Estamos_Muertos_check", "Riverdale_check", "Go_Vive_a_tu_Manera_check", "Yo_Nunca_check",
            "Un_Amor_Tan_Hermoso_check", "El_Juego_del_Calamar_check", "Cielo_Grande_check", "La_Casa_de_Papel_check",
            "Avengers_check", "Eternals_check", "The_Boys_check", "Wanda_Vision_check")
# Cargar archivos con los scripts
source('ScriptsDatos.R')
source('AlgoritmoNetflix.R')
source('ScriptsVisuales.R')

# Para borrar celdas específicas de la tabla
submatriz = loadData()$new
# Para pintar celdas especificas
indexes = reactiveValues()
indexes$rows = NULL
indexes$cols = NULL

cantidad = reactiveValues()
cantidad$entradas = dim(submatriz)[2]
cantidad$nombres = colnames(submatriz)

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
  "    var newDiv = $('<div>', {height: 'auto'});",
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
    
            
    
    titlePanel(
      windowTitle = "Netflix",
      title = tags$head(tags$link(rel="shortcut icon", 
                                  href="https://www.unl.edu.ar/wp-content/uploads/2018/05/cropped-favicon.png", 
                                  type="image/vnd.microsoft.icon"))),

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
     background-color: #9ff081;
     z-index: 105;
    }
   
    @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
    @import url('https://fonts.googleapis.com/css2?family=Libre+Franklin&family=Open+Sans&family=Roboto+Condensed&display=swap');
body {
  background-color:  white;
  color: rgb(54, 31, 31);
  }

.shiny-output-error {
  visibility: hidden;
  }
.shiny-output-error:before {
  visibility: hidden;
  }
.dataTables_filter {
  display: none;
  }


.title-netflix {
    font-family: 'Roboto Condensed';
    text-transform: uppercase;
    font-weight: 900;
    font-size: 65px;
    color: #E50914;
    text-align: center;
  }
.title-unl {
  font-family: 'Libre Franklin';
  font-weight: 900;
  font-size: 70px;
  color: #008bb8;;
  }

.td-text-center {
  width = '10px';
  }

/* Make text visible on inputs */
.shiny-input-container {
  color: #564d4d;
  }

.numeric-input.focus {
    border-color: #ffc133;
    outline: 0;
    -webkit-box-shadow: inset 0 1px 1px rgba(255,193,51,.075), 0 0 8px rgba(255,193,51,.075);
    box-shadow: inset 0 1px 1px rgba(255,193,51,.075), 0 0 8px rgba(255,193,51,.075);
  }

.div_contenedor {
  width: 100%;
}

"),
    tags$style(HTML('
              #submit{
                border-width: 2px;
                border-color:#fabb28;
              }
              #submit:hover{
                background-color:#fabb28;
                transition: background-color 0.3s ease-in-out;
              }
              #completar_pintados{
                border-width: 2px;
                border-color:#8233af;
              }
              #completar_pintados:hover{
                background-color:#8233af;
                color: white;
                transition: color 0.3s ease-in-out;
                transition: background-color 0.3s ease-in-out;
              }
              #completar{
                border-width: 2px;
                border-color:#1cb353;
              }
              #completar:hover{
                background-color:#1cb353;
                color: white;
                transition: color 0.3s ease-in-out;
                transition: background-color 0.3s ease-in-out;
              }
              #cambiar_valor_boton{
                border-width: 2px;
                border-color:#2d80f2;
              }
              #cambiar_valor_boton:hover{
                background-color:#2d80f2;
                color: white;
                transition: color 0.3s ease-in-out;
                transition: background-color 0.3s ease-in-out;
              }
              #elegir_col_cambiar {
                padding: 5px;
              }
              #elegir_row_cambiar {
                padding: 5px;
              }
              #cambiar_num {
                padding: 5px;
              }
              .group label {
                font-size: 12px;
                display: inline-block;
                width: 150%;
                text-align: center;
              }
              label {
                width: 700px;
              }
              span {
                margin-bottom: -5px;
              }
                    '))
  ),
  h2('Netflix', class='title-netflix'),
  # span('UNL',class='title-unl'),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage")), 
  
  div(style="display:flex; justify-content:center; margin-top: 10px;",
      div(style='padding:2px;', actionButton("completar_pintados","Completar Pintados")),
      div(style='padding:2px', actionButton("completar", "Completar Todo")),
  ),
  h3("Agregar Datos"),
    helpText("Complete los campos con números del 1 al 10 y no escriba nada si no vió la película o serie."),  
    textInput(
        inputId = "nombre",
        label = 'Nombre',
        value = '',
        width = '100px'
      ),
      div(numericInput(#1
        inputId = "Stranger Things", 
        label = "1- Stranger Things",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
       checkboxInput("Stranger_Things_check", "Pintar Stranger Things", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#2
        inputId = "Elite", 
        label = "2- Elite",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
       checkboxInput("Elite_check", "Pintar Elite", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#3
        inputId = "Spider Man", 
        label = "3- Spider Man",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
       checkboxInput("Spider_Man_check", "Pintar Spider Man", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#4
        inputId = "Cobra Kai", 
        label = "4- Cobra Kai",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
       checkboxInput("Cobra_Kai_check", "Pintar Cobra Kai", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#5
        inputId = "Rebelde Way", 
        label = "5- Rebelde Way",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Rebelde_Way_check", "Pintar Rebelde Way", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#6
        inputId = "A Dos Metros de Ti", 
        label = "6- A Dos Metros de Ti",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("A_Dos_Metros_de_Ti_check", "Pintar A Dos Metros de Ti", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#7
        inputId = "The Umbrella Academy", 
        label = "7- The Umbrella Academy",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("The_Umbrella_Academy_check", "Pintar The Umbrella Academy", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#8
        inputId = "Anne With an e", 
        label = "8- Anne With an e",
        value = NA,
        width = '110px',
        min = 1,
        max = 10),
      checkboxInput("Anne_With_an_e_check", "Pintar Anne With an e", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#9
        inputId = "Estamos Muertos", 
        label = "9- Estamos Muertos",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Estamos_Muertos_check", "Pintar Estamos Muertos", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#10
        inputId = "Riverdale", 
        label = "10- Riverdale",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Riverdale_check", "Pintar Riverdale", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#11
        inputId = "Go Vive a tu Manera", 
        label = "11- Go! Vive a tu Manera",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Go_Vive_a_tu_Manera_check", "Pintar Go! Vive a tu Manera", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#12
        inputId = "Yo Nunca", 
        label = "12- Yo Nunca",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Yo_Nunca_check", "Pintar Yo Nunca", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#13
        inputId = "Un Amor Tan Hermoso", 
        label = "13- Un Amor tan Hermoso",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Un_Amor_Tan_Hermoso_check", "Pintar Un Amor tan Hermoso", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#14
        inputId = "El Juego del Calamar", 
        label = "14- El Juego del Calamar",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("El_Juego_del_Calamar_check", "Pintar El Juego del Calamar", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#15
        inputId = "Cielo Grande", 
        label = "15- Cielo Grande",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Cielo_Grande_check", "Pintar Cielo Grande", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#16
        inputId = "La Casa de Papel", 
        label = "16- La Casa de Papel",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("La_Casa_de_Papel_check", "Pintar La Casa de Papel", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#17
        inputId = "Avengers", 
        label = "17- Avengers",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Avengers_check", "Pintar Avengers", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#18
        inputId = "Eternals", 
        label = "18- Eternals",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Eternals_check", "Pintar Eternals", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#19
        inputId = "The Boys", 
        label = "19- The Boys",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("The_Boys_check", "Pintar The Boys", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#20
        inputId = "Wanda Vision", 
        label = "20- Wanda Vision",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Wanda_Vision_check", "Pintar Wanda Vision", FALSE), class = 'div_contenedor'),
    
  
      div(style='padding:2px', actionButton("submit", "Submit")),
  
      # 
      # h3("Pintar celda"),
      # helpText("Elija la columna y fila de la celda que desea pintar."),
      # 
      # div(style="display:flex; justify-content:left",
      #     div(style='padding:2px',
      #         numericInput(
      #           inputId = 'pintar_row',
      #           label = 'Fila',
      #           value = NA,
      #           width = '60px')),
      #     div(style='padding:2px',
      #         numericInput(
      #           inputId = 'pintar_col',
      #           label = 'Columna',
      #           value = NA,
      #           width = '60px')),
      #     div(style='margin-top:27px', actionButton('pintar', 'Pintar')),
      # ),
      
      # CAMBIAR VALOR
      h3("Cambiar Dato"),
      helpText("Elija la columna y fila del dato mal ingresado para cambiarlo."),
      div(style="display:flex; justify-content:center;", class='group',
        div(style='padding:2px',
        numericInput(
        inputId = 'elegir_row_cambiar',
        label = 'Fila',
        value = NA,
        width = '60px')),
      div(style='padding:2px',
        # textInput(
        #   inputId = "elegir_col_cambiar",
        #   label = 'Columna',
        #   value = '',
        #   width = '100px'
        # ),
        numericInput(
        inputId = 'elegir_col_cambiar',
        label = 'Columna',
        value = NA,
        width = '60px')),
      div(style='padding:2px',
          numericInput(
            inputId = 'cambiar_num',
            label = 'Valor',
            value = NA,
            width = '60px')),
      div(style='margin-top:26px', actionButton('cambiar_valor_boton', 'Cambiar'))
      ),
),
  
  mainPanel(
    div(style = 'padding-top: 35px;', dataTableOutput("responses"))
  )
  ),
  
  
  server = function(input, output, session) {
    
    # Se completa una persona
    formData <- reactive({
      data <- sapply(fields2, function(x) ifelse(input[[x]]!=0, input[[x]], NA))
      data
    })

    # GUARDAR NUEVA PERSONA
    observeEvent(input$submit, {
      saveData(formData())
      submatriz = loadData()$new
      cantidad$entradas = dim(submatriz)[2]
      loadData()$new
    })

    # TABLA INCOMPLETA
    output$responses <- renderDataTable({
      pintar = loadPainted()
      datatable(loadData()$new, options = list(
        rowCallback = JS(changeCellColorRed(pintar$rows, pintar$cols)),
        headerCallback = JS(headerCallback),
        autoWidth = FALSE,
        pageLength = 24,
        searching = FALSE,
        scrollX = TRUE,
        columnDefs = list(list(width = '5px', targets =1:cantidad$entradas, className = 'dt-center'))
      ),
      selection = "none")
    })

    # COMPLETAR SOLO COLOREADOS
    observeEvent(input$completar_pintados, {
      pintar = loadPainted()
      output$responses <- renderDataTable({
        datatable(mostrarPintados(pintar$rows, pintar$cols), options = list(
          rowCallback = JS(changeCellColorGreen(pintar$rows, pintar$cols)),
          headerCallback = JS(headerCallback),
          autoWidth = FALSE,
          pageLength = 24,
          searching = FALSE,
          scrollX = TRUE,
          columnDefs = list(list(width = '5px', targets =1:cantidad$entradas, className = 'dt-center'))
        ),
        selection = "none")
      })
    })

    # COMPLETAR TODO
    observeEvent(input$completar, {
      output$responses <- renderDataTable({
        allData = loadData()
        pintar = areEmpty(allData$new)
        datatable(netflix(allData$new, allData$inicial), options = list(
        rowCallback = JS(changeCellColorGreen(pintar$rows, pintar$cols)),
        headerCallback = JS(headerCallback),
        autoWidth = FALSE,
        pageLength = 24,
        searching = FALSE,
        scrollX = TRUE,
        columnDefs = list(list(width = '5px', targets =1:cantidad$entradas, className = 'dt-center'))
        ),
        selection = "none")
      })
    })

    # # # PINTAR CELDA
    # # observeEvent(input$pintar, {
    # #   indexes$rows = rbind(indexes$rows, input$pintar_row)
    # #   indexes$cols = rbind(indexes$cols, input$pintar_col)
    # #   print(indexes$cols)
    # #   print(indexes$rows)
    # #   output$responses = renderDataTable({
    # #     datatable(loadData(), options = list(
    # #       rowCallback = JS(changeCellColorRed(indexes$rows, indexes$cols)),
    # #       headerCallback = JS(headerCallback), 
    # #       autoWidth = FALSE,
    # #       searching = FALSE,
    # #       scrollX = TRUE,
    # #       columnDefs = list(list(width = '5px', targets =1:cantidad$entradas, className = 'dt-center'))
    # #     ),
    # #     selection = "none")
    # #   })
    # # })
    # 
    # CAMBIAR VALOR DE CELDA
    observeEvent(input$cambiar_valor_boton, {
      pintar = loadPainted()
      changeData(input$elegir_row_cambiar, input$elegir_col_cambiar, input$cambiar_num)
      allData = loadData()
      output$responses = renderDataTable({
        datatable(allData$new, options = list(
          headerCallback = JS(headerCallback),
          rowCallback = JS(changeCellColorRed(pintar$rows, pintar$cols)),
          autoWidth = FALSE,
          pageLength = 24,
          searching = FALSE,
          scrollX = TRUE,
          columnDefs = list(list(width = '5px', targets =1:cantidad$entradas, className = 'dt-center'))
        ),
        selection = "none")
      })
    })
  }
)
