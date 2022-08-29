library(Matrix)
library(softImpute)
library(DT)
# fields <- c("nombre", "peli 1", "peli dos", "peli3", "peli1_check", "peli2_check", "peli3_check")
fields2 = c("nombre", "Toy Story", "Kung Fu Panda", "Encanto", "Sonic", "Ralph el Demoledor", "Minions", "Coco", "Intensamente",
            "Cars", "Buscando a Nemo", "Avengers", "Hotel Transilvania", "Frozen", "Red", "Luca", "Eternals",
            "Los Increibles", "Unidos", "El Rey León", "Grandes Heroes", "Cruella", "Spider Man", "Soul",
            "Toy_Story_check", "Kung_Fu_Panda_check", "Encanto_check", "Sonic_check", "Ralph_el_Demoledor_check", "Minions_check", "Coco_check", "Intensamente_check",
            "Cars_check", "Buscando_a_Nemo_check", "Avengers_check", "Hotel_Transilvania_check", "Frozen_check", "Red_check", "Luca_check", "Eternals_check",
            "Los_Increibles_check", "Unidos_check", "El_Rey_León_check", "Grandes_Heroes_check", "Cruella_check", "Spider_Man_check", "Soul_check")
# Cargar archivos con los scripts
source('ScriptsDatos.R')
source('AlgoritmoNetflix.R')
source('ScriptsVisuales.R')

# Para borrar celdas específicas de la tabla
submatriz = loadData()
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
    helpText("Complete los campos con números del 1 al 10 y no escriba nada si no vió la película."),  
    textInput(
        inputId = "nombre",
        label = 'Nombre',
        value = '',
        width = '100px'
      ),
      div(numericInput(#1
        inputId = "Toy Story", 
        label = "1- Toy Story",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
       checkboxInput("Toy_Story_check", "Pintar Toy Story", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#2
        inputId = "Kung Fu Panda", 
        label = "2- Kung Fu Panda",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
       checkboxInput("Kung_Fu_Panda_check", "Pintar Kung Fu Panda", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#3
        inputId = "Encanto", 
        label = "3- Encanto",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
       checkboxInput("Encanto_check", "Pintar Encanto", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#4
        inputId = "Sonic", 
        label = "4- Sonic",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
       checkboxInput("Sonic_check", "Pintar Sonic", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#5
        inputId = "Ralph el Demoledor", 
        label = "5- Ralph el Demoledor",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Ralph_el_Demoledor_check", "Pintar Ralph el Demoledor", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#6
        inputId = "Minions", 
        label = "6- Minions",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Minions_check", "Pintar Minions", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#7
        inputId = "Coco", 
        label = "7- Coco",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Coco_check", "Pintar Coco", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#8
        inputId = "Intensamente", 
        label = "8- Intensamente",
        value = NA,
        width = '110px',
        min = 1,
        max = 10),
      checkboxInput("Intensamente_check", "Pintar Intensamente", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#9
        inputId = "Cars", 
        label = "9- Cars",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Cars_check", "Pintar Cars", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#10
        inputId = "Buscando a Nemo", 
        label = "10- Buscando a Nemo",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Buscando_a_Nemo_check", "Pintar Buscando a Nemo", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#11
        inputId = "Avengers", 
        label = "11- Avengers",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Avengers_check", "Pintar Avengers", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#12
        inputId = "Hotel Transilvania", 
        label = "12- Hotel Transilvania",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Hotel_Transilvania_check", "Pintar Hotel Transilvania", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#13
        inputId = "Frozen", 
        label = "13- Frozen",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Frozen_check", "Pintar Frozen", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#14
        inputId = "Red", 
        label = "14- Red",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Red_check", "Pintar Red", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#15
        inputId = "Luca", 
        label = "15- Luca",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Luca_check", "Pintar Luca", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#16
        inputId = "Eternals", 
        label = "16- Eternals",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Eternals_check", "Pintar Eternals", FALSE), class = 'div_contenedor'),
  
      div(numericInput(#17
        inputId = "Los Increibles", 
        label = "17- Los Increibles",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Los_Increibles_check", "Pintar Los Increibles", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#18
        inputId = "Unidos", 
        label = "18- Unidos",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Unidos_check", "Pintar Unidos", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#19
        inputId = "El Rey León", 
        label = "19- El Rey León",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("El_Rey_León_check", "Pintar El Rey León", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#20
        inputId = "Grandes Heroes", 
        label = "20- Grandes Heroes",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Grandes_Heroes_check", "Pintar Grandes Heroes", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#21
        inputId = "Cruella", 
        label = "21- Cruella",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Cruella_check", "Pintar Cruella", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#22
        inputId = "Spider Man", 
        label = "22- Spider Man",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Spider_Man_check", "Pintar Spider Man", FALSE), class = 'div_contenedor'),
      
      div(numericInput(#23
        inputId = "Soul", 
        label = "23- Soul",
        value = NA,
        width = '100px',
        min = 1,
        max = 10),
      checkboxInput("Soul_check", "Pintar Soul", FALSE), class = 'div_contenedor'),
  
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
      saveData(formData(), colnames(submatriz))
      submatriz = loadData()
      cantidad$entradas = dim(submatriz)[2]
      loadData()
    })

    # TABLA INCOMPLETA
    output$responses <- renderDataTable({
      pintar = loadPainted()
      datatable(loadData(), options = list(
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
        pintar = areEmpty(allData)
        datatable(netflix(allData), options = list(
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
      allData = loadData()
      changeData(input$elegir_row_cambiar, input$elegir_col_cambiar, input$cambiar_num)
      output$responses = renderDataTable({
        datatable(allData, options = list(
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
