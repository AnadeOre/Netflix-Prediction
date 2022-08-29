# Funciones para cambiar el color a las celdas

changeCellColorRed <- function(i, j){
  color <- '#ef7777'
  c(
    "function(row, data, num, index){",
    sprintf("var i = [%s], j = [%s];", 
            toString(i-1), toString(j), toString('#ef7777')),
    "  var n = i.length;",
    "  for(let k=0; k < n; k++){",
    "    if(index == i[k]){",
    "      $('td:eq(' + j[k] + ')', row)",
    "        .css({'background-color': '#ef7777'});",
    "    }",
    "  }",
    "}"  
  )
}

changeCellColorGreen <- function(i, j){
  color <- '#9ff081'
  c(
    "function(row, data, num, index){",
    sprintf("var i = [%s], j = [%s];", 
            toString(i-1), toString(j), toString('#9ff081')),
    "  var n = i.length;",
    "  for(let k=0; k < n; k++){",
    "    if(index == i[k]){",
    "      $('td:eq(' + j[k] + ')', row)",
    "        .css({'background-color': '#9ff081'});",
    "    }",
    "  }",
    "}"  
  )
}