changeCellColorRed <- function(i, j){
  color <- '#ff6a5b'
  c(
    "function(row, data, num, index){",
    sprintf("  var i = [%s], j = [%s];", 
            toString(i-1), toString(j), toString('#ff6a5b')),
    "  var n = i.length;",
    "  for(let k=0; k < n; k++){",
    "    if(index == i[k]){",
    "      $('td:eq(' + j[k] + ')', row)",
    "        .css({'background-color': '#ff6a5b'});",
    "    }",
    "  }",
    "}"  
  )
}

changeCellColorGreen <- function(i, j){
  color <- '#74ff74'
  c(
    "function(row, data, num, index){",
    sprintf("  var i = [%s], j = [%s];", 
            toString(i-1), toString(j), toString('#74ff74')),
    "  var n = i.length;",
    "  for(let k=0; k < n; k++){",
    "    if(index == i[k]){",
    "      $('td:eq(' + j[k] + ')', row)",
    "        .css({'background-color': '#74ff74'});",
    "    }",
    "  }",
    "}"  
  )
}