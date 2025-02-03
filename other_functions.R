# function to calculate reduction
calcpecentage <- function(micro){
  v <- aggregate(time ~ expr, micro, mean)
  i <- 1
  j <- 2
  if(v$time[i] < v$time[j]){
    i <- 2
    j <- 1
  }
  reduction <- ((v$time[i] - v$time[j])*100)/v$time[i]
  reduction <- reduction |> round(digits = 2)
  return(glue("{reduction} % reduction using {v$expr[j]}"))
}

fbox_plot <- function(data, my_scale){
  data <- as.data.table(data)
  my_fig <- plot_ly(data, x = ~log(time), y = ~expr, type = "box", 
                 orientation = "h", color = ~expr) |> 
    layout(
      title = paste0("Microbenchmark Results ", calcpecentage(data)),
      xaxis = list(title = paste0("Time in ", my_scale)),
      yaxis = list(title = "Expression"),
      plot_bgcolor = 'black', 
      paper_bgcolor = 'black',
      font = list(color = 'white')
    )
  return(my_fig)
}