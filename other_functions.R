# function to calculate reduction
calcpecentage <- function(micro){
  v <- aggregate(time ~ expr, micro, mean) |>
    arrange(time)

  i <- 1
  j <- nrow(v)

  reduction <- ((v$time[j] - v$time[i])*100)/v$time[j]
  reduction <- reduction |> round(digits = 2)

  return(glue("{reduction}% reduction using {v$expr[i]} compared to {v$expr[j]}"))
}

fbox_plot <- function(data, my_scale){
  data <- as.data.table(data)
  my_fig <- plot_ly(data, x = ~log(time), y = ~expr, type = "box",
                 orientation = "h", color = ~expr) |>
    layout(
      title = calcpecentage(data),
      xaxis = list(title = paste0("Time in ", my_scale)),
      yaxis = list(title = "Expression"),
      plot_bgcolor = 'black',
      paper_bgcolor = 'black',
      font = list(color = 'white')
    )
  return(my_fig)
}
