input <- 'mask' %>%
      str_split("#") %>%
      .[[1]]
    data <- getRedditTrend(connr, keywords = input, period=c('2020-01-22', '2020-09-27')) %>% 
      mutate(date=ymd(date))%>%
      left_join(spread_data,'date')
    fig <- plot_ly(data,
      x = ~date, y = ~positiveIncrease, color = I("black"),
      name = "positiveIncrease", type = "scatter", mode = "lines+markers"
    )
    n <- nrow(data)
    color <- data$sentiment %>%
      {
        (.[1:(n - 1)] + .[2:n]) / 2
      } %>%
      {
        ifelse(. > 0, "green", "red")
      }

    fig <- fig %>%
      add_trace(
        x = data$date[1:2], y = data$number[1:2], color = I("blue"),
        name = input[1], mode = "lines+markers", yaxis = "y2",
        visible = "legendonly"
      )
    for (i in 1:(n - 1)) {
      fig <- fig %>%
        add_trace(
          x = data$date[i:(i + 1)], y = data$number[i:(i + 1)], color = I(color[i]),
          showlegend = F, mode = "lines+markers", yaxis = "y2"
        )
    }
    fig %>% layout(title = "Twitter Sentiment trends", yaxis2 = ay, xaxis = list(title = "x"))
