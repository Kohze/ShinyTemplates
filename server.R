library(jsonlite)
library(ggplot2)

function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot2 <- renderPlot({
    plot(iris$Sepal.Length,iris$Sepal.Width)
  })
  
  output$plot3 <- renderPlot({
    p <- ggplot(iris, aes(Sepal.Length, Sepal.Width))
    p + geom_point(aes(colour = factor(Petal.Width)))
  })
  
  output$plot4 <- renderPlot({
    p <- ggplot(mtcars, aes(wt*input$controller, mpg*(1/input$controller)))
    p + geom_point(aes(colour = factor(cyl)))
  })
  
  output$data1 <- reactive({
    RJSONIO::toJSON(iris, 
                    byrow=T, colNames=T)
  })
    
  observe({
    session$sendCustomMessage(type = 'testmessage',
                              message = list(a = 1, b = 'text',
                                             controller = input$controller))
  })
  
}