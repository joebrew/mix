
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source('helpers.R')

shinyServer(function(input, output) {
  
  df <- reactive({
    x <- sub_mix(input$region, input$range)
    x$MFI.name <- gsub("[^a-zA-Z0-9]","",x$MFI.name )
    x
  })

    output$plot1 <- renderPlot({
      df <- df()
      outcome(var = input$var,
              data = df)
      
    })
  
  output$table1 <- renderTable({
    var_formatted <- gsub(' ', '.',input$var)
    x <- df()[,c('MFI.name', 'Fiscal.Year', var_formatted)]
    names(x) <- gsub('[.]', ' ', names(x))
    x
  })
  
  output$text1 <- renderText({
    paste0(length(unique(df()$MFI.name)), ' MFIs selected')
  })
  
  output$text2 <- renderPrint({
    df <- df()
    # Put in R space format
    var_formatted <- gsub(' ', '.',input$var)
    # Put in dataframe
    val <- df[,var_formatted]
    summary(val)
    
  })
})
