library(gammaSpec)

# Define a server for the Shiny app
function(input, output, session) {
  
  values <- reactiveValues(file = NULL, result = NULL)
  
  # check and read in file (DATA SET 1)
  observeEvent(input$file, {
    inFile<- input$file
    
    if(is.null(inFile)) 
      return(NULL) # if no file was uploaded return NULL
    
    values$file <- tryCatch({read_SPE(file = inFile$datapath, data.table = FALSE)},
                            error = function(e) { NULL } ) 
  })
  
  # Fill in the spot we created for a plot
  observeEvent(input$btn, {
    
    if (is.null(values$file))
      return(NULL)
    
    output$plot <- renderPlot({
      
      withProgress(message = "Calculation in Progress", detail = "This may take a while...",
                   value = 1, {
                     
                     values$result <- calc_DoseRate(isolate(values$file),
                                                    energy.min = isolate(input$energy[1]),
                                                    energy.max = isolate(input$energy[2]),
                                                    background.correction = isolate(input$bg),
                                                    cex = 1.05, 
                                                    verbose = FALSE)
                     
                   })
      
    })
    
    
    output$console <- renderUI({
      tags$p(
        tags$b("Estimated in-situe gamma dose rate:"),
        tags$br(),
        tags$code(round(values$result$summary[1], 3), 
                  HTML("&plusmn;"), 
                  round(values$result$summary[2], 3),
                  HTML("Gy ka<sup>-1</sup>"))
      )
    }) 
    
  })
}