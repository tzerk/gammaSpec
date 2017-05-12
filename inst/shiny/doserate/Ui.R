library(shiny)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel(NULL),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      tags$div(align = "center",
               tags$p(tags$b("Estimate the in-situ gamma dose rate"))
               ),
      hr(),
      fileInput(inputId = "file",
                       multiple = FALSE, 
                       label = "Upload SPE file", 
                       placeholder = "Upload a file..."),
      hr(),
      sliderInput(inputId = "energy", label = "Integrated energy range (keV)", 
                  min = 0, max = 5000, value = c(500, 2000), step = 10),
      checkboxInput(inputId = "bg", label = "Background correction", value = TRUE),
      hr(),
      actionButton(inputId = "btn", label = "Calculate", icon = icon("rocket")),
      br(),
      br(),
      htmlOutput("console")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",
                 plotOutput("plot", height = "1000px")
                 ),
        tabPanel("Info",
                 tags$br(),
                 tags$p(HTML("Please see the Vignettes of the <b>R</b> package <code>gammaSpec</code>"),
                        tags$br(), tags$br(),
                        tags$a(href = "https://github.com/tzerk/gammaSpec", 
                               "Package source code on GitHub", target = "_blank"),
                        tags$br(),
                        tags$a(href = "https://github.com/tzerk/gammaSpec/tree/master/inst/shiny/doserate", 
                               "Shiny app source code on GitHub", target = "_blank"),
                        tags$hr(),
                        tags$p(tags$b("Author:"), "Christoph Burow")
                        )
                 )
      )
    )
    
  )
)