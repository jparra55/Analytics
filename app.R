#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Ejecutar script
source("C://Users//Jinvestigador02//Documents//RIPS//CodigoHospitalizacion.R")
## Generar los tiempos
hospitalizacion$Tiempo <- difftime(hospitalizacion$FECHA.EGRESO.COMP
                                   ,hospitalizacion$FECHA.INGRESO.COMP, 
                                   units = "hours")

# Definimos un app para graficar el indicador Promedio de permanencia de un paciente con histograma
ui <- fluidPage(
   
   # Application title
   titlePanel("Promedio de permanencia de un paciente"),
   
   # caracteristicas y discriminantes de la grafica
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 1000,
                     value = 600)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("histograma")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$histograma <- renderPlot({
      
     # generar las barras input$bins from ui.R
     x <- hospitalizacion$Tiempo
     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
      # draw the histogram with the specified number of bins
      ggplot(data = hospitalizacion, aes(x = Tiempo)) + geom_histogram(breaks = bins, 
                                                                       fill = "green", color = "steel blue") + 
        coord_cartesian(xlim = c(0,1000))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

