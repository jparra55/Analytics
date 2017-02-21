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
library(dplyr)
library(lubridate)

# Ejecutar script
source("C://Users//Jinvestigador02//Documents//RIPS//CodigoHospitalizacion.R")

## Generar los tiempos
hospitalizacion$Tiempo <- difftime(hospitalizacion$FECHA.EGRESO.COMP
                                   ,hospitalizacion$FECHA.INGRESO.COMP, 
                                   units = "hours")

## Generar variables de agrupacion, por mes y por dia de la semana
hospitalizacion$Mes <- months(hospitalizacion$FECHA.EGRESO.COMP)
hospitalizacion$diaSemana <- weekdays(hospitalizacion$FECHA.EGRESO.COMP)

# Definimos un app para graficar el indicador Promedio de permanencia de un paciente con histograma
ui <- fluidPage(
   
   # Application title
   titlePanel("Promedio de permanencia de un paciente"),
   
   # caracteristicas y discriminantes de la grafica
   sidebarLayout(
      sidebarPanel(
         checkboxGroupInput("checkbox",
                     "Seleccione mes:",
                     choices = unique(hospitalizacion$Mes),
                     selected = unique(hospitalizacion$Mes))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         
         tabsetPanel(
           tabPanel("Plot", plotOutput("histograma")),
           tabPanel("Summary", verbatimTextOutput("summary")),
           tabPanel("Table", tableOutput("table"))
         )
         
      
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$histograma <- renderPlot({
      
     # Filtrar Datos y agrupar segun la eleccion
     hosp <- hospitalizacion %>% filter(Mes %in% input$checkbox) %>% select(Tiempo, Mes) %>%
       group_by(Mes) %>% summarize(Tiempo.Promedio = mean(Tiempo))
    
      # draw the histogram with the specified number of bins
      ggplot(data = hosp, aes(x = Mes, y = Tiempo.Promedio)) + 
        geom_col(fill = "dark green", color = "steel blue") + geom_smooth()
   })
   
   output$summary <- renderPrint({
     # Filtrar Datos y agrupar segun la eleccion
     hosp <- hospitalizacion %>% filter(Mes %in% input$checkbox) %>% select(Tiempo, Mes) %>%
       group_by(Mes) %>% summarize(Tiempo.Promedio = mean(Tiempo))
     
     summary(as.numeric(hosp$Tiempo.Promedio))
   })
   
   output$table <- renderTable({
     # Filtrar Datos y agrupar segun la eleccion
     hosp <- hospitalizacion %>% filter(Mes %in% input$checkbox) %>% select(Tiempo, Mes) %>%
       group_by(Mes) %>% summarize(Tiempo.Promedio = mean(Tiempo))
     
     hosp
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

