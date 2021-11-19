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
library(plotly)

raquetas <- readRDS("raquetas.rds")
coldisp <- colnames(raquetas)[!(colnames(raquetas) %in% c("Name","marca","imagen","current","Composition"))]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Racquet Selector"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2,
            h4("Plot"),
            selectInput(inputId = "xaxis",label = "X Axis",choices = coldisp,selected = "StrungWeight",multiple = FALSE),
            selectInput(inputId = "yaxis",label = "Y Axis",choices = coldisp,selected = "HeadSize",multiple = FALSE),
            selectInput(inputId = "facet",label = "Facet",choices = c("ninguna",coldisp),selected = "PowerLevel",multiple = FALSE),
            selectInput(inputId = "color",label = "Color",choices = coldisp,selected = "Stiffness",multiple = FALSE),
            checkboxInput(inputId = "current",label = "Show only current",value = FALSE),
            h4("Highlight racquet"),
            selectInput(inputId = "marca",label = "Manufacturer",choices = c("ninguna",unique(raquetas$Marca)),selected = "ninguna"),
            uiOutput("modelo")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$modelo <- renderUI({
        modelos <- raquetas[raquetas$Marca==input$marca,]$Name
        selectizeInput(inputId = "modelo",label = "Model",choices = modelos)
    })

    output$distPlot <- renderPlotly({
        raquetas_graf <- raquetas
        if(input$current){
            raquetas_graf <- raquetas %>% dplyr::filter(current==TRUE)
        }
        if(input$facet!="ninguna"){
            raquetas_graf <- raquetas_graf[raquetas_graf[[input$facet]]!="" & !is.na(raquetas_graf[[input$facet]]),]
        }
        raquetas_graf <- raquetas_graf %>% dplyr::filter(Stiffness>30)
        grafico1 <- ggplot() + geom_point(data = raquetas_graf,aes_string(x=input$xaxis,y=input$yaxis,color=input$color,text="Name"))
        if(input$facet!="ninguna"){
            if(input$facet=="PowerLevel"){
                grafico1 <- grafico1 + facet_wrap(as.formula(paste("~", input$facet)),nrow = 1)
            } else {
                grafico1 <- grafico1 + facet_wrap(as.formula(paste("~", input$facet)))
            }
        }
        if(is.numeric(raquetas[[input$color]])){
            grafico1 <- grafico1 + scale_color_gradient(low = "green",high = "red")
        }
        if(input$modelo!=""){
          grafico1 <- grafico1 + geom_point(data=raquetas[raquetas$Name==input$modelo,],aes_string(x=input$xaxis,y=input$yaxis),color="blue",shape=3)
        }
        grafico1 <- grafico1 + theme_light()
        ggplotly(grafico1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
