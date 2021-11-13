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

raquetas <- readRDS("../raquetas.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Racquet Selector"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "xaxis",label = "X Axis",choices = colnames(raquetas),selected = "StrungWeight",multiple = FALSE),
            selectInput(inputId = "yaxis",label = "Y Axis",choices = colnames(raquetas),selected = "HeadSize",multiple = FALSE),
            selectInput(inputId = "facet",label = "Facet",choices = colnames(raquetas),selected = "PowerLevel",multiple = FALSE),
            selectInput(inputId = "color",label = "Color",choices = colnames(raquetas),selected = "Stiffness",multiple = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        raquetas_graf <- raquetas %>% dplyr::filter(Stiffness>30)
        grafico1 <- ggplot() + geom_point(data = raquetas_graf,aes_string(x=input$xaxis,y=input$yaxis,color=input$color,text="Name"))  + facet_wrap(as.formula(paste("~", input$facet)),nrow = 1) + scale_color_gradient(low = "green",high = "red")
        #grafico1 <- grafico1 + geom_point(data=raquetas[raquetas$Name=="Wilson KBlade Team",],aes(x=StrungWeight,y=HeadSize),color="blue",shape=3)
        ggplotly(grafico1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
