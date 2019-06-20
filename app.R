#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny,
               pracma,
               tidyverse,
               readr,
               plotly,
               TSEntropies,
               magrittr)

source('sampense.R')
source('makerun.R')


source('sampense.R')
source('makerun.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Sample Entropy Diagnostics"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("file", "File:",
                        c("Pink" = "pinknoises.txt",
                          "White" = "whitenoises.txt",
                          "Lorenz" = "lorenz.txt",
                          "ClassX" = "classX.txt",
                          "ClassY" = "classY.txt"))
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("sampEnPlot"),
            plotlyOutput("sampEnSEPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$sampEnPlot <- renderPlotly({
        fileName <- input$file
        plotData <- readr::read_csv(fileName, col_names = TRUE)
        plotData$m %<>% as_factor()
        
        p <- ggplot(data = plotData,mapping = aes(x = r,y = medE, group = m, color=m)) +
            geom_line(aes()) +
            geom_point(aes()) +
            xlab("radius") + ylab("Median SampEn \n")
        
        ggplotly(p)
    })
    
    output$sampEnSEPlot <- renderPlotly({
        fileName <- input$file
        plotData <- readr::read_csv(fileName, col_names = TRUE)
        plotData$m %<>% as_factor()
        
        p <- ggplot(data = plotData,mapping = aes(x = r,y = medQ, group = m, color=m)) +
            geom_line(aes()) +
            geom_point(aes()) +
            xlab("radius") + ylab("Median Approximate std. error of SampEn estimate \n")
        
        ggplotly(p)
        
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
