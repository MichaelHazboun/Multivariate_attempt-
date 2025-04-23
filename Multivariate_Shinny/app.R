#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse) #loaded tidyverse because I'll most probably be using it
library(rsconnect)
library(robustbase)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Multivariate Presentation"),
    
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("outliers",
                        "Number of outliers:",
                        min = 0,
                        max = 15,
                        value = 0),
            sliderInput("outliers_magnitude",
                        "How big of an outlier:",
                        min = 0,
                        max = 50,
                        value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("mean"),
          textOutput("median"),
          textOutput("rmean"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


      output$distPlot <- renderPlot({
        x <- c(1,1,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,7,7)  
        thing    <- data.frame(x)
        thing$x[(length(thing$x)-input$outliers):length(thing$x)] <- thing$x[(length(thing$x)-input$outliers):length(thing$x)] + input$outliers_magnitude
        
      # thing$x[(length(thing$x)-4):length(thing$x)] <- thing$x[(length(thing$x)-4):length(thing$x)] + 10
        ggplot(thing, aes(x = x)) +
          geom_dotplot(binwidth = 1,dotsize = 1) +
          labs(x = "Value", y = "Count") +
          theme_minimal()
    })
    output$mean <- renderText({
      x <- c(1,1,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,7,7)  
      thing    <- data.frame(x)
      thing$x[(length(thing$x)-input$outliers):length(thing$x)] <- thing$x[(length(thing$x)-input$outliers):length(thing$x)] + input$outliers_magnitude
      r_mean<- huberM(thing$x)
      paste("The mean is: ",mean(thing$x))
    })
    output$rmean <- renderText({
      x <- c(1,1,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,7,7)  
      thing    <- data.frame(x)
      thing$x[(length(thing$x)-input$outliers):length(thing$x)] <- thing$x[(length(thing$x)-input$outliers):length(thing$x)] + input$outliers_magnitude
      r_mean<- huberM(thing$x)
      paste("A robust mean is: ",r_mean$mu)
    })
    output$median <- renderText({
      x <- c(1,1,2,2,2,3,3,3,4,4,4,4,5,5,5,6,6,6,7,7)  
      thing    <- data.frame(x)
      thing$x[(length(thing$x)-input$outliers):length(thing$x)] <- thing$x[(length(thing$x)-input$outliers):length(thing$x)] + input$outliers_magnitude
      Median_x<- median(thing$x)
      paste("The median is: ",Median_x)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
