#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

A2010 <- read.csv("BP Apprehensions 2010.csv")
A2017 <- read.csv("BP Apprehensions 2017.csv")

compmnth2010 <- apply(A2010[,-1], 2, sum)
compmnth2017 <- apply(A2017[,-1], 2, sum)
compmnth10.17 <- data.frame('Year 2010'=compmnth2010, 'Year 2017'=compmnth2017, row.names=A2010$Month)
rownames(compmnth10.17) <- c("October","November","December","January","February","March","April","May","June","July","August","September")

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bp Apprhensions 2010-2017 Monthly Comparsion"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("Year",
                    "Select Year to display:",
                    choice = rownames(compmnth10.17))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("barplot")
      )
   )
)

# Define server logic required to draw a barplot
server <- function(input, output) {
  
  output$barplot <- renderPlot({
    
    barplot(as.matrix(compmnth10.17[input$Year,]),
            main = input$Year,
            beside = TRUE, 
            ylab = "Number of Apprehensions",
            xlab = "Year",
            space = c(0.1, 0.1),
            col = c("Green","yellow")
    )
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

