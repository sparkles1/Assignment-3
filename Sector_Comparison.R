#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)


A2010 <- read.csv("BP Apprehensions 2010.csv")
A2017 <- read.csv("BP Apprehensions 2017.csv")
compsect2010 <- apply(A2010[,-1], 1, sum)
compsect2017 <- apply(A2017[,-1], 1, sum)
compsect10.17 <- data.frame('Year 2010'=compsect2010, 'Year 2017'=compsect2017, row.names=A2010$Sector)

# Define UI for application that draws a barplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Bp Apprhensions 2010-2017 Sector Comparsion"),
  
  
  #Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("Sectorname",
                  "Select Sector to display:",
                  choice = rownames(compsect10.17))
                  
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
    
    barplot(as.matrix(compsect10.17[input$Sectorname,]),
            main = input$Sectorname,
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

