library(shinydashboard)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
    dashboardHeader(title = "Online shoppers dashboard"),
    dashboardSidebar(collapsed = TRUE),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(plotOutput("plot1", height = 350)),
            
            box(
                title = "Controls",
                selectInput("website", "Website:", 
                            choices=c("Administrative", "Informational", "ProductRelated"))
            )
        ),
        fluidRow(
            box(plotOutput("plot2")),
            box(
            selectInput("month", "Month:", 
                        choices=c("Feb", "Mar", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
            )
        )
        
    )
)

load_data <- function() {
    df <- read.csv("../online_shoppers_intention.csv")
    df$OperatingSystems <- factor(df$OperatingSystems)
    df$Browser <- factor(df$Browser)
    df$Region <- factor(df$Region)
    df$TrafficType <- factor(df$TrafficType)
    df$VisitorType <- factor(df$VisitorType)
    df$Month <- factor(df$Month, levels = c("Feb", "Mar", "May", "June", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    df
}

server <- function(input, output) {
    df <- load_data()
    
    output$plot1 <- renderPlot({
        df %>%
            filter_(paste(input$website,">",0)) %>%
            ggplot(mapping = aes_string(x = input$website, fill = "Revenue")) + 
            geom_density(alpha=.5) +
            ggtitle(paste("Density of revenue on", input$website, "pages"))
    })
    
    output$plot2 <- renderPlot({
        df %>%
            filter(Month == input$month) %>%
            group_by(Revenue) %>%
            summarise(n = n()) %>%
            mutate(freq = n / sum(n)) %>%
            ggplot(aes(x="", y=freq, fill=Revenue)) +
            geom_bar(width = 1, stat = "identity") +
            coord_polar("y", start=0) +
            ggtitle(paste("Revenue distribution in", input$month))
      
    })
}

shinyApp(ui, server)