library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

tech_layoffs <- read_csv("tech_layoffs.csv")

tech_layoffs <- tech_layoffs %>%
  mutate(
    reported_date = as.Date(reported_date, format = "%m/%d/%Y"),
    total_layoffs = as.numeric(ifelse(total_layoffs == "Unclear", NA, total_layoffs)),
    year = year(reported_date)
  ) %>%
  filter(!is.na(total_layoffs))

ui <- fluidPage(
  titlePanel("Tech Industry Layoff Trends"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("location", "Select Location:",
                  choices = c("All", unique(tech_layoffs$headquarter_location)),
                  selected = "All"),
      sliderInput("date_range", "Select Date Range:",
                  min = min(tech_layoffs$reported_date, na.rm = TRUE),
                  max = max(tech_layoffs$reported_date, na.rm = TRUE),
                  value = c(min(tech_layoffs$reported_date, na.rm = TRUE), 
                            max(tech_layoffs$reported_date, na.rm = TRUE)),
                  timeFormat = "%Y-%m-%d"),
      radioButtons("selected_year", "Select Year:",
                   choices = c("All", "2022", "2023"),
                   selected = "All",
                   inline = TRUE),
      sliderInput("max_layoffs", "Max Layoffs to Display:", 
                  min = 100, max = max(tech_layoffs$total_layoffs, na.rm = TRUE),
                  value = min(5000, max(tech_layoffs$total_layoffs, na.rm = TRUE)))
    ),
    
    mainPanel(
      plotOutput("trend_plot", brush = brushOpts(id = "plot_brush", fill = "#9ecae1", stroke = "#3182bd")),
      dataTableOutput("brushed_data")
    )
  )
)

server <- function(input, output) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    data <- tech_layoffs
    
    if (input$location != "All") {
      data <- data[data$headquarter_location == input$location, ]
    }
    
    data <- data[data$reported_date >= input$date_range[1] & 
                   data$reported_date <= input$date_range[2], ]
    
    if (input$selected_year != "All") {
      data <- data[data$year == as.numeric(input$selected_year), ]
    }
    
    data
  })
  
  # Create the trend plot
  output$trend_plot <- renderPlot({
    plot <- ggplot(filtered_data(), aes(x = reported_date, y = total_layoffs, color = industry)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(title = paste("Tech Industry Layoff Trends", 
                         if(input$selected_year != "All") input$selected_year else ""),
           x = "Date",
           y = "Total Layoffs",
           color = "Industry") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      coord_cartesian(ylim = c(0, input$max_layoffs))
    
    if (input$selected_year == "All") {
      plot <- plot + facet_wrap(~ year, scales = "free_x")
    }
    
    plot
  })
  
  # Create the data table for brushed data
  output$brushed_data <- renderDataTable({
    brushed_points <- brushedPoints(filtered_data(), input$plot_brush,
                                    xvar = "reported_date", yvar = "total_layoffs")
    
    if (nrow(brushed_points) == 0) {
      return(NULL)
    }
    
    brushed_points %>%
      select(company, industry, reported_date, total_layoffs, headquarter_location) %>%
      arrange(company, industry)
  })
}

shinyApp(ui, server)
